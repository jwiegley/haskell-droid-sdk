use serde_json::{value::RawValue, Value};
use std::collections::BTreeMap;
use std::io::{self, BufRead, Read, Write};

const MAX_REQUEST_BYTES: u64 = 10 * 1024 * 1024;

// Preserve object kinds instead of interpreting user keys as serde markers.
fn parse_json(input: &str) -> Result<Value, serde_json::Error> {
    fn convert(raw: &RawValue) -> Result<Value, serde_json::Error> {
        match raw.get().trim_start().as_bytes().first() {
            Some(b'{') => {
                let fields: BTreeMap<String, &RawValue> = serde_json::from_str(raw.get())?;
                fields
                    .into_iter()
                    .map(|(key, value)| Ok((key, convert(value)?)))
                    .collect::<Result<_, _>>()
                    .map(Value::Object)
            }
            Some(b'[') => {
                let values: Vec<&RawValue> = serde_json::from_str(raw.get())?;
                values
                    .into_iter()
                    .map(convert)
                    .collect::<Result<_, _>>()
                    .map(Value::Array)
            }
            _ => serde_json::from_str(raw.get()),
        }
    }
    convert(serde_json::from_str::<&RawValue>(input)?)
}

fn validate(request: &Value) -> &'static str {
    if request.get("protocol").and_then(Value::as_u64) != Some(1) {
        return "invalid_request";
    }
    let Some(schema) = request.get("schema") else {
        return "invalid_request";
    };
    let validator = match jsonschema::options()
        .offline()
        .should_validate_formats(false)
        // Disable every built-in content check in the pinned backend. The SDK
        // treats these as annotations without changing the schema's dialect.
        .without_content_encoding_support("base16")
        .without_content_encoding_support("base32")
        .without_content_encoding_support("base32hex")
        .without_content_encoding_support("base64")
        .without_content_encoding_support("base64url")
        .without_content_media_type_support("application/json")
        .with_pattern_options(jsonschema::PatternOptions::fancy_regex().backtrack_limit(usize::MAX))
        .build(schema)
    {
        Ok(validator) => validator,
        Err(_) => return "invalid_schema",
    };
    match request.get("value") {
        None => "valid",
        Some(value) if validator.is_valid(value) => "valid",
        Some(_) => "invalid",
    }
}

fn run() -> &'static str {
    let mut input = Vec::new();
    if io::stdin()
        .lock()
        .take(MAX_REQUEST_BYTES + 1)
        .read_until(b'\n', &mut input)
        .is_err()
    {
        return "invalid_request";
    }
    if input.len() as u64 > MAX_REQUEST_BYTES {
        return "request_too_large";
    }
    if input.last() != Some(&b'\n') {
        return "invalid_request";
    }
    let request = match std::str::from_utf8(&input)
        .ok()
        .and_then(|text| parse_json(text).ok())
    {
        Some(request) => request,
        None => return "invalid_request",
    };
    validate(&request)
}

fn main() {
    if std::env::args().nth(1).as_deref() == Some("--licenses") {
        if io::stdout()
            .write_all(include_str!("../THIRD_PARTY_LICENSES.txt").as_bytes())
            .is_err()
        {
            std::process::exit(1);
        }
        return;
    }
    // A caught backend panic must not turn into a false (or negated true) match.
    std::panic::set_hook(Box::new(|_| std::process::exit(2)));
    let status = run();
    let response = serde_json::json!({"protocol": 1, "status": status});
    let mut output = io::stdout().lock();
    if writeln!(output, "{response}")
        .and_then(|()| output.flush())
        .is_err()
    {
        std::process::exit(1);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn exact_numbers_and_json_kinds() {
        for (value, expected) in [
            ("0.100000000000000000001", "valid"),
            ("0.1", "invalid"),
            (
                r#"{"$serde_json::private::Number":"0.100000000000000000001"}"#,
                "invalid",
            ),
            (
                r#"{"$serde_json::private::RawValue":"0.100000000000000000001"}"#,
                "invalid",
            ),
        ] {
            let request = parse_json(&format!(
                r#"{{"protocol":1,"schema":{{"const":0.100000000000000000001}},"value":{value}}}"#
            ))
            .unwrap();
            assert_eq!(validate(&request), expected);
        }
    }

    #[test]
    fn compilation_and_invalid_schemas_are_distinct_from_invalid_values() {
        assert_eq!(
            validate(&serde_json::json!({"protocol":1,"schema":{"type":"integer"}})),
            "valid"
        );
        assert_eq!(
            validate(&serde_json::json!({"protocol":1,"schema":{"type":"integer"},"value":null})),
            "invalid"
        );
        assert_eq!(
            validate(&serde_json::json!({"protocol":1,"schema":{"type":"not-a-type"}})),
            "invalid_schema"
        );
        assert_eq!(
            validate(&serde_json::json!({"schema":true,"value":1})),
            "invalid_request"
        );
    }

    #[test]
    fn content_keywords_are_annotations_across_drafts() {
        for draft in [
            None,
            Some("http://json-schema.org/draft-04/schema#"),
            Some("http://json-schema.org/draft-06/schema#"),
            Some("http://json-schema.org/draft-07/schema#"),
            Some("https://json-schema.org/draft/2019-09/schema"),
            Some("https://json-schema.org/draft/2020-12/schema"),
        ] {
            for annotation in [
                serde_json::json!({"contentEncoding":"base16"}),
                serde_json::json!({"contentEncoding":"base32"}),
                serde_json::json!({"contentEncoding":"base32hex"}),
                serde_json::json!({"contentEncoding":"base64"}),
                serde_json::json!({"contentEncoding":"base64url"}),
                serde_json::json!({"contentMediaType":"application/json"}),
                serde_json::json!({"contentEncoding":"base64","contentMediaType":"application/json"}),
                serde_json::json!({"contentMediaType":"application/json","contentSchema":false}),
                serde_json::json!({"contentEncoding":"fixture-unknown"}),
                serde_json::json!({"contentMediaType":"application/fixture-unknown"}),
            ] {
                let mut schema = annotation.clone();
                schema["type"] = serde_json::json!("string");
                schema["minLength"] = serde_json::json!(2);
                let mut negated = serde_json::json!({"not":annotation});
                if let Some(draft) = draft {
                    schema["$schema"] = serde_json::json!(draft);
                    negated["$schema"] = serde_json::json!(draft);
                }
                for (value, expected) in [
                    (serde_json::json!("??"), "valid"),
                    (serde_json::json!("YQ=="), "valid"),
                    (serde_json::json!(1), "invalid"),
                    (serde_json::json!("?"), "invalid"),
                ] {
                    let request = serde_json::json!({"protocol":1,"schema":schema,"value":value});
                    assert_eq!(
                        validate(&request),
                        expected,
                        "draft={draft:?}, schema={schema}"
                    );
                }
                assert_eq!(
                    validate(&serde_json::json!({"protocol":1,"schema":negated,"value":"??"})),
                    "invalid",
                    "draft={draft:?}, negated={negated}"
                );
            }
        }
    }

    #[test]
    fn declared_dialects_and_content_keyword_shapes_are_preserved() {
        for (draft, expected) in [
            ("http://json-schema.org/draft-07/schema#", "valid"),
            ("https://json-schema.org/draft/2020-12/schema", "invalid"),
        ] {
            let schema = serde_json::json!({
                "$schema":draft, "$ref":"#/definitions/integer",
                "definitions":{"integer":{"type":"integer"}}, "minimum":10
            });
            assert_eq!(
                validate(&serde_json::json!({"protocol":1,"schema":schema,"value":1})),
                expected
            );
        }
        let schema = serde_json::json!({
            "$schema":"http://json-schema.org/draft-04/schema#",
            "type":"number", "minimum":0, "exclusiveMinimum":true
        });
        for (value, expected) in [(0, "invalid"), (1, "valid")] {
            assert_eq!(
                validate(&serde_json::json!({"protocol":1,"schema":schema,"value":value})),
                expected
            );
        }
        for draft in [
            "http://json-schema.org/draft-06/schema#",
            "http://json-schema.org/draft-07/schema#",
        ] {
            for keyword in ["contentEncoding", "contentMediaType"] {
                let mut schema = serde_json::json!({"$schema":draft});
                schema[keyword] = serde_json::json!(false);
                assert_eq!(
                    validate(&serde_json::json!({"protocol":1,"schema":schema})),
                    "invalid_schema"
                );
            }
        }
    }

    #[test]
    fn references_never_retrieve_external_resources() {
        for reference in ["https://offline.invalid/schema", "file:///OFFLINE_ONLY"] {
            assert_eq!(
                validate(&serde_json::json!({"protocol":1,"schema":{"$ref":reference}})),
                "invalid_schema"
            );
        }
    }
}
