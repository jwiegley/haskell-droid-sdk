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
    fn references_never_retrieve_external_resources() {
        for reference in ["https://offline.invalid/schema", "file:///OFFLINE_ONLY"] {
            assert_eq!(
                validate(&serde_json::json!({"protocol":1,"schema":{"$ref":reference}})),
                "invalid_schema"
            );
        }
    }
}
