#!/usr/bin/env python3
"""Verify the immutable reference schemas and inventory their public definitions."""

import argparse
import hashlib
import json
from pathlib import Path

SCHEMA_DIR = Path(__file__).resolve().parents[1] / "schema"


def nodes(value, pointer=""):
    if isinstance(value, dict):
        yield pointer, value
        for key, child in value.items():
            escaped = key.replace("~", "~0").replace("/", "~1")
            yield from nodes(child, f"{pointer}/{escaped}")
    elif isinstance(value, list):
        for index, child in enumerate(value):
            yield from nodes(child, f"{pointer}/{index}")


def resolve(reference, owner, documents):
    filename, separator, pointer = reference.partition("#")
    if not separator or (pointer and not pointer.startswith("/")):
        raise ValueError(f"Unsupported schema reference: {reference}")
    value = documents[filename or owner]
    for part in pointer.split("/")[1:]:
        key = part.replace("~1", "/").replace("~0", "~")
        value = value[int(key)] if isinstance(value, list) else value[key]
    return value


def declared_method(schema):
    method = schema.get("properties", {}).get("method", {}).get("const")
    if method is not None:
        return method
    for member in schema.get("allOf", []):
        method = declared_method(member)
        if method is not None:
            return method
    return None


def inventory(documents):
    index = documents["protocol.schema.json"]["definitions"]
    version = documents["protocol.schema.json"]["x-factory-protocol-version"]
    owned = {
        f"{filename}#/definitions/{name}"
        for filename, document in documents.items()
        if filename != "protocol.schema.json"
        for name in document["definitions"]
    }
    indexed = [entry["$ref"] for entry in index.values()]
    if len(indexed) != len(owned) or set(indexed) != owned:
        raise ValueError("Protocol index does not cover each owned definition exactly once")

    reference_count = 0
    for filename, document in documents.items():
        if document["x-factory-protocol-version"] != version:
            raise ValueError(f"Protocol version mismatch: {filename}")
        for pointer, node in nodes(document):
            if "$ref" in node:
                try:
                    resolve(node["$ref"], filename, documents)
                except (KeyError, IndexError, TypeError, ValueError) as error:
                    raise ValueError(f"Unresolved reference at {filename}#{pointer}") from error
                reference_count += 1
            for annotation in ("x-factory-response", "x-factory-result"):
                if annotation in node and node[annotation] not in index:
                    raise ValueError(f"Unknown {annotation} at {filename}#{pointer}")

    definitions = []
    for name, reference in sorted(index.items()):
        schema = resolve(reference["$ref"], "protocol.schema.json", documents)
        definitions.append(
            {
                "name": name,
                "reference": reference["$ref"],
                "method": declared_method(schema),
                "response": schema.get("x-factory-response"),
                "result": schema.get("x-factory-result"),
                "runtimeConstraints": [
                    {"pointer": pointer, "constraint": node["x-factory-runtime-only"]}
                    for pointer, node in nodes(schema)
                    if "x-factory-runtime-only" in node
                ],
            }
        )
    return {"protocolVersion": version, "references": reference_count, "definitions": definitions}


def load_verified(directory):
    manifest = json.loads((SCHEMA_DIR / "manifest.json").read_text())
    documents = {}
    for filename, expected in manifest["schemas"].items():
        contents = (directory / filename).read_bytes()
        if hashlib.sha256(contents).hexdigest() != expected["sha256"]:
            raise ValueError(f"Schema fingerprint mismatch: {filename}")
        document = json.loads(contents)
        if len(document["definitions"]) != expected["definitions"]:
            raise ValueError(f"Definition count mismatch: {filename}")
        if document["x-factory-protocol-version"] != manifest["protocolVersion"]:
            raise ValueError(f"Protocol version mismatch: {filename}")
        documents[filename] = document
    return documents


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("directory", type=Path, nargs="?", default=SCHEMA_DIR)
    parser.add_argument("--inventory", action="store_true", help="emit the complete JSON inventory")
    args = parser.parse_args()
    result = inventory(load_verified(args.directory))
    if args.inventory:
        print(json.dumps(result, indent=2))
    else:
        definitions = result["definitions"]
        methods = {entry["method"] for entry in definitions if entry["method"] is not None}
        constraints = sum(len(entry["runtimeConstraints"]) for entry in definitions)
        print(f"Protocol {result['protocolVersion']}: {len(definitions)} definitions, "
              f"{result['references']} resolved references, {len(methods)} distinct methods")
        print(f"{constraints} runtime-only constraints require source-level conformance tests")


if __name__ == "__main__":
    main()
