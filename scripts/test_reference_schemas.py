"""Regression checks for the reference baseline, independent of SDK runtimes."""

import copy
import tempfile
import unittest
from pathlib import Path

from reference_schemas import SCHEMA_DIR, inventory, load_verified


class ReferenceSchemasTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.documents = load_verified(SCHEMA_DIR)

    def test_complete_baseline(self):
        result = inventory(self.documents)
        self.assertEqual(result["protocolVersion"], "1.205.0")
        self.assertEqual(result["references"], 2209)
        self.assertEqual(len(result["definitions"]), 739)
        methods = {item["method"] for item in result["definitions"] if item["method"] is not None}
        self.assertEqual(len(methods), 183)
        self.assertEqual(sum(len(item["runtimeConstraints"]) for item in result["definitions"]), 84)

    def test_broken_cross_file_reference(self):
        documents = copy.deepcopy(self.documents)
        documents["shared.schema.json"]["definitions"]["ContentBlockSchema"]["anyOf"][0] = {
            "$ref": "droid.schema.json#/definitions/DoesNotExist"
        }
        with self.assertRaisesRegex(ValueError, "Unresolved reference"):
            inventory(documents)

    def test_missing_index_entry(self):
        documents = copy.deepcopy(self.documents)
        del documents["protocol.schema.json"]["definitions"]["SdkClientMetadataSchema"]
        with self.assertRaisesRegex(ValueError, "cover each owned definition"):
            inventory(documents)

    def test_mixed_protocol_versions(self):
        documents = copy.deepcopy(self.documents)
        documents["daemon.schema.json"]["x-factory-protocol-version"] = "different"
        with self.assertRaisesRegex(ValueError, "Protocol version mismatch"):
            inventory(documents)

    def test_fingerprint_detects_modified_schema(self):
        with tempfile.TemporaryDirectory() as directory:
            destination = Path(directory)
            for source in SCHEMA_DIR.glob("*.schema.json"):
                (destination / source.name).write_bytes(source.read_bytes() + b"\n")
            with self.assertRaisesRegex(ValueError, "Schema fingerprint mismatch"):
                load_verified(destination)


if __name__ == "__main__":
    unittest.main()
