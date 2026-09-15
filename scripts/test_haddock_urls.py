from pathlib import Path
import tempfile
import unittest

from check_haddock_urls import check_documentation


class HaddockURLsTest(unittest.TestCase):
    def test_expanded_and_local_urls(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory).resolve()
            (root / "haddock-bundle.min.js").write_text("", encoding="utf-8")
            (root / "index.html").write_text(
                '<a href="https://hackage.haskell.org/package/base-4.21.2.0/docs/Data-Maybe.html">Maybe</a>'
                '<script src="haddock-bundle.min.js"></script>'
                '<p>Literal prose ${pkgroot} is not a URL.</p>',
                encoding="utf-8",
            )
            self.assertEqual(check_documentation(root), (1, 2, []))

    def test_unexpanded_urls(self):
        for value in ("${pkgroot}/base/docs", "https://hackage.haskell.org/package/$pkg-$version/docs", "&#36;{pkgroot}/docs"):
            with self.subTest(value=value), tempfile.TemporaryDirectory() as directory:
                root = Path(directory).resolve()
                page = root / "index.html"
                page.write_text(f'<a href="{value}">type</a>', encoding="utf-8")
                self.assertEqual(check_documentation(root), (1, 1, [(page, 1, "unresolved URL template")]))

    def test_missing_package_local_file(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory).resolve()
            page = root / "index.html"
            page.write_text('<a href="target%20page.html#target">type</a>', encoding="utf-8")
            self.assertEqual(check_documentation(root), (1, 1, [(page, 1, "missing package-local file")]))
            (root / "target page.html").write_text('<p id="target">definition</p>', encoding="utf-8")
            self.assertEqual(check_documentation(root), (2, 1, []))

    def test_empty_documentation_is_not_success(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory).resolve()
            with self.assertRaisesRegex(ValueError, "No generated HTML"):
                check_documentation(root)
            (root / "index.html").write_text("<p>No links</p>", encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "No documentation URLs"):
                check_documentation(root)


if __name__ == "__main__":
    unittest.main()
