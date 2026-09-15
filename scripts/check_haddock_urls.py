#!/usr/bin/env python3
"""Check generated documentation URL templates and package-local files."""
from html.parser import HTMLParser
from pathlib import Path
import sys
from urllib.parse import unquote, urlsplit


class DocumentationURLs(HTMLParser):
    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.references = []

    def handle_starttag(self, tag, attrs):
        for name, value in attrs:
            if name in {"href", "src"} and value:
                self.references.append((self.getpos()[0], value))


def check_documentation(root):
    root = Path(root).resolve()
    pages = list(root.rglob("*.html"))
    if not pages:
        raise ValueError("No generated HTML documentation found")
    links = 0
    failures = []
    for page in pages:
        parser = DocumentationURLs()
        parser.feed(page.read_text(encoding="utf-8"))
        links += len(parser.references)
        for line, reference in parser.references:
            if any(token in reference for token in ("${pkgroot}", "$pkg", "$version")):
                failures.append((page, line, "unresolved URL template"))
                continue
            url = urlsplit(reference)
            if url.netloc or url.scheme not in {"", "file"}:
                continue
            target = (page.parent / unquote(url.path)).resolve() if url.path else page
            if target.is_relative_to(root) and not target.is_file():
                failures.append((page, line, "missing package-local file"))
    if not links:
        raise ValueError("No documentation URLs were checked")
    return len(pages), links, failures


if __name__ == "__main__":
    pages, links, failures = check_documentation(sys.argv[1])
    print(f"Checked {pages} HTML pages and {links} URLs; {len(failures)} URL/file failures")
    for path, line, reason in failures:
        print(f"{path}:{line}: {reason}")
    sys.exit(bool(failures))
