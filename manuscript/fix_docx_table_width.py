#!/usr/bin/env python3
"""Post-process Quarto/Pandoc docx output so nested gt tables autofit.

Quarto wraps labelled gt tables in an outer table with w:tblLayout=fixed,
which Word resolves by squeezing the inner table. Changing those wrappers
to autofit (together with the 100% width knit_print patch in SI.qmd) makes
tables use the full page width.

Usage:
  python3 manuscript/fix_docx_table_width.py manuscript/SI.docx
"""

from __future__ import annotations

import argparse
import shutil
import tempfile
import zipfile
from pathlib import Path


def fix_docx(path: Path) -> int:
    path = path.resolve()
    with tempfile.TemporaryDirectory() as td:
        td_path = Path(td)
        with zipfile.ZipFile(path, "r") as zf:
            zf.extractall(td_path)

        doc_xml = td_path / "word" / "document.xml"
        text = doc_xml.read_text(encoding="utf-8")
        n = text.count('w:tblLayout w:type="fixed"')
        if n == 0:
            return 0

        doc_xml.write_text(
            text.replace('w:tblLayout w:type="fixed"', 'w:tblLayout w:type="autofit"'),
            encoding="utf-8",
        )

        tmp_out = path.with_suffix(".docx.tmp")
        if tmp_out.exists():
            tmp_out.unlink()
        with zipfile.ZipFile(tmp_out, "w", compression=zipfile.ZIP_DEFLATED) as zf:
            for file_path in td_path.rglob("*"):
                if file_path.is_file():
                    zf.write(file_path, file_path.relative_to(td_path).as_posix())
        shutil.move(tmp_out, path)
        return n


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("docx", type=Path, help="Path to .docx file to patch")
    args = parser.parse_args()
    n = fix_docx(args.docx)
    print(f"Updated {n} fixed table layout(s) to autofit in {args.docx}")


if __name__ == "__main__":
    main()
