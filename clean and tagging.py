"""
  Clean-up
"""

from __future__ import annotations
import re
import csv
from pathlib import Path

IN_DIR   = Path(r"path0")
OUT_DIR  = Path(r"path1")
OUT_DIR.mkdir(parents=True, exist_ok=True)

DOCX_FILES = {                     
    "WHA77": "WHA77CommissionB_20240530.docx",
    "WHA78": "WHA78CommissionB_20250526.docx"
}


CHAIR_START_RE = re.compile(r"^\s*thank you\.\s*$|^\s*$", re.IGNORECASE)

CHAIR_END_RE = re.compile(
    r"(give the floor to|invite the delegate from|you have the floor|"
    r"have the floor over to you|invite the representatives of|"
    r"the floor is open for discussion|open the floor for|"
    r"please give the microphone to|\bplease\b\s*$)",
    re.IGNORECASE
)

OTHER_START_RE = re.compile(
    r"^\s*thank you\b|^\s*honourable\s+chair\b",
    re.IGNORECASE
)

OTHER_END_RE = re.compile(
    r"\bthank you\b(\s+(madam|mr\.?|mrs\.?)\s*chair)?\.?\s*$",
    re.IGNORECASE
)

def classify_role(text: str) -> str:
    """Return 'chair' if both chair-start and chair-end cues match, else 'other'."""
    if CHAIR_START_RE.search(text) and CHAIR_END_RE.search(text):
        return "chair"
    return "other"


def process_docx(tag: str, filename: str) -> None:
    from docx import Document   

    in_path  = IN_DIR  / filename
    out_path = OUT_DIR / f"{Path(filename).stem}_clean.csv"

    doc       = Document(str(in_path))
    seg_count = 0

    with out_path.open("w", newline="", encoding="utf-8") as fout:
        writer = csv.writer(fout)
        writer.writerow(["segment_id", "year", "speaker", "role", "speech_text"])

        for para in doc.paragraphs:
            raw = para.text.strip()
            if not raw or ":" not in raw:
                continue                                           

            speaker, speech = map(str.strip, raw.split(":", 1))
            seg_count += 1
            segment_id = f"{tag}_{seg_count:04d}"
            role       = classify_role(speech)

            writer.writerow([segment_id, tag[-2:], speaker, role, speech])

    print(f"[✓] {filename}  →  {out_path.name}   ({seg_count} segments)")

if __name__ == "__main__":
    for tag, fname in DOCX_FILES.items():
        process_docx(tag, fname)
##END


"""
  Tagging
"""
from pathlib import Path
import pandas as pd
import re
import glob

ROOT       = Path(r"path")
CLEAN_DIR  = ROOT / "cleaned"
DICT_PATH  = ROOT / "meta" / "theme_dictionary.xlsx"
OUT_SUFFIX = "_auto.csv"


df_dict = pd.read_excel(DICT_PATH, engine="openpyxl")

if {"code", "keywords"}.issubset(df_dict.columns) is False:
    df_dict = df_dict.rename(columns={df_dict.columns[0]: "code",
                                      df_dict.columns[2]: "keywords"})

theme_regex = {}
for code, kw_cell in zip(df_dict["code"], df_dict["keywords"]):
    phrases = [p.strip().lower() for p in str(kw_cell).split(";") if p.strip()]
    phrases = [re.sub(r"\s*\([^)]*\)", "", p) for p in phrases]  
    escaped = [re.escape(p) for p in phrases]
    if not escaped:
        continue
    pattern = r"\b(" + "|".join(escaped) + r")\b"
    theme_regex[code.upper()] = re.compile(pattern, re.IGNORECASE)



def tag_themes(text: str) -> dict:
    return {code: int(bool(rgx.search(text))) for code, rgx in theme_regex.items()}

for csv_in in glob.glob(str(CLEAN_DIR / "*_clean.csv")):
    df_seg = pd.read_csv(csv_in, encoding="utf-8")

    tag_df  = df_seg["speech_text"].apply(lambda t: pd.Series(tag_themes(t)))
    df_out  = pd.concat([df_seg, tag_df], axis=1)

    out_path = Path(csv_in).with_name(Path(csv_in).stem.replace("_clean", "") + OUT_SUFFIX)
    df_out.to_csv(out_path, index=False, encoding="utf-8")
    print(f"[✓] {Path(csv_in).name} → {out_path.name}")
##END
