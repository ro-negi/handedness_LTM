# -*- coding: utf-8 -*-
"""
Created on Mon May  4 15:40:10 2026

@author: rohit_negi
"""

# -*- coding: utf-8 -*-
"""
Handedness Project — Sea almond nut extraction

Input:
    2025.12.19 - 2026.04.01_focal_data_RN.csv

Output:
    2025.12.19 - 2026.04.01_sea_almond_nut_RN.csv
"""

import os
from pathlib import Path
import pandas as pd


# ===== CONFIG =====
WORKDIR = Path(
    r"C:\Users\rohit_negi\Desktop\Max Planck PhD_RN\3. project III - handedness_RN\data\raw\nut_cracking\2026"
)

INPUT_FILE = "2025.12.19 - 2026.04.01_focal_data_RN.csv"
OUTPUT_FILE = "2025.12.19 - 2026.04.01_sea_almond_nut_RN.csv"

ENCODINGS_TRY = ["utf-8", "utf-8-sig", "cp1252", "latin1", "utf-16"]

TARGET_FOOD = "Terminalia catappa (Sea almond nut)"

FOOD_COL = "Fruit/nut species with tool"
SUBJ_COL = "Focal ID"
FOCAL_ID_COL = "focal_id"
DATE_COL = "Date (Capture local)"
TIME_COL = "Time (Capture local)"
SUCCESS_COL = "Success to open and feed"
STRIKES_COL = "Number of strikes/poundings"
HAND_FOCAL_COL = "Hand used with tools"
# ===================


def read_csv_robust(path: Path):
    last_err = None
    for enc in ENCODINGS_TRY:
        try:
            return pd.read_csv(path, encoding=enc, low_memory=False)
        except Exception as e:
            last_err = e
    raise RuntimeError(f"Failed to read CSV: {last_err}")


def detect_hand_from_text(text):
    if pd.isna(text):
        return None

    s = str(text).strip().lower()
    if s == "":
        return None

    has_right = "right" in s or s == "r"
    has_left = "left" in s or s == "l"
    has_both = "both" in s

    if has_both or (has_right and has_left):
        return "both"

    if has_right:
        return "right"

    if has_left:
        return "left"

    return None


def detect_hand_from_entire_row(row):
    has_right = False
    has_left = False
    has_both = False

    for val in row.values:
        if pd.isna(val):
            continue

        s = str(val).strip().lower()
        if s == "":
            continue

        if "both" in s:
            has_both = True
        if "right" in s:
            has_right = True
        if "left" in s:
            has_left = True

    if has_both or (has_right and has_left):
        return "both"
    if has_right:
        return "right"
    if has_left:
        return "left"

    return None


def detect_hand_from_focal(df_full, focal_id):
    if pd.isna(focal_id):
        return None

    focal_rows = df_full[df_full[FOCAL_ID_COL] == focal_id]

    hands_found = []

    for val in focal_rows[HAND_FOCAL_COL]:
        hand = detect_hand_from_text(val)
        if hand is not None:
            hands_found.append(hand)

    if not hands_found:
        return None

    if "both" in hands_found:
        return "both"
    if "right" in hands_found and "left" in hands_found:
        return "both"
    if "right" in hands_found:
        return "right"
    if "left" in hands_found:
        return "left"

    return None


def hand_to_columns(hand):
    if hand == "right":
        return pd.Series([1, 0, 0])
    if hand == "left":
        return pd.Series([0, 1, 0])
    if hand == "both":
        return pd.Series([0, 0, 1])
    return pd.Series([pd.NA, pd.NA, pd.NA])


def main():
    os.chdir(WORKDIR)

    df = read_csv_robust(WORKDIR / INPUT_FILE)

    # --- Extract sea almond rows ---
    mask = df[FOOD_COL].astype(str).str.contains(
        TARGET_FOOD, case=False, na=False, regex=False
    )

    df_sea = df.loc[mask].copy()

    if df_sea.empty:
        raise ValueError("No sea almond rows found.")

    detected_hands = []

    for _, row in df_sea.iterrows():
        hand = detect_hand_from_entire_row(row)

        if hand is None:
            hand = detect_hand_from_focal(df, row[FOCAL_ID_COL])

        detected_hands.append(hand)

    df_sea["hand.detected"] = detected_hands

    df_sea[["right.hand", "left.hand", "both.hands"]] = (
        df_sea["hand.detected"].apply(hand_to_columns)
    )

    output = pd.DataFrame({
        "subj.id": df_sea[SUBJ_COL],
        "focal.id": df_sea[FOCAL_ID_COL],
        "date": df_sea[DATE_COL],
        "time": df_sea[TIME_COL],
        "success": df_sea[SUCCESS_COL],
        "video": "no",
        "strikes": df_sea[STRIKES_COL],
        "right.hand": df_sea["right.hand"],
        "left.hand": df_sea["left.hand"],
        "both.hands": df_sea["both.hands"],
    })

    output.to_csv(WORKDIR / OUTPUT_FILE, index=False)

    print(f"Saved output → {WORKDIR / OUTPUT_FILE}")


if __name__ == "__main__":
    main()
    
    # -*- coding: utf-8 -*-
"""
Handedness Project — Nut cracking final summary
"""

import os
from pathlib import Path
import pandas as pd

# ===== CONFIG =====
WORKDIR = Path(
    r"C:\Users\rohit_negi\Desktop\Max Planck PhD_RN\3. project III - handedness_RN\data\raw\nut_cracking\2026"
)

INPUT_FILE = "2024.03.12-2026.04.01_nut_cracking_RN.csv"
OUTPUT_FILE = "2024.03.12-2026.04.01_nut_cracking_summary_RN.csv"

# ===================


def main():
    os.chdir(WORKDIR)

    df = pd.read_csv(INPUT_FILE)

    # --- STEP 1: Convert both.hands into right + left ---
    df["right.hand"] = df["right.hand"].fillna(0) + df["both.hands"].fillna(0)
    df["left.hand"] = df["left.hand"].fillna(0) + df["both.hands"].fillna(0)

    # --- STEP 2: Create custom focal key ---
    def create_key(row):
        if pd.notna(row["focal.id"]):
            return f"{row['focal.id']}_{row['date']}"
        else:
            return f"{row['date']}"

    df["focal_key"] = df.apply(create_key, axis=1)

    # --- STEP 3: Aggregate per subject ---
    summary = (
        df.groupby("subj.id")
        .agg(
            right_hand=("right.hand", "sum"),
            left_hand=("left.hand", "sum"),
            N=("focal_key", "nunique"),
        )
        .reset_index()
    )

    summary.rename(
        columns={
            "right_hand": "right.hand",
            "left_hand": "left.hand"
        },
        inplace=True
    )

    # --- STEP 4: Add behavior ---
    summary["behavior"] = "nut.cracking"

    # --- STEP 5: HI ---
    denom = summary["right.hand"] + summary["left.hand"]

    summary["HI"] = (summary["right.hand"] - summary["left.hand"]) / denom
    summary["abs.HI"] = summary["HI"].abs()

    # --- STEP 6: Add sex if present ---
    if "sex" in df.columns:
        sex_map = (
            df.groupby("subj.id")["sex"]
            .agg(lambda x: x.dropna().iloc[0] if x.dropna().size > 0 else pd.NA)
        )
        summary["sex"] = summary["subj.id"].map(sex_map)
    else:
        summary["sex"] = pd.NA

    # --- STEP 7: Final order ---
    summary = summary[
        ["subj.id", "sex", "behavior",
         "right.hand", "left.hand", "HI", "abs.HI", "N"]
    ]

    summary = summary.sort_values(by="subj.id")

    summary.to_csv(OUTPUT_FILE, index=False)

    print(f"Saved final summary → {OUTPUT_FILE}")


if __name__ == "__main__":
    main()