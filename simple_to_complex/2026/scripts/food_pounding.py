# -*- coding: utf-8 -*-
"""
Created on Mon May  4 11:38:52 2026

@author: rohit_negi
"""

# -*- coding: utf-8 -*-
"""
Handedness Project — Food pounding handedness summary

Input:
    2024.03.12 - 2026.04.01_focal_data_RN.csv

Output:
    2024.03.12 - 2026.04.01_food_pounding_RN.csv

Rules:
    right hand = +1 right.hand
    left hand  = +1 left.hand
    both hands = +1 right.hand and +1 left.hand
"""

import os
from pathlib import Path
import pandas as pd


# ===== CONFIG =====
WORKDIR = Path(
    r"C:\Users\rohit_negi\Desktop\New folder (2)\food_pounding\2026"
)

RAW_FILE = "2024.03.12 - 2026.04.01_focal_data_RN.csv"
OUTPUT_FILE = "2024.03.12 - 2026.04.01_food_pounding_RN.csv"

ENCODINGS_TRY = ["utf-8", "utf-8-sig", "cp1252", "latin1", "utf-16"]

SUBJ_COL = "Focal ID"
FOCAL_ID_COL = "focal_id"
SEX_COL = "sex"
HANDEDNESS_COL = "Handedness"
EXCLUDE_SCAN_COL = "Hand used with tools"
# ===================


def read_csv_robust(path: Path) -> pd.DataFrame:
    last_err = None

    for enc in ENCODINGS_TRY:
        try:
            return pd.read_csv(path, encoding=enc, low_memory=False)
        except Exception as e:
            last_err = e

    raise RuntimeError(f"Failed to read CSV. Last error: {last_err}")


def normalize_handedness_value(val):
    """
    Normalize handedness values.

    Right / right / right hand -> right hand
    Left / left / left hand -> left hand
    Both / both hands -> both hands
    Right hand Left hand -> both hands
    Left hand Right hand -> both hands
    """

    if pd.isna(val):
        return pd.NA

    s = str(val).strip().lower()

    if s == "":
        return pd.NA

    has_right = "right" in s or s == "r"
    has_left = "left" in s or s == "l"
    has_both = "both" in s

    if has_both or (has_right and has_left):
        return "both hands"

    if has_right:
        return "right hand"

    if has_left:
        return "left hand"

    return pd.NA


def detect_row_handedness(row: pd.Series, all_columns):
    """
    Scan selected columns in the row for handedness information.
    The column 'Hand used with tools' is excluded from this scan.
    """

    has_right = False
    has_left = False
    has_both = False

    for col in all_columns:
        val = row[col]

        if pd.isna(val):
            continue

        text = str(val).strip().lower()

        if text == "":
            continue

        if "both hand" in text or "both hands" in text or text == "both":
            has_both = True

        if "right" in text:
            has_right = True

        if "left" in text:
            has_left = True

    if has_both or (has_right and has_left):
        return "both hands"

    if has_right:
        return "right hand"

    if has_left:
        return "left hand"

    return pd.NA


def normalize_sex_value(val):
    if pd.isna(val):
        return pd.NA

    s = str(val).strip().lower()

    if s in ["f", "female"]:
        return "f"

    if s in ["m", "male"]:
        return "m"

    return pd.NA


def hand_to_counts(hand):
    """
    Convert handedness category into right/left counts.
    both hands = one count for right and one count for left.
    """

    if hand == "right hand":
        return pd.Series([1, 0])

    if hand == "left hand":
        return pd.Series([0, 1])

    if hand == "both hands":
        return pd.Series([1, 1])

    return pd.Series([0, 0])


def main():
    os.chdir(WORKDIR)

    input_path = WORKDIR / RAW_FILE
    output_path = WORKDIR / OUTPUT_FILE

    df = read_csv_robust(input_path)

    required_cols = [SUBJ_COL, FOCAL_ID_COL, SEX_COL]

    missing = [col for col in required_cols if col not in df.columns]

    if missing:
        raise ValueError(f"Missing required columns: {missing}")

    # Extract rows where "clam" appears anywhere in the row
    clam_mask = df.astype(str).apply(
        lambda col: col.str.contains("clam", case=False, na=False)
    ).any(axis=1)

    df_clam = df.loc[clam_mask].copy()

    if df_clam.empty:
        raise ValueError("No rows containing 'clam' were found.")

    # Ensure Handedness column exists
    if HANDEDNESS_COL not in df_clam.columns:
        df_clam[HANDEDNESS_COL] = pd.NA

    # Columns to scan if Handedness is empty:
    # scan all columns except "Hand used with tools"
    all_cols = [
        col for col in df_clam.columns
        if col != EXCLUDE_SCAN_COL
    ]

    # First use Handedness column.
    # If empty, scan the whole row except "Hand used with tools".
    detected_hands = []

    for _, row in df_clam.iterrows():
        current_hand = normalize_handedness_value(row[HANDEDNESS_COL])

        if pd.isna(current_hand):
            current_hand = detect_row_handedness(row, all_cols)

        detected_hands.append(current_hand)

    df_clam[HANDEDNESS_COL] = detected_hands

    # Keep right, left, and both hands
    valid_hands = ["right hand", "left hand", "both hands"]

    df_valid = df_clam[df_clam[HANDEDNESS_COL].isin(valid_hands)].copy()

    if df_valid.empty:
        raise ValueError("No valid handedness rows found.")

    # Normalize sex
    df_valid[SEX_COL] = df_valid[SEX_COL].apply(normalize_sex_value)

    # Convert handedness into right/left counts
    df_valid[["right.hand", "left.hand"]] = df_valid[HANDEDNESS_COL].apply(
        hand_to_counts
    )

    # Summarize per subject
    summary = (
        df_valid
        .groupby(SUBJ_COL)
        .agg(
            sex=(SEX_COL, lambda x: x.dropna().iloc[0] if x.dropna().size > 0 else pd.NA),
            right_hand=("right.hand", "sum"),
            left_hand=("left.hand", "sum"),
            N=(FOCAL_ID_COL, "nunique")
        )
        .reset_index()
    )

    summary.rename(
        columns={
            SUBJ_COL: "subj.id",
            "right_hand": "right.hand",
            "left_hand": "left.hand"
        },
        inplace=True
    )

    summary["behavior"] = "food.pounding"

    denominator = summary["right.hand"] + summary["left.hand"]

    summary["HI"] = (
        summary["right.hand"] - summary["left.hand"]
    ) / denominator

    summary["abs.HI"] = summary["HI"].abs()

    summary = summary[
        [
            "subj.id",
            "sex",
            "behavior",
            "right.hand",
            "left.hand",
            "HI",
            "abs.HI",
            "N"
        ]
    ]

    summary = summary.sort_values(by="subj.id")

    summary.to_csv(output_path, index=False)

    print(f"Saved final food pounding handedness summary to:\n{output_path}")


if __name__ == "__main__":
    main()