# -*- coding: utf-8 -*-
"""
Created on Mon May  4 13:32:02 2026

@author: rohit_negi
"""

# -*- coding: utf-8 -*-
"""
Handedness Project — Food washing summary (FINAL with sex merge)

Input:
    1) washing_behavior_2026.csv
    2) Subj_ID_Naming_List_2024_11.csv

Output:
    2024.03.12 - 2026.04.01_food_washing_RN.csv

Final columns:
    subj.id, sex, behavior, right.hand, left.hand, HI, abs.HI, N
"""

import os
from pathlib import Path
import pandas as pd

# ===== CONFIG =====
WORKDIR = Path(
    r"C:\Users\rohit_negi\Desktop\Max Planck PhD_RN\3. project III - handedness_RN\data\raw\food_washing\2026"
)

INPUT_FILE = "washing_behavior_2026.csv"
SEX_FILE = "Subj_ID_Naming_List_2024_11.csv"

OUTPUT_FILE = "2024.03.12 - 2026.04.01_food_washing_RN.csv"

ENCODINGS_TRY = ["utf-8", "utf-8-sig", "cp1252", "latin1", "utf-16"]

SUBJ_COL = "Sub ID"
DATE_COL = "Date"
HAND_COL = "Dominant hand"
# ===================


def read_csv_robust(path: Path):
    last_err = None
    for enc in ENCODINGS_TRY:
        try:
            return pd.read_csv(path, encoding=enc, low_memory=False)
        except Exception as e:
            last_err = e
    raise RuntimeError(f"Failed to read CSV: {last_err}")


def detect_handedness(val):
    if pd.isna(val):
        return None

    s = str(val).strip().lower()

    if s == "":
        return None

    has_right = "right" in s
    has_left = "left" in s
    has_both = "both" in s

    if has_both or (has_right and has_left):
        return "both"
    if has_right:
        return "right"
    if has_left:
        return "left"

    return None


def hand_to_counts(hand):
    if hand == "right":
        return pd.Series([1, 0])
    if hand == "left":
        return pd.Series([0, 1])
    if hand == "both":
        return pd.Series([1, 1])
    return pd.Series([0, 0])


def normalize_sex(val):
    if pd.isna(val):
        return pd.NA

    s = str(val).strip().lower()

    if s.startswith("f"):
        return "f"
    if s.startswith("m"):
        return "m"

    return pd.NA


def main():
    os.chdir(WORKDIR)

    df = read_csv_robust(WORKDIR / INPUT_FILE)
    sex_df = read_csv_robust(WORKDIR / SEX_FILE)

    # --- Check columns ---
    if SUBJ_COL not in df.columns:
        raise ValueError(f"{SUBJ_COL} missing in washing file")

    if HAND_COL not in df.columns:
        raise ValueError(f"{HAND_COL} missing in washing file")

    # --- Prepare washing data ---
    df = df[[SUBJ_COL, DATE_COL, HAND_COL]].copy()

    df["hand"] = df[HAND_COL].apply(detect_handedness)
    df = df.dropna(subset=["hand"]).copy()

    # Convert to counts
    df[["right.hand", "left.hand"]] = df["hand"].apply(hand_to_counts)

    # --- Aggregate ---
    summary = (
        df
        .groupby(SUBJ_COL)
        .agg(
            right_hand=("right.hand", "sum"),
            left_hand=("left.hand", "sum"),
            N=(DATE_COL, "nunique")
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

    # =========================
    # MERGE SEX DATA
    # =========================

    # Try to detect correct column names in sex file
    possible_id_cols = ["Sub ID", "Focal ID", "subj.id", "Subj ID"]
    possible_sex_cols = ["sex", "Sex"]

    id_col = next((c for c in possible_id_cols if c in sex_df.columns), None)
    sex_col = next((c for c in possible_sex_cols if c in sex_df.columns), None)

    if id_col is None or sex_col is None:
        raise ValueError("Could not find ID or sex column in sex file")

    sex_df = sex_df[[id_col, sex_col]].copy()
    sex_df.rename(columns={id_col: "subj.id", sex_col: "sex"}, inplace=True)

    sex_df["sex"] = sex_df["sex"].apply(normalize_sex)

    # Merge
    summary = summary.merge(sex_df, on="subj.id", how="left")

    # =========================
    # FINAL CALCULATIONS
    # =========================

    summary["behavior"] = "food.washing"

    denom = summary["right.hand"] + summary["left.hand"]

    summary["HI"] = (summary["right.hand"] - summary["left.hand"]) / denom
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

    summary.to_csv(WORKDIR / OUTPUT_FILE, index=False)

    print(f"\nSaved final washing summary to:\n{WORKDIR / OUTPUT_FILE}")


if __name__ == "__main__":
    main()