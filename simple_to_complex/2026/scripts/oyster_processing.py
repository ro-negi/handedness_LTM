# -*- coding: utf-8 -*-
"""
Created on Mon May  4 16:15:48 2026

@author: rohit_negi
"""

# -*- coding: utf-8 -*-
"""
Handedness Project — Oyster processing final handedness summary

Inputs:
    2024.03.12 - 2026.04.01_focal_data_RN.csv

Outputs:
    1) oyster_behavior_counts_RN.csv
    2) 2024.03.12 - 2026.04.01_oyster_processing_RN.csv
"""

import os
from pathlib import Path
from datetime import datetime
import pandas as pd


# ===== CONFIG =====
WORKDIR = Path(
    r"C:\Users\rohit_negi\Desktop\Max Planck PhD_RN\3. project III - handedness_RN\data\raw\oyster_processing\2026"
)

INPUT_FILE = "2024.03.12 - 2026.04.01_focal_data_RN.csv"

INTERMEDIATE_OUTPUT_FILE = "oyster_behavior_counts_RN.csv"
FINAL_OUTPUT_FILE = "2024.03.12 - 2026.04.01_oyster_processing_RN.csv"

ENCODINGS_TRY = ["utf-8", "utf-8-sig", "cp1252", "latin1", "utf-16"]

OYSTER_VALUES = {
    "Oyster sessile (attached)",
    "Oyster sessile",
    "Oyster non-sessile",
}

HAND_VALUES = {"Right", "Left", "Both"}

DATE_COL = "Date (Capture local)"
FOCAL_COL = "focal_id"
SUBJ_COL = "Focal ID"
SEX_COL = "sex"
SPECIES_COL = "Shellfish species with tool"
HAND_TOOLS_COL = "Hand used with tools"
# ===================


def read_csv_robust(path: Path) -> pd.DataFrame:
    last_err = None

    for enc in ENCODINGS_TRY:
        try:
            return pd.read_csv(path, encoding=enc, low_memory=False)
        except Exception as e:
            last_err = e

    raise RuntimeError(f"Failed to read CSV. Last error: {last_err}")


def normalize_date_to_yyyymmdd(date_val):
    if pd.isna(date_val):
        return pd.NA

    s = str(date_val).strip()

    if s == "":
        return pd.NA

    for fmt in [
        "%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d",
        "%d/%m/%Y", "%d-%m-%Y", "%d.%m.%Y",
        "%m/%d/%Y", "%m-%d-%Y", "%m.%d.%Y"
    ]:
        try:
            d = datetime.strptime(s, fmt)
            return d.strftime("%Y.%m.%d")
        except Exception:
            pass

    try:
        d = pd.to_datetime(s, errors="coerce", dayfirst=False)

        if pd.isna(d):
            d = pd.to_datetime(s, errors="coerce", dayfirst=True)

        if pd.isna(d):
            return pd.NA

        return d.strftime("%Y.%m.%d")

    except Exception:
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
        return "Both"

    if has_right:
        return "Right"

    if has_left:
        return "Left"

    return None


def detect_hand_from_row(row: pd.Series):
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
        return "Both"

    if has_right:
        return "Right"

    if has_left:
        return "Left"

    return None


def get_fallback_hand_for_focal(subdf: pd.DataFrame):
    valid_hands = []

    for val in subdf[HAND_TOOLS_COL]:
        hand = detect_hand_from_text(val)

        if hand in HAND_VALUES:
            valid_hands.append(hand)

    if not valid_hands:
        return None

    return valid_hands[-1]


def make_n_key(row):
    focal = row["focal.id"]
    date = row["date"]

    if pd.notna(focal) and str(focal).strip() != "":
        return f"{focal}_{date}"

    return str(date)


def main():
    os.chdir(WORKDIR)

    input_path = WORKDIR / INPUT_FILE

    df = read_csv_robust(input_path)

    required_cols = [
        FOCAL_COL,
        SUBJ_COL,
        SEX_COL,
        SPECIES_COL,
        HAND_TOOLS_COL,
        DATE_COL,
    ]

    missing = [col for col in required_cols if col not in df.columns]

    if missing:
        raise ValueError(f"Missing required columns: {missing}")

    # Fallback hand per focal from full dataset
    fallback_map = (
        df.groupby(FOCAL_COL, dropna=False)
        .apply(get_fallback_hand_for_focal)
        .to_dict()
    )

    # Identify oyster rows
    oyster_mask = df[SPECIES_COL].isin(OYSTER_VALUES)
    df_oysters = df.loc[oyster_mask].copy()

    if df_oysters.empty:
        raise ValueError("No oyster rows found.")

    # Resolve hand per oyster row
    row_hands = df_oysters.apply(detect_hand_from_row, axis=1)
    fallback_hands = df_oysters[FOCAL_COL].map(fallback_map)

    df_oysters["resolved_hand"] = row_hands.where(row_hands.notna(), fallback_hands)

    # Keep only valid handedness rows
    df_valid = df_oysters[df_oysters["resolved_hand"].isin(HAND_VALUES)].copy()

    if df_valid.empty:
        raise ValueError("No oyster rows with usable handedness found.")

    # Date per subject × focal
    date_map = (
        df.groupby([SUBJ_COL, FOCAL_COL], dropna=False)[DATE_COL]
        .agg(lambda x: x.dropna().iloc[0] if x.dropna().size > 0 else pd.NA)
        .reset_index()
        .rename(columns={DATE_COL: "date"})
    )

    date_map["date"] = date_map["date"].apply(normalize_date_to_yyyymmdd)

    # Count oysters per hand per subject × focal
    grouped = (
        df_valid
        .groupby([SUBJ_COL, FOCAL_COL, "resolved_hand"], dropna=False)
        .size()
        .unstack("resolved_hand", fill_value=0)
    )

    for hand in ["Right", "Left", "Both"]:
        if hand not in grouped.columns:
            grouped[hand] = 0

    grouped = grouped[["Right", "Left", "Both"]]

    summary = grouped.reset_index()

    summary.rename(
        columns={
            SUBJ_COL: "subj.id",
            FOCAL_COL: "focal.id",
            "Right": "right.hand",
            "Left": "left.hand",
            "Both": "both.hands",
        },
        inplace=True,
    )

    summary = summary.merge(
        date_map,
        left_on=["subj.id", "focal.id"],
        right_on=[SUBJ_COL, FOCAL_COL],
        how="left",
    )

    summary = summary.drop(columns=[SUBJ_COL, FOCAL_COL])

    summary["total"] = (
        summary["right.hand"] +
        summary["left.hand"] +
        summary["both.hands"]
    )

    summary = summary[
        [
            "subj.id",
            "focal.id",
            "date",
            "right.hand",
            "left.hand",
            "both.hands",
            "total",
        ]
    ]

    summary = summary.sort_values(by=["subj.id", "focal.id"])

    # Save intermediate per-focal file
    intermediate_path = WORKDIR / INTERMEDIATE_OUTPUT_FILE
    summary.to_csv(intermediate_path, index=False)

    print(f"Saved per-focal oyster file to:\n{intermediate_path}")

    # Final subject-level output
    summary_for_final = summary.copy()

    # both.hands = +1 right and +1 left
    summary_for_final["right.hand.final"] = (
        summary_for_final["right.hand"] + summary_for_final["both.hands"]
    )

    summary_for_final["left.hand.final"] = (
        summary_for_final["left.hand"] + summary_for_final["both.hands"]
    )

    # Sex mapping
    sex_map = df[[SUBJ_COL, SEX_COL]].dropna().copy()
    sex_map[SEX_COL] = sex_map[SEX_COL].apply(normalize_sex_value)

    sex_map = (
        sex_map
        .dropna(subset=[SEX_COL])
        .drop_duplicates(subset=[SUBJ_COL])
        .set_index(SUBJ_COL)[SEX_COL]
        .to_dict()
    )

    summary_for_final["sex"] = summary_for_final["subj.id"].map(sex_map)

    # N key:
    # if focal.id exists → focal.id + date
    # if focal.id missing → date only
    summary_for_final["N_key"] = summary_for_final.apply(make_n_key, axis=1)

    final = (
        summary_for_final
        .groupby("subj.id")
        .agg(
            sex=("sex", lambda x: x.dropna().iloc[0] if x.dropna().size > 0 else pd.NA),
            right_hand=("right.hand.final", "sum"),
            left_hand=("left.hand.final", "sum"),
            N=("N_key", "nunique"),
        )
        .reset_index()
    )

    final.rename(
        columns={
            "right_hand": "right.hand",
            "left_hand": "left.hand",
        },
        inplace=True,
    )

    final["behavior"] = "oyster.processing"

    denominator = final["right.hand"] + final["left.hand"]

    final["HI"] = (
        final["right.hand"] - final["left.hand"]
    ) / denominator

    final["abs.HI"] = final["HI"].abs()

    final = final[
        [
            "subj.id",
            "sex",
            "behavior",
            "right.hand",
            "left.hand",
            "HI",
            "abs.HI",
            "N",
        ]
    ]

    final = final.sort_values(by="subj.id")

    final_path = WORKDIR / FINAL_OUTPUT_FILE
    final.to_csv(final_path, index=False)

    print(f"Saved final oyster processing summary to:\n{final_path}")


if __name__ == "__main__":
    main()