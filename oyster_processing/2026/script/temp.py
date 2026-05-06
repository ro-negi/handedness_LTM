# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

# -*- coding: utf-8 -*-
"""
Oyster processing handedness analysis
Binomial test against 50:50 hand-use expectation.

Rules:
1. Keep oyster rows from "Shellfish species with tool".
2. First scan the oyster row itself for Right/Left.
3. If no row-level Right/Left is found, use focal-level fallback from
   "Hand used with tools" within the same focal_id.
4. Ignore Both completely.
"""

import os
from pathlib import Path
import pandas as pd
from scipy.stats import binomtest


# ===== CONFIG =====
WORKDIR = Path(
    r"C:\Users\rohit_negi\Desktop\handedness_LTM\oyster_processing\2026\data\raw"
)

INPUT_FILE = "2024.03.12 - 2026.04.01_focal_data_RN.csv"
OUTPUT_FILE = "2024.03.12 - 2026.04.01_oyster_processing_binomial_RN.csv"

ENCODINGS_TRY = ["utf-8", "utf-8-sig", "cp1252", "latin1", "utf-16"]

FOCAL_COL = "focal_id"
SUBJ_COL = "Focal ID"
SEX_COL = "sex"
SPECIES_COL = "Shellfish species with tool"
HAND_TOOLS_COL = "Hand used with tools"

OYSTER_VALUES = {
    "Oyster sessile (attached)",
    "Oyster sessile",
    "Oyster non-sessile",
}
# ===================


def read_csv_robust(path):
    last_err = None
    for enc in ENCODINGS_TRY:
        try:
            return pd.read_csv(path, encoding=enc, low_memory=False)
        except Exception as e:
            last_err = e
    raise RuntimeError(f"Failed to read CSV. Last error: {last_err}")


def normalize_sex_value(val):
    if pd.isna(val):
        return pd.NA

    s = str(val).strip().lower()

    if s in ["f", "female"]:
        return "f"
    if s in ["m", "male"]:
        return "m"

    return pd.NA


def detect_right_left_from_text(val):
    """
    Detect only Right or Left.
    Ignore Both completely.
    """
    if pd.isna(val):
        return None

    s = str(val).strip().lower()

    if s == "":
        return None

    # Ignore ambiguous/both values
    if "both" in s:
        return None

    has_right = (
        s == "right"
        or s == "r"
        or "right hand" in s
        or "right" in s
    )

    has_left = (
        s == "left"
        or s == "l"
        or "left hand" in s
        or "left" in s
    )

    if has_right and not has_left:
        return "Right"

    if has_left and not has_right:
        return "Left"

    return None


def detect_right_left_from_row(row):
    """
    Scan entire oyster row for Right/Left evidence.
    If both Right and Left are found in the same row, treat as ambiguous
    and return None.
    """
    found_right = False
    found_left = False

    for val in row.values:
        if pd.isna(val):
            continue

        s = str(val).strip().lower()

        if s == "":
            continue

        if "both" in s:
            continue

        if s == "right" or s == "r" or "right hand" in s or "right" in s:
            found_right = True

        if s == "left" or s == "l" or "left hand" in s or "left" in s:
            found_left = True

    if found_right and not found_left:
        return "Right"

    if found_left and not found_right:
        return "Left"

    return None


def get_focal_fallback_hand(subdf):
    """
    Within each focal_id, look at Hand used with tools.
    Use the last usable Right/Left value found.
    Ignore Both.
    """
    valid_hands = []

    for val in subdf[HAND_TOOLS_COL]:
        hand = detect_right_left_from_text(val)

        if hand in ["Right", "Left"]:
            valid_hands.append(hand)

    if len(valid_hands) == 0:
        return None

    return valid_hands[-1]


def binomial_p_value(right, left):
    total = right + left

    if total == 0:
        return pd.NA

    dominant_count = max(right, left)

    return binomtest(
        k=int(dominant_count),
        n=int(total),
        p=0.5,
        alternative="two-sided"
    ).pvalue


def main():
    os.chdir(WORKDIR)

    input_path = WORKDIR / INPUT_FILE
    output_path = WORKDIR / OUTPUT_FILE

    df = read_csv_robust(input_path)

    required_cols = [
        FOCAL_COL,
        SUBJ_COL,
        SEX_COL,
        SPECIES_COL,
        HAND_TOOLS_COL,
    ]

    missing = [col for col in required_cols if col not in df.columns]

    if missing:
        raise ValueError(f"Missing required columns: {missing}")

    # Keep oyster-processing rows
    df_oysters = df[df[SPECIES_COL].isin(OYSTER_VALUES)].copy()

    if df_oysters.empty:
        raise ValueError("No oyster-processing rows found.")

    # Focal-level fallback hand from Hand used with tools
    fallback_map = (
        df.groupby(FOCAL_COL, dropna=False)
        .apply(get_focal_fallback_hand)
        .to_dict()
    )

    # First detect hand from the oyster row itself
    df_oysters["row_hand"] = df_oysters.apply(detect_right_left_from_row, axis=1)

    # Then fallback to Hand used with tools from the same focal_id
    df_oysters["fallback_hand"] = df_oysters[FOCAL_COL].map(fallback_map)

    df_oysters["resolved_hand"] = df_oysters["row_hand"].where(
        df_oysters["row_hand"].notna(),
        df_oysters["fallback_hand"]
    )

    # Keep only Right/Left; Both already ignored
    df_valid = df_oysters[df_oysters["resolved_hand"].isin(["Right", "Left"])].copy()

    if df_valid.empty:
        raise ValueError("No oyster rows with usable Right/Left handedness found.")

    # Count Right/Left per subject
    counts = (
        df_valid
        .groupby([SUBJ_COL, "resolved_hand"], dropna=False)
        .size()
        .unstack("resolved_hand", fill_value=0)
    )

    for hand in ["Right", "Left"]:
        if hand not in counts.columns:
            counts[hand] = 0

    counts = counts[["Right", "Left"]].reset_index()

    counts.rename(
        columns={
            SUBJ_COL: "subj.id",
            "Right": "right",
            "Left": "left",
        },
        inplace=True,
    )

    # N = unique focal_id with resolved oyster processing per subject
    N_map = (
        df_valid
        .groupby(SUBJ_COL)[FOCAL_COL]
        .nunique()
        .reset_index()
        .rename(columns={SUBJ_COL: "subj.id", FOCAL_COL: "N"})
    )

    # Sex map
    sex_map = df[[SUBJ_COL, SEX_COL]].dropna().copy()
    sex_map[SEX_COL] = sex_map[SEX_COL].apply(normalize_sex_value)

    sex_map = (
        sex_map
        .dropna(subset=[SEX_COL])
        .drop_duplicates(subset=[SUBJ_COL])
        .rename(columns={SUBJ_COL: "subj.id", SEX_COL: "sex"})
    )

    final = counts.merge(sex_map, on="subj.id", how="left")
    final = final.merge(N_map, on="subj.id", how="left")

    # n = average number of oysters processed per focal
    final["n"] = (final["right"] + final["left"]) / final["N"]

    # Handedness Index
    total = final["right"] + final["left"]
    final["handedness.index"] = (final["right"] - final["left"]) / total

    # Binomial p-value
    final["p_value"] = final.apply(
        lambda row: binomial_p_value(row["right"], row["left"]),
        axis=1
    )

    final["n"] = final["n"].round(2)
    final["handedness.index"] = final["handedness.index"].round(3)

    final = final[
        [
            "subj.id",
            "sex",
            "right",
            "left",
            "N",
            "n",
            "handedness.index",
            "p_value",
        ]
    ]

    final = final.sort_values("subj.id")

    final.to_csv(output_path, index=False)

    print(f"Saved output to:\n{output_path}")

    print("\nSummary:")
    print(f"Total oyster rows: {len(df_oysters)}")
    print(f"Resolved Right/Left oyster rows: {len(df_valid)}")
    print(f"Dropped unresolved or Both rows: {len(df_oysters) - len(df_valid)}")


if __name__ == "__main__":
    main()