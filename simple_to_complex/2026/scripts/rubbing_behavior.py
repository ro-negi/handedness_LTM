# -*- coding: utf-8 -*-
"""
Created on Mon May  4 14:58:07 2026

@author: rohit_negi
"""

# -*- coding: utf-8 -*-
"""
Handedness Project — Rubbing behavior final summary

Input:
    2024.03.12 - 2026.04.01_focal_data_RN.csv

Output:
    2024.03.12 - 2026.04.01_rubbing_RN.csv

Final columns:
    subj.id, sex, behavior, right.hand, left.hand, HI, abs.HI, N

Rules:
    right hand = +1 right.hand
    left hand  = +1 left.hand
    both hands = +1 right.hand and +1 left.hand

Manual video data are added as separate focal data.
"""

import os
from pathlib import Path
import pandas as pd


# ===== CONFIG =====
WORKDIR = Path(
    r"C:\Users\rohit_negi\Desktop\Max Planck PhD_RN\3. project III - handedness_RN\data\raw\rubbing_behavior\2026"
)

INPUT_FILE = "2024.03.12 - 2026.04.01_focal_data_RN.csv"
OUTPUT_FILE = "2024.03.12 - 2026.04.01_rubbing_RN.csv"

ENCODINGS_TRY = ["utf-8", "utf-8-sig", "cp1252", "latin1", "utf-16"]

DATE_COL = "Date (Capture local)"
SUBJ_COL = "Focal ID"
FOCAL_ID_COL = "focal_id"
SEX_COL = "sex"
HAND_COL = "Handedness"
# ===================


def read_csv_robust(path: Path) -> pd.DataFrame:
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


def map_handedness(val):
    """
    Map handedness values.

    right / Right / right hand -> right
    left / Left / left hand -> left
    both / both hands -> both
    right hand left hand -> both
    left hand right hand -> both
    """

    if pd.isna(val):
        return None

    s = str(val).strip().lower()

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


def main():
    os.chdir(WORKDIR)

    df = read_csv_robust(WORKDIR / INPUT_FILE)

    required_cols = [SUBJ_COL, FOCAL_ID_COL, SEX_COL, HAND_COL]
    missing = [c for c in required_cols if c not in df.columns]

    if missing:
        raise ValueError(f"Missing required columns: {missing}")

    # =========================
    # STEP 1: Extract rattan rows
    # =========================

    rattan_mask = df.astype(str).apply(
        lambda col: col.str.contains("rattan", case=False, na=False)
    ).any(axis=1)

    df_rattan = df.loc[rattan_mask].copy()

    if df_rattan.empty:
        raise ValueError("No rows containing 'rattan' found.")

    # Normalize sex
    df_rattan[SEX_COL] = df_rattan[SEX_COL].apply(normalize_sex_value)

    # Map handedness
    df_rattan["hand_cat"] = df_rattan[HAND_COL].apply(map_handedness)

    # Keep only valid handedness rows
    df_rattan = df_rattan.dropna(subset=["hand_cat"]).copy()

    if df_rattan.empty:
        raise ValueError("Rattan rows found, but no valid handedness values found.")

    # Convert each row into right/left/both raw counts
    df_rattan["right.raw"] = (df_rattan["hand_cat"] == "right").astype(int)
    df_rattan["left.raw"] = (df_rattan["hand_cat"] == "left").astype(int)
    df_rattan["both.raw"] = (df_rattan["hand_cat"] == "both").astype(int)

    # Keep only needed columns
    df_rattan_counts = df_rattan[
        [SUBJ_COL, FOCAL_ID_COL, SEX_COL, "right.raw", "left.raw", "both.raw"]
    ].copy()

    df_rattan_counts.rename(
        columns={
            SUBJ_COL: "subj.id",
            FOCAL_ID_COL: "focal.id",
            SEX_COL: "sex",
        },
        inplace=True,
    )

    # =========================
    # STEP 2: Add manual video data
    # =========================

    manual_data = pd.DataFrame(
        [
            {
                "subj.id": "Jim_lim",
                "focal.id": "video",
                "sex": pd.NA,
                "right.raw": 1,
                "left.raw": 0,
                "both.raw": 6,
            },
            {
                "subj.id": "Robin",
                "focal.id": "video",
                "sex": pd.NA,
                "right.raw": 22,
                "left.raw": 4,
                "both.raw": 1,
            },
            {
                "subj.id": "Yaiko",
                "focal.id": "video",
                "sex": pd.NA,
                "right.raw": 5,
                "left.raw": 1,
                "both.raw": 0,
            },
        ]
    )

    # Get sex values from raw data where possible
    sex_map = (
        df[[SUBJ_COL, SEX_COL]]
        .dropna()
        .copy()
    )

    sex_map[SEX_COL] = sex_map[SEX_COL].apply(normalize_sex_value)

    sex_map = (
        sex_map
        .dropna(subset=[SEX_COL])
        .drop_duplicates(subset=[SUBJ_COL])
        .set_index(SUBJ_COL)[SEX_COL]
        .to_dict()
    )

    manual_data["sex"] = manual_data["subj.id"].map(sex_map)

    # Combine raw rattan data + manual video data
    df_all = pd.concat([df_rattan_counts, manual_data], ignore_index=True)

    # =========================
    # STEP 3: Convert both hands
    # =========================

    df_all["right.hand"] = df_all["right.raw"] + df_all["both.raw"]
    df_all["left.hand"] = df_all["left.raw"] + df_all["both.raw"]

    # =========================
    # STEP 4: Final subject summary
    # =========================

    summary = (
        df_all
        .groupby("subj.id")
        .agg(
            sex=("sex", lambda x: x.dropna().iloc[0] if x.dropna().size > 0 else pd.NA),
            right_hand=("right.hand", "sum"),
            left_hand=("left.hand", "sum"),
            N=("focal.id", "nunique"),
        )
        .reset_index()
    )

    summary.rename(
        columns={
            "right_hand": "right.hand",
            "left_hand": "left.hand",
        },
        inplace=True,
    )

    summary["behavior"] = "rubbing"

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
            "N",
        ]
    ]

    summary = summary.sort_values(by="subj.id")

    output_path = WORKDIR / OUTPUT_FILE
    summary.to_csv(output_path, index=False)

    print(f"Saved final rubbing summary to:\n{output_path}")


if __name__ == "__main__":
    main()