#!/usr/bin/env python3
"""Generate the golden-output fixture from the reference Python implementation.

Runs Gregory A. Greene's ``bcwft2018.FuelTyping`` over ``vri_sample.csv`` and
writes ``golden_output.csv`` (inputs + ``fuel_type`` + ``ft_modifier`` +
``exit_line`` + the ``current_year`` used). The R golden-output test then checks
that the R port reproduces these results row-for-row for the same year.

Requires only ``pandas`` / ``numpy`` and ``bcwft2018.py`` on the path -- NO
ArcGIS / arcpy licence is needed (the fuel-typing core is pure pandas).

Usage:
    python data-raw/generate_golden.py /path/to/BC_CFFBPS_FuelTyping_Tool
"""
import os
import sys
from datetime import datetime

import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
FIXTURES = os.path.join(HERE, "..", "tests", "testthat", "fixtures")

# Locate bcwft2018.py (clone of gagreene/BC_CFFBPS_FuelTyping_Tool).
repo = sys.argv[1] if len(sys.argv) > 1 else "."
sys.path.insert(0, repo)
import bcwft2018  # noqa: E402

DATE_COLS = ("HARVEST_DATE", "EARLIEST_NONLOGGING_DIST_DATE")
FLOAT_COLS = (
    "CROWN_CLOSURE", "PROJ_HEIGHT_1", "PROJ_AGE_1",
    "VRI_LIVE_STEMS_PER_HA", "VRI_DEAD_STEMS_PER_HA", "STAND_PERCENTAGE_DEAD",
    "SPECIES_PCT_1", "SPECIES_PCT_2", "SPECIES_PCT_3",
    "SPECIES_PCT_4", "SPECIES_PCT_5", "SPECIES_PCT_6",
)
# getFuelType() parameters, in order, excluding `season`.
PARAMS = [c for c in bcwft2018.FuelTyping().fldList if c not in
          ("BCWFT_rowRef", "FuelType", "FT_Modifier")]


def coerce(col, v):
    if v is None or v == "" or (isinstance(v, float) and pd.isna(v)):
        return None
    if col in DATE_COLS:
        return pd.to_datetime(v)
    if col in FLOAT_COLS:
        return float(v)
    if col == "NON_PRODUCTIVE_CD":
        return int(float(v))
    return str(v)


def main():
    inp = pd.read_csv(os.path.join(FIXTURES, "vri_sample.csv"), dtype=str)
    ft = bcwft2018.FuelTyping()
    current_year = datetime.now().year

    fuel_types, modifiers, exit_lines = [], [], []
    for _, row in inp.iterrows():
        kwargs = {c: coerce(c, row.get(c)) for c in PARAMS}
        line, fuel, mod = ft.getFuelType(season=row["season"], **kwargs)
        fuel_types.append(fuel)
        modifiers.append(mod)
        exit_lines.append(line)

    out = inp.copy()
    out["fuel_type"] = fuel_types
    out["ft_modifier"] = modifiers
    out["exit_line"] = exit_lines
    out["current_year"] = current_year
    dest = os.path.join(FIXTURES, "golden_output.csv")
    out.to_csv(dest, index=False)
    print(f"wrote {dest} ({len(out)} rows); current_year = {current_year}")


if __name__ == "__main__":
    main()
