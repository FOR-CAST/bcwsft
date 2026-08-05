# bcwsft

An R port of the **BC Wildfire Service fuel-typing algorithm**: the
expert decision tree of Perrakis, Eade & Hicks (2018) that assigns
Canadian FBP System fuel types to VRI polygons from forest-inventory
attributes.

This is a faithful translation of Gregory A. Greene’s Python
implementation
[`bcwft2018`](https://github.com/gagreene/BC_CFFBPS_FuelTyping_Tool),
with **no ArcGIS / arcpy dependency** (the Python core is pure
`pandas`/`numpy`; only the optional I/O wrappers used arcpy, and those
are not ported). It is intended as an independent, scriptable
cross-check on map-derived fuel-type products such as the BC Wildfire
“Fire Fuel Types - Public” layer.

## License

GPL-3. This package is a derivative work of GPL-3 code (`bcwft2018.py`)
and so must remain GPL-3. Apache-2.0 / MIT projects may **depend on and
call** this package (e.g. via
[`bcwsft::bcwsft_fuel_type()`](https://for-cast.github.io/bcwsft/reference/bcwsft_fuel_type.md))
without changing their own license, provided they do not copy this
package’s source into their tree. See `LICENSE.md`.

## Status

| Component | State |
|----|----|
| Input validation, constants (species/BEC lists) | ported (hand) |
| Predicates (`is_vegetated`, `is_logged`, `harv_lag`, `pct_conifer`, …) | ported (hand) |
| Decision tree: **non-vegetated** branch (`bcwft2018.py` ~L402-487) | ported |
| Decision tree: **vegetated** branch (~L488-1859) | ported (transpiled) |

The decision tree (`R/decision-tree.R`) is generated from the reference
Python by `data-raw/transpile.py` and validated against the reference
output across thousands of random VRI rows (0 mismatches; see
`data-raw/`). Rows whose required inventory attributes are missing error
out, matching the reference; use `on_error = "na"` to get `NA` for such
rows instead.

## Usage

``` r

library(bcwsft)

vri <- data.frame(
  BCLCS_LEVEL_1 = "N", COAST_INTERIOR_CD = "I", BEC_ZONE_CODE = "ICH",
  BEC_SUBZONE = "mc", HARVEST_DATE = "2018-06-01", SPECIES_CD_1 = "PL",
  SPECIES_PCT_1 = 80 # ... plus the remaining bcwsft_input_columns
)
bcwsft_fuel_type(vri, season = "growing", current_year = 2024)
#>   branch                              fuel_type ft_modifier
#> 1 NV/logged/harv<=24/other/growing    O-1b      NA
```

`bcwsft_input_columns` lists every VRI attribute the algorithm consumes.

## Validating against the reference implementation

The golden-output test compares this port to Greene’s Python tool
row-for-row. To generate the reference output (requires Python +
`pandas`/`numpy` and a clone of the original repo; **no ArcGIS licence
needed**):

``` sh
python data-raw/generate_golden.py /path/to/BC_CFFBPS_FuelTyping_Tool
# writes tests/testthat/fixtures/golden_output.csv
R -e 'testthat::test_local()'
```

If `golden_output.csv` is absent the golden test skips; the unit tests
for the ported branches (with a pinned `current_year`) always run.

## Attribution

- Algorithm: Perrakis, D.D.B., Eade, G. & Hicks, D. (2018). *British
  Columbia wildfire fuel typing and fuel type layer description.* Info.
  Report BC-X-444, Canadian Forest Service.
- Original Python implementation: Gregory A. Greene,
  <https://github.com/gagreene/BC_CFFBPS_FuelTyping_Tool>.
