# Changelog

## bcwsft 0.0.1

- Complete R port of the BC Wildfire Service fuel-typing algorithm
  (Greene’s `bcwft2018.py`; Perrakis et al. 2018), arcpy-free. The full
  decision tree (non-vegetated and vegetated branches) is now
  implemented; the vegetated branch was transpiled from the reference
  Python by `data-raw/transpile.py` and validated against the reference
  output across thousands of random VRI rows (0 mismatches).
- [`bcwsft_fuel_type()`](https://for-cast.github.io/bcwsft/reference/bcwsft_fuel_type.md)
  assigns FBP fuel types to a data frame of VRI records.
- [`bcwsft_fuel_type_one()`](https://for-cast.github.io/bcwsft/reference/bcwsft_fuel_type_one.md)
  assigns an FBP fuel type to a single VRI record.
