# Assign an FBP fuel type to a single VRI record

Mirrors `bcwft2018.py` `FuelTyping.getFuelType()`: assembles the derived
predicates and runs the decision tree for one polygon.

## Usage

``` r
bcwsft_fuel_type_one(
  record,
  season = c("growing", "dormant"),
  current_year = .this_year()
)
```

## Arguments

- record:

  named list (or one-row data frame) of VRI attributes; see
  [bcwsft_input_columns](https://for-cast.github.io/bcwsft/reference/bcwsft_input_columns.md).

- season:

  one of "growing" or "dormant".

- current_year:

  integer reference year used for harvest/disturbance lags (defaults to
  the current calendar year). Pin it for reproducible results.

## Value

a list with `branch`, `fuel_type`, and `ft_modifier`.
