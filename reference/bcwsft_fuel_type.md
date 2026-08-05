# Assign FBP fuel types to a data frame of VRI records

Vectorised (row-wise) wrapper around
[`bcwsft_fuel_type_one()`](https://for-cast.github.io/bcwsft/reference/bcwsft_fuel_type_one.md).
Row-wise rather than column-vectorised so the translation stays faithful
to the scalar Python logic; vectorise later if performance demands it.

## Usage

``` r
bcwsft_fuel_type(
  data,
  season = c("growing", "dormant"),
  current_year = .this_year(),
  on_error = c("error", "na")
)
```

## Arguments

- data:

  data frame whose columns include
  [bcwsft_input_columns](https://for-cast.github.io/bcwsft/reference/bcwsft_input_columns.md).

- season:

  one of "growing" or "dormant" (applied to all rows).

- current_year:

  integer reference year (see
  [`bcwsft_fuel_type_one()`](https://for-cast.github.io/bcwsft/reference/bcwsft_fuel_type_one.md)).

- on_error:

  "error" (default) to stop on an unported/failed branch, or "na" to
  return `NA` for that row and continue (useful while the tree is being
  ported incrementally).

## Value

a data frame with columns `branch`, `fuel_type`, `ft_modifier`, one row
per input row.
