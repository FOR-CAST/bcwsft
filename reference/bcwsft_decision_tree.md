# Run the fuel-typing decision tree on a prepared state

Run the fuel-typing decision tree on a prepared state

## Usage

``` r
bcwsft_decision_tree(s)
```

## Arguments

- s:

  named list of VRI attributes augmented with derived predicates
  (`is_vegetated`, `is_forested`, `is_logged`, `is_burned`, `harv_lag`,
  `dist_lag`, `pct_cnfr`, `dry_wet`, `stocking`, `season`), as assembled
  by
  [`bcwsft_fuel_type_one()`](https://for-cast.github.io/bcwsft/reference/bcwsft_fuel_type_one.md).

## Value

a list with `branch`, `fuel_type`, and `ft_modifier`.
