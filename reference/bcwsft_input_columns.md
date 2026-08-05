# VRI input attributes consumed by the algorithm

The forest-inventory columns
[`bcwsft_fuel_type()`](https://for-cast.github.io/bcwsft/reference/bcwsft_fuel_type.md)
expects in its input data frame (the algorithm's `season` is supplied as
a function argument, not a column). Ported from `bcwft2018.py`
`getFuelType()` signature.

## Usage

``` r
bcwsft_input_columns
```
