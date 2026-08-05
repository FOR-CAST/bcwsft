# Years since harvest (getHarvLag, L245-266)

Years since harvest (getHarvLag, L245-266)

## Usage

``` r
bcwsft_harv_lag(s, current_year = .this_year())
```

## Arguments

- s:

  named list of VRI attributes for one polygon.

- current_year:

  integer reference year (defaults to the current year).

## Value

integer years, `0` for future dates, or `NA` if not logged.
