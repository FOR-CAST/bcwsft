# Years since last non-logging disturbance (getDistLag, L268-287)

Years since last non-logging disturbance (getDistLag, L268-287)

## Usage

``` r
bcwsft_dist_lag(s, current_year = .this_year())
```

## Arguments

- s:

  named list of VRI attributes for one polygon.

- current_year:

  integer reference year (defaults to the current year).

## Value

integer years, `0` for future dates, or `NA` if absent.
