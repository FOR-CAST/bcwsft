# Do dominant conifers match a species list? (checkDomConifers, L361-396)

Returns `TRUE` when the highest-percentage conifer in `check_list` meets
or (on a tie, by leading-species order) beats the highest-percentage
conifer not in `check_list`.

## Usage

``` r
bcwsft_check_dom_conifers(s, check_list)
```

## Arguments

- s:

  named list of VRI attributes for one polygon.

- check_list:

  character vector of species codes to test for dominance.

## Value

logical scalar.
