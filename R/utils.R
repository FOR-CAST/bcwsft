## Internal helpers for null/NA handling, mirroring Python `None` semantics.

## Python `None` maps to R NULL or a length-1 NA.
.is_missing <- function(x) {
  is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))
}

## NA-safe scalar equality: `x == value` that is FALSE (never NA) when x is missing.
.eq <- function(x, value) {
  !.is_missing(x) && identical(as.character(x), as.character(value))
}

## NA-safe membership: `x %in% choices`, FALSE when x is missing.
.in_set <- function(x, choices) {
  !.is_missing(x) && as.character(x) %in% choices
}

`%||%` <- function(a, b) if (is.null(a)) b else a

## NA-safe scalar equality used by the transpiled decision tree, mirroring
## Python's None comparisons: `None == x` is FALSE, `None != x` is TRUE.
`%eq%` <- function(a, b) isTRUE(a == b)
`%ne%` <- function(a, b) !isTRUE(a == b)

.this_year <- function() as.integer(format(Sys.Date(), "%Y"))

## Extract a 4-digit calendar year from a Date, POSIXt, or string.
## Mirrors the Python handling of timezone-suffixed date strings (`split('+')`).
.year_of <- function(x) {
  if (.is_missing(x)) {
    return(NA_integer_)
  }
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    return(as.integer(format(x, "%Y")))
  }
  x <- as.character(x)
  x <- strsplit(x, "+", fixed = TRUE)[[1]][1]
  d <- suppressWarnings(as.Date(x))
  if (!is.na(d)) {
    return(as.integer(format(d, "%Y")))
  }
  m <- regmatches(x, regexpr("[0-9]{4}", x))
  if (length(m)) as.integer(m) else NA_integer_
}
