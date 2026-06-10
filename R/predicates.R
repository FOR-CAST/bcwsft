## Derived-attribute predicates, ported faithfully from `bcwft2018.py`
## (Greene, GPL-3). Each takes `s`, a named list of one polygon's attributes,
## and returns the derived value the decision tree consumes. Python line ranges
## are noted for audit.

#' Is the site vegetated? (`bcwft2018.py` isVegetated, L196-207)
#'
#' @param s named list of VRI attributes for one polygon.
#' @return `TRUE`, `FALSE`, or `NA` (mirrors Python `None`).
#' @export
bcwsft_is_vegetated <- function(s) {
  if (.eq(s$BCLCS_LEVEL_1, "V")) {
    TRUE
  } else if (.eq(s$BCLCS_LEVEL_1, "N")) {
    FALSE
  } else {
    NA
  }
}

#' Is the site forested? (isForested, L209-220)
#' @inheritParams bcwsft_is_vegetated
#' @return `TRUE`, `FALSE`, or `NA`.
#' @export
bcwsft_is_forested <- function(s) {
  if (.eq(s$BCLCS_LEVEL_2, "T")) {
    TRUE
  } else if (.eq(s$BCLCS_LEVEL_2, "N")) {
    FALSE
  } else {
    NA
  }
}

#' Is the site logged? (isLogged, L222-231)
#' @inheritParams bcwsft_is_vegetated
#' @return logical scalar.
#' @export
bcwsft_is_logged <- function(s) {
  !.is_missing(s$HARVEST_DATE)
}

#' Is the site burned? (isBurned, L233-243)
#' @inheritParams bcwsft_is_vegetated
#' @return logical scalar.
#' @export
bcwsft_is_burned <- function(s) {
  .in_set(s$EARLIEST_NONLOGGING_DIST_TYPE, bcwsft_burn_dist_types)
}

#' Years since harvest (getHarvLag, L245-266)
#' @inheritParams bcwsft_is_vegetated
#' @param current_year integer reference year (defaults to the current year).
#' @return integer years, `0` for future dates, or `NA` if not logged.
#' @export
bcwsft_harv_lag <- function(s, current_year = .this_year()) {
  if (.is_missing(s$HARVEST_DATE)) {
    return(NA_integer_)
  }
  yr <- .year_of(s$HARVEST_DATE)
  if (is.na(yr)) {
    return(NA_integer_)
  }
  if (yr > current_year) 0L else as.integer(current_year - yr)
}

#' Years since last non-logging disturbance (getDistLag, L268-287)
#' @inheritParams bcwsft_harv_lag
#' @return integer years, `0` for future dates, or `NA` if absent.
#' @export
bcwsft_dist_lag <- function(s, current_year = .this_year()) {
  if (.is_missing(s$EARLIEST_NONLOGGING_DIST_DATE)) {
    return(NA_integer_)
  }
  yr <- .year_of(s$EARLIEST_NONLOGGING_DIST_DATE)
  if (is.na(yr)) {
    return(NA_integer_)
  }
  if (yr > current_year) 0L else as.integer(current_year - yr)
}

#' Percent conifer across the leading species (getPrcntConifer, L289-323)
#' @inheritParams bcwsft_is_vegetated
#' @return numeric percentage, capped at 100.
#' @export
bcwsft_pct_conifer <- function(s) {
  total <- 0
  for (i in 1:6) {
    cd <- s[[paste0("SPECIES_CD_", i)]]
    pct <- s[[paste0("SPECIES_PCT_", i)]]
    if (!.is_missing(cd) && is.numeric(pct) && !is.na(pct) &&
      as.character(cd) %in% bcwsft_conifer_list) {
      total <- total + pct
    }
  }
  min(total, 100)
}

#' Dry/wet class from BEC subzone first letter (getDryWet, L325-342)
#' @inheritParams bcwsft_is_vegetated
#' @return one of "dry", "wet", "undifferentiated", or "Invalid Subzone".
#' @export
bcwsft_dry_wet <- function(s) {
  sz <- s$BEC_SUBZONE
  if (.is_missing(sz) || nchar(as.character(sz)) < 1L) {
    return("Invalid Subzone")
  }
  first <- substr(as.character(sz), 1L, 1L)
  switch(first,
    d = ,
    x = "dry",
    m = ,
    w = ,
    v = "wet",
    u = "undifferentiated",
    "Invalid Subzone"
  )
}

#' Total live + dead stems per ha (getStocking, L344-359)
#' @inheritParams bcwsft_is_vegetated
#' @return numeric stems per ha.
#' @export
bcwsft_stocking <- function(s) {
  live <- if (.is_missing(s$VRI_LIVE_STEMS_PER_HA)) 0 else s$VRI_LIVE_STEMS_PER_HA
  dead <- if (.is_missing(s$VRI_DEAD_STEMS_PER_HA)) 0 else s$VRI_DEAD_STEMS_PER_HA
  live + dead
}

#' Do dominant conifers match a species list? (checkDomConifers, L361-396)
#'
#' Returns `TRUE` when the highest-percentage conifer in `check_list` meets or
#' (on a tie, by leading-species order) beats the highest-percentage conifer not
#' in `check_list`.
#' @inheritParams bcwsft_is_vegetated
#' @param check_list character vector of species codes to test for dominance.
#' @return logical scalar.
#' @export
bcwsft_check_dom_conifers <- function(s, check_list) {
  codes <- vapply(1:6, function(i) {
    cd <- s[[paste0("SPECIES_CD_", i)]]
    if (.is_missing(cd)) NA_character_ else as.character(cd)
  }, character(1))
  pct <- vapply(1:6, function(i) {
    p <- s[[paste0("SPECIES_PCT_", i)]]
    if (.is_missing(p)) NA_real_ else as.numeric(p)
  }, numeric(1))

  in_check <- which(!is.na(codes) & codes %in% check_list)
  cnfr_pct <- if (length(in_check) == 0L) 0 else max(pct[in_check], na.rm = TRUE)

  alt <- which(!is.na(codes) & !(codes %in% check_list) & codes %in% bcwsft_conifer_list)
  alt_pct <- if (length(alt) == 0L) 0 else max(pct[alt], na.rm = TRUE)

  if (cnfr_pct != 0 && cnfr_pct == alt_pct) {
    in_check[1L] < alt[1L]
  } else {
    cnfr_pct > alt_pct
  }
}
