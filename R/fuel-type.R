## Public entry points, mirroring `bcwft2018.py` FuelTyping.getFuelType (L1860).

#' Assign an FBP fuel type to a single VRI record
#'
#' Mirrors `bcwft2018.py` `FuelTyping.getFuelType()`: assembles the derived
#' predicates and runs the decision tree for one polygon.
#'
#' @param record named list (or one-row data frame) of VRI attributes; see
#'   [bcwsft_input_columns].
#' @param season one of "growing" or "dormant".
#' @param current_year integer reference year used for harvest/disturbance lags
#'   (defaults to the current calendar year). Pin it for reproducible results.
#' @return a list with `branch`, `fuel_type`, and `ft_modifier`.
#' @export
bcwsft_fuel_type_one <- function(record, season = c("growing", "dormant"),
                                 current_year = .this_year()) {
  season <- match.arg(season)
  s <- as.list(record)
  s$season <- season

  ## derived predicates (bcwft2018.py getFuelType L1972-1980)
  s$is_vegetated <- bcwsft_is_vegetated(s)
  s$is_forested <- bcwsft_is_forested(s)
  s$is_logged <- bcwsft_is_logged(s)
  s$is_burned <- bcwsft_is_burned(s)
  s$harv_lag <- bcwsft_harv_lag(s, current_year)
  s$dist_lag <- bcwsft_dist_lag(s, current_year)
  s$pct_cnfr <- bcwsft_pct_conifer(s)
  s$dry_wet <- bcwsft_dry_wet(s)
  s$stocking <- bcwsft_stocking(s)

  bcwsft_decision_tree(s)
}

#' Assign FBP fuel types to a data frame of VRI records
#'
#' Vectorised (row-wise) wrapper around [bcwsft_fuel_type_one()]. Row-wise rather
#' than column-vectorised so the translation stays faithful to the scalar Python
#' logic; vectorise later if performance demands it.
#'
#' @param data data frame whose columns include [bcwsft_input_columns].
#' @param season one of "growing" or "dormant" (applied to all rows).
#' @param current_year integer reference year (see [bcwsft_fuel_type_one()]).
#' @param on_error "error" (default) to stop on an unported/failed branch, or
#'   "na" to return `NA` for that row and continue (useful while the tree is
#'   being ported incrementally).
#' @return a data frame with columns `branch`, `fuel_type`, `ft_modifier`,
#'   one row per input row.
#' @export
bcwsft_fuel_type <- function(data, season = c("growing", "dormant"),
                             current_year = .this_year(),
                             on_error = c("error", "na")) {
  season <- match.arg(season)
  on_error <- match.arg(on_error)
  stopifnot(is.data.frame(data))

  missing_cols <- setdiff(bcwsft_input_columns, names(data))
  if (length(missing_cols)) {
    warning(
      "bcwsft_fuel_type(): input is missing ", length(missing_cols),
      " expected column(s): ", paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  res <- lapply(seq_len(nrow(data)), function(i) {
    rec <- as.list(data[i, , drop = FALSE])
    tryCatch(
      bcwsft_fuel_type_one(rec, season = season, current_year = current_year),
      error = function(e) {
        if (on_error == "na") {
          list(branch = paste0("error: ", conditionMessage(e)),
               fuel_type = NA_character_, ft_modifier = NA_real_)
        } else {
          stop(e)
        }
      }
    )
  })

  data.frame(
    branch = vapply(res, function(x) x$branch %||% NA_character_, character(1)),
    fuel_type = vapply(res, function(x) x$fuel_type %||% NA_character_, character(1)),
    ft_modifier = vapply(res, function(x) as.numeric(x$ft_modifier %||% NA_real_), numeric(1)),
    stringsAsFactors = FALSE
  )
}
