## The fuel-typing decision tree, ported from `bcwft2018.py` FuelTyping.decisionTree
## (L398-1859). The NON-VEGETATED branch (L402-487) is ported faithfully below.
## The VEGETATED branch (~L488-1859) and a few non-vegetated leaves past L487 are
## not yet ported and raise a clear error (see `.not_yet_ported()`).
##
## The Python tree returns (exit_line_number, fuel_type, modifier). We return a
## branch *label* instead of a source line number -- more meaningful for auditing
## and stable across edits.

## Construct a decision-tree result.
.ft <- function(branch, fuel_type, ft_modifier = NA_real_) {
  list(branch = branch, fuel_type = fuel_type, ft_modifier = as.numeric(ft_modifier))
}

## A leaf where the Python tree falls through all branches and returns None
## (e.g. an unrecognised BEC zone). Faithful behaviour: NA fuel type.
.unmatched <- function(branch) {
  list(branch = branch, fuel_type = NA_character_, ft_modifier = NA_real_)
}

## A branch that exists in `bcwft2018.py` but has not yet been translated.
.not_yet_ported <- function(branch) {
  stop(
    sprintf(
      "bcwsft: decision-tree branch not yet ported: '%s'. Port the corresponding block from bcwft2018.py.",
      branch
    ),
    call. = FALSE
  )
}

#' Run the fuel-typing decision tree on a prepared state
#'
#' @param s named list of VRI attributes augmented with derived predicates
#'   (`is_vegetated`, `is_logged`, `is_burned`, `harv_lag`, `dist_lag`,
#'   `dry_wet`, `season`, ...), as assembled by [bcwsft_fuel_type_one()].
#' @return a list with `branch`, `fuel_type`, and `ft_modifier`.
#' @export
bcwsft_decision_tree <- function(s) {
  dormant <- identical(s$season, "dormant")

  ## ============================ NON-VEGETATED SITE ============================
  ## Python: `if (not self.is_vegetated) or (self.is_vegetated is None)` (L403)
  if (!isTRUE(s$is_vegetated)) {
    ## ------------------------------ SITE LOGGED ------------------------------
    if (isTRUE(s$is_logged)) {
      ## harvested within last 6 years (L407)
      if (s$harv_lag <= 6) {
        if (.eq(s$COAST_INTERIOR_CD, "C")) {
          return(.ft("NV/logged/harv<=6/coast", "S-3"))
        } else {
          return(.ft("NV/logged/harv<=6/interior", "S-1"))
        }
        ## harvested within last 7-24 years (L413)
      } else if (s$harv_lag <= 24) {
        if (.in_set(s$BEC_ZONE_CODE, c("CWH", "MH", "ICH"))) {
          if (dormant) {
            return(.ft("NV/logged/harv<=24/wetbelt/dormant", "D-1"))
          } else {
            return(.ft("NV/logged/harv<=24/wetbelt/growing", "D-2"))
          }
        } else {
          if (dormant) {
            return(.ft("NV/logged/harv<=24/other/dormant", "O-1a"))
          } else {
            return(.ft("NV/logged/harv<=24/other/growing", "O-1b"))
          }
        }
        ## harvested longer than 24 years ago (L425)
      } else {
        if (.in_set(s$BEC_ZONE_CODE, c("CMA", "IMA"))) {
          return(.ft("NV/logged/harv>24/alpine", "N"))
        } else if (.in_set(s$BEC_ZONE_CODE, c("BAFA", "MH"))) {
          if (dormant) {
            return(.ft("NV/logged/harv>24/parkland/dormant", "D-1"))
          } else {
            return(.ft("NV/logged/harv>24/parkland/growing", "D-2"))
          }
        } else if (
          .in_set(s$BEC_ZONE_CODE, c("CWH", "CDF", "ICH")) && identical(s$dry_wet, "wet")
        ) {
          return(.ft("NV/logged/harv>24/wetbelt-wet", "C-5"))
        } else if (.in_set(s$BEC_ZONE_CODE, "BWBS")) {
          return(.ft("NV/logged/harv>24/bwbs", "C-2"))
        } else if (.eq(s$BEC_ZONE_CODE, "SWB")) {
          if (dormant) {
            return(.ft("NV/logged/harv>24/swb/dormant", "M-1", 50))
          } else {
            return(.ft("NV/logged/harv>24/swb/growing", "M-2", 50))
          }
        } else if (
          .eq(s$BEC_ZONE_CODE, "SBS") ||
            (.eq(s$BEC_ZONE_CODE, "IDF") && identical(s$dry_wet, "wet")) ||
            (.eq(s$BEC_ZONE_CODE, "ICH") && identical(s$dry_wet, "dry"))
        ) {
          return(.ft("NV/logged/harv>24/sbs-or-idfwet-or-ichdry", "C-3"))
        } else if (
          .in_set(s$BEC_ZONE_CODE, c("SBPS", "MS", "ESSF")) ||
            (.in_set(s$BEC_ZONE_CODE, c("IDF", "CDF")) && identical(s$dry_wet, "dry"))
        ) {
          return(.ft("NV/logged/harv>24/montane", "C-7"))
        } else if (.in_set(s$BEC_ZONE_CODE, c("PP", "BG"))) {
          if (dormant) {
            return(.ft("NV/logged/harv>24/grassland/dormant", "O-1a"))
          } else {
            return(.ft("NV/logged/harv>24/grassland/growing", "O-1b"))
          }
        } else if (.eq(s$BEC_ZONE_CODE, "CWH") && identical(s$dry_wet, "dry")) {
          if (dormant) {
            return(.ft("NV/logged/harv>24/cwh-dry/dormant", "M-1", 40))
          } else {
            return(.ft("NV/logged/harv>24/cwh-dry/growing", "M-2", 40))
          }
        } else {
          ## Python tree falls through (implicit None) for unhandled BEC zones.
          return(.unmatched("NV/logged/harv>24/unmatched-bec"))
        }
      }
      ## ----------------------------- SITE UNLOGGED -----------------------------
    } else {
      ## recently burned (L461)
      if (isTRUE(s$is_burned) && !.is_missing(s$dist_lag) && s$dist_lag < 11) {
        if (s$dist_lag <= 3) {
          return(.ft("NV/unlogged/burned/<=3", "N"))
        } else if (s$dist_lag <= 6) {
          if (dormant) {
            return(.ft("NV/unlogged/burned/<=6/dormant", "D-1"))
          } else {
            return(.ft("NV/unlogged/burned/<=6/growing", "D-2"))
          }
        } else if (s$dist_lag <= 10) {
          if (dormant) {
            return(.ft("NV/unlogged/burned/<=10/dormant", "O-1a"))
          } else {
            return(.ft("NV/unlogged/burned/<=10/growing", "O-1b"))
          }
        }
        ## not recently burned (L475)
      } else {
        ## Python: `if self.BCLCS_LEVEL_2 in ['L', None]` (shrub or unknown)
        if (.in_set(s$BCLCS_LEVEL_2, "L") || .is_missing(s$BCLCS_LEVEL_2)) {
          if (!.is_missing(s$SPECIES_CD_1)) {
            if (.in_set(s$BEC_ZONE_CODE, c("CWH", "MH", "ICH"))) {
              if (dormant) {
                return(.ft("NV/unlogged/unburned/shrub/wetbelt/dormant", "D-1"))
              } else {
                return(.ft("NV/unlogged/unburned/shrub/wetbelt/growing", "D-2"))
              }
            } else {
              if (dormant) {
                return(.ft("NV/unlogged/unburned/shrub/other/dormant", "O-1a"))
              } else {
                return(.ft("NV/unlogged/unburned/shrub/other/growing", "O-1b"))
              }
            }
          } else {
            ## bcwft2018.py continues past L487 (shrub, no leading species)
            .not_yet_ported("NV/unlogged/unburned/shrub/no-species (bcwft2018.py >L487)")
          }
        } else {
          ## bcwft2018.py continues past L487 (treed/other non-shrub land cover)
          .not_yet_ported("NV/unlogged/unburned/non-shrub (bcwft2018.py >L487)")
        }
      }
    }
    ## ============================== VEGETATED SITE ==============================
  } else {
    ## bcwft2018.py L488-1859: the main forested decision tree.
    .not_yet_ported("vegetated-site (bcwft2018.py ~L488-1859)")
  }
}
