## Code lists used by the BC Wildfire Service fuel-typing algorithm.
## Ported verbatim from `bcwft2018.py` (Greene, GPL-3), FuelTyping.__init__.

#' Conifer species codes recognised by the algorithm
#'
#' Ported from `bcwft2018.py` `self.coniferList`.
#' @export
bcwsft_conifer_list <- c(
  "C", "CW",
  "Y", "YC",
  "F", "FD", "FDC", "FDI",
  "B", "BA", "BG", "BL",
  "H", "HM", "HW", "HXM",
  "J", "JR",
  "P", "PJ", "PF", "PL", "PLI", "PXJ", "PY", "PLC", "PW", "PA",
  "S", "SB", "SE", "SS", "SW", "SX", "SXB", "SXE", "SXL", "SXS", "SXW", "SXX",
  "T", "TW"
)

## BEC zone codes (self.becZones).
bcwsft_bec_zones <- c(
  "BAFA", "BG", "BWBS", "CDF", "CMA", "CWH", "ESSF", "ICH",
  "IDF", "IMA", "MH", "MS", "PP", "SBPS", "SBS", "SWB"
)

## Dry BEC zones (self.dryBECzones).
bcwsft_dry_bec_zones <- c("BG", "PP", "IDF", "MS")

## Boreal BEC zones (self.borealBECzonse).
## NOTE: 'BBWS' is preserved verbatim from the source; it is almost certainly a
## typo for 'BWBS'. Kept as-is for faithful golden-output matching; revisit if
## upstream corrects it.
bcwsft_boreal_bec_zones <- c("BBWS", "SWB")

## EARLIEST_NONLOGGING_DIST_TYPE codes treated as "burned" (isBurned()).
bcwsft_burn_dist_types <- c("B", "BE", "BG", "BW", "BR", "NB")

#' VRI input attributes consumed by the algorithm
#'
#' The forest-inventory columns `bcwsft_fuel_type()` expects in its input data
#' frame (the algorithm's `season` is supplied as a function argument, not a
#' column). Ported from `bcwft2018.py` `getFuelType()` signature.
#' @export
bcwsft_input_columns <- c(
  "COAST_INTERIOR_CD",
  "BCLCS_LEVEL_1", "BCLCS_LEVEL_2", "BCLCS_LEVEL_3", "BCLCS_LEVEL_4", "BCLCS_LEVEL_5",
  "BEC_ZONE_CODE", "BEC_SUBZONE",
  "EARLIEST_NONLOGGING_DIST_TYPE", "EARLIEST_NONLOGGING_DIST_DATE", "HARVEST_DATE",
  "CROWN_CLOSURE", "PROJ_HEIGHT_1", "PROJ_AGE_1",
  "VRI_LIVE_STEMS_PER_HA", "VRI_DEAD_STEMS_PER_HA", "STAND_PERCENTAGE_DEAD",
  "INVENTORY_STANDARD_CD", "NON_PRODUCTIVE_CD", "LAND_COVER_CLASS_CD_1",
  "SPECIES_CD_1", "SPECIES_PCT_1", "SPECIES_CD_2", "SPECIES_PCT_2",
  "SPECIES_CD_3", "SPECIES_PCT_3", "SPECIES_CD_4", "SPECIES_PCT_4",
  "SPECIES_CD_5", "SPECIES_PCT_5", "SPECIES_CD_6", "SPECIES_PCT_6"
)
