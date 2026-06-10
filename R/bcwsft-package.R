#' bcwsft: BC Wildfire Service Fuel Typing Algorithm (R port)
#'
#' An R port of the British Columbia Wildfire Service fuel-typing algorithm
#' (Perrakis, Eade & Hicks 2018), translated faithfully from Gregory A. Greene's
#' Python implementation `bcwft2018`. It assigns Canadian Forest Fire Behaviour
#' Prediction (FBP) System fuel types to VRI polygons from forest-inventory
#' attributes, with no ArcGIS dependency.
#'
#' @section License:
#' GPL-3, as a derivative of GPL-3 source. See `LICENSE.md`.
#'
#' @section Provenance:
#' Original Python: <https://github.com/gagreene/BC_CFFBPS_FuelTyping_Tool>.
#' Each ported block carries a comment pointing back to the corresponding
#' `bcwft2018.py` line range so the translation can be audited.
#'
#' @keywords internal
"_PACKAGE"
