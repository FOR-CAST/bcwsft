## Tests for the ported (non-vegetated) decision-tree branches.
## Expected fuel types are derived directly from the verbatim logic in
## bcwft2018.py L402-487 (not independent expectations).

## A record with every input column present but empty (NA).
rec0 <- function(...) {
  base <- as.list(stats::setNames(rep(NA, length(bcwsft_input_columns)), bcwsft_input_columns))
  modifyList(base, list(...))
}

cy <- 2024L # pinned reference year

test_that("logged slash branches (harvested <= 6 years)", {
  coast <- rec0(
    BCLCS_LEVEL_1 = "N",
    COAST_INTERIOR_CD = "C",
    HARVEST_DATE = "2020-06-01",
    BEC_ZONE_CODE = "CWH",
    BEC_SUBZONE = "vm"
  )
  expect_identical(bcwsft_fuel_type_one(coast, "growing", cy)$fuel_type, "S-3")

  interior <- rec0(
    BCLCS_LEVEL_1 = "N",
    COAST_INTERIOR_CD = "I",
    HARVEST_DATE = "2020-06-01",
    BEC_ZONE_CODE = "ICH",
    BEC_SUBZONE = "mc"
  )
  expect_identical(bcwsft_fuel_type_one(interior, "growing", cy)$fuel_type, "S-1")
})

test_that("logged wet-belt 7-24 years is season-dependent deciduous", {
  r <- rec0(
    BCLCS_LEVEL_1 = "N",
    COAST_INTERIOR_CD = "I",
    HARVEST_DATE = "2010-06-01",
    BEC_ZONE_CODE = "ICH",
    BEC_SUBZONE = "mc"
  )
  expect_identical(bcwsft_fuel_type_one(r, "growing", cy)$fuel_type, "D-2")
  expect_identical(bcwsft_fuel_type_one(r, "dormant", cy)$fuel_type, "D-1")
})

test_that("logged > 24 years resolves by BEC zone and dry/wet", {
  ich_wet <- rec0(
    BCLCS_LEVEL_1 = "N",
    HARVEST_DATE = "1980-06-01",
    BEC_ZONE_CODE = "ICH",
    BEC_SUBZONE = "mc"
  ) # wet
  expect_identical(bcwsft_fuel_type_one(ich_wet, "growing", cy)$fuel_type, "C-5")

  ich_dry <- rec0(
    BCLCS_LEVEL_1 = "N",
    HARVEST_DATE = "1980-06-01",
    BEC_ZONE_CODE = "ICH",
    BEC_SUBZONE = "dk"
  ) # dry
  expect_identical(bcwsft_fuel_type_one(ich_dry, "growing", cy)$fuel_type, "C-3")

  essf <- rec0(
    BCLCS_LEVEL_1 = "N",
    HARVEST_DATE = "1980-06-01",
    BEC_ZONE_CODE = "ESSF",
    BEC_SUBZONE = "mc"
  )
  expect_identical(bcwsft_fuel_type_one(essf, "growing", cy)$fuel_type, "C-7")

  swb <- rec0(
    BCLCS_LEVEL_1 = "N",
    HARVEST_DATE = "1980-06-01",
    BEC_ZONE_CODE = "SWB",
    BEC_SUBZONE = "mk"
  )
  res <- bcwsft_fuel_type_one(swb, "growing", cy)
  expect_identical(res$fuel_type, "M-2")
  expect_equal(res$ft_modifier, 50)
})

test_that("unlogged recently burned branches", {
  burned_recent <- rec0(
    BCLCS_LEVEL_1 = "N",
    EARLIEST_NONLOGGING_DIST_TYPE = "B",
    EARLIEST_NONLOGGING_DIST_DATE = "2022-06-01",
    BEC_ZONE_CODE = "ICH",
    BEC_SUBZONE = "mc"
  )
  expect_identical(bcwsft_fuel_type_one(burned_recent, "growing", cy)$fuel_type, "N") # lag 2

  burned_older <- rec0(
    BCLCS_LEVEL_1 = "N",
    EARLIEST_NONLOGGING_DIST_TYPE = "B",
    EARLIEST_NONLOGGING_DIST_DATE = "2016-06-01",
    BEC_ZONE_CODE = "ICH",
    BEC_SUBZONE = "mc"
  )
  expect_identical(bcwsft_fuel_type_one(burned_older, "growing", cy)$fuel_type, "O-1b") # lag 8
})

test_that("unlogged unburned shrub with a leading species", {
  shrub <- rec0(
    BCLCS_LEVEL_1 = "N",
    BCLCS_LEVEL_2 = "L",
    SPECIES_CD_1 = "PL",
    SPECIES_PCT_1 = 100,
    BEC_ZONE_CODE = "ICH",
    BEC_SUBZONE = "mc"
  )
  expect_identical(bcwsft_fuel_type_one(shrub, "growing", cy)$fuel_type, "D-2")
})

test_that("vegetated sites raise a clear not-yet-ported error", {
  veg <- rec0(
    BCLCS_LEVEL_1 = "V",
    BCLCS_LEVEL_2 = "T",
    BEC_ZONE_CODE = "ICH",
    BEC_SUBZONE = "mc",
    SPECIES_CD_1 = "CW",
    SPECIES_PCT_1 = 100
  )
  expect_snapshot(bcwsft_fuel_type_one(veg, "growing", cy), error = TRUE)
})

test_that("bcwsft_fuel_type(on_error = 'na') tolerates unported branches", {
  df <- data.frame(
    BCLCS_LEVEL_1 = c("N", "V"),
    HARVEST_DATE = c("2020-06-01", NA),
    COAST_INTERIOR_CD = c("C", NA),
    BEC_ZONE_CODE = c("CWH", "ICH"),
    BEC_SUBZONE = c("vm", "mc"),
    stringsAsFactors = FALSE
  )
  out <- suppressWarnings(bcwsft_fuel_type(df, "growing", cy, on_error = "na"))
  expect_identical(out$fuel_type[1], "S-3")
  expect_identical(out$fuel_type[2], NA_character_) # vegetated -> NA, not an error
})
