## Unit tests for the derived-attribute predicates (faithful port of bcwft2018.py).

test_that("is_vegetated maps BCLCS_LEVEL_1", {
  expect_true(bcwsft_is_vegetated(list(BCLCS_LEVEL_1 = "V")))
  expect_false(bcwsft_is_vegetated(list(BCLCS_LEVEL_1 = "N")))
  expect_true(is.na(bcwsft_is_vegetated(list(BCLCS_LEVEL_1 = NA))))
  expect_true(is.na(bcwsft_is_vegetated(list(BCLCS_LEVEL_1 = "X"))))
})

test_that("is_burned recognises burn disturbance codes", {
  expect_true(bcwsft_is_burned(list(EARLIEST_NONLOGGING_DIST_TYPE = "B")))
  expect_true(bcwsft_is_burned(list(EARLIEST_NONLOGGING_DIST_TYPE = "NB")))
  expect_false(bcwsft_is_burned(list(EARLIEST_NONLOGGING_DIST_TYPE = "IBM")))
  expect_false(bcwsft_is_burned(list(EARLIEST_NONLOGGING_DIST_TYPE = NA)))
})

test_that("dry_wet keys off the BEC subzone first letter", {
  expect_identical(bcwsft_dry_wet(list(BEC_SUBZONE = "dk")), "dry")
  expect_identical(bcwsft_dry_wet(list(BEC_SUBZONE = "xc")), "dry")
  expect_identical(bcwsft_dry_wet(list(BEC_SUBZONE = "mc")), "wet")
  expect_identical(bcwsft_dry_wet(list(BEC_SUBZONE = "vm")), "wet")
  expect_identical(bcwsft_dry_wet(list(BEC_SUBZONE = "un")), "undifferentiated")
  expect_identical(bcwsft_dry_wet(list(BEC_SUBZONE = NA)), "Invalid Subzone")
})

test_that("pct_conifer sums only conifer species and caps at 100", {
  rec <- list(
    SPECIES_CD_1 = "PL", SPECIES_PCT_1 = 60,
    SPECIES_CD_2 = "AT", SPECIES_PCT_2 = 40 # AT (aspen) is not a conifer
  )
  expect_equal(bcwsft_pct_conifer(rec), 60)

  rec2 <- list(
    SPECIES_CD_1 = "PL", SPECIES_PCT_1 = 70,
    SPECIES_CD_2 = "SX", SPECIES_PCT_2 = 70
  )
  expect_equal(bcwsft_pct_conifer(rec2), 100) # capped
})

test_that("harv_lag uses the supplied reference year", {
  expect_equal(bcwsft_harv_lag(list(HARVEST_DATE = "2018-06-01"), current_year = 2024), 6)
  expect_equal(bcwsft_harv_lag(list(HARVEST_DATE = "2030-06-01"), current_year = 2024), 0) # future
  expect_true(is.na(bcwsft_harv_lag(list(HARVEST_DATE = NA), current_year = 2024)))
})
