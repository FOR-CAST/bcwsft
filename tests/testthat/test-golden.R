## Golden-output test: the R port must reproduce the reference Python tool
## (bcwft2018) row-for-row. The golden file is generated locally by
## data-raw/generate_golden.py (requires Python + pandas; no ArcGIS). When it is
## absent (e.g. a fresh clone), this test skips -- the ported-branch unit tests
## in test-decision-tree.R still run.

test_that("R port matches the Python reference output", {
  input_path <- test_path("fixtures", "vri_sample.csv")
  golden_path <- test_path("fixtures", "golden_output.csv")
  skip_if_not(
    file.exists(golden_path),
    "golden_output.csv not generated; run data-raw/generate_golden.py"
  )

  input <- utils::read.csv(input_path, colClasses = "character", na.strings = "")
  golden <- utils::read.csv(golden_path, colClasses = "character", na.strings = "")
  current_year <- as.integer(golden$current_year[1])

  ## Numeric VRI attributes arrive as numeric from sf/bcdata; coerce here since
  ## read.csv(colClasses = "character") makes everything a string.
  numeric_cols <- c(
    "CROWN_CLOSURE",
    "PROJ_HEIGHT_1",
    "PROJ_AGE_1",
    "VRI_LIVE_STEMS_PER_HA",
    "VRI_DEAD_STEMS_PER_HA",
    "STAND_PERCENTAGE_DEAD",
    "NON_PRODUCTIVE_CD",
    paste0("SPECIES_PCT_", 1:6)
  )
  for (col in intersect(numeric_cols, names(input))) {
    input[[col]] <- suppressWarnings(as.numeric(input[[col]]))
  }

  ## Run the R port row-wise, honouring each row's `season`.
  got <- lapply(seq_len(nrow(input)), function(i) {
    rec <- as.list(input[i, , drop = FALSE])
    bcwsft_fuel_type_one(rec, season = input$season[i], current_year = current_year)
  })
  got_ft <- vapply(got, function(x) x$fuel_type, character(1))
  got_mod <- vapply(got, function(x) as.numeric(x$ft_modifier), numeric(1))

  expect_identical(got_ft, golden$fuel_type)
  expect_equal(got_mod, suppressWarnings(as.numeric(golden$ft_modifier)))
})
