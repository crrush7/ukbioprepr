# test-extract_all_values.R
library(testthat)
library(terra)

# Helper to detect offline
is_online <- function() {
  res <- try(utils::download.file("https://zenodo.org/robots.txt",
                                  tempfile(), quiet = TRUE, mode = "wb"),
             silent = TRUE)
  !inherits(res, "try-error")
}

# =============================================================================
# INPUT VALIDATION TESTS - BASIC
# =============================================================================

test_that("function validates data frame input", {
  expect_error(
    extract_all_values("grid", NULL),
    "Input must be a data frame"
  )

  expect_error(
    extract_all_values("grid", "not a dataframe"),
    "Input must be a data frame"
  )
})

test_that("function validates type parameter", {
  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)

  expect_error(
    extract_all_values("invalid", df),
    "Invalid type"
  )
})

test_that("grid type requires gridRef column", {
  df <- data.frame(year = 2020, month = 6)

  expect_error(
    extract_all_values("grid", df),
    "required columns are missing.*gridRef"
  )
})

test_that("coords type requires X and Y columns", {
  df1 <- data.frame(Y = 100000, year = 2020, month = 6)
  expect_error(
    extract_all_values("coords", df1, crs = "EPSG:29903"),
    "required columns are missing.*X"
  )

  df2 <- data.frame(X = 334000, year = 2020, month = 6)
  expect_error(
    extract_all_values("coords", df2, crs = "EPSG:29903"),
    "required columns are missing.*Y"
  )
})

test_that("coords type requires CRS parameter", {
  df <- data.frame(X = 334000, Y = 380000, year = 2020, month = 6)

  expect_error(
    extract_all_values("coords", df),
    "CRS must be provided"
  )
})

test_that("coords type requires CRS to start with EPSG:", {
  df <- data.frame(X = 334000, Y = 380000, year = 2020, month = 6)

  expect_error(
    extract_all_values("coords", df, crs = "29903"),
    "crs must start with 'EPSG:'"
  )
})

# =============================================================================
# INPUT VALIDATION TESTS - CLIMATE-SPECIFIC
# =============================================================================

test_that("climate=TRUE requires year and month columns", {
  df1 <- data.frame(gridRef = "J3480", year = 2020)
  expect_error(
    extract_all_values("grid", df1, climate = TRUE),
    "required columns are missing.*month"
  )

  df2 <- data.frame(gridRef = "J3480", month = 6)
  expect_error(
    extract_all_values("grid", df2, climate = TRUE),
    "required columns are missing.*year"
  )
})

test_that("climate validates climtime parameter", {
  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)

  expect_error(
    extract_all_values("grid", df, climate = TRUE, climtime = "invalid"),
    "Invalid 'climtime' value"
  )
})

test_that("climate validates climvar parameter", {
  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)

  expect_error(
    extract_all_values("grid", df, climate = TRUE, climvar = "temperature"),
    "Invalid 'climvar' value"
  )
})

test_that("climate validates annualstartmonth when annual in climtime", {
  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)

  expect_error(
    extract_all_values("grid", df, climate = TRUE, climtime = "annual"),
    "'annualstartmonth' must be provided"
  )

  expect_error(
    extract_all_values("grid", df, climate = TRUE, climtime = "annual", annualstartmonth = 0),
    "'annualstartmonth' must be a numeric value between 1 and 12"
  )

  expect_error(
    extract_all_values("grid", df, climate = TRUE, climtime = "annual", annualstartmonth = 13),
    "'annualstartmonth' must be a numeric value between 1 and 12"
  )
})

test_that("climate validates year range (2000-2023)", {
  df1 <- data.frame(gridRef = "J3480", year = 1999, month = 6)
  expect_error(
    extract_all_values("grid", df1, climate = TRUE, annualstartmonth = 1),
    "Invalid year.*2000.*2023"
  )

  df2 <- data.frame(gridRef = "J3480", year = 2024, month = 6)
  expect_error(
    extract_all_values("grid", df2, climate = TRUE, annualstartmonth = 1),
    "Invalid year.*2000.*2023"
  )
})

test_that("climate validates month range (1-12)", {
  df1 <- data.frame(gridRef = "J3480", year = 2020, month = 0)
  expect_error(
    extract_all_values("grid", df1, climate = TRUE, annualstartmonth = 1),
    "Invalid month.*1.*12"
  )

  df2 <- data.frame(gridRef = "J3480", year = 2020, month = 13)
  expect_error(
    extract_all_values("grid", df2, climate = TRUE, annualstartmonth = 1),
    "Invalid month.*1.*12"
  )
})

# =============================================================================
# INPUT VALIDATION TESTS - LANDCOVER-SPECIFIC
# =============================================================================

test_that("landcover=TRUE requires year column", {
  df <- data.frame(gridRef = "J3480")

  expect_error(
    extract_all_values("grid", df, landcover = TRUE, climate = FALSE),
    "required columns are missing.*year"
  )
})

test_that("landcover validates year range (2000-2023)", {
  df1 <- data.frame(gridRef = "J3480", year = 1999)
  expect_error(
    extract_all_values("grid", df1, landcover = TRUE, climate = FALSE),
    "Invalid year.*2000.*2023"
  )

  df2 <- data.frame(gridRef = "J3480", year = 2024)
  expect_error(
    extract_all_values("grid", df2, landcover = TRUE, climate = FALSE),
    "Invalid year.*2000.*2023"
  )
})

# =============================================================================
# INPUT VALIDATION TESTS - SOIL-SPECIFIC
# =============================================================================

test_that("soil validates soilprops parameter", {
  df <- data.frame(gridRef = "J3480")

  expect_error(
    extract_all_values("grid", df, soil = TRUE, soilprops = 123,
                       landcover = FALSE, climate = FALSE),
    "Properties must be a character vector"
  )
})

test_that("soil warns about invalid properties", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480")

  expect_warning(
    extract_all_values("grid", df, soil = TRUE, soilprops = c("clay", "invalid"),
                       landcover = FALSE, climate = FALSE),
    "not available"
  )
})

test_that("soil stops if all properties are invalid", {
  df <- data.frame(gridRef = "J3480")

  expect_warning(
    expect_error(
      extract_all_values("grid", df, soil = TRUE, soilprops = c("fake1", "fake2"),
                         landcover = FALSE, climate = FALSE),
      "None of the provided soil properties are valid"
    ),
    "not available"
  )
})

# =============================================================================
# GRID REFERENCE DETECTION TESTS
# =============================================================================

test_that("Irish grid references are detected correctly", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480")
  result <- extract_all_values("grid", df, soil = TRUE, landcover = FALSE, climate = FALSE)

  expect_equal(result$gridType[1], "Irish Grid")
})

test_that("British grid references are detected correctly", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "NT1565")
  result <- extract_all_values("grid", df, soil = TRUE, landcover = FALSE, climate = FALSE)

  expect_equal(result$gridType[1], "British National Grid")
})

test_that("mixed grid references produce message", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = c("J3480", "NT1565"))

  expect_message(
    extract_all_values("grid", df, soil = TRUE, landcover = FALSE, climate = FALSE),
    "both Irish and British National Grid"
  )
})

test_that("invalid grid references produce warning", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = c("INVALID", "J3480"))

  expect_warning(
    extract_all_values("grid", df, soil = TRUE, landcover = FALSE, climate = FALSE),
    "Coordinates could not be returned"
  )
})

# =============================================================================
# COORDINATE TRANSFORMATION TESTS
# =============================================================================

test_that("Irish Grid coords retain original CRS", {
  skip_if_not(is_online())

  df <- data.frame(X = 334000, Y = 380000)
  result <- extract_all_values("coords", df, crs = "EPSG:29903",
                               soil = TRUE, landcover = FALSE, climate = FALSE)

  expect_equal(result$X_transformed[1], result$X[1])
  expect_equal(result$Y_transformed[1], result$Y[1])
  expect_equal(result$gridType[1], "Irish Grid")
})

test_that("British Grid coords retain original CRS", {
  skip_if_not(is_online())

  df <- data.frame(X = 315000, Y = 665000)
  result <- extract_all_values("coords", df, crs = "EPSG:27700",
                               soil = TRUE, landcover = FALSE, climate = FALSE)

  expect_equal(result$X_transformed[1], result$X[1])
  expect_equal(result$Y_transformed[1], result$Y[1])
  expect_equal(result$gridType[1], "British National Grid")
})

test_that("other CRS coordinates are reprojected with message", {
  skip_if_not(is_online())

  df <- data.frame(X = -6.0, Y = 54.5)

  expect_message(
    result <- extract_all_values("coords", df, crs = "EPSG:4326",
                                 soil = TRUE, landcover = FALSE, climate = FALSE),
    "Reprojecting.*British National Grid"
  )

  expect_false(result$X_transformed[1] == result$X[1])
  expect_equal(result$gridType[1], "British National Grid")
})

# =============================================================================
# SINGLE EXTRACTION TESTS
# =============================================================================

test_that("soil-only extraction works", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480")
  result <- extract_all_values("grid", df,
                               soil = TRUE, soilprops = "clay",
                               landcover = FALSE, climate = FALSE)

  expect_true(is.data.frame(result))
  expect_true(any(grepl("clay_", colnames(result))))
  expect_false(any(grepl("landcover|monthly|annual|seasonal", colnames(result))))
})

test_that("landcover-only extraction works", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020)
  result <- extract_all_values("grid", df,
                               soil = FALSE, landcover = TRUE, climate = FALSE)

  expect_true(is.data.frame(result))
  expect_true(any(grepl("blw|sw|urb", colnames(result))))
  expect_false(any(grepl("clay|sand|monthly|annual", colnames(result))))
})

test_that("climate-only extraction works", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)
  result <- extract_all_values("grid", df,
                               soil = FALSE, landcover = FALSE, climate = TRUE,
                               climtime = "monthly")

  expect_true(is.data.frame(result))
  expect_true(any(grepl("monthly_", colnames(result))))
  expect_false(any(grepl("clay|sand|landcover", colnames(result))))
})

# =============================================================================
# COMBINED EXTRACTION TESTS
# =============================================================================

test_that("soil + landcover extraction works", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020)
  result <- extract_all_values("grid", df,
                               soil = TRUE, soilprops = "clay",
                               landcover = TRUE, climate = FALSE)

  expect_true(any(grepl("clay_", colnames(result))))
  expect_true(any(grepl("blw|sw|urb", colnames(result))))
  expect_false(any(grepl("monthly|annual", colnames(result))))
})

test_that("soil + climate extraction works", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)
  result <- extract_all_values("grid", df,
                               soil = TRUE, soilprops = "sand",
                               landcover = FALSE, climate = TRUE,
                               climtime = "monthly")

  expect_true(any(grepl("sand_", colnames(result))))
  expect_true(any(grepl("monthly_", colnames(result))))
  expect_false(any(grepl("landcover", colnames(result))))
})

test_that("landcover + climate extraction works", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)
  result <- extract_all_values("grid", df,
                               soil = FALSE, landcover = TRUE, climate = TRUE,
                               climtime = "monthly")

  expect_true(any(grepl("blw|sw|urb", colnames(result))))
  expect_true(any(grepl("monthly_", colnames(result))))
  expect_false(any(grepl("clay|sand", colnames(result))))
})

test_that("all three extractions work together", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)
  result <- extract_all_values("grid", df,
                               soil = TRUE, soilprops = "clay",
                               landcover = TRUE, climate = TRUE,
                               climtime = "monthly")

  expect_true(any(grepl("clay_", colnames(result))))
  expect_true(any(grepl("blw|sw|urb", colnames(result))))
  expect_true(any(grepl("monthly_", colnames(result))))
})

# =============================================================================
# OUTPUT STRUCTURE TESTS
# =============================================================================

test_that("output has correct structure for grid type", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)
  result <- extract_all_values("grid", df, climate = TRUE, climtime = "monthly")

  expect_true(is.data.frame(result))
  expect_true("gridRef" %in% colnames(result))
  expect_true("X_transformed" %in% colnames(result))
  expect_true("Y_transformed" %in% colnames(result))
  expect_true("gridType" %in% colnames(result))
})

test_that("output has correct structure for coords type", {
  skip_if_not(is_online())

  df <- data.frame(X = 334000, Y = 380000, year = 2020, month = 6)
  result <- extract_all_values("coords", df, crs = "EPSG:29903",
                               climate = TRUE, climtime = "monthly")

  expect_true(is.data.frame(result))
  expect_true("X" %in% colnames(result))
  expect_true("Y" %in% colnames(result))
  expect_true("X_transformed" %in% colnames(result))
  expect_true("Y_transformed" %in% colnames(result))
  expect_true("gridType" %in% colnames(result))
})

test_that("output preserves row count", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = c("J3480", "J3580", "J3680"))
  result <- extract_all_values("grid", df, soil = TRUE,
                               landcover = FALSE, climate = FALSE)

  expect_equal(nrow(result), nrow(df))
})

test_that("single row input works", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480")
  result <- extract_all_values("grid", df, soil = TRUE,
                               landcover = FALSE, climate = FALSE)

  expect_equal(nrow(result), 1)
})

test_that("multiple rows work", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = rep("J3480", 5))
  result <- extract_all_values("grid", df, soil = TRUE,
                               landcover = FALSE, climate = FALSE)

  expect_equal(nrow(result), 5)
})

# =============================================================================
# CLIMATE PARAMETER TESTS
# =============================================================================

test_that("climate monthly extraction works", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)
  result <- extract_all_values("grid", df,
                               soil = FALSE, landcover = FALSE, climate = TRUE,
                               climvar = "rain", climtime = "monthly")

  expect_true("monthly_rain" %in% colnames(result))
})

test_that("climate seasonal extraction works", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)
  result <- extract_all_values("grid", df,
                               soil = FALSE, landcover = FALSE, climate = TRUE,
                               climvar = "tas", climtime = "seasonal")

  expect_true(any(grepl("(winter|spring|summer|autumn)_tas", colnames(result))))
})

test_that("climate annual extraction works with January start", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)
  result <- extract_all_values("grid", df,
                               soil = FALSE, landcover = FALSE, climate = TRUE,
                               climvar = "rain", climtime = "annual",
                               annualstartmonth = 1)

  expect_true("annual_rain" %in% colnames(result))
  expect_true(is.numeric(result$annual_rain))
})

test_that("climate multiple variables work", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)
  result <- extract_all_values("grid", df,
                               soil = FALSE, landcover = FALSE, climate = TRUE,
                               climvar = c("rain", "tas"), climtime = "monthly")

  expect_true("monthly_rain" %in% colnames(result))
  expect_true("monthly_tas" %in% colnames(result))
})

test_that("climate multiple time aggregations work", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)
  result <- extract_all_values("grid", df,
                               soil = FALSE, landcover = FALSE, climate = TRUE,
                               climvar = "tas",
                               climtime = c("monthly", "annual"),
                               annualstartmonth = 1)

  expect_true("monthly_tas" %in% colnames(result))
  expect_true("annual_tas" %in% colnames(result))
})

# =============================================================================
# SOIL PARAMETER TESTS
# =============================================================================

test_that("soil extracts all properties when soilprops = NULL", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480")
  result <- extract_all_values("grid", df, soil = TRUE, soilprops = NULL,
                               landcover = FALSE, climate = FALSE)

  # Should have many soil property columns
  soil_cols <- grepl("clay|sand|silt|bdod|ocd", colnames(result))
  expect_true(sum(soil_cols) > 10)
})

test_that("soil extracts only specified properties", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480")
  result <- extract_all_values("grid", df, soil = TRUE, soilprops = c("clay", "sand"),
                               landcover = FALSE, climate = FALSE)

  expect_true(any(grepl("clay_", colnames(result))))
  expect_true(any(grepl("sand_", colnames(result))))
  expect_false(any(grepl("silt_", colnames(result))))
})

# =============================================================================
# LANDCOVER PARAMETER TESTS
# =============================================================================

test_that("landcover extracts for different years", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = c("J3480", "J3480"), year = c(2015, 2020))
  result <- extract_all_values("grid", df, soil = FALSE, landcover = TRUE, climate = FALSE)

  expect_equal(nrow(result), 2)
  expect_true(any(grepl("blw|sw|urb", colnames(result))))
})

test_that("landcover shows message for years < 2015", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2010)

  expect_message(
    extract_all_values("grid", df, soil = FALSE, landcover = TRUE, climate = FALSE),
    "aggregated land cover"
  )
})

# =============================================================================
# EDGE CASES AND SPECIAL SCENARIOS
# =============================================================================

test_that("all extractions disabled returns basic structure", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480")
  result <- extract_all_values("grid", df,
                               soil = FALSE, landcover = FALSE, climate = FALSE)

  expect_true(is.data.frame(result))
  expect_true("gridRef" %in% colnames(result))
  expect_true("gridType" %in% colnames(result))
})

test_that("mixed Irish and British grids work", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = c("J3480", "NT1565"), year = 2020, month = 6)

  expect_message(
    result <- extract_all_values("grid", df, climate = TRUE, climtime = "monthly"),
    "both Irish and British"
  )

  expect_equal(nrow(result), 2)
  expect_equal(result$gridType[1], "Irish Grid")
  expect_equal(result$gridType[2], "British National Grid")
})

test_that("UK data shows large file warning", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "NT1565", year = 2020, month = 6)

  expect_message(
    extract_all_values("grid", df, climate = TRUE, climtime = "monthly"),
    "UK data.*large.*timeout"
  )
})

# =============================================================================
# COMPLETE WORKFLOW TESTS
# =============================================================================

test_that("complete workflow with all features works", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = c("J3480", "NT1565"), year = 2020, month = 6)

  result <- extract_all_values("grid", df,
                               soil = TRUE, soilprops = c("clay", "sand"),
                               landcover = TRUE,
                               climate = TRUE,
                               climvar = c("rain", "tas"),
                               climtime = c("monthly", "seasonal"))

  expect_equal(nrow(result), 2)
  expect_true(any(grepl("clay_", colnames(result))))
  expect_true(any(grepl("sand_", colnames(result))))
  expect_true(any(grepl("blw|sw|urb", colnames(result))))
  expect_true("monthly_rain" %in% colnames(result))
  expect_true("monthly_tas" %in% colnames(result))
  expect_true(any(grepl("(winter|spring|summer|autumn)_", colnames(result))))
})
