# test-extract_climate_values.R
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
    extract_climate_values("grid", NULL),
    "Input must be a data frame"
  )

  expect_error(
    extract_climate_values("grid", "not a dataframe"),
    "Input must be a data frame"
  )
})

test_that("function validates type parameter", {
  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)

  expect_error(
    extract_climate_values("invalid", df, time = "monthly"),
    "'type' must be either 'grid' or 'coords'"
  )
})

test_that("grid type requires gridRef column", {
  df <- data.frame(year = 2020, month = 6)

  expect_error(
    extract_climate_values("grid", df),
    "required columns are missing.*gridRef"
  )
})

test_that("grid type requires year and month columns", {
  df1 <- data.frame(gridRef = "J3480", year = 2020)
  expect_error(
    extract_climate_values("grid", df1),
    "required columns are missing.*month"
  )

  df2 <- data.frame(gridRef = "J3480", month = 6)
  expect_error(
    extract_climate_values("grid", df2),
    "required columns are missing.*year"
  )
})

test_that("coords type requires X, Y, year, month columns", {
  df1 <- data.frame(Y = 100000, year = 2020, month = 6)
  expect_error(
    extract_climate_values("coords", df1, crs = "EPSG:29903"),
    "required columns are missing.*X"
  )

  df2 <- data.frame(X = 334000, year = 2020, month = 6)
  expect_error(
    extract_climate_values("coords", df2, crs = "EPSG:29903"),
    "required columns are missing.*Y"
  )

  df3 <- data.frame(X = 334000, Y = 380000, year = 2020)
  expect_error(
    extract_climate_values("coords", df3, crs = "EPSG:29903"),
    "required columns are missing.*month"
  )

  df4 <- data.frame(X = 334000, Y = 380000, month = 6)
  expect_error(
    extract_climate_values("coords", df4, crs = "EPSG:29903"),
    "required columns are missing.*year"
  )
})

test_that("coords type requires CRS parameter", {
  df <- data.frame(X = 334000, Y = 380000, year = 2020, month = 6)

  expect_error(
    extract_climate_values("coords", df),
    "CRS must be provided"
  )
})

test_that("coords type requires CRS to start with EPSG:", {
  df <- data.frame(X = 334000, Y = 380000, year = 2020, month = 6)

  expect_error(
    extract_climate_values("coords", df, crs = "29903"),
    "crs must start with 'EPSG:'"
  )
})

# =============================================================================
# INPUT VALIDATION TESTS - CLIMATE-SPECIFIC
# =============================================================================

test_that("time parameter validates correctly", {
  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)

  expect_error(
    extract_climate_values("grid", df, time = "invalid"),
    "Invalid 'time' value"
  )
})

test_that("climvar parameter validates correctly", {
  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)

  expect_error(
    extract_climate_values("grid", df, climvar = "temperature", time = "monthly"),
    "Invalid 'climvar' value"
  )
})

test_that("annualstartmonth is required when time includes annual", {
  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)

  expect_error(
    extract_climate_values("grid", df, time = "annual"),
    "'annualstartmonth' must be provided"
  )
})

test_that("annualstartmonth must be 1-12", {
  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)

  expect_error(
    extract_climate_values("grid", df, time = "annual", annualstartmonth = 0),
    "'annualstartmonth' must be a numeric value between 1 and 12"
  )

  expect_error(
    extract_climate_values("grid", df, time = "annual", annualstartmonth = 13),
    "'annualstartmonth' must be a numeric value between 1 and 12"
  )
})

test_that("annualstartmonth warning when not using annual", {
  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)

  expect_warning(
    extract_climate_values("grid", df, time = "monthly", annualstartmonth = 1),
    "'annualstartmonth' is provided but 'time' does not include 'annual'"
  )
})

test_that("year must be between 1999 and 2023", {
  df1 <- data.frame(gridRef = "J3480", year = 1998, month = 6)
  expect_error(
    extract_climate_values("grid", df1, time = "monthly"),
    "'start' year must be between 1999 and 2023"
  )

  df2 <- data.frame(gridRef = "J3480", year = 2024, month = 6)
  expect_error(
    extract_climate_values("grid", df2, time = "monthly"),
    "year must be between 1999 and 2023"  # Matches either start or end year error
  )
})

test_that("month must be between 1 and 12", {
  df1 <- data.frame(gridRef = "J3480", year = 2020, month = 0)
  expect_error(
    extract_climate_values("grid", df1, time = "monthly"),
    "month.*must only contain numeric values from 1 to 12"
  )

  df2 <- data.frame(gridRef = "J3480", year = 2020, month = 13)
  expect_error(
    extract_climate_values("grid", df2, time = "monthly"),
    "month.*must only contain numeric values from 1 to 12"
  )
})

test_that("time parameter accepts valid values", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)

  expect_no_error({
    extract_climate_values("grid", df, time = "monthly")
  })

  expect_no_error({
    extract_climate_values("grid", df, time = "seasonal")
  })

  expect_no_error({
    extract_climate_values("grid", df, time = "annual", annualstartmonth = 1)
  })
})

# =============================================================================
# GRID REFERENCE DETECTION TESTS
# =============================================================================

test_that("Irish grid references are detected correctly", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)
  result <- extract_climate_values("grid", df, time = "monthly")

  expect_equal(result$gridType[1], "Irish Grid")
})

test_that("British grid references are detected correctly", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "NT1565", year = 2020, month = 6)
  result <- extract_climate_values("grid", df, time = "monthly")

  expect_equal(result$gridType[1], "British National Grid")
})

test_that("mixed grid references produce message", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = c("J3480", "NT1565"), year = 2020, month = 6)

  expect_message(
    extract_climate_values("grid", df, time = "monthly"),
    "both Irish and British"
  )
})

test_that("invalid grid references produce warning", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = c("INVALID", "J3480"), year = 2020, month = 6)

  expect_warning(
    extract_climate_values("grid", df, time = "monthly"),
    "Coordinates could not be returned"
  )
})

# =============================================================================
# COORDINATE TRANSFORMATION TESTS
# =============================================================================

test_that("Irish Grid coordinates retain original CRS", {
  skip_if_not(is_online())

  df <- data.frame(X = 334000, Y = 380000, year = 2020, month = 6)
  result <- extract_climate_values("coords", df, crs = "EPSG:29903", time = "monthly")

  expect_equal(result$X_transformed[1], result$X[1])
  expect_equal(result$Y_transformed[1], result$Y[1])
  expect_equal(result$gridType[1], "Irish Grid")
})

test_that("British Grid coordinates retain original CRS", {
  skip_if_not(is_online())

  df <- data.frame(X = 315000, Y = 665000, year = 2020, month = 6)
  result <- extract_climate_values("coords", df, crs = "EPSG:27700", time = "monthly")

  expect_equal(result$X_transformed[1], result$X[1])
  expect_equal(result$Y_transformed[1], result$Y[1])
  expect_equal(result$gridType[1], "British National Grid")
})

test_that("other CRS coordinates are reprojected with message", {
  skip_if_not(is_online())

  df <- data.frame(X = -6.0, Y = 54.5, year = 2020, month = 6)

  expect_message(
    result <- extract_climate_values("coords", df, crs = "EPSG:4326", time = "monthly"),
    "Reprojecting.*British National Grid"
  )

  expect_false(result$X_transformed[1] == result$X[1])
  expect_equal(result$gridType[1], "British National Grid")
})

# =============================================================================
# MONTHLY EXTRACTION TESTS
# =============================================================================

test_that("monthly extraction works for single climate variable", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)
  result <- extract_climate_values("grid", df, climvar = "rain", time = "monthly")

  expect_true("monthly_rain" %in% colnames(result))
  expect_true(is.numeric(result$monthly_rain))
})

test_that("monthly extraction works for multiple climate variables", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)
  result <- extract_climate_values("grid", df,
                                   climvar = c("rain", "tas", "tasmin", "tasmax"),
                                   time = "monthly")

  expect_true("monthly_rain" %in% colnames(result))
  expect_true("monthly_tas" %in% colnames(result))
  expect_true("monthly_tasmin" %in% colnames(result))
  expect_true("monthly_tasmax" %in% colnames(result))
})

test_that("monthly extraction works with coords type", {
  skip_if_not(is_online())

  df <- data.frame(X = 334000, Y = 380000, year = 2020, month = 6)
  result <- extract_climate_values("coords", df, crs = "EPSG:29903",
                                   climvar = "tas", time = "monthly")

  expect_true("monthly_tas" %in% colnames(result))
})

# =============================================================================
# ANNUAL EXTRACTION TESTS
# =============================================================================

test_that("annual extraction works with January start", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)
  result <- extract_climate_values("grid", df, climvar = "rain",
                                   time = "annual", annualstartmonth = 1)

  expect_true("annual_rain" %in% colnames(result))
  expect_true(is.numeric(result$annual_rain))
})

test_that("annual extraction works with custom start month", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)
  result <- extract_climate_values("grid", df, climvar = "tas",
                                   time = "annual", annualstartmonth = 4)

  expect_true("annual_tas" %in% colnames(result))
})

test_that("annual extraction works with December start", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)
  result <- extract_climate_values("grid", df, climvar = "rain",
                                   time = "annual", annualstartmonth = 12)

  expect_true("annual_rain" %in% colnames(result))
})

test_that("annual extraction works for multiple climate variables", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)
  result <- extract_climate_values("grid", df,
                                   climvar = c("rain", "tas", "tasmin", "tasmax"),
                                   time = "annual", annualstartmonth = 1)

  expect_true("annual_rain" %in% colnames(result))
  expect_true("annual_tas" %in% colnames(result))
  expect_true("annual_tasmin" %in% colnames(result))
  expect_true("annual_tasmax" %in% colnames(result))
})

# =============================================================================
# SEASONAL EXTRACTION TESTS
# =============================================================================

test_that("seasonal extraction works for single variable", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)
  result <- extract_climate_values("grid", df, climvar = "rain", time = "seasonal")

  expect_true(any(grepl("(winter|spring|summer|autumn)_rain", colnames(result))))
})

test_that("seasonal extraction works for multiple variables", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)
  result <- extract_climate_values("grid", df,
                                   climvar = c("rain", "tas"),
                                   time = "seasonal")

  expect_true(any(grepl("_rain", colnames(result))))
  expect_true(any(grepl("_tas", colnames(result))))
})

# =============================================================================
# COMBINED TIME AGGREGATION TESTS
# =============================================================================

test_that("monthly and annual extraction together works", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)
  result <- extract_climate_values("grid", df, climvar = "rain",
                                   time = c("monthly", "annual"), annualstartmonth = 1)

  expect_true("monthly_rain" %in% colnames(result))
  expect_true("annual_rain" %in% colnames(result))
})

test_that("monthly and seasonal extraction together works", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)
  result <- extract_climate_values("grid", df, climvar = "tas",
                                   time = c("monthly", "seasonal"))

  expect_true("monthly_tas" %in% colnames(result))
  expect_true(any(grepl("_tas$", colnames(result))))
})

test_that("annual and seasonal extraction together works", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)
  result <- extract_climate_values("grid", df, climvar = "rain",
                                   time = c("annual", "seasonal"), annualstartmonth = 1)

  expect_true("annual_rain" %in% colnames(result))
  expect_true(any(grepl("_rain$", colnames(result))))
})

# =============================================================================
# OUTPUT STRUCTURE TESTS - GRID TYPE
# =============================================================================

test_that("output has correct columns for grid type", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)
  result <- extract_climate_values("grid", df, time = "monthly")

  expect_true("gridRef" %in% colnames(result))
  expect_true("X_transformed" %in% colnames(result))
  expect_true("Y_transformed" %in% colnames(result))
  expect_true("gridType" %in% colnames(result))
  expect_true("year" %in% colnames(result))
  expect_true("month" %in% colnames(result))
})

test_that("output returns one row per input row", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)
  result <- extract_climate_values("grid", df, time = "monthly")

  expect_equal(nrow(result), nrow(df))
})

# =============================================================================
# OUTPUT STRUCTURE TESTS - COORDS TYPE
# =============================================================================

test_that("output has correct columns for coords type", {
  skip_if_not(is_online())

  df <- data.frame(X = 334000, Y = 380000, year = 2020, month = 6)
  result <- extract_climate_values("coords", df, crs = "EPSG:29903", time = "monthly")

  expect_true("X" %in% colnames(result))
  expect_true("Y" %in% colnames(result))
  expect_true("X_transformed" %in% colnames(result))
  expect_true("Y_transformed" %in% colnames(result))
  expect_true("gridType" %in% colnames(result))
  expect_true("year" %in% colnames(result))
  expect_true("month" %in% colnames(result))
})

# =============================================================================
# EDGE CASES
# =============================================================================

test_that("single row data frame works", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)
  result <- extract_climate_values("grid", df, time = "monthly")

  expect_equal(nrow(result), 1)
})

test_that("multiple rows work", {
  skip_if_not(is_online())

  df <- data.frame(
    gridRef = rep("J3480", 10),
    year = rep(2020, 10),
    month = rep(6, 10)
  )
  result <- extract_climate_values("grid", df, time = "monthly")

  expect_equal(nrow(result), 10)
})

test_that("different months work", {
  skip_if_not(is_online())

  df <- data.frame(
    gridRef = c("J3480", "J3480", "J3480"),
    year = c(2020, 2020, 2020),
    month = c(1, 6, 12)
  )
  result <- extract_climate_values("grid", df, time = "monthly")

  expect_equal(nrow(result), 3)
})

test_that("year boundaries work", {
  skip_if_not(is_online())

  df1 <- data.frame(gridRef = "J3480", year = 1999, month = 6)
  expect_no_error(extract_climate_values("grid", df1, time = "monthly"))

  df2 <- data.frame(gridRef = "J3480", year = 2023, month = 6)
  expect_no_error(extract_climate_values("grid", df2, time = "monthly"))
})

# =============================================================================
# DATA TYPE TESTS
# =============================================================================

test_that("output is a data frame", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)
  result <- extract_climate_values("grid", df, time = "monthly")

  expect_true(is.data.frame(result))
})

test_that("extracted values are numeric", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)
  result <- extract_climate_values("grid", df, climvar = "rain", time = "monthly")

  expect_true(is.numeric(result$monthly_rain))
})

# =============================================================================
# DOWNLOAD AND CACHING TESTS
# =============================================================================

test_that("download messages are present", {
  skip_if_not(is_online())

  # Clear cache first
  unlink(file.path(tempdir(), "ni_climate_2020.nc"))

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)

  expect_message(
    extract_climate_values("grid", df, time = "monthly"),
    "Downloaded|Using cached"
  )
})

test_that("caching works on second call", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)

  # First call
  extract_climate_values("grid", df, time = "monthly")

  # Second call should use cache
  expect_message(
    extract_climate_values("grid", df, time = "monthly"),
    "Using cached"
  )
})

# =============================================================================
# REGION-SPECIFIC TESTS
# =============================================================================

test_that("Irish grid uses NI rasters", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "J3480", year = 2020, month = 6)

  expect_message(
    extract_climate_values("grid", df, time = "monthly"),
    "ni_climate"
  )
})

test_that("British grid shows UK warning", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "NT1565", year = 2020, month = 6)

  expect_message(
    extract_climate_values("grid", df, time = "monthly"),
    "UK data.*large"
  )
})

test_that("British grid uses UK rasters", {
  skip_if_not(is_online())

  df <- data.frame(gridRef = "NT1565", year = 2020, month = 6)

  expect_message(
    extract_climate_values("grid", df, time = "monthly"),
    "uk_climate"
  )
})
