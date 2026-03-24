# test-extract_landcover_values.R
library(testthat)
library(terra)

# Helper to detect offline
#is_online <- function() {
 # res <- try(utils::download.file("https://zenodo.org/robots.txt",
  #                                tempfile(), quiet = TRUE, mode = "wb"),
  #           silent = TRUE)
 # !inherits(res, "try-error")
#}

# Helper function to clean temp directory
clean_temp_files <- function(pattern = "*.tif") {
  td <- tempdir()
  files <- list.files(td, pattern = pattern, full.names = TRUE)
  if (length(files) > 0) {
    file.remove(files)
  }
}

# ============================================================================
# INPUT VALIDATION TESTS - DATA FRAME
# ============================================================================

test_that("function stops if input is not a data frame", {
  expect_error(
    extract_landcover_values("grid", c("AB123456", "2020")),
    "Input must be a data frame"
  )
})

test_that("function stops if input is NULL", {
  expect_error(
    extract_landcover_values("grid", NULL),
    "Input must be a data frame"
  )
})

# ============================================================================
# INPUT VALIDATION TESTS - TYPE PARAMETER
# ============================================================================

test_that("function stops on invalid type", {
  df <- data.frame(gridRef = "J123456", year = 2020)
  expect_error(
    extract_landcover_values("invalid", df),
    "Invalid type"
  )
})

test_that("function stops on NULL type", {
  df <- data.frame(gridRef = "J123456", year = 2020)
  expect_error(
    extract_landcover_values(NULL, df),
    "argument is of length zero|is.character\\(type\\) is not TRUE"
  )
})

# ============================================================================
# INPUT VALIDATION TESTS - GRID TYPE COLUMNS
# ============================================================================

test_that("grid type requires gridRef column", {
  df <- data.frame(year = 2020)
  expect_error(
    extract_landcover_values("grid", df),
    "required columns are missing.*gridRef"
  )
})

test_that("grid type requires year column", {
  df <- data.frame(gridRef = "J123456")
  expect_error(
    extract_landcover_values("grid", df),
    "required columns are missing.*year"
  )
})

test_that("grid type works with both required columns", {
  skip_if_not(landcover_available(), "Land cover data temporarily unavailable")

  df <- data.frame(gridRef = "J326706", year = 2020)
  expect_no_error({
    result <- extract_landcover_values("grid", df)
  })
})

# ============================================================================
# INPUT VALIDATION TESTS - COORDS TYPE COLUMNS
# ============================================================================

test_that("coords type requires X column", {
  df <- data.frame(Y = 100000, year = 2020)
  expect_error(
    extract_landcover_values("coords", df, crs = "EPSG:27700"),
    "required columns are missing.*X"
  )
})

test_that("coords type requires Y column", {
  df <- data.frame(X = 326000, year = 2020)
  expect_error(
    extract_landcover_values("coords", df, crs = "EPSG:27700"),
    "required columns are missing.*Y"
  )
})

test_that("coords type requires year column", {
  df <- data.frame(X = 326000, Y = 370000)
  expect_error(
    extract_landcover_values("coords", df, crs = "EPSG:27700"),
    "required columns are missing.*year"
  )
})

test_that("coords type requires crs parameter", {
  df <- data.frame(X = 326000, Y = 370000, year = 2020)
  expect_error(
    extract_landcover_values("coords", df),
    "CRS must be provided"
  )
})

test_that("coords type requires crs to start with EPSG:", {
  df <- data.frame(X = 326000, Y = 370000, year = 2020)
  expect_error(
    extract_landcover_values("coords", df, crs = "27700"),
    "crs must start with 'EPSG:'"
  )
})

test_that("coords type works with all required parameters", {
  skip_if_not(landcover_available(), "Land cover data temporarily unavailable")

  df <- data.frame(X = 326000, Y = 370000, year = 2020)
  expect_no_error({
    result <- extract_landcover_values("coords", df, crs = "EPSG:27700")
  })
})

# ============================================================================
# INPUT VALIDATION TESTS - YEAR VALIDATION
# ============================================================================

test_that("year column must be numeric", {
  df <- data.frame(gridRef = "J326706", year = "2020")
  expect_error(
    extract_landcover_values("grid", df),
    "'year' column must be numeric"
  )
})

test_that("year must be between 2000 and 2023", {
  df1 <- data.frame(gridRef = "J326706", year = 1999)
  expect_error(
    extract_landcover_values("grid", df1),
    "Invalid year.*2000 - 2023"
  )

  df2 <- data.frame(gridRef = "J326706", year = 2024)
  expect_error(
    extract_landcover_values("grid", df2),
    "Invalid year.*2000 - 2023"
  )
})

test_that("year boundary values work correctly (2000)", {
  skip_if_not(landcover_available(), "Land cover data temporarily unavailable")

  df1 <- data.frame(gridRef = "J326706", year = 2000)
  expect_no_error({
    result <- extract_landcover_values("grid", df1)
  })
})

test_that("year boundary values work correctly (2023)", {
  skip_if_not(landcover_available(), "Land cover data temporarily unavailable")

  df2 <- data.frame(gridRef = "J326706", year = 2023)
  expect_no_error({
    result <- extract_landcover_values("grid", df2)
  })
})

# ============================================================================
# INPUT VALIDATION TESTS - MISSING VALUES
# ============================================================================

test_that("function stops on NA in gridRef", {
  df <- data.frame(gridRef = c("J326706", NA), year = c(2020, 2021))
  expect_error(
    extract_landcover_values("grid", df),
    "missing values"
  )
})

test_that("function stops on NA in year", {
  df <- data.frame(gridRef = c("J326706", "J326707"), year = c(2020, NA))
  expect_error(
    extract_landcover_values("grid", df),
    "missing values"
  )
})

# ============================================================================
# GRID REFERENCE DETECTION TESTS
# ============================================================================

test_that("Irish grid references are detected correctly", {
  skip_if_not(landcover_available(), "Land cover data temporarily unavailable")

  df <- data.frame(gridRef = "J326706", year = 2020)
  result <- extract_landcover_values("grid", df)

  expect_equal(result$gridType[1], "Irish Grid")
})

test_that("British grid references are detected correctly", {
  skip_if_not(landcover_available(), "Land cover data temporarily unavailable")

  df <- data.frame(gridRef = "NT270700", year = 2020)
  result <- extract_landcover_values("grid", df)

  expect_equal(result$gridType[1], "British National Grid")
})

test_that("mixed Irish and British grid references produce message", {
  skip_if_not(landcover_available(), "Land cover data temporarily unavailable")

  df <- data.frame(
    gridRef = c("J326706", "NT270700"),
    year = c(2020, 2020)
  )

  expect_message(
    result <- extract_landcover_values("grid", df),
    "both Irish and British National Grid"
  )
})

test_that("invalid grid references produce warning and NA values", {
  skip_if_not(landcover_available(), "Land cover data temporarily unavailable")

  df <- data.frame(gridRef = c("INVALID", "J326706"), year = c(2020, 2020))

  expect_warning(
    result <- extract_landcover_values("grid", df),
    "Coordinates could not be returned"
  )

  expect_true(is.na(result$X_transformed[1]))
  expect_true(is.na(result$Y_transformed[1]))
})

# ============================================================================
# COORDINATE TRANSFORMATION TESTS
# ============================================================================

test_that("Irish Grid coords retain original CRS", {
  skip_if_not(landcover_available(), "Land cover data temporarily unavailable")

  df <- data.frame(X = 326000, Y = 370000, year = 2020)
  result <- extract_landcover_values("coords", df, crs = "EPSG:29903")

  expect_equal(result$X_transformed[1], result$X[1])
  expect_equal(result$Y_transformed[1], result$Y[1])
  expect_equal(result$gridType[1], "Irish Grid")
})

test_that("British National Grid coords retain original CRS", {
  skip_if_not(landcover_available(), "Land cover data temporarily unavailable")

  df <- data.frame(X = 327000, Y = 670000, year = 2020)
  result <- extract_landcover_values("coords", df, crs = "EPSG:27700")

  expect_equal(result$X_transformed[1], result$X[1])
  expect_equal(result$Y_transformed[1], result$Y[1])
  expect_equal(result$gridType[1], "British National Grid")
})

test_that("other CRS coordinates are reprojected with message", {
  skip_if_not(landcover_available(), "Land cover data temporarily unavailable")

  df <- data.frame(X = -6.0, Y = 54.5, year = 2020)

  expect_message(
    result <- extract_landcover_values("coords", df, crs = "EPSG:4326"),
    "Reprojecting.*British National Grid"
  )

  expect_false(result$X_transformed[1] == result$X[1])
  expect_false(result$Y_transformed[1] == result$Y[1])
  expect_equal(result$gridType[1], "British National Grid")
})

# ============================================================================
# AGGREGATION MESSAGE TESTS
# ============================================================================

test_that("years before 2015 trigger aggregation message", {
  skip_if_not(landcover_available(), "Land cover data temporarily unavailable")

  df <- data.frame(gridRef = "J326706", year = 2014)

  expect_message(
    result <- extract_landcover_values("grid", df),
    "aggregated land cover classes"
  )
})

test_that("years from 2015 onwards do not trigger aggregation message", {
  skip_if_not(landcover_available(), "Land cover data temporarily unavailable")

  df <- data.frame(gridRef = "J326706", year = 2015)

  messages <- capture_messages(
    result <- extract_landcover_values("grid", df)
  )

  expect_false(any(grepl("aggregated", messages)))
})

test_that("mixed years spanning 2015 trigger aggregation message", {
  skip_if_not(landcover_available(), "Land cover data temporarily unavailable")

  df <- data.frame(
    gridRef = c("J326706", "J326706"),
    year = c(2014, 2016)
  )

  expect_message(
    result <- extract_landcover_values("grid", df),
    "aggregated land cover classes"
  )
})

# ============================================================================
# OUTPUT STRUCTURE TESTS - GRID TYPE
# ============================================================================

test_that("grid type output contains correct columns", {
  skip_if_not(landcover_available(), "Land cover data temporarily unavailable")

  df <- data.frame(gridRef = "J326706", year = 2020)
  result <- extract_landcover_values("grid", df)

  expect_true("gridRef" %in% colnames(result))
  expect_true("X_transformed" %in% colnames(result))
  expect_true("Y_transformed" %in% colnames(result))
  expect_true("gridType" %in% colnames(result))
  expect_true("year" %in% colnames(result))
})

test_that("grid type output has land cover columns", {
  skip_if_not(landcover_available(), "Land cover data temporarily unavailable")

  df <- data.frame(gridRef = "J326706", year = 2020)
  result <- extract_landcover_values("grid", df)

  # Should have some land cover columns
  land_cover_cols <- setdiff(colnames(result),
                             c("gridRef", "X_transformed", "Y_transformed",
                               "gridType", "year"))
  expect_true(length(land_cover_cols) > 0)
})

test_that("grid type output has one row per input row", {
  skip_if_not(landcover_available(), "Land cover data temporarily unavailable")

  df <- data.frame(
    gridRef = c("J326706", "J327707", "J328708"),
    year = c(2020, 2020, 2021)
  )
  result <- extract_landcover_values("grid", df)

  expect_equal(nrow(result), nrow(df))
})

# ============================================================================
# OUTPUT STRUCTURE TESTS - COORDS TYPE
# ============================================================================

test_that("coords type output contains correct columns", {
  skip_if_not(landcover_available(), "Land cover data temporarily unavailable")

  df <- data.frame(X = 326000, Y = 370000, year = 2020)
  result <- extract_landcover_values("coords", df, crs = "EPSG:29903")

  expect_true("X" %in% colnames(result))
  expect_true("Y" %in% colnames(result))
  expect_true("X_transformed" %in% colnames(result))
  expect_true("Y_transformed" %in% colnames(result))
  expect_true("gridType" %in% colnames(result))
  expect_true("year" %in% colnames(result))
})

test_that("coords type output has land cover columns", {
  skip_if_not(landcover_available(), "Land cover data temporarily unavailable")

  df <- data.frame(X = 326000, Y = 370000, year = 2020)
  result <- extract_landcover_values("coords", df, crs = "EPSG:29903")

  # Should have some land cover columns
  land_cover_cols <- setdiff(colnames(result),
                             c("X", "Y", "X_transformed", "Y_transformed",
                               "gridType", "year"))
  expect_true(length(land_cover_cols) > 0)
})

test_that("coords type output has one row per input row", {
  skip_if_not(landcover_available(), "Land cover data temporarily unavailable")

  df <- data.frame(
    X = c(326000, 327000, 328000),
    Y = c(370000, 371000, 372000),
    year = c(2020, 2020, 2021)
  )
  result <- extract_landcover_values("coords", df, crs = "EPSG:29903")

  expect_equal(nrow(result), nrow(df))
})

# ============================================================================
# LAND COVER CLASS TESTS
# ============================================================================

test_that("2015+ years return detailed land cover classes", {
  skip_if_not(landcover_available(), "Land cover data temporarily unavailable")

  df <- data.frame(gridRef = "J326706", year = 2020)
  result <- extract_landcover_values("grid", df)

  # Check for some expected detailed classes
  expected_classes <- c("blw", "cw", "ara", "ig", "urb", "sub")
  present_classes <- intersect(expected_classes, colnames(result))

  expect_true(length(present_classes) > 0)
})

test_that("pre-2015 years return aggregated land cover classes", {
  skip_if_not(landcover_available(), "Land cover data temporarily unavailable")

  df <- data.frame(gridRef = "J326706", year = 2010)
  result <- extract_landcover_values("grid", df)

  # Check for aggregated classes
  expect_true(any(c("grassagg", "upland") %in% colnames(result)))
})

# ============================================================================
# MULTIPLE YEARS TEST
# ============================================================================

test_that("multiple years extract correctly", {
  skip_if_not(landcover_available(), "Land cover data temporarily unavailable")

  df <- data.frame(
    gridRef = c("J326706", "J326706"),
    year = c(2020, 2021)
  )
  result <- extract_landcover_values("grid", df)

  expect_equal(nrow(result), 2)
  expect_equal(result$year, c(2020, 2021))
})

# ============================================================================
# DOWNLOAD AND CACHING TESTS
# ============================================================================

test_that("raster downloads show download message", {
  skip_if_not(landcover_available(), "Land cover data temporarily unavailable")

  # Clear a specific file to force download
  temp_file <- file.path(tempdir(), "2022ni.tif")
  if (file.exists(temp_file)) file.remove(temp_file)

  df <- data.frame(gridRef = "J326706", year = 2022)

  messages <- capture_messages(
    result <- extract_landcover_values("grid", df)
  )

  expect_true(any(grepl("Downloaded:|already downloaded", messages)))
})

test_that("cached rasters show cache message", {
  skip_if_not(landcover_available(), "Land cover data temporarily unavailable")

  df <- data.frame(gridRef = "J326706", year = 2020)

  # First call
  result1 <- extract_landcover_values("grid", df)

  # Second call should use cache
  messages <- capture_messages(
    result2 <- extract_landcover_values("grid", df)
  )

  expect_true(any(grepl("using cached version", messages)))
})

# ============================================================================
# EDGE CASES
# ============================================================================

test_that("single row data frame works", {
  skip_if_not(landcover_available(), "Land cover data temporarily unavailable")

  df <- data.frame(gridRef = "J326706", year = 2020)
  expect_no_error({
    result <- extract_landcover_values("grid", df)
  })
  expect_equal(nrow(result), 1)
})

test_that("data frame with many rows works", {
  skip_if_not(landcover_available(), "Land cover data temporarily unavailable")

  df <- data.frame(
    gridRef = rep("J326706", 10),
    year = rep(2020, 10)
  )
  expect_no_error({
    result <- extract_landcover_values("grid", df)
  })
  expect_equal(nrow(result), 10)
})

test_that("extraction message is shown", {
  skip_if_not(landcover_available(), "Land cover data temporarily unavailable")

  df <- data.frame(gridRef = "J326706", year = 2020)

  expect_message(
    result <- extract_landcover_values("grid", df),
    "Performing land cover extractions"
  )
})

# ============================================================================
# DATA TYPE TESTS
# ============================================================================

test_that("output is a data frame", {
  skip_if_not(landcover_available(), "Land cover data temporarily unavailable")

  df <- data.frame(gridRef = "J326706", year = 2020)
  result <- extract_landcover_values("grid", df)

  expect_true(is.data.frame(result))
})

test_that("extracted land cover values are numeric", {
  skip_if_not(landcover_available(), "Land cover data temporarily unavailable")

  df <- data.frame(gridRef = "J326706", year = 2020)
  result <- extract_landcover_values("grid", df)

  # Get land cover columns (exclude metadata columns)
  land_cover_cols <- setdiff(colnames(result),
                             c("gridRef", "X", "Y", "X_transformed",
                               "Y_transformed", "gridType", "year"))

  if (length(land_cover_cols) > 0) {
    expect_true(all(sapply(result[, land_cover_cols], is.numeric)))
  }
})
