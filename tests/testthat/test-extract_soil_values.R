# test-extract_soil_values.R
library(testthat)
library(terra)

# Helper to detect offline
is_online <- function() {
  res <- try(utils::download.file("https://zenodo.org/robots.txt",
                                  tempfile(), quiet = TRUE, mode = "wb"),
             silent = TRUE)
  !inherits(res, "try-error")
}

# ============================================================================
# INPUT VALIDATION TESTS - DATA FRAME
# ============================================================================

test_that("function stops if input is not a data frame", {
  expect_error(
    extract_soil_values("grid", c("AB123456")),
    "Input must be a data frame"
  )
})

test_that("function stops if input is NULL", {
  expect_error(
    extract_soil_values("grid", NULL),
    "Input must be a data frame"
  )
})

# ============================================================================
# INPUT VALIDATION TESTS - TYPE PARAMETER
# ============================================================================

test_that("function stops on invalid type", {
  df <- data.frame(gridRef = "J123456")
  expect_error(
    extract_soil_values("invalid", df),
    "Invalid type"
  )
})

test_that("function stops on NULL type", {
  df <- data.frame(gridRef = "J123456")
  expect_error(
    extract_soil_values(NULL, df),
    "argument is of length zero|is.character\\(type\\) is not TRUE"
  )
})

# ============================================================================
# INPUT VALIDATION TESTS - GRID TYPE COLUMNS
# ============================================================================

test_that("grid type requires gridRef column", {
  df <- data.frame(someOtherCol = "test")
  expect_error(
    extract_soil_values("grid", df),
    "required columns are missing.*gridRef"
  )
})

test_that("grid type works with gridRef column", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  df <- data.frame(gridRef = "J326706")
  expect_no_error({
    result <- extract_soil_values("grid", df)
  })
})

# ============================================================================
# INPUT VALIDATION TESTS - COORDS TYPE COLUMNS
# ============================================================================

test_that("coords type requires X column", {
  df <- data.frame(Y = 100000)
  expect_error(
    extract_soil_values("coords", df, crs = "EPSG:27700"),
    "required columns are missing.*X"
  )
})

test_that("coords type requires Y column", {
  df <- data.frame(X = 326000)
  expect_error(
    extract_soil_values("coords", df, crs = "EPSG:27700"),
    "required columns are missing.*Y"
  )
})

test_that("coords type requires crs parameter", {
  df <- data.frame(X = 326000, Y = 370000)
  expect_error(
    extract_soil_values("coords", df),
    "CRS must be provided"
  )
})

test_that("coords type requires crs to start with EPSG:", {
  df <- data.frame(X = 326000, Y = 370000)
  expect_error(
    extract_soil_values("coords", df, crs = "27700"),
    "crs must start with 'EPSG:'"
  )
})

test_that("coords type works with all required parameters", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  df <- data.frame(X = 326000, Y = 370000)
  expect_no_error({
    result <- extract_soil_values("coords", df, crs = "EPSG:29903")
  })
})

# ============================================================================
# INPUT VALIDATION TESTS - PROPERTY PARAMETER
# ============================================================================

test_that("prop parameter must be character", {
  df <- data.frame(gridRef = "J326706")
  expect_error(
    extract_soil_values("grid", df, prop = 123),
    "Properties must be a character vector"
  )
})

test_that("invalid properties generate warning", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  df <- data.frame(gridRef = "J326706")
  expect_warning(
    result <- extract_soil_values("grid", df, prop = c("clay", "invalid_prop")),
    "not available"
  )
})

test_that("all invalid properties result in error", {
  df <- data.frame(gridRef = "J326706")
  expect_warning(
    expect_error(
      extract_soil_values("grid", df, prop = c("fake1", "fake2")),
      "No valid properties were selected"
    ),
    "not available"
  )
})

test_that("NULL prop defaults to all properties", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  df <- data.frame(gridRef = "J326706")
  result <- extract_soil_values("grid", df, prop = NULL)

  # Should have many columns (all soil properties)
  expect_true(ncol(result) > 10)
})

test_that("mix of valid and invalid properties processes valid ones", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  df <- data.frame(gridRef = "J326706")
  expect_warning(
    result <- extract_soil_values("grid", df, prop = c("clay", "notreal", "sand")),
    "not available"
  )

  # Should have columns for clay and sand
  expect_true(any(grepl("clay", colnames(result))))
  expect_true(any(grepl("sand", colnames(result))))
  expect_false(any(grepl("notreal", colnames(result))))
})

test_that("all valid soil properties are recognized", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  all_props <- c("ocd", "bdod", "clay", "cfvo", "sand", "silt",
                 "wv0010", "wv0033", "wv1500", "cec", "nitrogen",
                 "phh2o", "soc", "ocs")

  df <- data.frame(gridRef = "J326706")

  # Should not produce warnings about invalid properties
  expect_no_warning({
    result <- extract_soil_values("grid", df, prop = all_props)
  })
})

# ============================================================================
# GRID REFERENCE DETECTION TESTS
# ============================================================================

test_that("Irish grid references are detected correctly", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  df <- data.frame(gridRef = "J326706")
  result <- extract_soil_values("grid", df, prop = "clay")

  expect_equal(result$gridType[1], "Irish Grid")
})

test_that("British grid references are detected correctly", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  df <- data.frame(gridRef = "NT270700")
  result <- extract_soil_values("grid", df, prop = "clay")

  expect_equal(result$gridType[1], "British National Grid")
})

test_that("mixed Irish and British grid references produce message", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  df <- data.frame(gridRef = c("J326706", "NT270700"))

  expect_message(
    result <- extract_soil_values("grid", df, prop = "clay"),
    "both Irish and British National Grid"
  )
})

test_that("invalid grid references produce warning and NA values", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  df <- data.frame(gridRef = c("INVALID", "J326706"))

  expect_warning(
    result <- extract_soil_values("grid", df, prop = "clay"),
    "Coordinates could not be returned"
  )

  expect_true(is.na(result$X_transformed[1]))
  expect_true(is.na(result$Y_transformed[1]))
})

# ============================================================================
# COORDINATE TRANSFORMATION TESTS
# ============================================================================

test_that("Irish Grid coords retain original CRS", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  df <- data.frame(X = 326000, Y = 370000)
  result <- extract_soil_values("coords", df, crs = "EPSG:29903", prop = "clay")

  expect_equal(result$X_transformed[1], result$X[1])
  expect_equal(result$Y_transformed[1], result$Y[1])
  expect_equal(result$gridType[1], "Irish Grid")
})

test_that("British National Grid coords retain original CRS", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  df <- data.frame(X = 327000, Y = 670000)
  result <- extract_soil_values("coords", df, crs = "EPSG:27700", prop = "clay")

  expect_equal(result$X_transformed[1], result$X[1])
  expect_equal(result$Y_transformed[1], result$Y[1])
  expect_equal(result$gridType[1], "British National Grid")
})

test_that("other CRS coordinates are reprojected with message", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  df <- data.frame(X = -6.0, Y = 54.5)

  expect_message(
    result <- extract_soil_values("coords", df, crs = "EPSG:4326", prop = "clay"),
    "Reprojecting.*British National Grid"
  )

  expect_false(result$X_transformed[1] == result$X[1])
  expect_false(result$Y_transformed[1] == result$Y[1])
  expect_equal(result$gridType[1], "British National Grid")
})

# ============================================================================
# OUTPUT STRUCTURE TESTS - GRID TYPE
# ============================================================================

test_that("grid type output contains correct columns", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  df <- data.frame(gridRef = "J326706")
  result <- extract_soil_values("grid", df, prop = "clay")

  expect_true("gridRef" %in% colnames(result))
  expect_true("X_transformed" %in% colnames(result))
  expect_true("Y_transformed" %in% colnames(result))
  expect_true("gridType" %in% colnames(result))
})

test_that("grid type output has soil property columns with depth info", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  df <- data.frame(gridRef = "J326706")
  result <- extract_soil_values("grid", df, prop = "clay")

  # Should have clay columns with depth information
  soil_cols <- grep("clay_", colnames(result), value = TRUE)
  expect_true(length(soil_cols) > 0)

  # Depth format should be present (e.g., D0to5cm)
  expect_true(any(grepl("D\\d+to\\d+cm", soil_cols)))
})

test_that("grid type output has one row per input row", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  df <- data.frame(gridRef = c("J326706", "J327707", "J328708"))
  result <- extract_soil_values("grid", df, prop = "clay")

  expect_equal(nrow(result), nrow(df))
})

# ============================================================================
# OUTPUT STRUCTURE TESTS - COORDS TYPE
# ============================================================================

test_that("coords type output contains correct columns", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  df <- data.frame(X = 326000, Y = 370000)
  result <- extract_soil_values("coords", df, crs = "EPSG:29903", prop = "clay")

  expect_true("X" %in% colnames(result))
  expect_true("Y" %in% colnames(result))
  expect_true("X_transformed" %in% colnames(result))
  expect_true("Y_transformed" %in% colnames(result))
  expect_true("gridType" %in% colnames(result))
})

test_that("coords type output has soil property columns", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  df <- data.frame(X = 326000, Y = 370000)
  result <- extract_soil_values("coords", df, crs = "EPSG:29903", prop = "sand")

  # Should have sand columns
  soil_cols <- grep("sand_", colnames(result), value = TRUE)
  expect_true(length(soil_cols) > 0)
})

test_that("coords type output has one row per input row", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  df <- data.frame(
    X = c(326000, 327000, 328000),
    Y = c(370000, 371000, 372000)
  )
  result <- extract_soil_values("coords", df, crs = "EPSG:29903", prop = "clay")

  expect_equal(nrow(result), nrow(df))
})

# ============================================================================
# MULTIPLE PROPERTIES TEST
# ============================================================================

test_that("multiple properties extract correctly", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  df <- data.frame(gridRef = "J326706")
  result <- extract_soil_values("grid", df, prop = c("clay", "sand"))

  # Should have columns for both properties
  expect_true(any(grepl("clay_", colnames(result))))
  expect_true(any(grepl("sand_", colnames(result))))
})

test_that("all depth layers are extracted for multi-layer properties", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  df <- data.frame(gridRef = "J326706")
  result <- extract_soil_values("grid", df, prop = "clay")

  # Clay should have multiple depth layers (0-5, 5-15, 15-30, etc.)
  clay_cols <- grep("clay_", colnames(result), value = TRUE)
  expect_true(length(clay_cols) >= 5)  # At least 5 depth layers
})

test_that("ocs has single depth layer", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  df <- data.frame(gridRef = "J326706")
  result <- extract_soil_values("grid", df, prop = "ocs")

  # OCS should have only one depth layer (0-30cm)
  ocs_cols <- grep("ocs_", colnames(result), value = TRUE)
  expect_equal(length(ocs_cols), 1)
})

# ============================================================================
# DOWNLOAD AND CACHING TESTS
# ============================================================================

test_that("raster downloads show download message", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  # Clear a specific file to force download
  temp_file <- file.path(tempdir(), "niocd.tif")
  if (file.exists(temp_file)) file.remove(temp_file)

  df <- data.frame(gridRef = "J326706")

  messages <- capture_messages(
    result <- extract_soil_values("grid", df, prop = "ocd")
  )

  expect_true(any(grepl("Downloaded:|already downloaded", messages)))
})

test_that("cached rasters show cache message", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  df <- data.frame(gridRef = "J326706")

  # First call
  result1 <- extract_soil_values("grid", df, prop = "clay")

  # Second call should use cache
  messages <- capture_messages(
    result2 <- extract_soil_values("grid", df, prop = "clay")
  )

  expect_true(any(grepl("using cached version", messages)))
})

# ============================================================================
# EDGE CASES
# ============================================================================

test_that("single row data frame works", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  df <- data.frame(gridRef = "J326706")
  expect_no_error({
    result <- extract_soil_values("grid", df, prop = "clay")
  })
  expect_equal(nrow(result), 1)
})

test_that("data frame with many rows works", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  df <- data.frame(gridRef = rep("J326706", 10))
  expect_no_error({
    result <- extract_soil_values("grid", df, prop = "clay")
  })
  expect_equal(nrow(result), 10)
})

# ============================================================================
# DATA TYPE TESTS
# ============================================================================

test_that("output is a data frame", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  df <- data.frame(gridRef = "J326706")
  result <- extract_soil_values("grid", df, prop = "clay")

  expect_true(is.data.frame(result))
})

test_that("extracted soil values are numeric", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  df <- data.frame(gridRef = "J326706")
  result <- extract_soil_values("grid", df, prop = "clay")

  # Get soil property columns (exclude metadata columns)
  soil_cols <- grep("clay_", colnames(result), value = TRUE)

  if (length(soil_cols) > 0) {
    expect_true(all(sapply(result[, soil_cols], is.numeric)))
  }
})

# ============================================================================
# REGION-SPECIFIC TESTS
# ============================================================================

test_that("NI grid references use NI rasters", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  df <- data.frame(gridRef = "J326706")

  messages <- capture_messages(
    result <- extract_soil_values("grid", df, prop = "silt")
  )

  # Should download or use NI raster
  expect_true(any(grepl("nisilt", messages)))
})

test_that("UK grid references use UK rasters", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  df <- data.frame(gridRef = "NT270700")

  messages <- capture_messages(
    result <- extract_soil_values("grid", df, prop = "silt")
  )

  # Should download or use UK raster
  expect_true(any(grepl("uksilt", messages)))
})
