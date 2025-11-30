# test-fetch_soil_raster.R
library(testthat)
library(terra)
library(withr)

# Helper function to clean temp directory for a specific test
clean_temp_files <- function(pattern = "*.tif") {
  td <- tempdir()
  files <- list.files(td, pattern = pattern, full.names = TRUE)
  if (length(files) > 0) {
    file.remove(files)
  }
}

# Helper to detect offline
is_online <- function() {
  res <- try(utils::download.file("https://zenodo.org/robots.txt",
                                  tempfile(), quiet = TRUE, mode = "wb"),
             silent = TRUE)
  !inherits(res, "try-error")
}

# ============================================================================
# INPUT VALIDATION TESTS
# ============================================================================

test_that("fetch_soil_raster stops on invalid region", {
  expect_error(fetch_soil_raster("scotland"),
               "Invalid region input")
})

test_that("fetch_soil_raster stops if prop is not character", {
  expect_error(fetch_soil_raster("uk", prop = 123),
               "Properties must be a character vector")
})

test_that("NULL region input is caught", {
  expect_error(fetch_soil_raster(reg = NULL), "argument is of length zero")
})

test_that("numeric region input is caught", {
  expect_error(fetch_soil_raster(123), "Invalid region input")
})

test_that("invalid soil properties give warning and are removed", {
  clean_temp_files()
  expect_warning(
    out <- fetch_soil_raster("uk", prop = c("clay", "madeup")),
    "not available"
  )
})

test_that("multiple invalid properties generate warning", {
  expect_warning(
    expect_error(
      fetch_soil_raster("uk", prop = c("fake1", "fake2", "fake3")),
      "No valid rasters could be loaded"
    ),
    "not available"
  )
})

test_that("all invalid properties result in error", {
  expect_warning(
    expect_error(
      fetch_soil_raster("uk", prop = c("fake1", "fake2")),
      "No valid rasters could be loaded"
    ),
    "not available"
  )
})

test_that("empty character vector for prop results in error", {
  expect_error(
    fetch_soil_raster("uk", prop = character(0)),
    "No valid rasters could be loaded"
  )
})

test_that("mix of valid and invalid properties processes valid ones", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  clean_temp_files()

  expect_warning(
    res <- fetch_soil_raster("ni", prop = c("clay", "notreal", "sand")),
    "not available"
  )
  expect_type(res, "list")
  expect_true("clay" %in% names(res))
  expect_true("sand" %in% names(res))
  expect_false("notreal" %in% names(res))
})

# ============================================================================
# DOWNLOAD AND CACHING TESTS
# ============================================================================

test_that("fresh download: downloads a single property and returns SpatRaster", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  clean_temp_files()

  res <- fetch_soil_raster("uk", prop = "clay")
  expect_s4_class(res, "SpatRaster")
})

test_that("fresh download: downloads multiple properties and returns list", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  clean_temp_files()

  res <- fetch_soil_raster("ni", prop = c("clay", "sand"))
  expect_type(res, "list")
  expect_true("clay" %in% names(res))
  expect_true("sand" %in% names(res))
  expect_s4_class(res$clay, "SpatRaster")
  expect_s4_class(res$sand, "SpatRaster")
})

test_that("cached valid file uses cache and returns raster", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  # First call to download
  res1 <- fetch_soil_raster("uk", prop = "silt")

  # Second call should use cache
  expect_message(
    res2 <- fetch_soil_raster("uk", prop = "silt"),
    "File already downloaded during this session"
  )
  expect_s4_class(res2, "SpatRaster")
})

test_that("corrupt cached file triggers re-download", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  td <- tempdir()
  target_file <- file.path(td, "ukbdod.tif")

  # Create a corrupt file
  writeLines("not a raster", target_file)

  expect_message(
    out <- fetch_soil_raster("uk", prop = "bdod"),
    "Cached file is corrupt or unreadable. Redownloading"
  )
  expect_s4_class(out, "SpatRaster")
})

test_that("successful download produces download message", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  clean_temp_files("*ocd.tif")

  expect_message(
    fetch_soil_raster("uk", prop = "ocd"),
    "Downloaded:.*ocd"
  )
})

# ============================================================================
# RETURN VALUE TESTS
# ============================================================================

test_that("single property returns SpatRaster not list", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  clean_temp_files()

  res <- fetch_soil_raster("uk", prop = "clay")
  expect_s4_class(res, "SpatRaster")
  expect_false(is.list(res))
})

test_that("two properties return named list", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  clean_temp_files()

  res <- fetch_soil_raster("ni", prop = c("clay", "sand"))
  expect_type(res, "list")
  expect_equal(length(res), 2)
  expect_named(res, c("clay", "sand"))
})

test_that("list elements are all SpatRaster objects", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  clean_temp_files()

  res <- fetch_soil_raster("uk", prop = c("clay", "sand", "silt"))
  expect_true(all(sapply(res, function(x) inherits(x, "SpatRaster"))))
})

# ============================================================================
# REGION-SPECIFIC TESTS
# ============================================================================

test_that("northern ireland region produces correct filenames", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  clean_temp_files("niclay.tif")

  res <- fetch_soil_raster("ni", prop = "clay")
  expected_file <- file.path(tempdir(), "niclay.tif")
  expect_true(file.exists(expected_file))
})

test_that("uk region produces correct filenames", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  clean_temp_files("uksand.tif")

  res <- fetch_soil_raster("uk", prop = "sand")
  expected_file <- file.path(tempdir(), "uksand.tif")
  expect_true(file.exists(expected_file))
})

# ============================================================================
# PROPERTY LIST VALIDATION
# ============================================================================

test_that("all 14 properties are recognized as valid", {
  all_props <- c("ocd", "bdod", "clay", "cfvo", "sand", "silt",
                 "wv0010", "wv0033", "wv1500", "cec", "nitrogen",
                 "phh2o", "soc", "ocs")

  # Create a simple mock that prevents actual downloads but doesn't error on property validation
  # We just check that no warning about invalid properties is raised
  expect_no_warning({
    result <- tryCatch({
      # This will fail at download/raster stage but shouldn't warn about invalid props
      suppressMessages(fetch_soil_raster("uk", prop = all_props))
    }, error = function(e) {
      # Expected to error at some point, but not due to invalid properties
      return("ok")
    })
  })
})

# ============================================================================
# DEFAULT BEHAVIOR TEST
# ============================================================================

test_that("default prop parameter attempts to load all properties", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  skip("This test is slow - run manually if needed")

  clean_temp_files()
  res <- fetch_soil_raster("ni")
  expect_type(res, "list")
  expect_true(length(res) >= 10)  # Should have most/all properties
})


test_that("OFFLINE: invalid region caught without network", {
  expect_error(fetch_soil_raster("scotland"), "Invalid region input")
})

test_that("OFFLINE: property type validation works without network", {
  expect_error(fetch_soil_raster("uk", prop = 123),
               "Properties must be a character vector")
})
