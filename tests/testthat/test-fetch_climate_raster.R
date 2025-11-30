# test-fetch_climate_raster.R
library(testthat)
library(terra)
library(withr)

# Helper function to clean temp directory
clean_temp_files <- function(pattern = "*.nc") {
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

# Helper to check if we should run slow tests
run_slow_tests <- function() {
  # Run if explicitly requested via environment variable
  # OR if running on CI (detected by CI environment variable)
  nzchar(Sys.getenv("RUN_SLOW_TESTS")) || nzchar(Sys.getenv("CI"))
}

# ============================================================================
# INPUT VALIDATION TESTS - REGION
# ============================================================================

test_that("fetch_climate_raster stops on invalid region", {
  expect_error(
    fetch_climate_raster("scotland", "tas", "2020_01", "2020_12", time = "monthly"),
    "Invalid region"
  )
})

test_that("fetch_climate_raster stops on NULL region", {
  expect_error(
    fetch_climate_raster(NULL, "tas", "2020_01", "2020_12", time = "monthly"),
    "argument is of length zero"
  )
})

test_that("fetch_climate_raster accepts valid regions", {
  # Just test that validation passes (no actual download)
  expect_no_error({
    tryCatch(
      fetch_climate_raster("uk", "tas", "2020_01", "2020_01", time = "monthly"),
      error = function(e) {
        # Only pass if error is NOT about invalid region
        if (!grepl("Invalid region", e$message)) {
          return(TRUE)
        } else {
          stop(e)
        }
      }
    )
  })
})

# ============================================================================
# INPUT VALIDATION TESTS - CLIMATE VARIABLE
# ============================================================================

test_that("fetch_climate_raster stops on invalid climate variable", {
  expect_error(
    fetch_climate_raster("uk", "humidity", "2020_01", "2020_12", time = "monthly"),
    "Invalid climate variable choice"
  )
})

test_that("fetch_climate_raster accepts all valid climate variables", {
  valid_vars <- c("tas", "tasmax", "tasmin", "rain")

  for (var in valid_vars) {
    expect_no_error({
      tryCatch(
        fetch_climate_raster("ni", var, "2020_01", "2020_01", time = "monthly"),
        error = function(e) {
          # Only pass if error is NOT about invalid variable
          if (!grepl("Invalid climate variable", e$message)) {
            return(TRUE)
          } else {
            stop(e)
          }
        }
      )
    })
  }
})

# ============================================================================
# INPUT VALIDATION TESTS - TIME PARAMETER
# ============================================================================

test_that("fetch_climate_raster stops when time is NULL", {
  expect_error(
    fetch_climate_raster("uk", "tas", "2020_01", "2020_12", time = NULL),
    "Invalid time choice"
  )
})

test_that("fetch_climate_raster stops on invalid time choice", {
  expect_error(
    fetch_climate_raster("uk", "tas", "2020_01", "2020_12", time = "daily"),
    "Invalid time choice"
  )
})

test_that("fetch_climate_raster accepts all valid time choices", {
  valid_times <- c("monthly", "seasonal", "annual")

  for (tm in valid_times) {
    expect_no_error({
      tryCatch(
        fetch_climate_raster("ni", "tas", "2020_01", "2020_12", time = tm),
        error = function(e) {
          # Only pass if error is NOT about invalid time
          if (!grepl("Invalid time choice", e$message)) {
            return(TRUE)
          } else {
            stop(e)
          }
        }
      )
    })
  }
})

# ============================================================================
# INPUT VALIDATION TESTS - AGGREGATION
# ============================================================================

test_that("monthly data ignores agg parameter with message", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  expect_message(
    fetch_climate_raster("ni", "tas", "2020_01", "2020_02", time = "monthly", agg = "mean"),
    "Aggregation function is not needed for monthly data"
  )
})

test_that("fetch_climate_raster stops on invalid aggregation", {
  expect_error(
    fetch_climate_raster("uk", "tas", "2020_01", "2020_12", time = "annual", agg = "median"),
    "Invalid aggregation"
  )
})

test_that("fetch_climate_raster accepts valid aggregation functions", {
  valid_aggs <- c("mean", "max", "min", "sum")

  for (agg in valid_aggs) {
    expect_no_error({
      tryCatch(
        fetch_climate_raster("ni", "tas", "2020_01", "2020_12", time = "annual", agg = agg),
        error = function(e) {
          # Only pass if error is NOT about invalid aggregation
          if (!grepl("Invalid aggregation", e$message)) {
            return(TRUE)
          } else {
            stop(e)
          }
        }
      )
    })
  }
})

test_that("default aggregation is set correctly for each variable", {
  # rain should default to sum
  # tas should default to mean
  # tasmax should default to max
  # tasmin should default to min
  # We can't fully test this without downloading, but we can check no error about agg

  expect_no_error({
    tryCatch(
      fetch_climate_raster("ni", "rain", "2020_01", "2020_12", time = "annual"),
      error = function(e) {
        if (!grepl("Invalid aggregation", e$message)) {
          return(TRUE)
        } else {
          stop(e)
        }
      }
    )
  })
})

# ============================================================================
# INPUT VALIDATION TESTS - DATE FORMAT
# ============================================================================

test_that("fetch_climate_raster stops on invalid start date format", {
  expect_error(
    fetch_climate_raster("uk", "tas", "2020-01", "2020_12", time = "monthly"),
    "Please provide valid 'start' and 'end' dates in 'YYYY_MM' format"
  )
})

test_that("fetch_climate_raster stops on invalid end date format", {
  expect_error(
    fetch_climate_raster("uk", "tas", "2020_01", "2020/12", time = "monthly"),
    "Please provide valid 'start' and 'end' dates in 'YYYY_MM' format"
  )
})

test_that("fetch_climate_raster stops on completely wrong date format", {
  expect_error(
    fetch_climate_raster("uk", "tas", "January 2020", "December 2020", time = "monthly"),
    "Please provide valid 'start' and 'end' dates in 'YYYY_MM' format"
  )
})

test_that("fetch_climate_raster stops on numeric dates", {
  expect_error(
    fetch_climate_raster("uk", "tas", 202001, 202012, time = "monthly"),
    "Please provide valid 'start' and 'end' dates in 'YYYY_MM' format"
  )
})

# ============================================================================
# INPUT VALIDATION TESTS - DATE LOGIC
# ============================================================================

test_that("fetch_climate_raster stops when start date is after end date", {
  expect_error(
    fetch_climate_raster("uk", "tas", "2020_12", "2020_01", time = "monthly"),
    "'start' date must be earlier than or equal to 'end' date"
  )
})

test_that("fetch_climate_raster accepts equal start and end dates", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  skip("Skipping slow download test - run manually if needed")

  expect_no_error({
    res <- fetch_climate_raster("ni", "tas", "2020_01", "2020_01", time = "monthly")
  })
})

test_that("fetch_climate_raster stops when start date is before 2000_01", {
  expect_error(
    fetch_climate_raster("uk", "tas", "1999_12", "2020_01", time = "monthly"),
    "Dates must be between '2000_01' and '2023_12'"
  )
})

test_that("fetch_climate_raster stops when end date is after 2023_12", {
  expect_error(
    fetch_climate_raster("uk", "tas", "2020_01", "2024_01", time = "monthly"),
    "Dates must be between '2000_01' and '2023_12'"
  )
})

test_that("fetch_climate_raster accepts boundary dates", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  skip("Skipping slow download test - run manually if needed")

  # Test 2000_01
  expect_no_error({
    res <- fetch_climate_raster("ni", "tas", "2000_01", "2000_01", time = "monthly")
  })

  # Test 2023_12
  expect_no_error({
    res <- fetch_climate_raster("ni", "tas", "2023_12", "2023_12", time = "monthly")
  })
})

# ============================================================================
# DOWNLOAD AND CACHING TESTS
# ============================================================================

test_that("UK region shows timeout message", {
  skip("Skipping UK download test - file is 1.5GB and causes timeouts")
  skip_if_not(is_online(), "No internet - skipping integration test")

  messages <- capture_messages({
    res <- fetch_climate_raster("uk", "tas", "2020_01", "2020_01", time = "monthly")
  })

  expect_true(any(grepl("large files", messages)))
  expect_true(any(grepl("timeout", messages)))
})

test_that("NI region does not show timeout message for first download", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  clean_temp_files("tasmonthlyni.nc")

  messages <- capture_messages({
    res <- fetch_climate_raster("ni", "tas", "2020_01", "2020_01", time = "monthly")
  })

  expect_false(any(grepl("large files", messages)))
  expect_false(any(grepl("timeout", messages)))
})

test_that("fresh download shows download message", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  clean_temp_files("rainmonthlyni.nc")

  messages <- capture_messages({
    res <- fetch_climate_raster("ni", "rain", "2020_01", "2020_01", time = "monthly")
  })

  expect_true(any(grepl("Downloaded:", messages)))
})

test_that("cached file shows cache message", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  # First call may or may not download
  res1 <- fetch_climate_raster("ni", "tas", "2020_01", "2020_01", time = "monthly")

  # Second call should definitely use cache
  messages <- capture_messages({
    res2 <- fetch_climate_raster("ni", "tas", "2020_01", "2020_01", time = "monthly")
  })

  expect_true(any(grepl("using cached version", messages)))
})

test_that("corrupt file triggers redownload", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  td <- tempdir()
  target_file <- file.path(td, "tasminmonthlyni.nc")

  # Create corrupt file
  writeLines("not a raster", target_file)

  messages <- capture_messages({
    res <- fetch_climate_raster("ni", "tasmin", "2020_01", "2020_01", time = "monthly")
  })

  expect_true(any(grepl("corrupt or unreadable", messages)))
  expect_s4_class(res, "SpatRaster")
})

# ============================================================================
# RETURN VALUE TESTS - MONTHLY
# ============================================================================

test_that("monthly data returns SpatRaster with correct layers", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  res <- fetch_climate_raster("ni", "tas", "2020_01", "2020_03", time = "monthly")
  expect_s4_class(res, "SpatRaster")
  expect_equal(nlyr(res), 3)
})

test_that("single month returns SpatRaster with one layer", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  res <- fetch_climate_raster("ni", "tas", "2020_01", "2020_01", time = "monthly")
  expect_s4_class(res, "SpatRaster")
  expect_equal(nlyr(res), 1)
})

test_that("monthly data filters correctly by date range", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  res <- fetch_climate_raster("ni", "tas", "2020_03", "2020_05", time = "monthly")
  expect_s4_class(res, "SpatRaster")
  expect_equal(nlyr(res), 3)
  expect_true(all(grepl("2020_0[345]", names(res))))
})

# ============================================================================
# RETURN VALUE TESTS - SEASONAL
# ============================================================================

test_that("seasonal data returns SpatRaster with seasonal layers", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  res <- fetch_climate_raster("ni", "tas", "2020_01", "2020_12",
                              time = "seasonal", agg = "mean")
  expect_s4_class(res, "SpatRaster")
  # Should have 3-4 complete seasons
  expect_true(nlyr(res) >= 3)
})

test_that("incomplete seasonal data shows warning and errors", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  # Only 2 months - not enough for any complete season
  expect_warning(
    expect_error(
      fetch_climate_raster("ni", "tas", "2020_01", "2020_02",
                           time = "seasonal", agg = "mean"),
      "No complete seasonal data available"
    ),
    "Incomplete"
  )
})

test_that("seasonal data with insufficient range errors", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  expect_error(
    fetch_climate_raster("ni", "tas", "2020_01", "2020_01",
                         time = "seasonal", agg = "mean"),
    "No complete seasonal data available"
  )
})

# ============================================================================
# RETURN VALUE TESTS - ANNUAL
# ============================================================================

test_that("annual data returns SpatRaster with annual layers", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  res <- fetch_climate_raster("ni", "tas", "2020_01", "2021_12",
                              time = "annual", agg = "mean")
  expect_s4_class(res, "SpatRaster")
  expect_equal(nlyr(res), 2)
})

test_that("annual data starting from January works correctly", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  res <- fetch_climate_raster("ni", "tas", "2020_01", "2020_12",
                              time = "annual", agg = "mean")
  expect_s4_class(res, "SpatRaster")
  expect_equal(nlyr(res), 1)
  expect_true(grepl("2020", names(res)))
  expect_false(grepl("2021", names(res)))
})

test_that("annual data starting from non-January works correctly", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  # July to June year
  res <- fetch_climate_raster("ni", "tas", "2020_07", "2021_06",
                              time = "annual", agg = "mean")
  expect_s4_class(res, "SpatRaster")
  expect_equal(nlyr(res), 1)
  # Layer name should indicate the custom year range
  expect_true(grepl("2020", names(res)))
  expect_true(grepl("2021", names(res)))
})

test_that("incomplete annual data shows warning and errors", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  # Only 6 months - not enough for full year
  expect_warning(
    expect_error(
      fetch_climate_raster("ni", "tas", "2020_01", "2020_06",
                           time = "annual", agg = "mean"),
      "No complete annual data available"
    ),
    "Incomplete annual data"
  )
})

test_that("annual data with insufficient range errors", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  expect_error(
    fetch_climate_raster("ni", "tas", "2020_01", "2020_03",
                         time = "annual", agg = "mean"),
    "No complete annual data available"
  )
})

# ============================================================================
# FILENAME GENERATION TESTS
# ============================================================================

test_that("correct filename is generated for each climate variable and region", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  # Just test a couple combinations to avoid downloading all files
  res_ni_tas <- fetch_climate_raster("ni", "tas", "2020_01", "2020_01", time = "monthly")
  expect_true(file.exists(file.path(tempdir(), "tasmonthlyni.nc")))

  res_ni_rain <- fetch_climate_raster("ni", "rain", "2020_01", "2020_01", time = "monthly")
  expect_true(file.exists(file.path(tempdir(), "rainmonthlyni.nc")))
})

# ============================================================================
# DOWNLOAD FAILURE SCENARIOS (MOCKED)
# ============================================================================

test_that("MOCK: download failure stops with error", {
  clean_temp_files()

  with_mocked_bindings(
    download.file = function(...) stop("Network unavailable"),
    .package = "utils",
    {
      expect_error(
        fetch_climate_raster("ni", "tas", "2020_01", "2020_01", time = "monthly"),
        "Download failed"
      )
    }
  )
})

test_that("MOCK: corrupt file triggers redownload attempt", {
  td <- tempdir()
  target_file <- file.path(td, "tasmonthlyni.nc")

  # Create corrupt file
  writeLines("corrupt", target_file)

  with_mocked_bindings(
    download.file = function(...) stop("Network error"),
    .package = "utils",
    {
      expect_error(
        fetch_climate_raster("ni", "tas", "2020_01", "2020_01", time = "monthly"),
        "Download failed again|still invalid"
      )
    }
  )
})

# ============================================================================
# OFFLINE TESTS
# ============================================================================

test_that("OFFLINE: region validation works without network", {
  expect_error(
    fetch_climate_raster("scotland", "tas", "2020_01", "2020_12", time = "monthly"),
    "Invalid region"
  )
})

test_that("OFFLINE: climate variable validation works without network", {
  expect_error(
    fetch_climate_raster("uk", "wind", "2020_01", "2020_12", time = "monthly"),
    "Invalid climate variable"
  )
})

test_that("OFFLINE: time validation works without network", {
  expect_error(
    fetch_climate_raster("uk", "tas", "2020_01", "2020_12", time = NULL),
    "Invalid time choice"
  )

  expect_error(
    fetch_climate_raster("uk", "tas", "2020_01", "2020_12", time = "daily"),
    "Invalid time choice"
  )
})

test_that("OFFLINE: date format validation works without network", {
  expect_error(
    fetch_climate_raster("uk", "tas", "2020-01", "2020_12", time = "monthly"),
    "YYYY_MM' format"
  )
})

test_that("OFFLINE: date range validation works without network", {
  expect_error(
    fetch_climate_raster("uk", "tas", "1999_12", "2020_01", time = "monthly"),
    "between '2000_01' and '2023_12'"
  )

  expect_error(
    fetch_climate_raster("uk", "tas", "2020_12", "2020_01", time = "monthly"),
    "earlier than or equal to 'end'"
  )
})

test_that("OFFLINE: aggregation validation works without network", {
  expect_error(
    fetch_climate_raster("uk", "tas", "2020_01", "2020_12",
                         time = "annual", agg = "median"),
    "Invalid aggregation"
  )
})
