# test-fetch_landcover_raster.R
library(testthat)
library(terra)
library(withr)

# Helper function to clean temp directory
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
# INPUT VALIDATION TESTS - REGION
# ============================================================================

test_that("fetch_landcover_raster stops on invalid region", {
  expect_error(
    fetch_landcover_raster("scotland", 2020, 2021),
    "Invalid region input"
  )
})

test_that("fetch_landcover_raster stops on NULL region", {
  expect_error(
    fetch_landcover_raster(NULL, 2020, 2021),
    "argument is of length zero"
  )
})

test_that("fetch_landcover_raster stops on numeric region", {
  expect_error(
    fetch_landcover_raster(123, 2020, 2021),
    "Invalid region input"
  )
})

# ============================================================================
# INPUT VALIDATION TESTS - YEARS
# ============================================================================

test_that("fetch_landcover_raster stops if startyear is not numeric", {
  expect_error(
    fetch_landcover_raster("uk", "2020", 2021),
    "Start and end years must be numeric"
  )
})

test_that("fetch_landcover_raster stops if endyear is not numeric", {
  expect_error(
    fetch_landcover_raster("uk", 2020, "2021"),
    "Start and end years must be numeric"
  )
})

test_that("fetch_landcover_raster stops if startyear is before 2000", {
  expect_error(
    fetch_landcover_raster("uk", 1999, 2020),
    "Years must be between 2000 and 2023"
  )
})

test_that("fetch_landcover_raster stops if endyear is after 2023", {
  expect_error(
    fetch_landcover_raster("uk", 2020, 2024),
    "Years must be between 2000 and 2023"
  )
})

test_that("fetch_landcover_raster stops if startyear > endyear", {
  expect_error(
    fetch_landcover_raster("uk", 2021, 2020),
    "Start year must be before or equal to end year"
  )
})

test_that("fetch_landcover_raster accepts equal start and end years", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  clean_temp_files("2020*.tif")

  expect_no_error({
    res <- fetch_landcover_raster("ni", 2020, 2020)
  })
})

test_that("fetch_landcover_raster stops if both years out of range", {
  expect_error(
    fetch_landcover_raster("uk", 1995, 1998),
    "Years must be between 2000 and 2023"
  )
})

# ============================================================================
# AGGREGATION MESSAGE TESTS
# ============================================================================

test_that("years before 2015 trigger aggregation message", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  clean_temp_files("*agg.tif")

  expect_message(
    fetch_landcover_raster("ni", 2010, 2012),
    "Years before 2015 use aggregated land cover classes"
  )
})

test_that("years from 2015 onwards do not trigger aggregation message", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  clean_temp_files("2020*.tif")

  # Should not get the aggregation message
  result <- capture_messages(
    fetch_landcover_raster("ni", 2020, 2020)
  )
  expect_false(any(grepl("aggregated", result)))
})

test_that("mixed years (before and after 2015) trigger aggregation message", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  clean_temp_files()

  expect_message(
    fetch_landcover_raster("ni", 2014, 2016),
    "Years before 2015 use aggregated land cover classes"
  )
})

# ============================================================================
# DOWNLOAD AND CACHING TESTS
# ============================================================================

test_that("fresh download: single year returns SpatRaster", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  clean_temp_files("2021*.tif")

  res <- fetch_landcover_raster("uk", 2021, 2021)
  expect_s4_class(res, "SpatRaster")
})

test_that("fresh download: multiple years return named list", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  clean_temp_files("2020*.tif")
  clean_temp_files("2021*.tif")

  res <- fetch_landcover_raster("ni", 2020, 2021)
  expect_type(res, "list")
  expect_length(res, 2)
  expect_named(res, c("2020", "2021"))
  expect_s4_class(res$`2020`, "SpatRaster")
  expect_s4_class(res$`2021`, "SpatRaster")
})

test_that("fresh download: aggregated files use correct naming", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  clean_temp_files("*niagg.tif")

  res <- fetch_landcover_raster("ni", 2010, 2010)

  # Check that aggregated file was created
  expected_file <- file.path(tempdir(), "2010niagg.tif")
  expect_true(file.exists(expected_file))
})

test_that("fresh download: non-aggregated files use correct naming", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  clean_temp_files("2020ni.tif")

  res <- fetch_landcover_raster("ni", 2020, 2020)

  # Check that non-aggregated file was created
  expected_file <- file.path(tempdir(), "2020ni.tif")
  expect_true(file.exists(expected_file))
})

test_that("cached file uses cache and shows message", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  # First call downloads
  res1 <- fetch_landcover_raster("uk", 2022, 2022)

  # Second call should use cache
  expect_message(
    res2 <- fetch_landcover_raster("uk", 2022, 2022),
    "Using cached version"
  )
  expect_s4_class(res2, "SpatRaster")
})

test_that("successful download produces download message", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  clean_temp_files("2019uk.tif")

  expect_message(
    fetch_landcover_raster("uk", 2019, 2019),
    "Downloaded:.*2019uk"
  )
})

test_that("corrupt cached file triggers re-download", {
  skip_if_not(is_online(), "No internet - skipping integration test")

  td <- tempdir()
  target_file <- file.path(td, "2018uk.tif")

  # Create a corrupt file
  writeLines("not a raster", target_file)

  expect_message(
    out <- fetch_landcover_raster("uk", 2018, 2018),
    "Corrupt or unreadable raster. Redownloading"
  )
  expect_s4_class(out, "SpatRaster")
})

# ============================================================================
# RETURN VALUE TESTS
# ============================================================================

test_that("single year returns SpatRaster not list", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  clean_temp_files("2017*.tif")

  res <- fetch_landcover_raster("uk", 2017, 2017)
  expect_s4_class(res, "SpatRaster")
  expect_false(is.list(res))
})

test_that("two years return named list", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  clean_temp_files("2016*.tif")
  clean_temp_files("2017*.tif")

  res <- fetch_landcover_raster("ni", 2016, 2017)
  expect_type(res, "list")
  expect_equal(length(res), 2)
  expect_named(res, c("2016", "2017"))
})

test_that("multiple years return list with correct year names", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  clean_temp_files("2020*.tif")
  clean_temp_files("2021*.tif")
  clean_temp_files("2022*.tif")

  res <- fetch_landcover_raster("uk", 2020, 2022)
  expect_type(res, "list")
  expect_length(res, 3)
  expect_named(res, c("2020", "2021", "2022"))
})

test_that("list elements are all SpatRaster objects", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  clean_temp_files("2019*.tif")
  clean_temp_files("2020*.tif")
  clean_temp_files("2021*.tif")

  res <- fetch_landcover_raster("uk", 2019, 2021)
  expect_true(all(sapply(res, function(x) inherits(x, "SpatRaster"))))
})

# ============================================================================
# REGION-SPECIFIC TESTS
# ============================================================================

test_that("northern ireland region produces correct filenames", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  clean_temp_files("2015ni.tif")

  res <- fetch_landcover_raster("ni", 2015, 2015)
  expected_file <- file.path(tempdir(), "2015ni.tif")
  expect_true(file.exists(expected_file))
})

test_that("uk region produces correct filenames", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  clean_temp_files("2015uk.tif")

  res <- fetch_landcover_raster("uk", 2015, 2015)
  expected_file <- file.path(tempdir(), "2015uk.tif")
  expect_true(file.exists(expected_file))
})

# ============================================================================
# YEAR RANGE TESTS
# ============================================================================

test_that("boundary years work correctly (2000)", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  clean_temp_files("2000*.tif")

  expect_no_error({
    res <- fetch_landcover_raster("ni", 2000, 2000)
    expect_s4_class(res, "SpatRaster")
  })
})

test_that("boundary years work correctly (2023)", {
  skip_if_not(is_online(), "No internet - skipping integration test")
  clean_temp_files("2023*.tif")

  expect_no_error({
    res <- fetch_landcover_raster("ni", 2023, 2023)
    expect_s4_class(res, "SpatRaster")
  })
})

test_that("full range 2000-2023 can be requested", {
  skip("This test is very slow - run manually if needed")

  clean_temp_files()
  expect_no_error({
    res <- fetch_landcover_raster("ni", 2000, 2023)
    expect_type(res, "list")
    expect_length(res, 24)
  })
})



# ============================================================================
# OFFLINE TESTS
# ============================================================================

test_that("OFFLINE: invalid region caught without network", {
  expect_error(
    fetch_landcover_raster("scotland", 2020, 2021),
    "Invalid region input"
  )
})

test_that("OFFLINE: year validation works without network", {
  expect_error(
    fetch_landcover_raster("uk", 1999, 2020),
    "Years must be between 2000 and 2023"
  )

  expect_error(
    fetch_landcover_raster("uk", 2020, 2024),
    "Years must be between 2000 and 2023"
  )

  expect_error(
    fetch_landcover_raster("uk", 2021, 2020),
    "Start year must be before or equal to end year"
  )
})

test_that("OFFLINE: year type validation works without network", {
  expect_error(
    fetch_landcover_raster("uk", "2020", 2021),
    "Start and end years must be numeric"
  )

  expect_error(
    fetch_landcover_raster("uk", 2020, "2021"),
    "Start and end years must be numeric"
  )
})
