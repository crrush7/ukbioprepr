#Helper to check if land cover data is accessible
landcover_available <- function() {
  url <- "https://zenodo.org/records/14849882/files/2020ni.tif"
  tmp <- tempfile()
  res <- suppressWarnings(
    try(
      download.file(url, tmp, quiet = TRUE, mode = "wb"),
      silent = TRUE
    )
  )
  if (inherits(res, "try-error") || !identical(res, 0L)) return(FALSE)
  # Check file is actually a tif (not an HTML error page)
  # HTML error pages are tiny (~6KB), real tif files are much larger
  file.size(tmp) > 100000
}
