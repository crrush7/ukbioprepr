#' Download land cover raster files
#'
#' This function downloads prepared land cover raster files from Zenodo online repository and therefore requires an internet connection.\cr
#' Raster values are percentage cover for 1km grid squares.\cr
#' Raster files are of all of the United Kingdom (excluding Isle of Man) in British National Grid (EPSG:27700) or Northern Ireland in Irish Grid (EPSG:29903).\cr
#' Output rasters are in line with grid of chosen region.\cr
#' Users indicate the years they require years for \cr
#' The ouput is a list of rasters, with a raster for each year, with layers corresponding to each land cover class at 1km resolution.\cr
#' There are two sets of land cover rasters, 2000 - 2023 and 2015 - 2023, as from 2015 there are more detailed classes\cr
#' If 'year' is from 2015 onwards, land cover classes are: \cr
#' ‘blw’: broad leaved woodland \cr
#' ‘cw’: coniferous woodland \cr
#' ‘ara’: arable land \cr
#' ‘ig’: improved grassland \cr
#' ‘ng’: neutral grassland \cr
#' ‘cg’: calcareous grassland \cr
#' ‘ag’: acid grassland \cr
#' ‘fen’: fen \cr
#' ‘hea’: heather \cr
#' ‘hgl’: heather grassland \cr
#' ‘bog’: bog \cr
#' ‘inr’: inland rock \cr
#' ‘sw’: saltwater \cr
#' ‘fw’: freshwater \cr
#' ‘slr’: supralittoral rock \cr
#' ‘sls’” supralittoral sediment \cr
#' ‘lr’: littoral rock \cr
#' ‘ls’: littoral sediment \cr
#' ‘sm’: saltmarsh \cr
#' ‘urb’: urban \cr
#' ‘sub’: suburban \cr
#' If ‘startyear’ is before 2015, the class column names would instead be: \cr
#' ‘ara’, ‘blw’, ‘cw’, ‘fen’, ‘fw’, ‘lr’, ‘ls’, ‘slr’, ‘sls’, ‘sm’, ‘sub’, ‘sw’ and ‘urb’ as above and two aggregated classes of \cr
#' ‘grassagg’ for grasses and \cr
#' ‘upland’ for upland classes \cr
#' For more information on these, please read the accompanying documentation.\cr
#' @import terra
# Arguments:
#' @param   reg Either 'uk' for all of UK in EPSG:27700 British National Grid or 'ni' for Northern Ireland in EPSG:29902 Irish Grid
#' @param   startyear The start year for required land cover. Must be numeric and from 2000 - 2023. startyear must be before endyear.
#' @param endyear The end year for required land cover. Must be numeric and from 2000 - 2023. endyear must be after startyear.
#' @return a list of rasters of specified region for specified time frame. Each raster is for one year and is at 1km resolution, with layers corresponding to land cover classes.
#' @export
fetch_landcover_raster <- function(reg, startyear, endyear) {
  baseUrl <- "https://zenodo.org/records/14849882/files/"

  # Validate region input
  if (!reg %in% c('ni', 'uk')) {
    stop('Invalid region input. Choose either "ni" for Northern Ireland or "uk" for United Kingdom')
  }

  # Validate year input
  if (!is.numeric(startyear) || !is.numeric(endyear)) {
    stop('Start and end years must be numeric.')
  }
  if (!startyear %in% 2000:2023 || !endyear %in% 2000:2023) {
    stop('Years must be between 2000 and 2023.')
  }
  if (startyear > endyear) {
    stop('Start year must be before or equal to end year.')
  }

  useagg <- startyear < 2015
  if (useagg) {
    message("Years before 2015 use aggregated land cover classes.")
  }

  tempDir <- tempdir()
  rastList <- list()
  yearlist <- startyear:endyear

  for (y in yearlist) {
    fileName <- if (useagg) paste0(y, reg, "agg.tif") else paste0(y, reg, ".tif")
    fileUrl <- paste0(baseUrl, fileName)
    temp <- file.path(tempDir, fileName)

    # Download if missing
    if (!file.exists(temp)) {
      tryCatch({
        download.file(fileUrl, temp, mode = "wb")
        message("Downloaded: ", fileName)
      }, error = function(e) {
        warning("Failed to download: ", fileName, " — ", e$message)
        message("Skipping year ", y)
        next
      })
    } else {
      message("Using cached version: ", fileName)
    }

    # Try to read raster
    r <- suppressWarnings(try(rast(temp), silent = TRUE))

    if (inherits(r, "try-error") || nlyr(r) == 0) {
      message("Corrupt or unreadable raster. Redownloading: ", fileName)
      file.remove(temp)

      tryCatch({
        download.file(fileUrl, temp, mode = "wb")
        message("Redownloaded: ", fileName)
      }, error = function(e) {
        warning("Redownload failed: ", fileName, " — ", e$message)
        message("Skipping year ", y)
        next
      })

      r <- suppressWarnings(try(rast(temp), silent = TRUE))
      if (inherits(r, "try-error") || nlyr(r) == 0) {
        warning("Redownloaded file still invalid: ", fileName)
        message("Skipping year ", y)
        next
      }
    }

    rastList[[as.character(y)]] <- r
  }

  if (length(rastList) == 0) {
    stop("No valid rasters could be loaded.")
  } else if (length(rastList) == 1) {
    return(rastList[[1]])
  } else {
    return(rastList)
  }
}
