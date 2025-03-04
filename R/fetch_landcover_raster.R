#' Download land cover raster files
#'
#' This function downloads prepared land cover raster files from Zenodo online repository
#' Raster values are percentage cover for 1km grid squares.
#' Raster files are of all of the United Kingdom (excluding Isle of Man) in British National Grid (EPSG:27700) or Northern Ireland in Irish Grid (EPSG:29903)
#' Output rasters are in line with grid of chosen region
#' Users indicate the years they require years for.
#' The ouput is a list of rasters, with a raster for each year, with layers corresponding to each land cover class at 1km resolution.
#' There are two sets of land cover rasters, 2000 - 2023 and 2015 - 2023, as from 2015 there are more detailled classes.
#' If ‘starty’ is from 2015 onwards, land cover classes are: ‘blw’: broad leaved woodland, ‘cw’: coniferous woodland, ‘ara’: arable land, ‘ig’: improved grassland, ‘ng’: neutral grassland, ‘cg’: calcareous grassland, ‘ag’: acid grassland, ‘fen’: fen, ‘hea’: heather, ‘hgl’: heather grassland, ‘bog’: bog, ‘inr’: inland rock, ‘sw’: saltwater, ‘fw’: freshwater, ‘slr’: supralittoral rock, ‘sls’” supralittoral sediment, ‘lr’: littoral rock, ‘ls’: littoral sediment, ‘sm’: saltmarsh, ‘urb’: urban, ‘sub’: suburban. If ‘starty’ is before 2015, the class names would instead be: ‘ara’, ‘blw’, ‘cw’, ‘fen’, ‘fw’, ‘lr’, ‘ls’, ‘slr’, ‘sls’, ‘sm’, ‘sub’, ‘sw’ and ‘urb’ as above and two aggregated classes of ‘grassagg’ for grasses and ‘upland’ for upland classes. For more information on these, please read the accompanying documentation.
#' @import terra
# Arguments:
#' @param   reg   - Either 'uk' for all of UK in EPSG:27700 British National Grid or 'ni' for Northern Ireland in EPSG:29902 Irish Grid
#' @param   starty   - The start year for required land cover. Must be numeric and from 2000 - 2023. starty must be before endy.
#' @param endy  - The end year for required land cover. Must be numeric and from 2000 - 2023. endy must be after starty.
#' @return a list of rasters of specified region for specified time frame. Each raster is for one year and is at 1km resolution, with layers corresponding to land cover classes.

fetch_landcover_raster <- function(reg, starty, endy) {

  #define base url for zenodo repository
  baseUrl <- "https://zenodo.org/records/14849882/files/"

  #Validate region input
  if (!reg %in% c('ni', 'uk')) {
    stop(
      'Invalid region input. Choose either "ni" for Northern Ireland or "uk" for United Kingdom'
    )
  }

  #Validate year input
  if (!starty %in% 2000:2023 || !endy %in% 2000:2023) {
    stop('Invalid year input. Years must be between 2000 - 2023.')
  }
  if (starty > endy) {
    stop('Invalid year input. Start year must be before end year.')
  }
  if (!is.numeric(starty) || !is.numeric(endy)) {
    stop('Invalid year input. Start year and end year must be numeric.')
  }

  #warn user about aggregated classes
  useagg <- starty < 2015
  if (useagg) {
    warning(
      'As your start year is before 2015, the output rasters will have aggregated land cover classes.
            Full land cover classes are available for years 2015 - 2023.'
    )
  }
  #temp directory that lasts for session
  tempDir <- tempdir()

  #initalise raster list
  rastList <- list()
  #create year list
  yearlist <- starty:endy

  #loop through the years, create file name depending on years
  for (y in yearlist) {
    fileName <- if (useagg) {
      paste0(y, reg, "agg.tif")
    } else {
      paste0(y, reg, ".tif")
    }
    fileUrl <- paste0(baseUrl, fileName)
    temp <- file.path(tempDir, fileName)

    #download the files
    tryCatch({
      download.file(fileUrl, temp, mode = "wb")
      message("Downloaded: ", fileName)

      #store raster in list
      rastList[[as.character(y)]] <- rast(temp)
    }, error = function(e) {
      warning("Failed to download: ", fileName, ". Error: ", e$message)
    })
  }
  #return raster if only one in list
  if (length(rastList) == 1) {
    return(rastList[[1]])
  }
  return(rastList)
}
