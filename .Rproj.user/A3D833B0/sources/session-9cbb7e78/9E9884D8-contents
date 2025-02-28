fetch_landcover_raster <- function(reg, starty, endy) {
  #   reg   - Region: Either UK or NI for UK in epsg:27700 or NI in epsg:29902
  #   starty  - Start year in YYYY as numeric
  #   endy  - End year in YYYY as numeric

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
