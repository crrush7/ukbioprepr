#function for downloading soil rasters
fetch_soil_raster <- function(reg, prop = NULL) {
  #   reg   - Region: Either UK or NI for UK in epsg:27700 or NI in epsg:29902
  #   prop  - Soil property as a string or character vector, default is all properties

  #define base url for zenodo repository
  baseUrl <- "https://zenodo.org/records/14852620/files/"

  #Validate region input
  if (!reg %in% c('ni', 'uk')) {
    stop(
      'Invalid region input. Choose either "ni" for Northern Ireland or "uk" for United Kingdom'
    )
  }

  #list all soil properties
  allProp <- c(
    "ocd",
    #organic carbon desnity kg m-3
    "bdod",
    #bulk dens of fine earth fraction kg dm-3
    "clay",
    #clay (<0.002) in fine earth %
    "cfvo",
    #vol fraction of coarse fragments (>2mm) %
    "sand",
    #sand (> 0.05mm) in fine earth %
    "silt",
    #silt (0.002-0.05mm) in fine earth %
    "wv0010",
    #vol of water content at -10kPa (10-2cm3 cm-3)*10
    "wv0033",
    #vol of water content at -33kPa (10-2cm3 cm-3)*10
    "wv1500",
    #vol of water content at -1500kPa (10-2cm3 cm-3)*10
    "cec",
    #cation exchange capacity cmol(+)kg-1
    "nitrogen",
    #total nitrogen g kg-1
    "phh2o",
    #pH (H20)
    "soc" #soil organic carbon in fine earth g kg-1
  )
  #if no properties are entered, default is all
  if (is.null(prop)) {
    prop <- allProp
  }

  #Validate property input
  if (!is.character(prop)) {
    stop("Properties must be a character vector (e.g., c('clay', 'sand')) or a single string.")
  }
  invalprop <- setdiff(prop, allProp)
  if (length(invalprop) > 0) {
    warning(
      'The following properties are not available and will be ignored: ',
      paste(invalprop, collapse = ', ')
    )
    #remove any invalid
    prop <- setdiff(prop, invalprop)
  }

  if (!prop %in% allProp) {
    stop(
      "No valid properties were selected. Please choose from: ",
      paste(allProp, collapse = ", ")
    )
  }
  #temp directory that lasts for session
  tempDir <- tempdir()

  #initalise raster list
  rastList <- list()

  #loop through the properties, create file name
  for (p in prop) {
    fileName <- paste0(reg, p, ".tif")
    fileUrl <- paste0(baseUrl, fileName)
    temp <- file.path(tempDir, fileName)

    #download the files
    tryCatch({
      download.file(fileUrl, temp, mode = "wb")
      message("Downloaded: ", fileName)

      #store raster in list
      rastList[[p]] <- rast(temp)
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

