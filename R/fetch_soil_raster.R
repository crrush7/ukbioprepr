#' Download and create soil property raster files
#'
#' This function downloads prepared soil property raster files from Zenodo online repository and therefore requires an internet connection. \cr
#' Raster files are of all of the United Kingdom in British National Grid (EPSG:27700) or Northern Ireland in Irish Grid (EPSG:29903). \cr
#' Output rasters are in line with grid of chosen region. \cr
#' Users can choose from specific soil properties. \cr
#' The output is a list of rasters, with a raster for each property, with layers corresponding to all available depths at 1km resolution. \cr
#' Users may want to consider increasing time out time to allow all relevant data to be downloaded: options(timeout = x). Data products are downloaded to a temporary directory once during each session and are removed when the session ends. \cr
#' @import terra
# Arguments:
#' @param   reg   - Either 'uk' for all of UK in EPSG:27700 British National Grid or 'ni' for Northern Ireland in EPSG:29902 Irish Grid
#' Character vector of all soil properties a user wishes to extract values for. The default is all properties. \cr
#' Properties can be the following:\cr
#' "ocd", organic carbon density kg m-3 \cr
#' "bdod", bulk dens of fine earth fraction kg dm-3 \cr
#' "clay", clay (<0.002) in fine earth (percentage) \cr
#' "cfvo", vol fraction of coarse fragments (>2mm) (percentage) \cr
#' "sand", sand (> 0.05mm) in fine earth (percentage) \cr
#' "silt", silt (0.002-0.05mm) in fine earth (percentage) \cr
#' "wv0010", vol of water content at -10kPa (10-2cm3 cm-3)*10 \cr
#' "wv0033", vol of water content at -33kPa (10-2cm3 cm-3)*10 \cr
#' "wv1500", vol of water content at -1500kPa (10-2cm3 cm-3)*10 \cr
#' "cec", cation exchange capacity cmol(+)kg-1 \cr
#' "nitrogen", total nitrogen g kg-1 \cr
#' "phh2o", pH  \cr
#' "soc", #soil organic carbon in fine earth g kg-1 \cr
#' "ocs" #organic carbon stocks kg m-2 \cr
#' All soil properties are extracted at their available depths: 0-5cm, 5-15cm, 15-30cm, 30-60cm, 60-100cm, 100-200cm for all except ocs, organic carbon stocks which is only available at 0-30cm depth.
#' @return a list of rasters of specified region for specified soil properties. Each raster is for one soil property, at 1km resolution, with layers corresponding to available depths.
#' @export
fetch_soil_raster <- function(reg, prop = NULL) {


  #define base url for zenodo repository
  baseUrl <- "https://zenodo.org/records/14973735/files/"

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
    "soc", #soil organic carbon in fine earth g kg-1
    "ocs" #organic carbon stocks kg m-2
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

  tempDir <- tempdir()
  rastList <- list()

  for (p in prop) {
    fileName <- paste0(reg, p, ".tif")
    fileUrl <- paste0(baseUrl, fileName)
    temp <- file.path(tempDir, fileName)

    # Download if file doesn't exist
    if (!file.exists(temp)) {
      tryCatch({
        download.file(fileUrl, temp, mode = "wb")
        message("Downloaded: ", fileName)
      }, error = function(e) {
        warning("Failed to download: ", fileName, ". Error: ", e$message)
      })
    } else {
      message("File already downloaded during this session: ", fileName)
    }

    # Try to open the raster
    r <- suppressWarnings(try(rast(temp), silent = TRUE))

    # If reading failed or raster is invalid, re-download
    if (inherits(r, "try-error") || nlyr(r) == 0) {
      message("Cached file is corrupt or unreadable. Redownloading: ", fileName)
      file.remove(temp)
      tryCatch({
        download.file(fileUrl, temp, mode = "wb")
        message("Downloaded again: ", fileName)
      }, error = function(e) {
        warning("Redownload failed: ", fileName, ". Error: ", e$message)
      })

      r <- suppressWarnings(try(rast(temp), silent = TRUE))

      if (inherits(r, "try-error") || nlyr(r) == 0) {
        warning("Redownloaded file still invalid: ", fileName)
        next
      }
    }

    rastList[[p]] <- r
  }

  if (length(rastList) == 1) {
    return(rastList[[1]])
  } else if (length(rastList) == 0) {
    stop("No valid rasters could be loaded.")
  } else {
    return(rastList)
  }
  return(rastList)
}

