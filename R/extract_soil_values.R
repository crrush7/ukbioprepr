#' Extract values on soil properties from raster files
#'
#' This function extracts values of chosen soil properties from 1km resolution raster files. Raster files are stored in an online repository, therefore this function relies on an internet connection.\cr
#' Raster files cover soil properties from 1999 - 2023. Raster files are either of the whole of the United Kingdom in EPSG:27700, British National Grid, or of Northern Ireland in EPSG:29903, Irish Grid.\cr
#' These data products have been created using original datasets from SoilGrids250m2.0
#' Users may want to consider increasing time out time to allow all relevant data to be downloaded: options(timeout = x)

#' @import terra
#' @import igr
#' @import rnrfa
#' @param type Either 'grid' if using grid references or 'coords' if using co-ordinates.
#' @param df a data frame. \cr
#' If type = ‘grid,’ df must contain a column of grid references 'gridRef'. If type = ‘coords’,  df must contain columns for coordinates 'X' and 'Y'. \cr
#' When type = ‘grid,’ this function will detect whether the input grid references belong to British National Grid (EPSG:27700) or Irish Grid (EPSG:29903). When type = ‘coords’, an additional argument is required to specify the coordinate reference system that ‘X’ and ‘Y’ are projected in.
#' @param crs Required when type = 'coords', the co-ordinate reference system of the X and Y coordinates. Must be in the format of 'EPSG:X'. \cr
#' If crs is not ‘EPSG:29903’ for Irish Grid, or ‘EPSG:27700’ for British National Grid, this function will project the co-ordinates to EPSG:27700 so that extractions can be carried out using the UK wide rasters in EPSG:27700.
#' @param prop Character vector of all soil properties a user wishes to extract values for. The default is all properties. \cr
#' Properties can be the following:\cr
#' "ocd", organic carbon density kg m-3 \cr
#' "bdod", bulk dens of fine earth fraction kg dm-3 \cr
#' "clay", clay (<0.002) in fine earth %, \cr
#' "cfvo", vol fraction of coarse fragments (>2mm) % \cr
#' "sand", sand (> 0.05mm) in fine earth % \cr
#' "silt", silt (0.002-0.05mm) in fine earth % \cr
#' "wv0010", vol of water content at -10kPa (10-2cm3 cm-3)*10 \cr
#' "wv0033", vol of water content at -33kPa (10-2cm3 cm-3)*10 \cr
#' "wv1500", vol of water content at -1500kPa (10-2cm3 cm-3)*10 \cr
#' "cec", cation exchange capacity cmol(+)kg-1 \cr
#' "nitrogen", total nitrogen g kg-1 \cr
#' "phh2o", pH (H20) \cr
#' "soc", #soil organic carbon in fine earth g kg-1 \cr
#' "ocs" #organic carbon stocks kg m-2 \cr
#' All soil properties are extracted at their available depths: 0-5cm, 5-15cm, 15-30cm, 30-60cm, 60-100cm, 100-200cm for all except ocs, organic carbon stocks which is only available at 0-30cm depth.
#' @return A data frame containing grid reference if type = 'grid', or the input coordinates if type = ‘coords.’ \cr
#' Two columns, ‘X_transformed’ and ‘Y_transformed’ detail either the coordinates used for extraction if type = ‘grid’ (the bottom left coordinate of the grid reference), or the projected coordinates if type = ‘coords.’ If type = ‘coords’, and crs = either ‘EPSG:29903’ or ‘EPSG:27700’, these columns will be identical to the input ‘X’ and ‘Y’ coordinates. A column ‘gridType’ will indicate whether extractions have been performed using rasters of the United Kingdom on EPSG:27700 as ‘British National Grid’ or rasters of Northern Ireland on EPSG:29903 as ‘Irish Grid’. \cr
#' Remaining columns are of extracted values and will be named indicating the soil property and the depth, e.g. ocd_D0to5cm, bdod_D100to200cm
#' @export
extract_soil_values <- function(type, df, crs = NULL, prop = NULL) {
  #Checking input is data frame
  if (!is.data.frame(df)) {
    stop("Input must be a data frame containing columns: 'gridRef'.")
  }

  #Check if columns exist
  if (type == 'grid') {
    required <- c("gridRef")
  } else if (type == 'coords') {
    required <- c("X", "Y")
    if (is.null(crs)) {
      stop("CRS must be provided when using type = 'coords'.")
    }
    if (!grepl("^EPSG:", crs)) {
      stop("crs must start with 'EPSG:' .")
    }
  } else {
    stop("Invalid type. Please choose either 'grid' or 'coords'.")
  }
  missing <- setdiff(required, colnames(df))
  if (length(missing) > 0) {
    stop(
      "The following required columns are missing from the input data frame: ",
      paste(missing, collapse = ", ")
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
    "soc",
    #soil organic carbon in fine earth g kg-1,
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
  # Stop if no valid properties remain
  if (length(prop) == 0) {
    stop(
      "No valid properties were selected. Please choose from: ",
      paste(allProp, collapse = ", ")
    )
  }
  #Coordinates
  if (type == 'coords') {
    isIrish <- crs == 'EPSG:29903'
    isBritish <- crs == 'EPSG:27700'

    #Initialize transformed coordinates
    df$X_transformed <- df$X
    df$Y_transformed <- df$Y
    df$gridType <- NA  #Create an empty column for grid type

    #Reproject if not in Irish or British grid
    if (!isIrish & !isBritish) {
      message(
        "Reprojecting your coordinates to British National Grid (EPSG:27700) for extraction."
      )
      coordsVect <- terra::vect(df[, c("X", "Y")], crs = crs)
      transformedCoords <- terra::project(coordsVect, "EPSG:27700")

      df$X_transformed <- terra::geom(transformedCoords)[, "x"]
      df$Y_transformed <- terra::geom(transformedCoords)[, "y"]
      df$gridType <- "British National Grid"  # Assign as British after reprojecting
    } else {
      df$gridType <- ifelse(isIrish, "Irish Grid", "British National Grid")
    }

    #Select output cols
    resultDf <- df[, c("X", "Y", "X_transformed", "Y_transformed", "gridType")]
  }

  if (type == 'grid') {
    #initialise refrences
    refs <- df$gridRef

    #Classify grid references
    isIrish <- sapply(refs, function(ref) {
      grepl("^[A-TV-Z]\\d", ref)  # Irish grid refs start with a single letter A-Z (excluding I) followed by a digit
    })
    isBritish <- sapply(refs, function(ref) {
      grepl("^[A-HJ-Z][A-HJ-Z]", ref)  # British grid refs start with two letters A-H, J-Z (excluding I)
    })

    #Check if both Irish and British grid references are present in dataset
    if (any(isIrish) & any(isBritish)) {
      message("Grid references contain both Irish and British National Grid coordinates.")
    }

    #failedRef
    failedRefs <- character()

    #Convert references to coordinates
    coords <- lapply(seq_along(refs), function(i) {
      ref <- refs[i]
      if (isIrish[i]) {
        result <- tryCatch(
          igr::igr_to_ig(ref),
          error = function(e)
            NULL
        )
        if (!is.null(result) && length(result) == 2) {
          return(
            data.frame(
              gridRef = ref,
              X_transformed = as.numeric(result[1]),
              Y_transformed = as.numeric(result[2]),
              gridType = "Irish Grid"
            )
          )
        }
      } else if (isBritish[i]) {
        result <- tryCatch(
          rnrfa::osg_parse(ref),
          error = function(e)
            NULL
        ) # British grid reference to coords in BNG
        if (!is.null(result) &&
            all(c("easting", "northing") %in% names(result))) {
          return(
            data.frame(
              gridRef = ref,
              X_transformed = as.numeric(result$easting),
              Y_transformed = as.numeric(result$northing),
              gridType = "British National Grid"
            )
          )
        }
      }
      #If we reach this point, log the failed ref
      failedRefs <<- c(failedRefs, ref)
      return(
        data.frame(
          gridRef = ref,
          X_transformed = NA_real_,
          Y_transformed = NA_real_,
          gridType = NA_character_
        )
      )
    })
    #create dataframe
    resultDf <- as.data.frame(do.call(rbind, coords))
    #Issue a warning if any failed
    if (length(failedRefs) > 0) {
      warning(
        "Coordinates could not be returned for the following references: ",
        paste(failedRefs, collapse = ", "),
        ". NA values were returned instead."
      )
    }
  }
  #Download data from Zenodo
  baseUrl <- "https://zenodo.org/records/14973735/files/"

  #Determine what region is needed
  dlUK <- ("British National Grid" %in% resultDf$gridType)
  dlNI <- ("Irish Grid" %in% resultDf$gridType)

  #temp directory
  tempDir <- tempdir()

  #initialise list
  rastList <- list()

  #dl rasters
  for (p in prop) {
    #NI
    if (dlNI) {
      fileNameNI <- paste0("ni", p, ".tif")
      fileUrlNI <- paste0(baseUrl, fileNameNI)
      tempNI <- file.path(tempDir, fileNameNI)

      tryCatch({
        download.file(fileUrlNI, tempNI, mode = "wb")
        message("Downloaded: ", fileNameNI)
        rastList[[paste0("ni_", p)]] <- rast(tempNI)
      }, error = function(e) {
        warning("Failed to download: ", fileNameNI, ". Error: ", e$message)
      })
    }
    #UK
    if (dlUK) {
      fileNameUK <- paste0("uk", p, ".tif")
      fileUrlUK <- paste0(baseUrl, fileNameUK)
      tempUK <- file.path(tempDir, fileNameUK)

      tryCatch({
        download.file(fileUrlUK, tempUK, mode = "wb")
        message("Downloaded: ", fileNameUK)
        rastList[[paste0("uk_", p)]] <- rast(tempUK)
      }, error = function(e) {
        warning("Failed to download: ", fileNameUK, ". Error: ", e$message)
      })
    }
  }

  #Extract soil data
  for (p in prop) {
    if (dlNI & paste0("ni_", p) %in% names(rastList)) {
      niRast <- rastList[[paste0("ni_", p)]]
      nilocs <- which(resultDf$gridType == 'Irish Grid')
      if (length(nilocs) > 0) {
        extractedNI <- terra::extract(niRast, resultDf[nilocs, c("X_transformed", "Y_transformed")])[, -1]
        #some rasters have one layer
        if (is.vector(extractedNI)) {
          extractedNI <- data.frame(extractedNI)
        }
        #Get layer names for columns
        colnames(extractedNI) <- paste0(p, "_", names(niRast))
        #Add extracted values to results df
        resultDf[nilocs, colnames(extractedNI)] <- extractedNI
      }
    }
    if (dlUK & paste0("uk_", p) %in% names(rastList)) {
      ukRast <- rastList[[paste0("uk_", p)]]
      uklocs <- which(resultDf$gridType == 'British National Grid')
      if (length(uklocs) > 0) {
        extractedUK <- terra::extract(ukRast, resultDf[uklocs, c("X_transformed", "Y_transformed")])[, -1]
        #some rasters have one layer
        if (is.vector(extractedUK)) {
          extractedNI <- data.frame(extractedNI)
        }
        #Get layer names for columns
        colnames(extractedUK) <- paste0(p, "_", names(ukRast))
        #Add extracted values to results df
        resultDf[uklocs, colnames(extractedUK)] <- extractedUK
      }
    }
  }

  return(resultDf)
}
