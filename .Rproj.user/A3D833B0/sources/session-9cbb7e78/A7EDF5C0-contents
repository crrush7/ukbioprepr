extract_landcover_values <- function(df, type = 'grid', crs = NULL) {
  #df must contain a year column and gridRef column
  #Common coordinates for all data points in EPSG:3034 to be part of the final data frame
  #Checking input is data frame
  if (!is.data.frame(df)) {
    stop("Input must be a data frame containing columns: 'gridRef' and 'year'.")
  }

  #Check if columns exist
  if (type == 'grid') {
    required <- c("gridRef", "year")
  } else if (type == 'coords') {
    required <- c("X", "Y", "year")
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

  #Check year is numeric
  if (!is.numeric(df$year)) {
    stop("'year' column must be numeric.")
  }

  #Check for NA
  if (any(is.na(df$gridRef)) | any(is.na(df$year))) {
    stop("Input data frame contains missing values.")
  }

  #list years from df
  yearstot <- sort(unique(df$year))

  #Verify years
  #Validate year input
  if (any(yearstot < 2000 | yearstot > 2023)) {
    stop('Invalid year. Years must be between 2000 - 2023.')
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
    resultDf <- df[, c("X",
                       "Y",
                       "X_transformed",
                       "Y_transformed",
                       "gridType",
                       "year")]
  }
  if (type == 'grid') {
    #initialise refrences
    refs <- df$gridRef
    years <- df$year
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


    #Convert references to coordinates
    coords <- lapply(seq_along(refs), function(i) {
      ref <- refs[i]
      year <- years[i]
      if (isIrish[i]) {
        result <- igr::igr_to_ig(ref) # Irish grid to coords
        return(
          data.frame(
            gridRef = ref,
            X_transformed = as.numeric(result[1]),
            Y_transformed = as.numeric(result[2]),
            gridType = "Irish Grid",
            year = year
          )
        )
      } else if (isBritish[i]) {
        result <- rnrfa::osg_parse(ref) # British grid reference to coords in BNG
        return(
          data.frame(
            gridRef = ref,
            X_transformed = as.numeric(result$easting),
            Y_transformed = as.numeric(result$northing),
            gridType = "British National Grid",
            year = year
          )
        )
      } else {
        stop("Unrecognized grid reference: ", ref)
      }
    })
    #create dataframe
    resultDf <- as.data.frame(do.call(rbind, coords))
  }

  #Download data from Zenodo
  baseUrl <- "https://zenodo.org/records/14849882/files/"

  #Determine what region is needed
  dlNI <- "Irish Grid" %in% resultDf$gridType
  dlUK <- "British National Grid" %in% resultDf$gridType

  #Determine what rasters to use (modern or aggregated)
  dlAgg <- any(yearstot < 2015)
  if (dlAgg) {
    message(
      "Because your data frame contains years earlier than 2015, your land cover extractions
            will be based on aggregated land cover classes. For more information, see guidance notes."
    )
  }
  #temp directory
  tempDir <- tempdir()

  #dl rasters
  dlRaster <- function(region, year, isAgg) {
    suffix <- ifelse(isAgg, "agg", "")
    fileName <- paste0(year, region, suffix, ".tif")
    fileUrl <- paste0(baseUrl, fileName)
    tempPath <- file.path(tempDir, fileName)

    tryCatch({
      download.file(fileUrl, tempPath, mode = "wb")
      message("Downloaded: ", fileName)
      r <- terra::rast(tempPath)
    }, error = function(e) {
      warning("Failed to download: ", fileName, ". Error: ", e$message)
    })
    return(r)
  }

  #initialise list
  rastList <- list()
  for (y in yearstot) {
    if (any(dlNI))
      rastList[[paste0("ni", "_", y)]] <- dlRaster("ni", y, dlAgg)
    if (any(dlUK))
      rastList[[paste0("uk", "_", y)]] <- dlRaster("uk", y, dlAgg)
  }
  #Extract land cover data per coordinate based on year
  message("Performing land cover extractions. Please be patient.")
  for (i in seq_len(nrow(resultDf))) {
    y <- resultDf$year[i]  #Year associated with the coordinate
    #NI
    if (resultDf$gridType[i] == "Irish Grid" &&
        paste0("ni_", y) %in% names(rastList)) {
      niRast <- rastList[[paste0("ni_", y)]]
      extractedNI <- terra::extract(niRast, resultDf[i, c("X_transformed", "Y_transformed")])[, -1]  #Remove cell ID

      if (!is.null(extractedNI) && ncol(extractedNI) > 0) {
        cover <- names(niRast)
        colnames(extractedNI) <- cover
        resultDf[i, cover] <- extractedNI
      }
    }

    #UK
    if (resultDf$gridType[i] == "British National Grid" &&
        paste0("uk_", y) %in% names(rastList)) {
      ukRast <- rastList[[paste0("uk_", y)]]
      extractedUK <- terra::extract(ukRast, resultDf[i, c("X_transformed", "Y_transformed")])[, -1]  #Remove cell ID

      if (!is.null(extractedUK) && ncol(extractedUK) > 0) {
        cover <- names(ukRast)
        colnames(extractedUK) <- cover
        resultDf[i, cover] <- extractedUK
      }
    }
  }

  return(resultDf)
}
