extract_soil_values <- function(df,
                        type = 'grid',
                        crs = NULL,
                        prop = NULL) {
  #df   -   data frame where one column is 'gridRef' or has x & y coords
  #prop  -   character vector of relevant soil properties
  #type   - either 'grid' or 'coords'

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
    "soc" #soil organic carbon in fine earth g kg-1
  )
  #if no properties are entered, default is all
  if (is.null(prop)) {
    props <- allProp
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


    #Convert references to coordinates
    coords <- lapply(seq_along(refs), function(i) {
      ref <- refs[i]
      if (isIrish[i]) {
        result <- igr::igr_to_ig(ref) # Irish grid to coords
        return(
          data.frame(
            gridRef = ref,
            X_transformed = as.numeric(result[1]),
            Y_transformed = as.numeric(result[2]),
            gridType = "Irish Grid"
          )
        )
      } else if (isBritish[i]) {
        result <- rnrfa::osg_parse(ref) # British grid reference to coords in BNG
        return(
          data.frame(
            gridRef = ref,
            X_transformed = as.numeric(result$easting),
            Y_transformed = as.numeric(result$northing),
            gridType = "British National Grid"
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
  baseUrl <- "https://zenodo.org/records/14852620/files/"

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
        #Get layer names for columns
        colnames(extractedUK) <- paste0(p, "_", names(ukRast))
        #Add extracted values to results df
        resultDf[uklocs, colnames(extractedUK)] <- extractedUK
      }
    }
  }

  return(resultDf)
}
