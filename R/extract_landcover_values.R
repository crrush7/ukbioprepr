#' Extract values on land cover from raster files
#' This function extracts values of land cover  from 1km resolution raster files over a number of years. Raster files are stored in an online repository, therefore this function relies on an internet connection.
#' Raster files cover land cover percentage summaries from 2000 - 2023. Raster files are either of the whole of the United Kingdom in EPSG:27700, British National Grid, or of Northern Ireland in EPSG:29903, Irish Grid.
#’ There are two sets of land cover rasters: from 2000 - 2023 and 2015 - 2023. From 2000, there are less land cover classes, some of which are aggregated, for example aggregated grasslands class, or upland habitats. Which set of land cover rasters are used in the extraction is determined by the years covered in the user’s input data frame. Users will be warned.
#' Users may want to consider increasing time out time to allow all relevant data to be downloaded: options(timeout = x)
#' @import terra
#' @import igr
#' @import rnrfa
#' @param type - Either 'grid' if using grid references or 'coords' if using co-ordinates. Default is 'gridRef'
#' @param df - a data frame. If type = ‘grid,’ df must contain either a column of grid references 'gridRef'. If type = ‘coords’,  df must contain columns for coordinates 'X' and 'Y'. When type = ‘grid,’ this function will detect whether the input grid references belong to British National Grid (EPSG:27700) or Irish Grid (EPSG:29903). When type = ‘coords’, an additional argument is required to specify the coordinate reference system that ‘X’ and ‘Y’ are projected in. df must also contain a numerical column of yearly values ‘year’. Land cover extractions  are performed at each location and year.
#' @param crs -  Required when type = coords, the crs of the X and Y coordinates. Must be in the format of 'EPSG:X'. If crs is not ‘EPSG:29903’ for Irish Grid, or ‘EPSG:27700’ for British National Grid, this function will project the co-ordinates to EPSG:27700 so that extractions can be carried out using the UK wide rasters in EPSG:27700.
#' @return A data frame containing grid reference if type = 'grid', or the input coordinates if type = ‘coords.’ Two columns, ‘X_transformed’ and ‘Y_transformed’ detail either the coordinates used for extraction if type = ‘grid’ (the bottom left coordinate of the grid reference), or the projected coordinates if type = ‘coords.’ If type = ‘coords’, and crs = either ‘EPSG:29903’ or ‘EPSG:27700’, these columns will be identical to the input ‘X’ and ‘Y’ coordinates. The input ‘year’ column is included. A column ‘gridType’ will indicate whether extractions have been performed using rasters of the United Kingdom on EPSG:27700 as ‘British National Grid’ or rasters of Northern Ireland on EPSG:29903 as ‘Irish Grid’. Remaining columns are of extracted values of each land cover class, corresponding to the year. Values are percentages of cover in a 1km grid square. If ‘year’ is from 2015 onwards, land cover classes are: ‘blw’: broad leaved woodland, ‘cw’: coniferous woodland, ‘ara’: arable land, ‘ig’: improved grassland, ‘ng’: neutral grassland, ‘cg’: calcareous grassland, ‘ag’: acid grassland, ‘fen’: fen, ‘hea’: heather, ‘hgl’: heather grassland, ‘bog’: bog, ‘inr’: inland rock, ‘sw’: saltwater, ‘fw’: freshwater, ‘slr’: supralittoral rock, ‘sls’” supralittoral sediment, ‘lr’: littoral rock, ‘ls’: littoral sediment, ‘sm’: saltmarsh, ‘urb’: urban, ‘sub’: suburban. If ‘year’ spans before 2015, the class column names would instead be: ‘ara’, ‘blw’, ‘cw’, ‘fen’, ‘fw’, ‘lr’, ‘ls’, ‘slr’, ‘sls’, ‘sm’, ‘sub’, ‘sw’ and ‘urb’ as above and two aggregated classes of ‘grassagg’ for grasses and ‘upland’ for upland classes. For more information on these, please read the accompanying documentation.

extract_landcover_values <- function(type = 'grid', df, crs = NULL) {

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
