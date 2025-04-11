#' Function for extracting data from all available environmental variable rasters
#'
#' This function extracts values of climate, soil and land cover from 1km resolution raster files over a specified time period. Raster files are stored in an online repository, therefore this function relies on an internet connection.\cr
#' Raster files cover climate variables from 1999 - 2023, soil properties at a range of depths and % cover of land classes between 2000 - 2023. \cr
#' Raster files are either of the whole of the United Kingdom in EPSG:27700, British National Grid, or of Northern Ireland in EPSG:29903, Irish Grid.
#' These raster data products have been created using original datasets from SoilGrids250m 2.0 for soils, HadUK Grid by UK Met Office for climate, and UK's Centre for Ecology and Hydrology for land cover.
#' Users may want to consider increasing time out time to allow all relevant data to be downloaded: options(timeout = x)
#' @import terra
#' @import igr
#' @import rnrfa
#' @param type Either 'grid' if using grid references or 'coords' if using co-ordinates.
#' @param df a data frame. \cr
#' If type = ‘grid,’ df must contain a column of grid references 'gridRef'. If type = ‘coords’,  df must contain columns for coordinates 'X' and 'Y'. \cr
#' When type = ‘grid,’ this function will detect whether the input grid references belong to British National Grid (EPSG:27700) or Irish Grid (EPSG:29903). When type = ‘coords’, an additional argument is required to specify the coordinate reference system that ‘X’ and ‘Y’ are projected in.\cr
#' When landcover = TRUE, a 'year' column is required. \cr
#' When climate = TRUE, a 'year' and 'month' column is required. \cr
#' @param crs Required when type = 'coords', the co-ordinate reference system of the X and Y coordinates. Must be in the format of 'EPSG:X'. \cr
#' If crs is not ‘EPSG:29903’ for Irish Grid, or ‘EPSG:27700’ for British National Grid, this function will project the co-ordinates to EPSG:27700 so that extractions can be carried out using the UK wide rasters in EPSG:27700.
#' @param soil Logical. Default is True. Determines whether the user would like extractions for soil properties.
#' @param soilprops Character vector of all soil properties a user wishes to extract values for. The default is all properties. \cr
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
#' @param landcover Logical. Default is True. Determines whether the user would like extractions for land cover. \cr
#' There are two sets of land cover rasters: from 2000 - 2023 and 2015 - 2023. \cr
#' From 2000, there are less land cover classes, than between 2015 - 2023, given the original datasets. Some classes are aggregated, for example aggregated grasslands class, or upland habitats. Which set of land cover rasters are used in the extraction is determined by the years covered in the user’s input data frame. Users will be warned.
#' @param climate Logical. Default is TRUE. Determines whether user would like extractions for climate variables.
#' @param climvar Character vector of climate variables a user wishes to extract data for. \cr
#' Can choose from ‘rain’ for rainfall, ‘tas’ for mean temperature, ‘tasmin’ for minimum temperature or ‘tasmax’ for maximum temperature. The default is all four variables. Rainfall is measured in millimetres and the three temperature variables are measured in degrees Celsius.
#' @param climtime Character vector of time aggregates for values. Can include ‘monthly’, ‘seasonal’ and ‘annual’. There is no default and must be chosen. \cr
#' If choosing ‘monthly’, each value will be extracted for each location at its month and year. \cr
#' If choosing ‘seasonal’, a value for each season will be extracted for the season that the month and year is in and the three seasons previous. \cr
#' For example, if your location has a year of 2015 and a month 7 (July), your seasonal values will be for Summer 2015, Spring 2015, Winter 2014/15 and Autumn 2014. \cr
#' Seasons are standardised as follows: \cr
#' Winter = December, January, February \cr
#' Spring = March, April, May \cr
#' Summer = June, July, August \cr
#' Autumn = September, October, November\cr
#' There are warnings for any incomplete seasons. \cr
#' If choosing ‘annual’, the annual values generated from annualstartmonth, for example, if annualstartmonth = 4, each annual value will be calculated from April to the following March each year. \cr
#' When choosing ‘seasonal’ and / or ‘annual’, the values of the variables are aggregated in the following way: \cr
#' ‘rain’ = total rainfall during time frame \cr
#' ‘tas’ = mean temperature during time frame \cr
#' ‘tasmin’ = minimum temperature during time frame \cr
#' ‘tasmax’ = maximum temperature during time frame \cr
#' If you are interested in alternative aggregations, for example the mean rainfall during a specified time frame, you can generate raster files using the fetch_climate_raster function.
#' @param annualstartmonth Numeric. Required if 'climtime' includes 'annual.' Must be an integer between 1 and 12, indicating the start month of the annual time period.
#' @return A data frame containing grid reference if type = 'grid', or the input coordinates if type = ‘coords.’ The original 'month' and 'year' columns are included if originally input. \cr
#' Two columns, ‘X_transformed’ and ‘Y_transformed’ detail either the coordinates used for extraction if type = ‘grid’ (the bottom left coordinate of the grid reference), or the projected coordinates if type = ‘coords.’ If type = ‘coords’, and crs = either ‘EPSG:29903’ or ‘EPSG:27700’, these columns will be identical to the input ‘X’ and ‘Y’ coordinates. \cr
#' If soil = true, columns include extracted values and will be named indicating the soil property and the depth, e.g. ocd_D0to5cm, bdod_D100to200cm. \cr
#' If landcover = true, columns will indicate the % cover of each land cover class in that 1km grid square.
#' #' If 'year' is from 2015 onwards, land cover classes are: \cr
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
#' If ‘year’ spans before 2015, the class column names would instead be: \cr
#' ‘ara’, ‘blw’, ‘cw’, ‘fen’, ‘fw’, ‘lr’, ‘ls’, ‘slr’, ‘sls’, ‘sm’, ‘sub’, ‘sw’ and ‘urb’ as above and two aggregated classes of \cr
#' ‘grassagg’ for grasses and \cr
#' ‘upland’ for upland classes \cr
#' For more information on these, please read the accompanying documentation. \cr
#' If climate = TRUE, columns will be available for the variable and time period e.g. monthly_tas, winter_rain, annual_tasmin.
#' @export

extract_all_values <- function(type,
                               df,
                               crs = NULL,
                               soil = TRUE,
                               soilprops = NULL,
                               landcover = TRUE,
                               climate = TRUE,
                               climvar = c('tas', 'tasmin', 'tasmax', 'rain'),
                               climtime = c('monthly', 'seasonal', 'annual'),
                               annualstartmonth = NULL) {
  #Checking inputs
  #Checking input is data frame
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }
  #Check if columns exist based on extraction type
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
  #Climate specific input checks
  if (climate == TRUE) {
    #Check if columns exist
    required <- c("year", "month")
    missing <- setdiff(required, colnames(df))
    if (length(missing) > 0) {
      stop(
        "The following required columns are missing from the input data frame: ",
        paste(missing, collapse = ", ")
      )
    }
    #Validate 'time' input
    valid_time_options <- c("annual", "seasonal", "monthly")
    if (!all(climtime %in% valid_time_options)) {
      invalid_time <- setdiff(climtime, valid_time_options)
      stop(
        paste(
          "Error: Invalid 'climtime' value(s):",
          paste(invalid_time, collapse = ", "),
          ". Must be one or more of 'annual', 'seasonal', or 'monthly'."
        )
      )
    }

    #Validate 'climvar' input
    valid_climvars <- c("rain", "tas", "tasmin", "tasmax")
    if (!all(climvar %in% valid_climvars)) {
      invalid_climvar <- setdiff(climvar, valid_climvars)
      stop(
        paste(
          "Error: Invalid 'climvar' value(s):",
          paste(invalid_climvar, collapse = ", "),
          ". Must be one or more of 'rain', 'tas', 'tasmin', 'tasmax'."
        )
      )
    }
    #Validate 'annualstartmonth' requirement and range
    if ("annual" %in% climtime) {
      if (is.null(annualstartmonth)) {
        stop(
          "Error: 'annualstartmonth' must be provided when 'climtime' includes 'annual'."
        )
      }
      if (!is.numeric(annualstartmonth) ||
          annualstartmonth < 1 || annualstartmonth > 12) {
        stop("Error: 'annualstartmonth' must be a numeric value between 1 and 12.")
      }
    } else if (!is.null(annualstartmonth)) {
      warning(
        "'annualstartmonth' is provided but 'climtime' does not include 'annual'. It will be ignored."
      )
    }
  }
  #Land cover specific warnings
  if (landcover == TRUE) {
    #check that year column exists
    required <- c("year")
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
    #list years from df
    yearstot <- sort(unique(df$year))

    #Verify years
    #Validate year input
    if (any(yearstot < 2000 | yearstot > 2023)) {
      stop('Invalid year. Years must be between 2000 - 2023.')
    }
  }
  #no soil specific warnings as all covered and all properties will be downloaded
  if (soil == TRUE) {
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
    if (is.null(soilprops)) {
      soilprops <- allProp
    }
    #Validate property input
    if (!is.character(soilprops)) {
      stop(
        "Properties must be a character vector (e.g., c('clay', 'sand')) or a single string."
      )
    }
    invalprop <- setdiff(soilprops, allProp)
    if (length(invalprop) > 0) {
      warning(
        'The following properties are not available and will be ignored: ',
        paste(invalprop, collapse = ', ')
      )
      #remove any invalid
      prop <- setdiff(soilprops, invalprop)
    }
    # Stop if no valid properties remain
    if (length(soilprops) == 0) {
      stop(
        "No valid properties were selected. Please choose from: ",
        paste(allProp, collapse = ", ")
      )
    }
  }
  #Coordinates
  if (type == 'coords') {
    message('type is coords')
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
    if (landcover) {
      resultDf <- df[, c("X",
                         "Y",
                         "X_transformed",
                         "Y_transformed",
                         "gridType",
                         "year")]
    }
    if (climate) {
      resultDf <- df[, c("X",
                         "Y",
                         "X_transformed",
                         "Y_transformed",
                         "gridType",
                         "year",
                         "month")]
    } else {
      resultDf <- df[, c("X", "Y", "X_transformed", "Y_transformed", "gridType")]
    }
  }
  #if type == grid references
  if (type == 'grid') {
    #initialise refrences
    refs <- df$gridRef
    years <- df$year
    months <- df$month
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
      year <- years[i]
      month <- months[i]

      if (isIrish[i]) {
        result <- tryCatch(
          igr::igr_to_ig(ref),
          error = function(e)
            NULL
        )
        if (!is.null(result) && length(result) == 2) {
          if (landcover && climate) {
            return(
              data.frame(
                gridRef = ref,
                X_transformed = as.numeric(result[1]),
                Y_transformed = as.numeric(result[2]),
                gridType = "Irish Grid",
                year = year,
                month = month
              )
            )
          }
          if (landcover) {
            return(
              data.frame(
                gridRef = ref,
                X_transformed = as.numeric(result[1]),
                Y_transformed = as.numeric(result[2]),
                gridType = "Irish Grid",
                year = year
              )
            )
          }
          if (climate) {
            return(
              data.frame(
                gridRef = ref,
                X_transformed = as.numeric(result[1]),
                Y_transformed = as.numeric(result[2]),
                gridType = "Irish Grid",
                year = year,
                month = month
              )
            )
          } else {
            return(
              data.frame(
                gridRef = ref,
                X_transformed = as.numeric(result[1]),
                Y_transformed = as.numeric(result[2]),
                gridType = "Irish Grid"
              )
            )
          }
        }

      } else if (isBritish[i]) {
        result <- tryCatch(
          rnrfa::osg_parse(ref),
          error = function(e)
            NULL
        ) # British grid reference to coords in BNG
        if (!is.null(result) &&
            all(c("easting", "northing") %in% names(result))) {
          if (landcover && climate) {
            return(
              data.frame(
                gridRef = ref,
                X_transformed = as.numeric(result[1]),
                Y_transformed = as.numeric(result[2]),
                gridType = "British National Grid",
                year = year,
                month = month
              )
            )
          }
          if (landcover) {
            return(
              data.frame(
                gridRef = ref,
                X_transformed = as.numeric(result[1]),
                Y_transformed = as.numeric(result[2]),
                gridType = "British National Grid",
                year = year
              )
            )
          }
          if (climate) {
            return(
              data.frame(
                gridRef = ref,
                X_transformed = as.numeric(result[1]),
                Y_transformed = as.numeric(result[2]),
                gridType = "British National Grid",
                year = year,
                month = month
              )
            )
          } else {
            return(
              data.frame(
                gridRef = ref,
                X_transformed = as.numeric(result[1]),
                Y_transformed = as.numeric(result[2]),
                gridType = "British National Grid"
              )
            )
          }
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
  ##Determine what region is needed
  dlNI <- "Irish Grid" %in% resultDf$gridType
  dlUK <- "British National Grid" %in% resultDf$gridType

  #soils
  if (soil) {
    #list all soil properties
    prop <- soilprops
    #Download data from Zenodo
    baseUrl <- "https://zenodo.org/records/14973735/files/"

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
          warning("Failed to download: ",
                  fileNameNI,
                  ". Error: ",
                  e$message)
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
          warning("Failed to download: ",
                  fileNameUK,
                  ". Error: ",
                  e$message)
        })
      }
    }
    message('Extracting soil data.')
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
          colnames(as.data.frame(extractedUK)) <- paste0(p, "_", names(ukRast))
          #Add extracted values to results df
          resultDf[uklocs, colnames(extractedUK)] <- extractedUK
        }
      }
    }
    #empty temp directory
    # unlink(tempDir, recursive=TRUE)
  }
  #land cover
  if (landcover) {
    #list years from df
    yearstot <- sort(unique(resultDf$year))
    #Download data from Zenodo
    baseUrl <- "https://zenodo.org/records/14849882/files/"

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
      r <- NULL
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
    message("Extracting land cover by year. This may take a while.")
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
    #empty temp directory
    #  unlink(tempDir, recursive=TRUE)
  }
  #climate
  if (climate) {
    #Extract earliest and latest year & month from resultDf
    minYear <- min(as.numeric(df$year))
    maxYear <- max(as.numeric(df$year))
    minMonth <- min(as.numeric(df$month[df$year == minYear]))
    maxMonth <- max(as.numeric(df$month[df$year == maxYear]))

    #Initialize final start and end dates
    startyear <- minYear
    startmonth <- minMonth
    endyear <- maxYear
    endmonth <- maxMonth

    #Adjust for seasonal and annual data
    if ("seasonal" %in% climtime | "annual" %in% climtime) {
      if (startmonth != 12) {
        startyear <- startyear - 1
      }
      if (endmonth %in% c(11, 12)) {
        endyear <- endyear + 1
      }
    }

    #Adjust for annual data
    if ("annual" %in% climtime) {
      startmonth <- annualstartmonth
      endmonth <- ifelse(startmonth == 1, 12, startmonth - 1)

      #If the annual period spans two calendar years, adjust endyear
      if (startmonth > endmonth) {
        endyear <- endyear + 1
      }
    }

    #Ensure startyear and endyear cover all necessary time periods
    final_startyear <- min(startyear, minYear)
    final_endyear <- max(endyear, maxYear)

    #Ensure startmonth and endmonth cover all necessary months
    final_startmonth <- min(startmonth, minMonth)
    final_endmonth <- max(endmonth, maxMonth)

    #Format into YYYY_MM strings
    start <- paste0(final_startyear, "_", sprintf("%02d", as.numeric(final_startmonth)))
    end <- paste0(final_endyear, "_", sprintf("%02d", final_endmonth))
    #Check if year is within valid range (1999-2023)
    if (!(final_startyear >= 1999 && final_startyear <= 2023)) {
      stop("'start' year must be between 1999 and 2023.")
    }
    if (!(final_endyear >= 1999 && final_endyear <= 2023)) {
      stop("'end' year must be between 1999 and 2023.")
    }
    #initalise years for dl rasters
    inputYears <- final_startyear:final_endyear
    #Generate possible layer names based on selected climate variables and date range
    lyears <- seq(as.integer(final_startyear),
                  as.integer(final_endyear),
                  by = 1)
    lmonths <- sprintf("%02d", 1:12)
    datecombo <- expand.grid(lyears, lmonths)
    datecombo <- apply(datecombo, 1, function(x)
      paste(x, collapse = "_"))
    datecombo <- datecombo[datecombo >= start & datecombo <= end]
    lnames <- sort(as.vector(outer(climvar, datecombo, paste, sep = "_")))

    #Download data from Zenodo
    baseUrl <- "https://zenodo.org/records/14913772/files/"

    if (dlUK) {
      message(
        "Your climate dataset contains UK data. These files are very large. Please consider increasing your timeout through options(timeout = x)."
      )
    }

    #temp directory
    tempDir <- tempdir()

    #initialise list
    rastList <- list()


    #dl rasters
    for (y in inputYears) {
      if (dlNI) {
        fileNameNI <- paste0("ni_climate_", y, ".nc")
        fileUrlNI <- paste0(baseUrl, fileNameNI)
        tempNI <- file.path(tempDir, fileNameNI)

        tryCatch({
          download.file(fileUrlNI, tempNI, mode = "wb")
          message("Downloaded: ", fileUrlNI)
          tempNI <- rast(tempNI)
          matching <- names(tempNI)[names(tempNI) %in% lnames]
          tempNI <- tempNI[[matching]]
          rastList[[paste0("ni_", y)]] <- tempNI
        }, error = function(e) {
          warning("Failed to download: ",
                  fileNameNI,
                  ". Error: ",
                  e$message)
        })
      }
      if (dlUK) {
        fileNameUK <- paste0("uk_climate_", y, ".nc")
        fileUrlUK <- paste0(baseUrl, fileNameUK)
        tempUK <- file.path(tempDir, fileNameUK)

        tryCatch({
          download.file(fileUrlUK, tempUK, mode = "wb")
          message("Downloaded: ", fileUrlUK)
          tempUK <- rast(tempUK)
          matching <- names(tempUK)[names(tempUK) %in% lnames]
          tempUK <- tempUK[[matching]]
          rastList[[paste0("uk_", y)]] <- tempUK
        }, error = function(e) {
          warning("Failed to download: ",
                  fileNameUK,
                  ". Error: ",
                  e$message)
        })
      }
    }
    annualseasonList <- list()
    monthlyList <- list()
    if ("annual" %in% climtime | "seasonal" %in% climtime) {
      annualseasonList <- rastList
    }
    if ('monthly' %in% climtime) {
      monthlyList <- rastList
    }
    if ('monthly' %in% climtime) {
      #extract climate data on selected vars based on year / month
      message("Extracting monthly values. This might take a while. ")
      for (i in seq_len(nrow(resultDf))) {
        rowYear <- resultDf$year[i]
        rowMonth <- sprintf("%02d", as.numeric(resultDf$month[i]))

        for (var in climvar) {
          layerName <- paste0(var, "_", rowYear, "_", rowMonth)

          if (dlNI &
              paste0('ni_', rowYear) %in% names(monthlyList)) {
            niRast <- monthlyList[[paste0('ni_', rowYear)]]
            if (layerName %in% names(niRast) &&
                resultDf$gridType[i] == "Irish Grid") {
              extractedVal <- terra::extract(niRast[[layerName]], resultDf[i, c("X_transformed", "Y_transformed")], ID = FALSE)
              resultDf[i, paste0("monthly_", var)] <- extractedVal
            }
          }

          if (dlUK &
              paste0('uk_', rowYear) %in% names(monthlyList)) {
            ukRast <- monthlyList[[paste0('uk_', rowYear)]]
            if (layerName %in% names(ukRast) &&
                resultDf$gridType[i] == "British National Grid") {
              extractedVal <- terra::extract(ukRast[[layerName]], resultDf[i, c("X_transformed", "Y_transformed")], ID = FALSE)
              resultDf[i, paste0("monthly_", var)] <- extractedVal
            }
          }
        }
      }
    }
    #if user has selected annual or seasonal split the rasters based on vars
    aRastList <- list()
    if ("annual" %in% climtime | "seasonal" %in% climtime) {
      for (var in climvar) {
        if (dlUK) {
          uklist <- annualseasonList[grepl("^uk", names(annualseasonList))]
          originalNames <- unlist(lapply(uklist, names))
          ukcombined <- rast(uklist)
          names(ukcombined) <- originalNames
          matching <- grep(paste0("^", var, "_"), names(ukcombined), value = TRUE)
          tempUK <- ukcombined[[matching]]
          aRastList[[paste0("uk_", var)]] <- tempUK
        }
        if (dlNI) {
          nilist <- annualseasonList[grepl("^ni", names(annualseasonList))]
          originalNames <- unlist(lapply(nilist, names))
          nicombined <- rast(nilist)
          names(nicombined) <- originalNames
          matching <- grep(paste0("^", var, "_"), names(nicombined), value = TRUE)
          tempNI <- nicombined[[matching]]
          aRastList[[paste0("ni_", var)]] <- tempNI
        }
      }
    } #handle annual
    if ('annual' %in% climtime) {
      annualRastList <- list()
      #get env names from list
      for (rastName in names(aRastList)) {
        x <- aRastList[[rastName]]
        envVar <- sub("^(uk_|ni_)", "", rastName)
        annualRasts <- list()
        layerNames <- sub(paste0("^", envVar, "_"), "", names(x))
        customYears <- seq(startyear, endyear)
        #determine aggregate function for creating annual rast
        aggFunct <- switch(
          envVar,
          "tasmin" = min,
          "tasmax" = max,
          "rain" = sum,
          "tas" = mean,
          mean
        )
        for (y in customYears) {
          yearStart <- paste0(y, "_", sprintf("%02d", as.numeric(startmonth)))
          if (startmonth == 1) {
            yearEnd <- paste0(y, "12")  # No underscore, two-digit month
            annName <- paste0(envVar,
                              "_",
                              y,
                              sprintf("%02d", startmonth),
                              "_",
                              yearEnd)
          } else {
            yearEnd <- paste0(y + 1, sprintf("%02d", as.numeric(startmonth) - 1))
            annName <- paste0(envVar,
                              "_",
                              y,
                              sprintf("%02d", as.numeric(startmonth)),
                              "_",
                              yearEnd)
          }
          annualLayers <- which(layerNames >= yearStart &
                                  layerNames <= yearEnd)
          #warnings for incomplete years
          if (length(annualLayers) < 12) {
            warning(
              paste(
                'Incomplete annual data from month',
                startmonth,
                ', year',
                y,
                '- skipping'
              )
            )
            next
          }
          annualRast <- app(x[[annualLayers]], aggFunct, na.rm = TRUE)
          names(annualRast) <- annName
          annualRasts <- c(annualRasts, annualRast)
        }
        if (length(annualRasts) == 0) {
          stop('No complete annual data availble for selected date range.')
        }
        inRangeRast <- do.call(c, annualRasts)
        annualRastList[[rastName]] <- inRangeRast
      }

      #extract annual data using the new list of rasters
      #perform extraction
      message("Extracting annual climate values. This may take a while.")
      for (i in seq_len(nrow(resultDf))) {
        rowYear <- resultDf$year[i]
        rowMonth <- sprintf("%02d", as.numeric(resultDf$month[i]))
        rowym <- paste(rowYear, "_", rowMonth)
        #Determine if uk or ni
        if (resultDf$gridType[i] == 'Irish Grid') {
          region_prefix <- "ni_"
        } else if (resultDf$gridType[i] == 'British National Grid') {
          region_prefix <- "uk_"
        } else {
          next  #Skip if grid type is unknown
        }
        #find correct raster based on row year and month
        for (cv in climvar) {
          matchingLayer <- NULL
          rastName <- paste0(region_prefix, cv)
          if (!rastName %in% names(annualRastList)) {
            warning(paste("No raster found for", rastName))
            next
          }
          raster <- annualRastList[[rastName]]
          layerNames <- names(raster)
          #find correct layer
          for (layer in layerNames) {
            #extract start and end from raster name
            timerange <- sub(paste0("^", cv, "_"), "", layer)
            parts <- unlist(strsplit(timerange, "_"))
            startym <- parts[1]
            endym <- parts[2]
            rowym_num <- as.numeric(paste0(rowYear, sprintf(
              "%02d", as.numeric(rowMonth)
            )))  #Convert row to YYYYMM
            if (rowym_num >= startym & rowym_num <= endym) {
              matchingLayer <- layer
              break
            }
          }
          if (!is.null(matchingLayer)) {
            extractVal <- terra::extract(raster[[matchingLayer]], resultDf[i, c("X_transformed", "Y_transformed")], ID = FALSE)
            resultDf[i, paste0('annual_', cv)] <- extractVal
          } else {
            warning(paste("No matching layer found for", rowym, "in", rastName))
          }
        }
      }
    }

    #handling if seasonal is selected
    if ('seasonal' %in% climtime) {
      #Define standard seasons
      seasonsDef <- list(
        "winter" = c(12, 1, 2),
        "spring" = c(3, 4, 5),
        "summer" = c(6, 7, 8),
        "autumn" = c(9, 10, 11)
      )
      seasonalRastList <- list()
      #get env names from list
      for (rastName in names(aRastList)) {
        x <- aRastList[[rastName]]
        envVar <- sub("^(uk_|ni_)", "", rastName)
        seasonalRasts <- list()
        layerNames <- sub(paste0("^", envVar, "_"), "", names(x))
        yearMonth <- do.call(rbind, strsplit(layerNames, "_"))
        years <- as.numeric(yearMonth[, 1])
        months <- as.numeric(yearMonth[, 2])
        #determine aggregate function for creating annual rast
        aggFunct <- switch(
          envVar,
          "tasmin" = min,
          "tasmax" = max,
          "rain" = sum,
          "tas" = mean,
          mean
        )
        #functions for season
        findSeason <- function(month) {
          month <- as.numeric(month)
          for (season in names(seasonsDef)) {
            if (month %in% seasonsDef[[season]])
              return(season)
          }
          return(NA)
        }
        getPrevSeasons <- function(rowSeason) {
          seasonOrder <- c("winter", "spring", "summer", "autumn")
          season_index <- match(rowSeason, seasonOrder)
          prevSeasons <- c(seasonOrder[season_index],
                           #Current season
                           seasonOrder[ifelse(season_index - 1 < 1, 4, season_index - 1)],
                           # -1 season
                           seasonOrder[ifelse(season_index - 2 < 1,
                                              4 + (season_index - 2),
                                              season_index - 2)],
                           # -2 seasons
                           seasonOrder[ifelse(season_index - 3 < 1,
                                              4 + (season_index - 3),
                                              season_index - 3)]  # -3 seasons))
                           return(prevSeasons)
        }
        #validating available seasons
        for (y in unique(years)) {
          for (season in names(seasonsDef)) {
            seasonMonths <- seasonsDef[[season]]
            #winter spans two calendar years
            if (season == "winter") {
              seasonLayers <- which((years == y &
                                       months == 12) |
                                      (years == (y + 1) &
                                         months %in% c(1, 2)))
              seasonYear <- paste0(y, "_", y + 1)
            } else {
              seasonLayers <- which(years == y & months %in% seasonMonths)
              seasonYear <- paste0(y, "_", y)
            }
            #Checking dates
            if (length(seasonLayers) < 3) {
              warning(paste('Incomplete', season, 'in year', y, '- skipping.'))
              next
            }
            seasonalRast <- app(x[[seasonLayers]], aggFunct, na.rm = TRUE)
            names(seasonalRast) <- paste0(envVar, "_", season, "_", seasonYear)
            seasonalRasts <- c(seasonalRasts, seasonalRast)
          }
        }
        if (length(seasonalRasts) == 0) {
          stop('No complete seasonal data availble for selected date range.')
        }
        inRangeRast <- do.call(c, seasonalRasts)
        seasonalRastList[[rastName]] <- inRangeRast
      }
      #extract seasonal data using the new list of rasters
      #extract for current season and three previous
      #every row has a value for every season
      #loop through
      message("Extracting seasonal climate values. This might take a while.")
      for (i in seq_len(nrow(resultDf))) {
        rowYear <- as.numeric(resultDf$year[i])
        rowMonth <- resultDf$month[i]
        rowSeason <- findSeason(rowMonth)
        seasonList <- getPrevSeasons(rowSeason)

        regPrefix <- ifelse(resultDf$gridType[i] == 'Irish Grid', 'ni_', 'uk_')

        for (cv in climvar) {
          rasterName <- paste0(regPrefix, cv)
          if (!rasterName %in% names(seasonalRastList)) {
            warning(paste("No raster found for ", rasterName))
            next
          }
          raster <- seasonalRastList[[rasterName]]
          for (season_index in seq_along(seasonList)) {
            season <- seasonList[season_index]

            #Determine the base year for the first season
            if (season_index == 1) {
              if (season == "winter") {
                #Winter spans two years: Dec (prev year) + Jan-Feb (curr year)
                seasonY1 <- ifelse(rowMonth == 12, rowYear, rowYear - 1)
                seasonY2 <- seasonY1 + 1
              } else {
                seasonY1 <- rowYear
                seasonY2 <- rowYear
              }
            } else {
              #If winter was the first season, shift all others back one year
              if (seasonList[1] == "winter") {
                seasonY1 <- rowYear - 1
                seasonY2 <- rowYear - 1
              } else if (season == "winter") {
                #If this season is winter, shift it back a year
                seasonY1 <- rowYear - 1
                seasonY2 <- rowYear
              } else if (season == "autumn" &&
                         "winter" %in% seasonList[1:season_index]) {
                #If winter already happened, autumn must be in the previous year
                seasonY1 <- rowYear - 1
                seasonY2 <- rowYear - 1
              } else {
                #Default case: use the same year as rowYear
                seasonY1 <- rowYear
                seasonY2 <- rowYear
              }
            }

            #Construct the correct seasonal layer name
            seasonLayerName <- paste(cv, season, seasonY1, seasonY2, sep = "_")

            if (seasonLayerName %in% names(raster)) {
              extractVal <- terra::extract(raster[[seasonLayerName]], resultDf[i, c("X_transformed", "Y_transformed")], ID = FALSE)
              resultDf[i, paste0(season, "_", cv)] <- extractVal
            } else {
              warning(paste("No matching layer found for", seasonLayerName))
            }
          }
        }
      }
    }
  }

  return(resultDf)
}
