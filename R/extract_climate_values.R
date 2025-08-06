#' Extract values for climate variables from raster files
#'
#' This function extracts values of chosen climate variables from 1km resolution raster files over a specified time period. Raster files are stored in an online repository, therefore this function relies on an internet connection.
#' Raster files cover climate variables from 1999 - 2023. Raster files are either of the whole of the United Kingdom in EPSG:27700, British National Grid, or of Northern Ireland in EPSG:29903, Irish Grid.
#' These data products were created using original data products from UK's Met Office HadUK Grid.
#' Users may want to consider increasing time out time to allow all relevant data to be downloaded: options(timeout = x)
#' @import terra
#' @import igr
#' @import rnrfa
#' @param type Either 'grid' if using grid references or 'coords' if using co-ordinates.
#' @param df a data frame that must contain a 'year' and 'month' column. \cr
#' If type = ‘grid,’ df must contain a column of grid references 'gridRef'. If type = ‘coords’,  df must contain columns for coordinates 'X' and 'Y'. \cr
#' When type = ‘grid,’ this function will detect whether the input grid references belong to British National Grid (EPSG:27700) or Irish Grid (EPSG:29903). When type = ‘coords’, an additional argument is required to specify the coordinate reference system that ‘X’ and ‘Y’ are projected in.
#' @param crs Required when type = 'coords', the co-ordinate reference system of the X and Y coordinates. Must be in the format of 'EPSG:X'. \cr
#' If crs is not ‘EPSG:29903’ for Irish Grid, or ‘EPSG:27700’ for British National Grid, this function will project the co-ordinates to EPSG:27700 so that extractions can be carried out using the UK wide rasters in EPSG:27700.
#' @param climvar Character vector of climate variables a user wishes to extract data for. \cr
#' Can choose from ‘rain’ for rainfall, ‘tas’ for mean temperature, ‘tasmin’ for minimum temperature or ‘tasmax’ for maximum temperature. The default is all four variables. Rainfall is measured in millimetres and the three temperature variables are measured in degrees Celsius.
#' @param time Character vector of time aggregates for values. Can include ‘monthly’, ‘seasonal’ and ‘annual’. There is no default and must be chosen. \cr
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
#' @param annualstartmonth Numeric. Required if 'time' includes 'annual.' Must be an integer between 1 and 12, indicating the start month of the annual time period.
#' @return A data frame containing grid reference if type = 'grid', or the input coordinates if type = ‘coords.’ The original 'month' and 'year' columns are included. \cr
#' Two columns, ‘X_transformed’ and ‘Y_transformed’ detail either the coordinates used for extraction if type = ‘grid’ (the bottom left coordinate of the grid reference), or the projected coordinates if type = ‘coords.’ If type = ‘coords’, and crs = either ‘EPSG:29903’ or ‘EPSG:27700’, these columns will be identical to the input ‘X’ and ‘Y’ coordinates. \cr
#' A column ‘gridType’ will indicate whether extractions have been performed using rasters of the United Kingdom on EPSG:27700 as ‘British National Grid’ or rasters of Northern Ireland on EPSG:29903 as ‘Irish Grid’. \cr
#' Remaining columns are of extracted values and will be named indicating the variable and time period e.g. monthly_tas, winter_rain, annual_tasmin.
#' @export

extract_climate_values <- function(type,
                                   df,
                                   crs = NULL,
                                   climvar = c('rain', 'tas', 'tasmin', 'tasmax'),
                                   time,
                                   annualstartmonth = NULL) {
  #Warnings
  #type check
  if (!type %in% c("grid", "coords")) {
    stop("Error: 'type' must be either 'grid' or 'coords'.")
  }
  #Checking input is data frame
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }
  #Check if columns exist
  if (type == 'grid') {
    required <- c("gridRef", "year", "month")
  } else if (type == 'coords') {
    required <- c("X", "Y", "year", "month")
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
  #Validate 'time' input
  valid_time_options <- c("annual", "seasonal", "monthly")
  if (!all(time %in% valid_time_options)) {
    invalid_time <- setdiff(time, valid_time_options)
    stop(
      paste(
        "Error: Invalid 'time' value(s):",
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
  if ("annual" %in% time) {
    if (is.null(annualstartmonth)) {
      stop("Error: 'annualstartmonth' must be provided when 'time' includes 'annual'.")
    }
    if (!is.numeric(annualstartmonth) ||
        annualstartmonth < 1 || annualstartmonth > 12) {
      stop("Error: 'annualstartmonth' must be a numeric value between 1 and 12.")
    }
  } else if (!is.null(annualstartmonth)) {
    warning(
      "'annualstartmonth' is provided but 'time' does not include 'annual'. It will be ignored."
    )
  }
if (!all(as.numeric(df$month) %in% 1:12)) {
  stop("The 'month' column must only contain numeric values from 1 to 12.")
}

  #Define standard seasons
  seasonsDef <- list(
    "winter" = c(12, 1, 2),
    "spring" = c(3, 4, 5),
    "summer" = c(6, 7, 8),
    "autumn" = c(9, 10, 11)
  )


  #Extract earliest and latest year & month from df
  minYear <- min(as.numeric(df$year))
  maxYear <- max(as.numeric(df$year))
  minMonth <- min(as.numeric(df$month[df$year == minYear]))
  maxMonth <- max(as.numeric(df$month[df$year == maxYear]))

  #Initialize final start and end dates
  startyear <- minYear
  startmonth <- minMonth
  endyear <- maxYear
  endmonth <- maxMonth

  #Seasonal Only
  if ("seasonal" %in% time && !("annual" %in% time)) {
    #Adjust for full seasons based on input data
    if (minMonth <= 2 | minMonth == 12) {
      startmonth <- 3  # Winter
    } else if (minMonth <= 5) {
      startmonth <- 6  # Spring
    } else if (minMonth <= 8) {
      startmonth <- 9  # Summer
    } else {
      startmonth <- 12  # Autumn
    }
    #Adjust end month similarly
    if (maxMonth == 1 | maxMonth == 12 | maxMonth == 2) {
      endmonth <- 2  # Include full winter
    } else if (maxMonth <= 5) {
      endmonth <- 5  # Include full spring
    } else if (maxMonth <= 8) {
      endmonth <- 8  # Include full summer
    } else {
      endmonth <- 11  # Include full autumn
    }

    #Set start and end year accordingly
    if (minMonth == 12) {
      endyear <- endyear + 1
      startyear <- startyear - 2
    } else {
      startyear <- startyear - 1
      endyear <- endyear}
  }
  #Annual Only
  if ("annual" %in% time && !("seasonal" %in% time)) {
    startmonth <- annualstartmonth
    endmonth <- ifelse(annualstartmonth == 12, 1, annualstartmonth - 1)
    #If the annual period spans two calendar years, adjust endyear
    if(minMonth < startmonth){
    startyear <- startyear - 1}
    if(maxMonth > endmonth)
    endyear <- endyear + 1
    }
  #Annual and seasonal
  if ('annual' %in% time && 'seasonal' %in% time){
    #adjust for full seasons based on data
    if (minMonth <= 2 | minMonth == 12) {
      seasonal_startmonth <- 3  # Winter
    } else if (minMonth <= 5) {
      seasonal_startmonth <- 6  # Spring
    } else if (minMonth <= 8) {
      seasonal_startmonth <- 9  # Summer
    } else {
      seasonal_startmonth <- 12  # Autumn
    }

    if (maxMonth == 1 | maxMonth == 12 | maxMonth == 2) {
      seasonal_endmonth <- 2  # Winter
    } else if (maxMonth <= 5) {
      seasonal_endmonth <- 5  # Spring
    } else if (maxMonth <= 8) {
      seasonal_endmonth <- 8  # Summer
    } else {
      seasonal_endmonth <- 11  # Autumn
    }

    seasonal_startyear <- ifelse(minMonth == 12, minYear - 2, minYear - 1)
    seasonal_endyear <- ifelse(minMonth == 12, maxYear + 1, maxYear)

    #annual calculations
    annual_startmonth <- annualstartmonth
    annual_endmonth <- ifelse(annual_startmonth == 12, 1, annual_startmonth - 1)

    annual_startyear <- ifelse(minMonth < annual_startmonth, minYear - 1, minYear)
    annual_endyear <- ifelse(maxMonth > annual_endmonth, maxYear + 1, maxYear)

    #use the *earliest* start year/month and *latest* end year/month from both
    startyear <- min(seasonal_startyear, annual_startyear)
    startmonth <- min(seasonal_startmonth, annual_startmonth)
    endyear <- max(seasonal_endyear, annual_endyear)
    endmonth <- max(seasonal_endmonth, annual_endmonth)
  }

  #Ensure startyear and endyear cover all necessary time periods
  final_startyear <- startyear
  final_endyear <- endyear

  #Ensure startmonth and endmonth cover all necessary months
  final_startmonth <- startmonth
  final_endmonth <- endmonth

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
      coordsVect <- terra::vect(df, geom = c("X", "Y"), crs = crs)
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
                       "year",
                       "month")]
  }
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
              gridType = "British National Grid",
              year = year,
              month = month
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
          gridType = NA_character_,
          year = year,
          month = month
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
  baseUrl <- "https://zenodo.org/records/14913772/files/"

  #Determine what region is needed
  dlNI <- "Irish Grid" %in% resultDf$gridType
  dlUK <- "British National Grid" %in% resultDf$gridType
  if (dlUK) {
    message(
      "Your dataset contains UK data. These files are very large. Please consider increasing your timeout through options(timeout = x)."
    )
  }

  #temp directory
  tempDir <- tempdir()

  #Initialise list
  rastList <- list()

  #Download and load rasters
  for (y in inputYears) {

    if (dlNI) {
      fileNameNI <- paste0("ni_climate_", y, ".nc")
      fileUrlNI <- paste0(baseUrl, fileNameNI)
      tempNI <- file.path(tempDir, fileNameNI)

      if (!file.exists(tempNI)) {
        tryCatch({
          download.file(fileUrlNI, tempNI, mode = "wb")
          message("Downloaded: ", fileNameNI)
        }, error = function(e) {
          warning("Failed to download: ", fileNameNI, " — ", e$message)
          next
        })
      } else {
        message("Using cached version: ", fileNameNI)
      }

      r <- suppressWarnings(try(rast(tempNI), silent = TRUE))

      if (inherits(r, "try-error") || nlyr(r) == 0) {
        message("Corrupt file detected: ", fileNameNI, ". Redownloading.")
        file.remove(tempNI)

        tryCatch({
          download.file(fileUrlNI, tempNI, mode = "wb")
          message("Redownloaded: ", fileNameNI)
        }, error = function(e) {
          warning("Redownload failed: ", fileNameNI, " — ", e$message)
          next
        })

        r <- suppressWarnings(try(rast(tempNI), silent = TRUE))
        if (inherits(r, "try-error") || nlyr(r) == 0) {
          warning("Still invalid after redownload: ", fileNameNI)
          next
        }
      }

      matching <- names(r)[names(r) %in% lnames]
      rastList[[paste0("ni_", y)]] <- r[[matching]]
    }

    if (dlUK) {
      fileNameUK <- paste0("uk_climate_", y, ".nc")
      fileUrlUK <- paste0(baseUrl, fileNameUK)
      tempUK <- file.path(tempDir, fileNameUK)

      if (!file.exists(tempUK)) {
        tryCatch({
          download.file(fileUrlUK, tempUK, mode = "wb")
          message("Downloaded: ", fileNameUK)
        }, error = function(e) {
          warning("Failed to download: ", fileNameUK, " — ", e$message)
          next
        })
      } else {
        message("Using cached version: ", fileNameUK)
      }

      r <- suppressWarnings(try(rast(tempUK), silent = TRUE))

      if (inherits(r, "try-error") || nlyr(r) == 0) {
        message("Corrupt file detected: ", fileNameUK, ". Redownloading.")
        file.remove(tempUK)

        tryCatch({
          download.file(fileUrlUK, tempUK, mode = "wb")
          message("Redownloaded: ", fileNameUK)
        }, error = function(e) {
          warning("Redownload failed: ", fileNameUK, " — ", e$message)
          next
        })

        r <- suppressWarnings(try(rast(tempUK), silent = TRUE))
        if (inherits(r, "try-error") || nlyr(r) == 0) {
          warning("Still invalid after redownload: ", fileNameUK)
          next
        }
      }

      matching <- names(r)[names(r) %in% lnames]
      rastList[[paste0("uk_", y)]] <- r[[matching]]
    }
  }

  annualseasonList <- list()
  monthlyList <- list()
  if ("annual" %in% time | "seasonal" %in% time) {
    annualseasonList <- rastList
  }
  if ('monthly' %in% time) {
    monthlyList <- rastList
  }
  if ('monthly' %in% time) {
    #extract climate data on selected vars based on year / month
    message("Extracting monthly values.")
    for (i in seq_len(nrow(resultDf))) {
      rowYear <- resultDf$year[i]
      rowMonth <- sprintf("%02d", as.numeric(resultDf$month[i]))
      rowRef <- resultDf$gridRef[i]
      #skip if NA values
      if (anyNA(c(rowYear, rowMonth, rowRef))) next

      for (var in climvar) {
        layerName <- paste0(var, "_", rowYear, "_", rowMonth)

        if (dlNI & paste0('ni_', rowYear) %in% names(monthlyList)) {
          niRast <- monthlyList[[paste0('ni_', rowYear)]]
          if (layerName %in% names(niRast) &&
              !is.na(resultDf$gridType[i]) &&
              resultDf$gridType[i] == "Irish Grid") {
            extractedVal <- terra::extract(niRast[[layerName]], resultDf[i, c("X_transformed", "Y_transformed")], ID = FALSE)
            resultDf[i, paste0("monthly_", var)] <- extractedVal
          }
        }

        if (dlUK & paste0('uk_', rowYear) %in% names(monthlyList)) {
          ukRast <- monthlyList[[paste0('uk_', rowYear)]]
          if (layerName %in% names(ukRast) &&
              !is.na(resultDf$gridType[i]) &&
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
  if ("annual" %in% time | "seasonal" %in% time) {
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
  if ('annual' %in% time) {
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
    message("Extracting annual values.")
    for (i in seq_len(nrow(resultDf))) {
      rowYear <- as.numeric(resultDf$year[i])
      rowMonth <- sprintf("%02d", as.numeric(resultDf$month[i]))
      rowRef <- resultDf$gridRef[i]
      #skip if NA values
      if (anyNA(c(rowYear, rowMonth, rowRef))) next
      rowym <- paste(rowYear, "_", rowMonth)
      #Determine if uk or ni
      if (resultDf$gridType[i] == 'Irish Grid' &&
          !is.na(resultDf$gridType[i])) {
        region_prefix <- "ni_"
      } else if (resultDf$gridType[i] == 'British National Grid' &&
                 !is.na(resultDf$gridType[i])) {
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
          rowym_num <- as.numeric(paste0(rowYear, sprintf("%02d", as.numeric(rowMonth))))  #Convert row to YYYYMM
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
  if('seasonal' %in% time) {
    message("Extracting seasonal values.")
    seasonalRastList <- list()

    #function to determine the correct season year range
    getSeasonYears <- function(rowYear, rowMonth, season) {
      if (season == "winter") {
        if (as.numeric(rowMonth) == 12) {
          seasonY1 <- rowYear
          seasonY2 <- seasonY1 + 1
        } else {
          seasonY1 <- rowYear - 1
          seasonY2 <- rowYear
        }

      } else {
        season_start_month <- min(seasonsDef[[season]])
        if (season_start_month > as.numeric(rowMonth)) {
          seasonY1 <- rowYear - 1
        } else {
          seasonY1 <- rowYear
        }
        seasonY2 <- seasonY1
      }
      return(c(seasonY1, seasonY2))
    }

    #Get env names from list
    for(rastName in names(aRastList)) {
      x <- aRastList[[rastName]]
      envVar <- sub("^(uk_|ni_)", "", rastName)
      seasonalRasts <- list()
      layerNames <- sub(paste0("^", envVar, "_"), "", names(x))
      yearMonth <- do.call(rbind, strsplit(layerNames, "_"))
      years <- as.numeric(yearMonth[, 1])
      months <- as.numeric(yearMonth[, 2])

      aggFunct <- switch(
        envVar,
        "tasmin" = min,
        "tasmax" = max,
        "rain" = sum,
        "tas" = mean,
        mean
      )

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
        prevSeasons <- c(
          seasonOrder[season_index],
          seasonOrder[ifelse(season_index - 1 < 1, 4, season_index - 1)],
          seasonOrder[ifelse(season_index - 2 < 1, 4 + (season_index - 2), season_index - 2)],
          seasonOrder[ifelse(season_index - 3 < 1, 4 + (season_index - 3), season_index - 3)]
        )
        return(prevSeasons)
      }

      for(y in unique(years)) {
        for(season in names(seasonsDef)) {
          seasonMonths <- seasonsDef[[season]]
          if(season == "winter") {
            seasonLayers <- which((years == y & months == 12) | (years == (y + 1) & months %in% c(1, 2)))
            seasonYear <- paste0(y, "_", y + 1)
          } else {
            seasonLayers <- which(years == y & months %in% seasonMonths)
            seasonYear <- paste0(y, "_", y)
          }

          if(length(seasonLayers) < 3) {
            next
          }

          seasonalRast <- app(x[[seasonLayers]], aggFunct, na.rm = TRUE)
          names(seasonalRast) <- paste0(envVar, "_", season, "_", seasonYear)
          seasonalRasts <- c(seasonalRasts, seasonalRast)
        }
      }

      if(length(seasonalRasts) == 0) {
        stop('No complete seasonal data available for selected date range.')
      }
      inRangeRast <- do.call(c, seasonalRasts)
      seasonalRastList[[rastName]] <- inRangeRast
    }

    for(i in seq_len(nrow(resultDf))) {
      rowYear <- resultDf$year[i]
      rowMonth <- sprintf("%02d", as.numeric(resultDf$month[i]))
      rowRef <- resultDf$gridRef[i]
      if (anyNA(c(rowYear, rowMonth, rowRef))) next
      rowSeason <- findSeason(rowMonth)
      seasonList <- getPrevSeasons(rowSeason)

      regPrefix <- if(!is.na(resultDf$gridType[i]) && resultDf$gridType[i] == 'Irish Grid') 'ni_' else 'uk_'

      for(cv in climvar) {
        rasterName <- paste0(regPrefix, cv)
        if (!rasterName %in% names(seasonalRastList)) {
          warning(paste("No raster found for ", rasterName))
          next
        }
        raster <- seasonalRastList[[rasterName]]

        for(season in seasonList) {
          years <- getSeasonYears(rowYear, rowMonth, season)
          seasonY1 <- years[1]
          seasonY2 <- years[2]

          seasonLayerName <- paste(cv, season, seasonY1, seasonY2, sep = "_")

          if(seasonLayerName %in% names(raster)) {
            extractVal <- terra::extract(raster[[seasonLayerName]], resultDf[i, c("X_transformed", "Y_transformed")], ID = FALSE)
            resultDf[i, paste0(season, "_", cv)] <- extractVal
          } else {
            warning(paste("No matching layer found for", seasonLayerName))
          }
        }
      }
    }
  }


  return(resultDf)
}
