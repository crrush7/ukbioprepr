#function for extracting data from all available datasets
extractAll <- function(df,
                       type = 'grid',
                       crs = NULL,
                       start,
                       end,
                       soil = TRUE,
                       landcover = TRUE,
                       climate = TRUE,
                       climtime = c('monthly', 'seasonal', 'annual')){

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
  if(climate == TRUE){
    #check if start and end is in correct format
    if (!grepl("^\\d{4}_\\d{2}$", start) ||
        !grepl("^\\d{4}_\\d{2}$", end)) {
      stop("Please provide valid 'start' and 'end' dates in 'YYYY_MM' format.")
    }
    #Extract year and month from start and end
    startyear <- as.numeric(substr(start, 1, 4))
    startmonth <- as.numeric(substr(start, 6, 7))
    endyear <- as.numeric(substr(end, 1, 4))
    endmonth <- as.numeric(substr(end, 6, 7))

    #Check if year is within valid range (1999-2023)
    if (!(startyear >= 1999 && startyear <= 2023)) {
      stop("'start' year must be between 1999 and 2023.")
    }
    if (!(endyear >= 1999 && endyear <= 2023)) {
      stop("'end' year must be between 1999 and 2023.")
    }

    #Check if month is valid (01-12)
    if (!(startmonth >= 1 && startmonth <= 12)) {
      stop("'start' month must be between 01 and 12.")
    }
    if (!(endmonth >= 1 && endmonth <= 12)) {
      stop("'end' month must be between 01 and 12.")
    }
    #Convert start and end to Date objects for comparison
    sdate <- as.Date(paste0(substr(start, 1, 4), "-", substr(start, 6, 7), "-01"))
    edate <- as.Date(paste0(substr(end, 1, 4), "-", substr(end, 6, 7), "-01"))

    #Ensure start date is before end date
    if (sdate > edate) {
      stop("Error: Start date must be before end date.")
    }

    #Calculate number of months in range
    num_months <- 12 * (as.numeric(format(edate, "%Y")) - as.numeric(format(sdate, "%Y"))) +
      (as.numeric(format(edate, "%m")) - as.numeric(format(sdate, "%m"))) + 1

    #Error handling for annual selection
    if ("annual" %in% climtime && num_months < 12) {
      stop("Error: The selected time range must cover at least 12 months for annual data.")
    }

    #Define standard seasons
    seasonsDef <- list(
      "winter" = c(12, 1, 2),
      "spring" = c(3, 4, 5),
      "summer" = c(6, 7, 8),
      "autumn" = c(9, 10, 11)
    )

    #Check for one complete season if season is selected
    if ("seasonal" %in% climtime) {
      available_months <- as.numeric(format(seq(sdate, edate, by = "month"), "%m"))
      complete_season <- any(sapply(seasonsDef, function(season) all(season %in% available_months)))

      if (!complete_season) {
        stop("Error: The selected time range must include at least one complete season for seasonal data.")
      }
    }
  }
  #Land cover specific warnings
  if(landcover == TRUE){
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
    if(landcover){
      resultDf <- df[, c("X",
                         "Y",
                         "X_transformed",
                         "Y_transformed",
                         "gridType",
                         "year")]
    } else {
      resultDf <- df[, c("X",
                         "Y",
                         "X_transformed",
                         "Y_transformed",
                         "gridType"
      )]
    }
  }
  #if type == grid references
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
      if(landcover){
        year <- years[i]
      }
      if (isIrish[i]) {
        result <- igr::igr_to_ig(ref) # Irish grid to coords
        if(landcover){
          return(
            data.frame(
              gridRef = ref,
              X_transformed = as.numeric(result[1]),
              Y_transformed = as.numeric(result[2]),
              gridType = "Irish Grid",
              year = year
            )
          )} else {
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
        result <- rnrfa::osg_parse(ref) # British grid reference to coords in BNG
        if(landcover){
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
          return(
            data.frame(
              gridRef = ref,
              X_transformed = as.numeric(result$easting),
              Y_transformed = as.numeric(result$northing),
              gridType = "British National Grid"
            )
          )
        }
      } else {
        stop("Unrecognized grid reference: ", ref)
      }
    })
    #create dataframe
    resultDf <- as.data.frame(do.call(rbind, coords))
  }

  ##Determine what region is needed
  dlNI <- "Irish Grid" %in% resultDf$gridType
  dlUK <- "British National Grid" %in% resultDf$gridType

  #soils
  if(soil){
    #list all soil properties
    prop <- c(
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
    #Download data from Zenodo
    baseUrl <- "https://zenodo.org/records/14945739/files/"

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
    message('Extracting soil data.')
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
    #empty temp directory
    # unlink(tempDir, recursive=TRUE)
  }
  #land cover
  if(landcover){
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
  if(climate){
    #Extract year and month from start and end
    startyear <- as.numeric(substr(start, 1, 4))
    startmonth <- as.numeric(substr(start, 6, 7))
    endyear <- as.numeric(substr(end, 1, 4))
    endmonth <- as.numeric(substr(end, 6, 7))
    #initalise years
    inputYears <- startyear:endyear
    #initalise climate variables
    climvar <- c('tas', 'tasmin', 'tasmax', 'rain')
    #initalise time
    time <- climtime
    #determine possible layers based on selected vars and dates
    lyears <- seq(as.integer(startyear), as.integer(endyear), by = 1)
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
        "Your dataset contains UK data. These files are very large. Please consider increasing your timeout through options(timeout = x)."
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
          warning("Failed to download: ", fileNameNI, ". Error: ", e$message)
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
          warning("Failed to download: ", fileNameUK, ". Error: ", e$message)
        })
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
    message('Extracting climate data.')

    if('monthly' %in% time) {
      #extract climate data on selected vars
      for (y in inputYears) {
        if (dlNI & paste0("ni_", y) %in% names(monthlyList)) {
          niRast <- monthlyList[[paste0('ni_', y)]]
          nilocs <- which(resultDf$gridType == 'Irish Grid')
          if (length(nilocs) > 0) {
            extractedNI <- terra::extract(niRast, resultDf[nilocs, c("X_transformed", "Y_transformed")], ID = FALSE)
            climnames <- names(niRast)
            colnames(extractedNI) <- climnames
            resultDf[nilocs, climnames] <- extractedNI
          }
        }
        if (dlUK & paste0("uk_", y) %in% names(monthlyList)) {
          ukRast <- monthlyList[[paste0('uk_', y)]]
          uklocs <- which(resultDf$gridType == 'British National Grid')
          if (length(uklocs) > 0) {
            extractedUK <- terra::extract(ukRast, resultDf[uklocs, c("X_transformed", "Y_transformed")], ID = FALSE)
            climnames <- names(ukRast)
            colnames(extractedUK) <- climnames
            resultDf[uklocs, climnames] <- extractedUK
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
          yearStart <- paste0(y, "_", sprintf("%02d", startmonth))
          if (startmonth == 1) {
            yearEnd <- paste0(y, "_12")
            annName <- paste0(envVar, "_annual_", y)
          } else {
            yearEnd <- paste0(y + 1, "_", sprintf("%02d", startmonth - 1))
            annName <- paste0(envVar, y, "_", startmonth, "-", y + 1, "_", startmonth -
                                1)
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
      for (cv in climvar) {
        if (dlNI & paste0("ni_", cv) %in% names(annualRastList)) {
          niRast <- annualRastList[[paste0('ni_', cv)]]
          nilocs <- which(resultDf$gridType == 'Irish Grid')
          if (length(nilocs) > 0) {
            extractedNI <- terra::extract(niRast, resultDf[nilocs, c("X_transformed", "Y_transformed")], ID = FALSE)
            climnames <- names(niRast)
            colnames(extractedNI) <- climnames
            resultDf[nilocs, climnames] <- extractedNI
          }
        }
        if (dlUK & paste0("uk_", cv) %in% names(annualRastList)) {
          ukRast <- annualRastList[[paste0('uk_', cv)]]
          uklocs <- which(resultDf$gridType == 'British National Grid')
          if (length(uklocs) > 0) {
            extractedUK <- terra::extract(ukRast, resultDf[uklocs, c("X_transformed", "Y_transformed")], ID = FALSE)
            climnames <- names(ukRast)
            colnames(extractedUK) <- climnames
            resultDf[uklocs, climnames] <- extractedUK
          }
        }
      }
    }

    #handling if seasonal is selected
    if('seasonal' %in% time){
      seasonalRastList <- list()
      #following standard seasons
      seasonsDef <- list(
        "winter" = c(12, 1, 2),
        "spring" = c(3, 4, 5),
        "summer" = c(6, 7, 8),
        "autumn" = c(9, 10, 11)
      )
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
        #validating available seasons
        for(y in unique(years)){
          for (season in names(seasonsDef)){
            seasonMonths <- seasonsDef[[season]]
            #winter spans two calendar years
            if(season == "winter"){
              seasonLayers <- which((years == y &
                                       months == 12) |
                                      (years == (y + 1) & months %in% c(1,2)))
              seasonYear <- paste0(y, "-", y + 1)
            } else {
              seasonLayers <- which(years == y & months %in% seasonMonths)
              seasonYear <- y
            }
            #Checking dates
            if (length(seasonLayers) < 3) {
              warning(paste('Incomplete', season, 'in year', y, '- skipping.'))
              next
            }
            seasonalRast <- app(x[[seasonLayers]], aggFunct, na.rm = TRUE)
            names(seasonalRast) <- paste(envVar, "_", season, "_", seasonYear)
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
      for (cv in climvar) {
        if (dlNI & paste0("ni_", cv) %in% names(seasonalRastList)) {
          niRast <- seasonalRastList[[paste0('ni_', cv)]]
          nilocs <- which(resultDf$gridType == 'Irish Grid')
          if (length(nilocs) > 0) {
            extractedNI <- terra::extract(niRast, resultDf[nilocs, c("X_transformed", "Y_transformed")], ID = FALSE)
            climnames <- names(niRast)
            colnames(extractedNI) <- climnames
            resultDf[nilocs, climnames] <- extractedNI
          }
        }
        if (dlUK & paste0("uk_", cv) %in% names(seasonalRastList)) {
          ukRast <- seasonalRastList[[paste0('uk_', cv)]]
          uklocs <- which(resultDf$gridType == 'British National Grid')
          if (length(uklocs) > 0) {
            extractedUK <- terra::extract(ukRast, resultDf[uklocs, c("X_transformed", "Y_transformed")], ID = FALSE)
            climnames <- names(ukRast)
            colnames(extractedUK) <- climnames
            resultDf[uklocs, climnames] <- extractedUK
          }
        }
      }
    }
    #empty temp directory
    # unlink(tempDir, recursive=TRUE)
  }

  return(resultDf)
}
