#' Function for extracting data from all available environmental variable rasters
#'
#' This function extracts values of climate, soil and land cover from 1km resolution raster files over a specified time period. Raster files are stored in an online repository, therefore this function relies on an internet connection.\cr
#' Raster files cover climate variables from 1999 - 2023, soil properties at a range of depths and % cover of land classes between 2000 - 2023. \cr
#' Raster files are either of the whole of the United Kingdom in EPSG:27700, British National Grid, or of Northern Ireland in EPSG:29903, Irish Grid.
#' These raster data products have been created using original datasets from SoilGrids250m 2.0 for soils, HadUK Grid by UK Met Office for climate, and UK's Centre for Ecology and Hydrology for land cover.
#' Users may want to consider increasing time out time to allow all relevant data to be downloaded: options(timeout = x). Data products are downloaded to a temporary directory once during each session and are removed when the session ends. \cr
#' @import terra
#' @import igr
#' @import rnrfa
#' @importFrom utils download.file
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
#' "clay", clay (<0.002) in fine earth (percentage) \cr
#' "cfvo", vol fraction of coarse fragments (>2mm) (percentage) \cr
#' "sand", sand (> 0.05mm) in fine earth (percentage) \cr
#' "silt", silt (0.002-0.05mm) in fine earth (percentage) \cr
#' "wv0010", vol of water content at -10kPa (10-2cm3 cm-3)*10 \cr
#' "wv0033", vol of water content at -33kPa (10-2cm3 cm-3)*10 \cr
#' "wv1500", vol of water content at -1500kPa (10-2cm3 cm-3)*10 \cr
#' "cec", cation exchange capacity cmol(+)kg-1 \cr
#' "nitrogen", total nitrogen g kg-1 \cr
#' "phh2o", pH \cr
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
#' ‘sls’: supralittoral sediment \cr
#' ‘lr’: littoral rock \cr
#' ‘ls’: littoral sediment \cr
#' ‘sm’: saltmarsh \cr
#' ‘urb’: urban \cr
#' ‘sub’: suburban \cr
#' If ‘year’ spans before 2015, the class column names would instead be: \cr
#' ‘ara’, ‘blw’, ‘cw’, ‘fen’, ‘fw’, ‘lr’, ‘ls’, ‘slr’, ‘sls’, ‘sm’, ‘sub’, ‘sw’ and ‘urb’ as above and two aggregated classes of \cr
#' ‘grassagg’ for grasses and \cr
#' ‘upland’ for upland classes \cr
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
    #Check month is numeric
    if (!is.numeric(df$month)) {
      stop("'month' column must be numeric.")
    }
    #list months from df
    monthstot <- sort(unique(df$month))

    #Verify months
    #Validate year input
    if (any(monthstot < 1 | monthstot > 12)) {
      stop('Invalid month. Months must be between 1 - 12.')
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
      prop <- allProp
    } else {
      # validate input
      if (!is.character(soilprops)) {
        stop(
          "Properties must be a character vector (e.g., c('clay', 'sand')) or a single string."
        )
      }

      # check for invalid properties
      invalprop <- setdiff(soilprops, allProp)
      prop <- setdiff(soilprops, invalprop)

      if (length(invalprop) > 0) {
        warning(
          "The following properties are not available and will be ignored: ",
          paste(invalprop, collapse = ", ")
        )
      }

      # stop if no valid properties remain
      if (length(prop) == 0) {
        stop(
          "None of the provided soil properties are valid. Please choose from: ",
          paste(allProp, collapse = ", ")
        )
      }
    }
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
      coordsVect <- terra::vect(df, geom = c("X", "Y"), crs = crs)
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

      is_irish <- isIrish[i]
      is_british <- isBritish[i]

      result <- NULL
      grid_type <- NA_character_

      #Get coordinates depending on grid type
      if (is_irish) {
        result <- tryCatch(igr::igr_to_ig(ref), error = function(e) NULL)
        grid_type <- "Irish Grid"
      } else if (is_british) {
        result <- tryCatch(rnrfa::osg_parse(ref), error = function(e) NULL)
        if (!is.null(result) && all(c("easting", "northing") %in% names(result))) {
          result <- c(result$easting, result$northing)
          grid_type <- "British National Grid"
        } else {
          result <- NULL
        }
      }
      if (!is.null(result) && length(result) == 2) {
        df <- data.frame(
          gridRef = ref,
          X_transformed = as.numeric(result[1]),
          Y_transformed = as.numeric(result[2]),
          gridType = grid_type,
          stringsAsFactors = FALSE
        )
        if (landcover) df$year <- year
        if (climate) {
          df$year <- year
          df$month <- month
        }
        return(df)
      }

      #Log failed refs
      fail_row <- data.frame(
        gridRef = ref,
        X_transformed = NA_real_,
        Y_transformed = NA_real_,
        gridType = NA_character_,
        stringsAsFactors = FALSE
      )

      if (landcover) fail_row$year <- year
      if (climate) {
        fail_row$year <- year
        fail_row$month <- month
      }

      failedRefs <<- rbind(failedRefs, fail_row)
      return(fail_row)
    })

    #create dataframe
    resultDf <- as.data.frame(do.call(rbind, coords))
    #Issue a warning if any failed
    if (length(failedRefs) > 0) {
      warning(
        "Coordinates could not be returned for the following references: ",
        paste(failedRefs$gridRef, collapse = ", "),
        ". NA values were returned instead."
      )
    }
  }
  ##Determine what region is needed
  dlNI <- "Irish Grid" %in% resultDf$gridType
  dlUK <- "British National Grid" %in% resultDf$gridType

  #temp directory
  tempDir <- tempdir()

  #soils
  if (soil) {
    #Download data from Zenodo
    baseUrl <- "https://zenodo.org/records/14973735/files/"

    #initialise list
    rastList <- list()

    #dl rasters
    for (p in prop) {
      #NI
      if (dlNI) {
        fileNameNI <- paste0("ni", p, ".tif")
        fileUrlNI <- paste0(baseUrl, fileNameNI)
        tempNI <- file.path(tempDir, fileNameNI)

        # Download if file doesn't exist
        if (!file.exists(tempNI)) {
          tryCatch({
            download.file(fileUrlNI, tempNI, mode = "wb")
            message("Downloaded: ", fileNameNI)
          }, error = function(e) {
            warning("Failed to download: ", fileNameNI, ". Error: ", e$message)
          })
        } else {
          message("File already downloaded this session: ", fileNameNI, " — using cached version.")
        }

        # Try loading raster
        r <- suppressWarnings(try(rast(tempNI), silent = TRUE))

        if (inherits(r, "try-error") || nlyr(r) == 0) {
          message("Cached NI file is corrupt or unreadable. Redownloading: ", fileNameNI)
          file.remove(tempNI)
          tryCatch({
            download.file(fileUrlNI, tempNI, mode = "wb")
            message("Downloaded again: ", fileNameNI)
          }, error = function(e) {
            warning("Redownload failed: ", fileNameNI, ". Error: ", e$message)
          })
          r <- suppressWarnings(try(rast(tempNI), silent = TRUE))
        }

        if (!inherits(r, "try-error") && nlyr(r) > 0) {
          rastList[[paste0("ni_", p)]] <- r
        } else {
          warning("Could not load NI raster for: ", p)
        }
      }

      # UK
      if (dlUK) {
        fileNameUK <- paste0("uk", p, ".tif")
        fileUrlUK <- paste0(baseUrl, fileNameUK)
        tempUK <- file.path(tempDir, fileNameUK)

        if (!file.exists(tempUK)) {
          tryCatch({
            download.file(fileUrlUK, tempUK, mode = "wb")
            message("Downloaded: ", fileNameUK)
          }, error = function(e) {
            warning("Failed to download: ", fileNameUK, ". Error: ", e$message)
          })
        } else {
          message("File already downloaded this session: ", fileNameUK, " — using cached version.")
        }

        r <- suppressWarnings(try(rast(tempUK), silent = TRUE))

        if (inherits(r, "try-error") || nlyr(r) == 0) {
          message("Cached UK file is corrupt or unreadable. Redownloading: ", fileNameUK)
          file.remove(tempUK)
          tryCatch({
            download.file(fileUrlUK, tempUK, mode = "wb")
            message("Downloaded again: ", fileNameUK)
          }, error = function(e) {
            warning("Redownload failed: ", fileNameUK, ". Error: ", e$message)
          })
          r <- suppressWarnings(try(rast(tempUK), silent = TRUE))
        }

        if (!inherits(r, "try-error") && nlyr(r) > 0) {
          rastList[[paste0("uk_", p)]] <- r
        } else {
          warning("Could not load UK raster for: ", p)
        }
      }
    }
    message('Extracting soil data.')
    #Extract soil data
    for (p in prop) {
      if (dlNI & paste0("ni_", p) %in% names(rastList)) {
        niRast <- rastList[[paste0("ni_", p)]]
        nilocs <- which(resultDf$gridType == 'Irish Grid')
        if (length(nilocs) > 0) {
          extractedNI <- terra::extract(niRast, resultDf[nilocs, c("X_transformed", "Y_transformed")], ID = FALSE)
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
          extractedUK <- terra::extract(ukRast, resultDf[uklocs, c("X_transformed", "Y_transformed")], ID = FALSE)
          #some rasters have one layer
          if (is.vector(extractedUK)) {
            extractedUK <- data.frame(extractedUK)
          }
          #Get layer names for columns
          colnames(extractedUK) <- paste0(p, "_", names(ukRast))
          #Add extracted values to results df
          resultDf[uklocs, colnames(extractedUK)] <- extractedUK
        }
      }
    }
  }
  #land cover
  if (landcover) {
    # Check if land cover data is currently available
    lc_check <- try(
      download.file(
        "https://zenodo.org/records/14849882/files/2020ni.tif",
        tempfile(), quiet = TRUE, mode = "wb"
      ),
      silent = TRUE
    )
    if (inherits(lc_check, "try-error") || !identical(lc_check, 0L)) {
      message(
        "Land cover data is temporarily unavailable. ",
        "The data repository is currently being updated. ",
        "Please check https://github.com/crrush7/ukbioprepr for updates. ",
        "Land cover extraction has been skipped. ",
        "Climate and soil extractions are unaffected."
      )
      landcover <- FALSE
    }
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

    #dl rasters
    #dl rasters
    dlRaster <- function(region, year, isAgg) {
      suffix <- ifelse(isAgg, "agg", "")
      fileName <- paste0(year, region, suffix, ".tif")
      fileUrl <- paste0(baseUrl, fileName)
      tempPath <- file.path(tempDir, fileName)

      #Check if file exists
      if (!file.exists(tempPath)) {
        tryCatch({
          download.file(fileUrl, tempPath, mode = "wb")
          message("Downloaded: ", fileName)
        }, error = function(e) {
          warning("Failed to download: ", fileName, ". Error: ", e$message)
        })
      } else {
        message("File already downloaded during this session: ", fileName, " — using cached version.")
      }

      #Try loading the raster
      r <- suppressWarnings(try(terra::rast(tempPath), silent = TRUE))

      if (inherits(r, "try-error") || terra::nlyr(r) == 0) {
        message("Cached file is corrupt or unreadable. Redownloading: ", fileName)
        file.remove(tempPath)
        tryCatch({
          download.file(fileUrl, tempPath, mode = "wb")
          message("Downloaded again: ", fileName)
        }, error = function(e) {
          warning("Redownload failed: ", fileName, ". Error: ", e$message)
        })
        r <- suppressWarnings(try(terra::rast(tempPath), silent = TRUE))
      }

      if (!inherits(r, "try-error") && terra::nlyr(r) > 0) {
        return(r)
      } else {
        warning("Could not load raster for: ", fileName)
        return(NULL)
      }
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
      rowRef <- resultDf$gridRef[i]
      #skip if any NA values
      if (anyNA(c(y, rowRef))) next
      #NI
      if (resultDf$gridType[i] == "Irish Grid" &&
          !is.na(resultDf$gridType[i]) &&
          paste0("ni_", y) %in% names(rastList)) {
        niRast <- rastList[[paste0("ni_", y)]]
        extractedNI <- terra::extract(niRast, resultDf[i, c("X_transformed", "Y_transformed")], ID = FALSE) #Remove cell ID

        if (!is.null(extractedNI) && ncol(extractedNI) > 0) {
          cover <- names(niRast)
          colnames(extractedNI) <- cover
          resultDf[i, cover] <- extractedNI
        }
      }

      #UK
      if (resultDf$gridType[i] == "British National Grid" &&
          !is.na(resultDf$gridType[i]) &&
          paste0("uk_", y) %in% names(rastList)) {
        ukRast <- rastList[[paste0("uk_", y)]]
        extractedUK <- terra::extract(ukRast, resultDf[i, c("X_transformed", "Y_transformed")], ID = FALSE) #Remove cell ID

        if (!is.null(extractedUK) && ncol(extractedUK) > 0) {
          cover <- names(ukRast)
          colnames(extractedUK) <- cover
          resultDf[i, cover] <- extractedUK
        }
      }
    }
  }
  #climate
  if (climate) {
    #Define standard seasons
    seasonsDef <- list(
      "winter" = c(12, 1, 2),
      "spring" = c(3, 4, 5),
      "summer" = c(6, 7, 8),
      "autumn" = c(9, 10, 11)
    )


    #Extract earliest and latest year & month from df
    minYear <- min(as.numeric(resultDf$year))
    maxYear <- max(as.numeric(resultDf$year))
    minMonth <- min(as.numeric(resultDf$month[resultDf$year == minYear]))
    maxMonth <- max(as.numeric(resultDf$month[resultDf$year == maxYear]))

    #Initialize final start and end dates
    startyear <- minYear
    startmonth <- minMonth
    endyear <- maxYear
    endmonth <- maxMonth

    #Seasonal Only
    if ("seasonal" %in% climtime && !("annual" %in% climtime)) {
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
    if ("annual" %in% climtime && !("seasonal" %in% climtime)) {
      startmonth <- annualstartmonth
      endmonth <- ifelse(annualstartmonth == 1, 12, annualstartmonth - 1)
      #If the annual period spans two calendar years, adjust endyear
      if(minMonth < startmonth){
        startyear <- startyear - 1}
      if(maxMonth > endmonth)
        endyear <- endyear + 1
    }
    #Annual and seasonal
    if ('annual' %in% climtime && 'seasonal' %in% climtime){
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
    #Download data from Zenodo
    baseUrl <- "https://zenodo.org/records/14913772/files/"

    if (dlUK) {
      message(
        "Your climate dataset contains UK data. These files are very large. Please consider increasing your timeout through options(timeout = x)."
      )
    }

    #initialise list
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

          })

          r <- suppressWarnings(try(rast(tempNI), silent = TRUE))
          if (inherits(r, "try-error") || nlyr(r) == 0) {
            warning("Still invalid after redownload: ", fileNameNI)

          }
        }

        rastList[[paste0("ni_", y)]] <- r
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

          })

          r <- suppressWarnings(try(rast(tempUK), silent = TRUE))
          if (inherits(r, "try-error") || nlyr(r) == 0) {
            warning("Still invalid after redownload: ", fileNameUK)

          }
        }

        rastList[[paste0("uk_", y)]] <- r
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
            yearEnd <- paste0(y, "_12")
            annName <- paste0(envVar,
                              "_",
                              y,
                              "_",              # ADD UNDERSCORE HERE TOO!
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
        rowym <- paste0(rowYear, "_", rowMonth)
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
            timerange <- sub(paste0("^", cv, "_"), "", layer)
            parts <- unlist(strsplit(timerange, "_"))
            startym <- paste0(parts[1], parts[2])  # Combine year + month
            endym <- paste0(parts[3], parts[4])
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
    if ('seasonal' %in% climtime) {
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
  }

  return(resultDf)
}
