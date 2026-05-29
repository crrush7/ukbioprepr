#' Download and create climate raster files
#'
#' This function downloads climate raster files from Zenodo online repository and therefore requires an internet connection. \cr
#' Raster files are of all of the United Kingdom in British National Grid (EPSG:27700) or Northern Ireland in Irish Grid (EPSG:29903).\cr
#' Output rasters are in line with grid of chosen region \cr
#' Files are originally monthly but users can choose for output rasters to be monthly, seasonal or annual with custom date range. \cr
#' Seasonal and annual rasters can be created using min, max, mean or sum as aggregations for example, the annual total rainfall, or the mean seasonal rainfall. \cr
#' Included variables are rain 'rain', average temperature 'tas', minimum temperature 'tasmin' and maximum temperature 'tasmax'. \cr
#' Rain is measured in (mm) and all temperatures are measured in degrees Celcius. \cr
#' Users may want to consider increasing time out time to allow all relevant data to be downloaded: options(timeout = x). Data products are downloaded to a temporary directory once during each session and are removed when the session ends. \cr
#' These data products were created using original datasets from UK's Met Office HadUk Grid.
#' @import terra
#' @importFrom utils download.file

# get climate data by region, variable and date range
#
# Arguments:
#' @param reg Either 'uk' for all of UK in EPSG:27700 British National Grid or 'ni' for Northern Ireland in EPSG:29902 Irish Grid
#' @param cv Climate variable as a string: 'tas', 'tasmax', 'tasmin', or 'rain'
#' @param start Start date for output raster. Must be in ‘YYYY_MM’ format. Cannot be earlier than ‘1999_01’ or later than ‘2023_12’. Start must be before end.
#' @param end End date for output raster. Must be in ‘YYYY_MM’ format. Cannot be earlier than ‘1999_01’ or later than ‘2023_12’. End must be later than start.
#' @param time String of chosen time aggregate for output raster. Can be ‘monthly’, ‘seasonal’ and ‘annual’. \cr
#' If choosing ‘monthly’, each layer will be a month between start and end inclusive. \cr
#' If choosing ‘seasonal’, layers will be created for all complete seasons between start and end.
#' Seasons are standardised as follows:\cr
#' Winter = December, January, February\cr
#' Spring = March, April, May \cr
#' Summer = June, July, August \cr
#' Autumn = September, October, November\cr
#' There are warnings for any incomplete seasons \cr
#' If choosing ‘annual’, layers are created from the start date, for example, if start = ‘2012_04’, each annual layer will be calculated from April each year.\cr
#' There are warnings for incomplete years.
#' @param  agg  Aggregation function when choosing seasonal or annual time. Can be 'mean', 'min', 'max' or 'sum.' Not required for time = 'monthly'. The default is as follows: \cr
#' ‘rain’ = total (sum) rainfall during time frame \cr
#' ‘tas’ = mean temperature during time frame \cr
#' ‘tasmin’ = minimum temperature during time frame \cr
#' ‘tasmax’ = maximum temperature during time frame \cr
# Returns:
#' @return  A subset raster of the selected climate variable, time parameter and date range at 1km resolution.
#' @export
fetch_climate_raster <- function(reg, cv, start, end, time=NULL, agg=NULL) {

  #Validate region
  if (!reg %in% c("uk", "ni")) {
    stop("Invalid region. Please choose 'uk' or 'ni'.")
  }
  #Validate variable choice
  if (!cv %in% c("tas", "tasmax", "tasmin", "rain")) {
    stop(
      "Invalid climate variable choice. Please choose 'tas', 'tasmax', 'tasmin' or 'rain'."
    )
  }
  #Validate time choice
  if(is.null(time)){
    stop("Invalid time choice. Please choose 'monthly', 'seasonal' or 'annual'.")
  }
  if (!time %in% c("monthly", "seasonal", "annual")) {
    stop("Invalid time choice. Please choose 'monthly', 'seasonal' or 'annual'.")
  }

  #Handle agg depending on time
  if (time == "monthly") {
    if (!is.null(agg)) {
      message("Aggregation function is not needed for monthly data and will be ignored.")
    }
    aggFunct <- NULL
  } else {
    #Set default agg if not provided
    if (is.null(agg)) {
      agg <- switch(cv,
                    "rain" = "sum",
                    "tas" = "mean",
                    "tasmax" = "max",
                    "tasmin" = "min")
    }

    #Validate agg value
    if (!agg %in% c("mean", "max", "min", "sum")) {
      stop("Invalid aggregation. Choose from 'mean', 'max', 'min', or 'sum'.")
    }

    #Assign actual function
    aggFunct <- switch(agg,
                       "mean" = mean,
                       "max" = max,
                       "min" = min,
                       "sum" = sum)
  }


  #check if start and end is in correct format
  if (!grepl("^\\d{4}_\\d{2}$", start) ||
      !grepl("^\\d{4}_\\d{2}$", end)) {
    stop("Please provide valid 'start' and 'end' dates in 'YYYY_MM' format.")
  }

  #Convert to Date objects for comparison
  start_date <- as.Date(paste0(start, "_01"), format = "%Y_%m_%d")
  end_date   <- as.Date(paste0(end, "_01"), format = "%Y_%m_%d")

  #Check that start is before or equal to end
  if (start_date > end_date) {
    stop("'start' date must be earlier than or equal to 'end' date.")
  }
  #Check range
  min_date <- as.Date("1999_01_01", format = "%Y_%m_%d")
  max_date <- as.Date("2023_12_01", format = "%Y_%m_%d")
  if (start_date < min_date || end_date > max_date) {
    stop("Dates must be between '1999_01' and '2023_12'.")
  }

 dlClimate <- function(reg, cv) {
   baseUrl <- "https://zenodo.org/records/14841658/files/"
   name <- paste0(cv, "monthly", reg, ".nc")
   link <- paste0(baseUrl, name)
   tempDir <- tempdir()
   temp <- file.path(tempDir, name)

   if (reg == 'uk') {
     message("The UK climate datasets are large files.")
     message("You may need to increase the timeout with options(timeout = x).")
   }

   # Download file if it doesn't exist
   if (!file.exists(temp)) {
     tryCatch({
       download.file(link, temp, mode = "wb")
       message("Downloaded: ", link)
     }, error = function(e) {
       stop("Download failed: ", e$message)
     })
   } else {
     message("File already downloaded during this session ", name, " - using cached version.")
   }

   #Try to open the raster
   cvraster <- suppressWarnings(try(rast(temp), silent = TRUE))

   #If reading failed or has zero layers, assume it's corrupt
   if (inherits(cvraster, "try-error") || nlyr(cvraster) == 0) {
     message("Cached file is corrupt or unreadable. Redownloading...")

     file.remove(temp)

     tryCatch({
       download.file(link, temp, mode = "wb")
       message("Downloaded: ", link)
     }, error = function(e) {
       stop("Download failed again: ", e$message)
     })

     # Try again after redownloading
     cvraster <- suppressWarnings(try(rast(temp), silent = TRUE))

     if (inherits(cvraster, "try-error") || nlyr(cvraster) == 0) {
       stop("Redownloaded file is still invalid. Please check your internet connection or increase timeout.")
     }
   }

   return(cvraster)
 }

  #Download raster
  x <- dlClimate(reg, cv)
  #Extract layer names and transform
  layerNames <- sub("^clim_", "", names(x))
  inRangeLayers <- which(layerNames >= start & layerNames <= end)
  #validate layers
  if (length(inRangeLayers) == 0) {
    stop("No data for this range.")
  }
  #subset the raster
  inRangeRast <- x[[inRangeLayers]]

  #Handling seasonal
  if (time == "seasonal") {
    #Extracting years and months from layer names
    year_month <- do.call(rbind, strsplit(layerNames[inRangeLayers], "_"))
    years <- as.integer(year_month[, 1])
    months <- as.integer(year_month[, 2])

    #following standard seasons
    seasonsDef <- list(
      "winter" = c(12, 1, 2),
      "spring" = c(3, 4, 5),
      "summer" = c(6, 7, 8),
      "autumn" = c(9, 10, 11)
    )
    #validating seasons available in choice
    seasonRasts <- list()
    for (y in unique(years)) {
      for (season in names(seasonsDef)) {
        seasonMonths <- seasonsDef[[season]]
        #Winter spans two calendar years
        if (season == "winter") {
          seasonLayers <- which((years == y &
                                   months == 12) |
                                  (years == y + 1 &
                                     months %in% c(1, 2)))
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
        #aggregate function
        seasonRast <- app(x[[inRangeLayers[seasonLayers]]], aggFunct, na.rm = TRUE)
        names(seasonRast) <- paste0("clim_", season, "_", seasonYear)
        seasonRasts <- c(seasonRasts, seasonRast)
      }
    }
    if (length(seasonRasts) == 0) {
      stop("No complete seasonal data available for selected date range.")
    }
    inRangeRast <- do.call(c, seasonRasts)

  }
  #annual
  if (time == 'annual') {
    #layer names
    year_month <- do.call(rbind, strsplit(layerNames[inRangeLayers], "_"))
    years <- as.integer(year_month[, 1])
    months <- as.integer(year_month[, 2])
    layerDates <- as.Date(sprintf("%d-%02d-01", years, months))

    #lookup
    layerLookup <- data.frame(
      name = layerNames[inRangeLayers],
      date = layerDates,
      index = inRangeLayers
    )

    #Extract start/end
    starty <- as.integer(substr(start, 1, 4))
    endy <- as.integer(substr(end, 1, 4))
    startm <- as.integer(substr(start, 6, 7))

    #Loop over years
    customYears <- seq(starty, endy)
    annualRasts <- list()

    for (y in customYears) {
      if (startm == 1) {
        yearStartDate <- as.Date(sprintf("%d-%02d-01", y, startm))
        yearEndDate   <- as.Date(sprintf("%d-12-01", y))
        annName <- paste0("clim_annual_", y)
      } else {
        yearStartDate <- as.Date(sprintf("%d-%02d-01", y, startm))
        yearEndDate   <- as.Date(sprintf("%d-%02d-01", y + 1, startm - 1))
        annName <- paste0("clim_annual_", y, "_", startm, "-", y + 1, "_", startm -1)
      }

      #Find matching layers
      inYear <- layerLookup$date >= yearStartDate & layerLookup$date <= yearEndDate

      if (sum(inYear) < 12) {
        warning(paste('Incomplete annual data from month', startm, ', year', y,  '- skipping'))
        next
      }

      annualLayers <- layerLookup$index[inYear]
      annualRast <- app(x[[annualLayers]], aggFunct, na.rm = TRUE)
      names(annualRast) <- annName
      annualRasts <- c(annualRasts, annualRast)
    }

    if (length(annualRasts) == 0) {
      stop('No complete annual data available for selected date range.')
    }

    inRangeRast <- do.call(c, annualRasts)
  }

  return(inRangeRast)
}
