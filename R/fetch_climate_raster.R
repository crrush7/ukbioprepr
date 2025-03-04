#' Download and create climate raster files
#'
#' This function downloads climate raster files from Zenodo online repository
#' Raster files are of all of the United Kingdom in British National Grid (EPSG:27700) or Northern Ireland in Irish Grid (EPSG:29903)
#' Output rasters are in line with grid of chosen region
#' Files are originally monthly but users can choose for output rasters to be monthly, seasonal or annual with custom date range
#' Seasonal and annual rasters can be created using min, max, mean or sum
#' Included variables are rain 'rain', average temperature 'tas', minimum temperature 'tasmin' and maximum temperature 'tasmax'
#' Rain is measured in (mm) and all temperatures are measured in degrees Celcius
#' @import terra

# get climate data by region, variable and date range
#
# Arguments:
#' @param   reg   - Either 'uk' for all of UK in EPSG:27700 British National Grid or 'ni' for Northern Ireland in EPSG:29902 Irish Grid
#' @param  cv    - Climate variable as a string: 'tas', 'tasmax', 'tasmin', or 'rain'
#' @param  start - Start date for output raster. Must be in ‘YYYY_MM’ format. Cannot be earlier than ‘1999_01’ or later than ‘2023_12’. Start must be before end.
#' @param  end   - End date for output raster. Must be in ‘YYYY_MM’ format. Cannot be earlier than ‘1999_01’ or later than ‘2023_12’. End must be later than start.
#' @param  time  - String of chosen time aggregate for output raster. Can be ‘monthly’, ‘seasonal’ and ‘annual’. The default is 'monthly.' If choosing ‘monthly’, each layer will be a month between start and end inclusive. If choosing ‘seasonal’, layers will be created for all complete seasons between start and end. Seasons are standardised as follows: Winter = December, January, February, Spring = March, April, May, Summer = June, July, August, Autumn = September, October, November. There are warnings for any incomplete seasons. If choosing ‘annual’, layers are created from the start date, for example, if start = ‘2012_04’, each annual layer will be calculated from April each year. There are warnings for incomplete years.
#' @param  agg   - Aggregation function when choosing seasonal or annual time. Can be 'mean', 'min', 'max' or 'sum.' Not required for time = 'monthly'.
# Returns:
#' @return  A subset raster of the selected climate variable, time parameter and date range at 1km resolution
#' @export
fetch_climate_raster <- function(reg, cv, start, end, time = 'monthly', agg = 0) {

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
  if (!time %in% c("monthly", "seasonal", "annual")) {
    stop("Invalid time choice. Please choose 'monthly', 'seasonal' or 'annual'.")
  }
  #Validate aggregation function choice
  if(time %in% c("seasonal", "annual") & missing(agg)) {
    stop("Please choose a function to aggregate the monthly data.")
  }
  if(time %in% c("seasonal", "annual") & (!agg %in% c("mean", "max", "min", "sum")))  {
    stop("Invalid function choice. Please enter 'mean', 'max', 'min' or 'sum'.")
  }
  aggFunct <- if (agg == 'min')
    min
  else if (agg == 'max')
    max
  else
    if (agg == 'sum')
      sum
  else
    if (agg == 'mean')
      mean
  else NULL

  #check if start and end is in correct format
  if (!grepl("^\\d{4}_\\d{2}$", start) ||
      !grepl("^\\d{4}_\\d{2}$", end)) {
    stop("Please provide valid 'start' and 'end' dates in 'YYYY_MM' format.")
  }
  dlClimate <- function(reg, cv) {
    #zenodo url once publihed
    baseUrl <- "https://zenodo.org/records/14841658/files/"

    #create file name
    name <- paste0(cv, "monthly", reg, ".nc")
    link <- paste0(baseUrl, name)

    #temp file location
    temp <- tempfile(fileext = ".nc")

    #warn user about changing timeout time if choosing uk
    if(reg == 'uk'){
      message(paste('The UK climate datasets are very large files. Increasing time out to 300 seconds.'))
      message(paste('You may need to increase it further to ensure full download by running options(timeout = x).'))
      options(timeout = 300)
    }
    #download
    tryCatch({
      download.file(link, temp, mode = "wb")
      message("Downloaded: ", link)
    }, error = function(e) {
      stop("Download failed: ", e$message)
    })

    #Load as spatraster
    cvraster <- rast(temp)
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
    year_month <- do.call(rbind, strsplit(layerNames[inRangeLayers], "_"))
    years <- as.integer(year_month[, 1])
    months <- as.integer(year_month[, 2])
    #extracting year and month from start and end date input YYYY_MM
    starty <- as.integer(substr(start, 1, 4))
    endy <- as.integer(substr(end, 1, 4))
    startm <- as.integer(substr(start, 6, 7))
    endm <- as.integer(substr(end, 6, 7))
    #define custom year (12 months)
    customYears <- seq(starty, endy)
    annualRasts <- list()

    for (y in customYears) {
      yearStart <- paste0(y, "_", sprintf("%02d", startm))
      if (startm == 1) {
        yearEnd <- paste0(y, "_12")
        annName <- paste0("clim_annual_", y)
      } else {
        yearEnd <- paste0(y + 1, "_", sprintf("%02d", startm - 1))
        annName <- paste0("clim_annual_", y, "_", startm, "-", y + 1, "_", startm -1)
      }
      annualLayers <- which(layerNames >= yearStart &
                              layerNames <= yearEnd)
      #warnings for incomplete years
      if (length(annualLayers) < 12) {
        warning(paste('Incomplete annual data from month', startm, ', year', y,  '- skipping'))
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
  }
  return(inRangeRast)
}
