<img src="man/figures/ukbiopreprlogobg.png" width="214"/>

# ukbioprepr

<!-- badges: start -->
[![codecov](https://codecov.io/github/crrush7/ukbioprepr/branch/main/graph/badge.svg?token=RMFJRR4YY2)](https://codecov.io/github/crrush7/ukbioprepr)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

**An R package to support reproducible preparation of environmental data for biodiversity modelling in the UK**

`ukbioprepr` is an R package designed to streamline the process of preparing environmental predictor variables for biodiversity modelling. It supports extractions using grid references and coordinates across the UK and Northern Ireland, and provides easy access to curated data products covering soil properties, land cover and climate variables. Both land cover and climate data are available from the years 2000 to 2023. Data products can be downloaded in raster format at a 1 km resolution for the entirety of the UK in British National Grid (EPSG:27700) or for Northern Ireland alone in Irish Grid (EPSG:29903).

------------------------------------------------------------------------

## Installation

Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("crrush7/ukbioprepr")
```

Once installed, load the package:

```r
library(ukbioprepr)
```

------------------------------------------------------------------------

## Data Product Information

The `ukbioprepr` package provides access to several classes of data products. All data products are available at 1 km resolution. 

### Soil Properties

All soil properties are available at a range of depths: 0-5 cm, 5-15 cm, 15-30 cm, 30-60 cm, 60-100 cm, 100-200 cm, except for Organic Carbon Stocks (ocs) which is available only at 0-3 cm depth. Soil property codes correspond to names of properties in either raster layers or outputted data frames. 

| Code     | Description                                    |
|----------|------------------------------------------------|
| ocd      | Organic carbon density (kg m⁻³)                |
| bdod     | Bulk density of fine earth (kg dm⁻³)           |
| clay     | Clay content (\<0.002 mm) in fine earth (%)    |
| cfvo     | Coarse fragments (\>2 mm) volume fraction (%)  |
| sand     | Sand content (\>0.05 mm) in fine earth (%)     |
| silt     | Silt content (0.002–0.05 mm) in fine earth (%) |
| wv0010   | Water content at −10 kPa (cm³ cm⁻³ ×10)        |
| wv0033   | Water content at −33 kPa (cm³ cm⁻³ ×10)        |
| wv1500   | Water content at −1500 kPa (cm³ cm⁻³ ×10)      |
| cec      | Cation exchange capacity (cmol⁺ kg⁻¹)          |
| nitrogen | Total nitrogen (g kg⁻¹)                        |
| phh2o    | Soil pH                                        |
| soc      | Soil organic carbon in fine earth (g kg⁻¹)     |
| ocs      | Organic carbon stocks (kg m⁻²)                 |

**Data Source:** Soil property data products are derived from [SoilGrids](https://soilgrids.org)

### Climate Variables

Available climate variables: 
- **Mean Temperature** (`tas`) 
- **Maximum Temperature** (`tasmax`) 
- **Minimum Temperature** (`tasmin`) 
- **Precipitation** (`rain`) 

Available time periods: 
- Monthly 
- Seasonal 
- Annual

**Coverage:** 1999-2023

**Data Source:** Climate variable data products are derived from the [Met Office HadUK-Grid](https://www.metoffice.gov.uk/research/climate/maps-and-data/data/haduk-grid/datasets)

### Land Cover

There are two sets of land cover data products: 2000-2023 and 2015-2023. The latter contain more detailed classes whilst the former has some aggregated classes due to changes in classification in original data sources. Land cover codes correspond to names of cover classes in either raster layers or outputted data frames.

#### 2015-2023 Land Cover Classes

| Code | Description            |
|------|------------------------|
| blw  | Broad-leaved woodland  |
| cw   | Coniferous Woodland    |
| ara  | Arable Land            |
| ig   | Improved Grassland     |
| ng   | Neutral Grassland      |
| cg   | Calcareous Grassland   |
| ag   | Acid Grassland         |
| fen  | Fen                    |
| hea  | Heather                |
| hgl  | Heather Grassland      |
| bog  | Bog                    |
| inr  | Inland Rock            |
| sw   | Saltwater              |
| fw   | Freshwater             |
| slr  | Supralittoral Rock     |
| sls  | Supralittoral Sediment |
| lr   | Littoral Rock          |
| ls   | Littoral Sediment      |
| sm   | Saltmarsh              |
| urb  | Urban                  |
| sub  | Suburban               |

#### 2000-2023 Land Cover Classes

| Code     | Description            |
|----------|------------------------|
| blw      | Broad-leaved woodland  |
| cw       | Coniferous Woodland    |
| ara      | Arable Land            |
| fen      | Fen                    |
| fw       | Freshwater             |
| sw       | Saltwater              |
| grassagg | Aggregated Grassland   |
| slr      | Supralittoral Rock     |
| sls      | Supralittoral Sediment |
| lr       | Littoral Rock          |
| ls       | Littoral Sediment      |
| sm       | Saltmarsh              |
| urb      | Urban                  |
| sub      | Suburban               |
| upland   | Aggregated Upland      |

**Data Format:** Land cover data products are percentage cover

**Data Source:** Derived from the [UK Centre for Ecology and Hydrology Land Cover Maps](https://www.ceh.ac.uk/data/ukceh-land-cover-maps)

------------------------------------------------------------------------

## Example Usage

### Fetch Climate Rasters

```r
library(ukbioprepr) 
library(terra)
library(tidyterra)
library(ggplot2)
library(sf)
library(dplyr)


#Download a raster of seasonal average temperatures in Northern Ireland 
#between 2010-2013
ni_seasonal_tas_raster <- fetch_climate_raster(
  reg = 'ni', 
  cv = 'tas', 
  start = '2010_12', 
  end = '2013_12', 
  time = 'seasonal', 
  agg = 'mean'
)

correct_order <- c(
  "clim_winter_2010-2011",
  "clim_spring_2011",
  "clim_summer_2011",
  "clim_autumn_2011",
  "clim_winter_2011-2012",
  "clim_spring_2012",
  "clim_summer_2012",
  "clim_autumn_2012",
  "clim_winter_2012-2013",
  "clim_spring_2013",
  "clim_summer_2013",
  "clim_autumn_2013"
)

#Reorder the raster layers
ni_seasonal_tas_raster <- ni_seasonal_tas_raster[[correct_order]]

#Now plot
seasonal_plot <- ggplot() +
  geom_spatraster(data = ni_seasonal_tas_raster) +
  facet_wrap(~factor(lyr, levels = correct_order), ncol = 4) +
  scale_fill_viridis_c(option = "plasma", name = "Temperature (°C)", na.value = "transparent") +
  coord_sf(crs = st_crs(29903), datum = st_crs(29903)) +
  guides(x = guide_axis(n.dodge = 1)) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 6, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 7),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    panel.spacing = unit(0.5, "lines")
  ) +
  labs(
    title = "Seasonal Temperature Averages - Northern Ireland",
    x = "Easting (m)",
    y = "Northing (m)"
  )
seasonal_plot

#Download a raster of total annual precipitation in the United Kingdom 
#between 2005-2016 where annual calculations run from March to February 
#of the following year
uk_annual_rain_raster <- fetch_climate_raster(
  reg = 'uk', 
  cv = 'rain', 
  start = '2005_03', 
  end = '2008_02', 
  time = 'annual', 
  agg = 'sum'
)
#Convert to dataframe
precip_df <- as.data.frame(uk_annual_rain_raster, xy = TRUE)

#Reshape to long format
precip_long <- precip_df %>%
  pivot_longer(cols = -c(x, y), 
               names_to = "layer", 
               values_to = "precipitation")

#Plot
precip_plot <- ggplot(precip_long, aes(x = x, y = y, fill = precipitation)) +
  geom_raster() +
  facet_wrap(~layer, ncol = 4) +
  scale_fill_viridis_c(option = "viridis", name = "Precipitation (mm)", na.value = "transparent") +
  coord_equal() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 6, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 7),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    panel.spacing = unit(0.5, "lines")
  ) +
  labs(
    title = "Annual Precipitation - United Kingdom",
    x = "Easting (m)",
    y = "Northing (m)"
  )

precip_plot
```
### Fetch Soil Rasters
```r
#fetching raster on pH at range of depths
uk_soil <- fetch_soil_raster('uk', prop = 'phh2o')
#Convert to dataframe
soil_df <- as.data.frame(uk_soil, xy = TRUE)

#Reshape to long format
soil_long <- soil_df %>%
  pivot_longer(cols = -c(x, y), 
               names_to = "layer", 
               values_to = "pH") %>%
  mutate(layer = factor(layer, levels = names(uk_soil)))

#Plot
ph_plot <- ggplot(soil_long, aes(x = x, y = y, fill = pH)) +
  geom_raster() +
  facet_wrap(~layer, ncol = 3) +
  scale_fill_viridis_c(option = "viridis", name = "pH", na.value = "transparent") +
  coord_equal() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 6, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 7),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    panel.spacing = unit(0.5, "lines")
  ) +
  labs(
    title = "pH - United Kingdom",
    x = "Easting (m)",
    y = "Northing (m)"
  )
ph_plot
```### Extract Soil Values

```r
# Extract values for selected soil properties using grid references 
# in both Irish and British grid
gridRef <- c('J3480', 'NT1565', 'SS9782', 'TQ2879', 'NW230721')
soildf <- as.data.frame(gridRef)

soilres <- extract_soil_values(
  type = 'grid', 
  df = soildf, 
  prop = c('nitrogen', 'phh2o', 'sand')
)
```

### Extract Land Cover Values

```r
# Extract values for land cover using coordinates in British National Grid 
X <- c(315000)
Y <- c(665000)
lcdf <- as.data.frame(X)
lcdf$Y <- Y
lcdf$year <- 2020

lcres <- extract_landcover_values(
  type = 'coords', 
  df = lcdf, 
  crs = 'EPSG:27700'
)
```

### Extract All Environmental Variables

```r
# Extract values for soil properties, land cover and climate variables 
# using longitude and latitude coordinates 
X <- c(-3.3599286, -7.86877046557228, -6.74336745843602, -3.7908580004141)
Y <- c(55.870643, 54.4262065989996, 54.6213752198463, 52.0828922843592)
alldf <- as.data.frame(X)
alldf$Y <- Y
alldf$year <- c(2020, 2021, 2022, 2020)
alldf$month <- c(4, 4, 5, 6)

allres <- extract_all_values(
  type = 'coords',
  crs = 'EPSG:4326',
  df = alldf,
  soil = TRUE,
  soilprops = c('nitrogen', 'cec'),
  landcover = TRUE,
  climate = TRUE, 
  climvar = c('tasmax', 'tasmin'), 
  climtime = c('monthly', 'annual'), 
  annualstartmonth = 1
)
```

------------------------------------------------------------------------

## Main Functions

- `fetch_climate_raster()` - Download climate NetCDF rasters
- `fetch_soil_raster()` - Download soil raster files  
- `fetch_landcover_raster()` - Download land cover rasters
- `extract_climate_values()` - Extract climate data for specific locations/times
- `extract_soil_values()` - Extract soil properties for specific locations
- `extract_landcover_values()` - Extract land cover for specific locations/years
- `extract_all_values()` - Batch extraction of all environmental variables

For detailed documentation on any function:

```r
?extract_climate_values
?extract_all_values
```

------------------------------------------------------------------------

## Features

- 🌍 **Flexible input formats**: Works with grid references (Irish/British) or coordinates (any CRS)
- 📊 **Multiple data types**: Climate, soil, and land cover in one package
- ⏱️ **Temporal flexibility**: Monthly, seasonal, and annual climate aggregations
- 💾 **Smart caching**: Downloaded rasters are cached locally to avoid re-downloading
- 🎯 **Batch processing**: Extract all variables in a single function call
- ✅ **Reproducible**: Consistent data sources and processing methods
- 📏 **High resolution**: All data at 1 km resolution

------------------------------------------------------------------------

## Package Quality

- **Test Coverage:** 86.87% with 584+ comprehensive tests
- **Documentation:** Complete roxygen2 documentation for all functions

------------------------------------------------------------------------

## Citation

If you use `ukbioprepr` in your research, please cite:

```r
citation("ukbioprepr")
```


------------------------------------------------------------------------

## Contributing

Contributions are welcome! 

- **Bug reports:** Use our [bug report template](.github/ISSUE_TEMPLATE/bug_report.md)
- **Feature requests:** Use our [feature request template](.github/ISSUE_TEMPLATE/feature_request.md)

------------------------------------------------------------------------

## Issues and Support

If you encounter any issues or have questions:

- **Bug reports:** Please open an issue on our [GitHub Issues page](https://github.com/crrush7/ukbioprepr/issues) using the bug report template
- **Feature requests:** Use the feature request template in Issues
- **Questions:** Open a discussion or contact the maintainer

When reporting bugs, please include:
- A minimal reproducible example
- Your R version and operating system
- Expected vs actual behavior
- Output from `sessionInfo()`

------------------------------------------------------------------------

## License

GPL-3

------------------------------------------------------------------------

## Acknowledgments

This package integrates data from several important sources:

- **Soil data:** [SoilGrids](https://soilgrids.org)
- **Climate data:** [Met Office HadUK-Grid](https://www.metoffice.gov.uk/research/climate/maps-and-data/data/haduk-grid/datasets)  
- **Land cover data:** [UK Centre for Ecology and Hydrology](https://www.ceh.ac.uk/data/ukceh-land-cover-maps)

We thank these organisations for making their data publicly available.

------------------------------------------------------------------------

## Contact

Charlotte R. Rush - cr23569@essex.ac.uk

Project Link: [https://github.com/crrush7/ukbioprepr](https://github.com/crrush7/ukbioprepr)
