<img src="man/figures/ukbiopreprlogobg.png" width="214"/>

# ukbioprepr

**An R package to support reproducible preparation of environmental data for biodiversity modelling in the UK**

`ukbioprepr` is an R package designed to streamline the process of preparing environmental predictor variables for biodiversity modelling. It supports extractions using grid references and coordinates across the UK and Northern Ireland, and provides easy access to curated data products covering soil properties, land cover and climate variables. Both land cover and climate data are available from the years 2000 to 2023. Data products can be downloaded in raster format at a 1 km resolution for the entirety of the UK in British National Grid (EPSG:27700) or for Northern Ireland alone in Irish Grid (EPSG:29903).

------------------------------------------------------------------------

## Installation

Install from GitHub

`remotes::install_github("crrush7/ukbioprepr")`

------------------------------------------------------------------------

## Data Product Information

The `ukbioprepr` package provides access to several classes of data products.

### Soil Properties

All soil properties are available at a range of depths: 0-5 cm, 5-15 cm, 15-30 cm, 30-60 cm, 60-100 cm, 100-200 cm, except for Organic Carbon Stocks (ocs) which is available only at 0-3 cm depth.

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

Soil property data products are derived from SoilGrids: <https://soilgrids.org>\

### Climate Variables

You can request: \
- **Mean Temperature** (`tas`) \
- **Maximum Temperature** (`tasmax`) \
- **Minimum Temperature** (`tasmin`) \
- **Precipitation** (`rain`) \
\
Available time periods: \
- Monthly \
- Seasonal \
- Annual\
\
Climate variable data products are derrived from the Met Office HadUKGrid: <https://www.metoffice.gov.uk/research/climate/maps-and-data/data/haduk-grid/datasets>\


### Land Cover

There are two sets of land cover data products: 2000-2023 and 2015-2023.\
The latter contain more detailed classes whilst the former has some aggregated classes due to changes in classification in original data sources.\
\
\
**2015 - 2023 Land Cover Classes**\

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

**2000 - 2023 Land Cover Classes**

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

Land cover data products are percentage cover and derived from the UK Centre for Hydrology and Ecology: <https://www.ceh.ac.uk/data/ukceh-land-cover-maps>

------------------------------------------------------------------------

## Example Usage

```         
#Load the package 
library(ukbioprepr) 

#Download a raster of seasonal average temperatures in Northern Ireland between 2010 - 2020

ni_seasonal_tas_raster <- fetch_climate_raster(
  reg = 'ni', 
  cv = 'tas', 
  start = '2010_01', 
  end = '2020_12', 
  time = 'seasonal', 
  agg = 'mean'
)

#Download a raster of total annual precipitation in the United Kingdom between 2005 - 2008 where annual calculations run from March to February of the following year. 

uk_annual_rain_raster <- fetch_climate_raster(
  reg = 'uk', 
  cv = 'rain', 
  start = '2005_03', 
  end = '2008_02', 
  time = 'annual', 
  agg = 'sum'
)

#Extract values for selected soil properties using grid references in both Irish and British grid
gridRef <- c('J3480', 'NT1565', 'SS9782', 'TQ2879', 'NW230721')
soildf <- as.data.frame(gridRef)
soilres <- extract_soil_values(
  type = 'grid', 
  df = soildf, 
  prop = c('nitrogen', 'phh2o', 'sand')
)

#Extract values for land cover using coordinates in British National Grid 
X <- c('315000')
Y <- c('665000')
lcdf <- as.data.frame(X)
lcdf$Y <- Y
lcdf$year <- 2020
lcres <- extract_landcover_values(
  type = 'coords', 
  df = lcdf, 
  crs = 'EPSG:27700'
)

#Extract values for soil properties, land cover and climate variables using longitude and latitude coordinates 
X <- c(-3.3599286, -7.86877046557228, -6.74336745843602, -3.7908580004141)
Y <- c(55.870643, 54.4262065989996, 54.6213752198463, 52.0828922843592)
alldf <- as.data.frame(X)
alldf$Y <- Y
alldf$year <- c(2020, 2021, 2022, 2020, 2022, 2020, 2020, 2021)
alldf$month <- c(4, 4, 5, 6, 1, 12, 11, 12)
allres <- extract_all_values(
  type = 'coords',
  crs = 'EPSG:4326',
  alldf,
  soil = TRUE,
  soilprops = c('nitrogen', 'cec'),
  landcover = TRUE,
  climate = TRUE, 
  climvar = c('tasmax', 'tasmin'), 
  climtime = c('monthly', 'annual'), 
  annualstartmonth = 01
)
```

## License
