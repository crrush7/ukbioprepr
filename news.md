# ukbioprepr 0.1.0

## Initial Release

**Status:** 

### Overview

First public release of `ukbioprepr` - an R package to support reproducible preparation of environmental data for biodiversity modelling in the UK.


### Key Features

* **Dual input**: Works with both grid references (Irish/British National Grid) and coordinates (any CRS)
* **Coordinate system handling**: Automatic CRS reprojection
* **Smart caching**: Downloaded rasters cached locally to avoid redundant downloads
* **Robust error handling**: Automatic retry logic for failed downloads
* **Comprehensive validation**: Input checking for all parameters
* **Flexible temporal aggregations**: Monthly, seasonal, and annual climate summaries
* **High spatial resolution**: All data products at 1 km resolution
* **Gridded data**: Rasters provided in line with chosen coordinate reference system

### Testing and Quality

* Comprehensive test suite with **86.87% code coverage**
* **584+ tests** covering all major functionality
* Full roxygen2 documentation for all exported functions
* Automated testing via GitHub Actions

### Data Sources

* **Climate:** Met Office HadUK-Grid (1999-2023)
* **Soil:** SoilGrids global soil database
* **Land Cover:** UKCEH Land Cover Maps (2000-2023)

### Known Limitations

* UK climate files are large (~250MB - 1.45B) and may timeout on slow connections
* Requires active internet connection for first-time raster downloads

---

## Future Releases


---

**For complete usage examples and documentation, see the package README.**
