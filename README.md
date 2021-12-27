
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DroughtData

<!-- badges: start -->

[![R-CMD-check](https://github.com/mountaindboz/DroughtData/workflows/R-CMD-check/badge.svg)](https://github.com/mountaindboz/DroughtData/actions)
<!-- badges: end -->

An R data package containing data sets used in the Drought Synthesis
project. This data package also contains useful metadata describing the
data sets, the processing scripts used to derive the data, and a couple
helper functions.

This package provides the following data tables:

-   `lt_seasonal`: long-term (1975-2021) integrated data set containing
    seasonal averages of various metrics for each year and across the
    entire Delta
-   `lt_regional`: long-term (1975-2021) integrated data set containing
    regional averages of various metrics for each year
-   `raw_hydro_1975_2021`: a data set containing raw daily values of the
    hydrology and Low Salinity Zone (LSZ) metrics for 1975-2021
-   `raw_wq_1975_2021`: a data set containing raw values of three water
    quality parameters (water temperature, salinity, and secchi depth)
    for 1975-2021
-   `raw_nutr_1975_2021`: a long-term data set containing raw values of
    three nutrient parameters (Dissolved Ammonia, Dissolved Nitrate +
    Nitrite, and Dissolved Ortho-phosphate) for 1975-2021
-   `raw_nutr_2013_2021`: a short-term data set containing raw values of
    three nutrient parameters (Dissolved Ammonia, Dissolved Nitrate +
    Nitrite, and Dissolved Ortho-phosphate) for 2013-2021

This package also contains the following helper functions specific to
the Drought Synthesis project:

-   `drt_avg_data`: Calculates seasonal and regional averages
-   `drt_add_yr_assign`: Adds year assignment information to a data
    frame. The year assignment information is used in the analyses for
    the Drought Synthesis.

## Installation

You can install the latest version from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github("mountaindboz/DroughtData")
```

## Data Sources

Sacramento Valley Water Year Index:
<https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST>

Dayflow: <https://data.cnra.ca.gov/dataset/dayflow>

DTO data from CDEC:
<https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=DTO>

`discretewq` data package (version 2.1.0):
<https://github.com/sbashevkin/discretewq/tree/78232cec30df1e2606431eeae3710ca25cf0c7f7>

Water Quality Portal hosted by the National Water Quality Monitoring
Council: <https://www.waterqualitydata.us/>
