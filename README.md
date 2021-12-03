
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DroughtData

<!-- badges: start -->
<!-- badges: end -->

An R data package containing data sets used in the Drought Synthesis
project. This data package also contains useful metadata describing the
data sets and the processing scripts used to derive the data.

This package provides the following data tables:

-   `lt_seasonal`: long-term (1975-2021) integrated data set containing
    seasonal averages of various metrics for each year and across the
    entire Delta
-   `lt_regional`: long-term (1975-2021) integrated data set containing
    regional averages of various metrics for each year
-   `raw_hydro_1975_2021`: a data set containing raw daily values of the
    hydrology and Low Salinity Zone (LSZ) metrics for 1975-2021
-   `raw_wq_1975_2021`: a data set containing raw values of the water
    quality parameters (water temperature, salinity, secchi depth) for
    1975-2021

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
<https://github.com/sbashevkin/discretewq/tree/1af04438c961c6d51f46cc777cbb6a142c1e3b30>
