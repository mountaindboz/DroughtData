
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

`discretewq` data package: <https://github.com/sbashevkin/discretewq>
