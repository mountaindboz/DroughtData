
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
    entire upper San Francisco Estuary (Delta)
-   `lt_regional`: long-term (1975-2021) integrated data set containing
    regional averages of various metrics for each year within the Delta
-   `raw_hydro_1975_2021`: a data set containing raw daily values of the
    hydrology and Low Salinity Zone (LSZ) metrics for 1975-2021 in the
    Delta
-   `raw_wq_1975_2021`: a data set containing raw values of three water
    quality parameters (water temperature, salinity, and secchi depth)
    for 1975-2021 within the Delta
-   `raw_nutr_1975_2021`: a long-term data set containing raw values of
    three nutrient parameters (Dissolved Ammonia, Dissolved Nitrate +
    Nitrite, and Dissolved Ortho-phosphate) for 1975-2021 within the
    Delta
-   `raw_nutr_2013_2021`: a short-term data set containing raw values of
    three nutrient parameters (Dissolved Ammonia, Dissolved Nitrate +
    Nitrite, and Dissolved Ortho-phosphate) for 2013-2021 within the
    Delta
-   `cont_do_daily`: A data set containing daily average values of
    continuous dissolved oxygen data from 2014-2021 at a few locations
    in the Delta
-   `raw_chla_1975_2021`: a long-term data set containing raw values of
    discrete chlorophyll-a water concentrations for 1975-2021 within the
    Delta

This package also contains the following helper functions specific to
the Drought Synthesis project:

-   `drt_avg_data()`: Calculates seasonal and regional averages
-   `drt_add_yr_assign()`: Adds year assignment information to a data
    frame. The year assignment information is used in the analyses for
    the Drought Synthesis.
-   `drt_color_pal_drought()` and `drt_color_pal_yrtype()`: Apply custom
    color palettes to the figures used in the Drought Synthesis report

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

USBR Delta Outflow Computation reports:
<https://www.usbr.gov/mp/cvo/pmdoc.html>

Hutton, P. H., J. S. Rath, and S. B Roy. 2017. Freshwater flow to the
San Francisco Bay-Delta estuary over nine decades (Part 1): Trend
evaluation. Hydrological Processes **31**(14): 2500-2515.
<https://doi.org/10.1002/hyp.11201>.

`discretewq` data package (version 2.3.2.9000):
<https://github.com/sbashevkin/discretewq/tree/dbb2638e800e637f3f2c70d47c9faad68f95db07>

Water Quality Portal hosted by the National Water Quality Monitoring
Council: <https://www.waterqualitydata.us/>

USGS National Water Information System (NWIS):
<https://nwis.waterdata.usgs.gov/nwis>
