# DroughtData 2.0.0

* Added the new data set `raw_chla_1975_2021` which contains raw values of discrete chlorophyll-a water concentrations from 1975-2021 within the Delta
* Corrected nutrient data in `raw_nutr_1975_2021` and `raw_nutr_2013_2021` so that only the intended <RL values with high RL values are excluded
* Updated the `raw_wq_1975_2021`, `raw_nutr_1975_2021`, and `raw_chla_1975_2021` data sets with the data in the latest version of the `discretewq` package (version 2.3.2.9000). Added additional surveys to the water quality and chlorophyll data sets. Implemented a filtering strategy to make sure that we are only including the subregions that were sampled adequately across seasons.
* Updated the `drt_avg_data()` function to allow for it to calculate seasonal-regional averages
* Replaced `lt_seasonal` and `lt_regional` data sets with `lt_avg_hydro` and `lt_avg_wq`. `lt_avg_hydro` contains long-term seasonal averages of three hydrology parameters (Delta Outflow, Delta Exports, X2), while `lt_avg_wq` contains long-term seasonal-regional averages of three water quality parameters (water temperature, salinity, and secchi depth).
* Added a new function `drt_replace_rl()` which replaces values measured below the analytical reporting limit with a random number

# DroughtData 1.1.0

* Updated `raw_hydro_1975_2021` and `lt_seasonal` data sets with recently released 2021 Dayflow data.
* Added Dayflow inflow metrics (Sacramento River, Yolo Bypass, Eastern Inflows, and Total) to `raw_hydro_1975_2021`

# DroughtData 1.0.0

* Package contains versions of all water quality and flow data analyzed in the Feb 2022 report.

# DroughtData 0.5.0

* Fixed code for cleaning raw data to refer to internal data for the region shapefile and assignments
* Updated to use version 2.3.1 of the `discretewq` data package
* Updated nutrient data in `raw_nutr_1975_2021` and `raw_nutr_2013_2021` so they reflect data used in the February 2022 Drought Synthesis Report
* Added daily averages of combined USGS outflow and Cache Slough flow to `raw_hydro_1975_2021`
* Added the new data set `cont_do_daily` which contains daily average values of continuous dissolved oxygen data from 2014-2021 at a few locations in the upper San Francisco Estuary

# DroughtData 0.4.0

* Added two new functions that define custom color palettes for the figures in the Drought Synthesis report - `drt_color_pal_drought()` and `drt_color_pal_yrtype()`

# DroughtData 0.3.0

* Updated the hydrology parameters (Delta Outflow, Delta Exports, and X2) in the raw and long-term integrated data sets with data from the USBR Delta Outflow Computation reports for October 2020 - November 2021

# DroughtData 0.2.1

* Added an additional helper function `drt_add_yr_assign()` which adds year assignment information to a data frame. The year assignment information is used in the analyses for the Drought Synthesis.

# DroughtData 0.2.0

* Added two raw nutrient data sets to the package: 
  * `raw_nutr_1975_2021` which contains long-term data from 1975-2021
  * `raw_nutr_2013_2021` which contains short-term data from 2013-2021
* Added a helper function `drt_avg_data()` which calculates either seasonal or regional averages of the raw drought data

# DroughtData 0.1.1

* Removed a single temperature outlier collected at S42 (EMP) on 5/3/1983 from all related data sets including `raw_wq_1975_2021`, `lt_seasonal`, and `lt_regional`.
* Assigned "Critical" Year Type to 2021

# DroughtData 0.1.0

* Updated raw and long-term integrated data sets with hydrology (Delta Outflow, Delta Exports, and X2) and WQ (Water Temperature, Salinity, and Secchi depth) data to be used in the February 2022 report
* Added a `NEWS.md` file to track changes to the package.
