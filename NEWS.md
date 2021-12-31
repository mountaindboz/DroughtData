# DroughtData (development version)

# DroughtData 0.2.1

* Added an additional helper function `drt_add_yr_assign` which adds year assignment information to a data frame. The year assignment information is used in the analyses for the Drought Synthesis.

# DroughtData 0.2.0

* Added two raw nutrient data sets to the package: 
  * `raw_nutr_1975_2021` which contains long-term data from 1975-2021
  * `raw_nutr_2013_2021` which contains short-term data from 2013-2021
* Added a helper function `drt_avg_data` which calculates either seasonal or regional averages of the raw drought data

# DroughtData 0.1.1

* Removed a single temperature outlier collected at S42 (EMP) on 5/3/1983 from all related data sets including `raw_wq_1975_2021`, `lt_seasonal`, and `lt_regional`.
* Assigned "Critical" Year Type to 2021

# DroughtData 0.1.0

* Updated raw and long-term integrated data sets with hydrology (Delta Outflow, Delta Exports, and X2) and WQ (Water Temperature, Salinity, and Secchi depth) data to be used in the February 2022 report
* Added a `NEWS.md` file to track changes to the package.
