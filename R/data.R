#' @title Long-term seasonal averages
#' @description A long-term (1975-2021) integrated data set containing seasonal
#'   averages of various metrics for each year and across the entire Delta. Used
#'   in the analyses for the Drought Synthesis.
#'
#' @format data frame with 188 rows and 11 columns
#' \describe{
#'   \item{YearAdj}{Adjusted calendar year - December-November, with December of
#'     the previous calendar year included with the following year. For example,
#'     December of 2019 is the first month of 2020 in this data set.}
#'   \item{Season}{Winter (Dec-Feb), Spring (Mar-May), Summer (Jun-Aug), or Fall
#'     (Sept-Nov)}
#'   \item{SVIndex}{Sacramento Valley Water Year Index}
#'   \item{YearType}{Water year type based on Sacramento Valley Water Year Index
#'     (Critical, Dry, Below Normal, Above Normal, or Wet). The Fall season
#'     (Sept-Nov) straddles two water years, but we assigned it to the previous
#'     water year.}
#'   \item{Drought}{Drought/Wet/Neutral classification. Multiple Dry, Critical,
#'     and Below Normal years in a row are a drought (D), multiple Wet or Above
#'     Normal years in a row are a wet period (W), and years that are not on a
#'     streak are neutral (N).}
#'   \item{Outflow}{Net Delta outflow in cubic feet per second (cfs) at Chipps
#'     Island from Dayflow. The values for October 2020 through October 2021 are
#'     estimated by data from the DTO station on CDEC.}
#'   \item{Export}{Total amount of water diverted from the Delta by the Federal
#'     and State governments to meet water agreements and contracts, expressed in
#'     cubic feet per second (cfs). From Dayflow. No data available for October
#'     2020 through October 2021 at this point.}
#'   \item{X2}{The distance from the Golden Gate Bridge to the point where daily
#'     average salinity is 2 parts per thousand at a depth of 1 meter off the
#'     bottom, expressed in kilometers. From Hutton et. al. for WY 1975-1996, from
#'     Dayflow for WY 1997-2020, and estimated using the Autoregressive Lag Model
#'     from Dayflow and Outflow from the DTO station on CDEC for October 2020
#'     through August 2021.}
#'   \item{Temperature}{Water temperature in degrees Celsius from the
#'     `discretewq` data package. Data from EMP and FMWT collected in 2021 were
#'     provided directly from staff who collected the data and should be
#'     considered provisional.}
#'   \item{Salinity}{Salinity value on the Practical Salinity Scale from the
#'     `discretewq` data package. Data from EMP and FMWT collected in 2021 were
#'     provided directly from staff who collected the data and should be
#'     considered provisional.}
#'   \item{Secchi}{Secchi depth in centimeters from the `discretewq` data
#'     package. Data from EMP and FMWT collected in 2021 were provided directly
#'     from staff who collected the data and should be considered provisional.}
#' }
#'
#' @source
#' * Sacramento Valley Water Year Index: <https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST>
#' * Dayflow: <https://data.cnra.ca.gov/dataset/dayflow>
#' * DTO data from CDEC: <https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=DTO>
#' * `discretewq` data package (version 2.1.0): <https://github.com/sbashevkin/discretewq/tree/1af04438c961c6d51f46cc777cbb6a142c1e3b30>
"lt_seasonal"

#' @title Long-term regional averages
#' @description A long-term (1975-2021) integrated data set containing regional
#'   averages of various metrics for each year. Used in the analyses for the
#'   Drought Synthesis.
#'
#' @format data frame with 235 rows and 8 columns
#' \describe{
#'   \item{YearAdj}{Adjusted calendar year - December-November, with December of
#'     the previous calendar year included with the following year. For example,
#'     December of 2019 is the first month of 2020 in this data set.}
#'   \item{Region}{Region designation within the larger Delta and Suisun
#'     Marsh/Bay (Confluence, North, SouthCentral, Suisun Bay, Suisun Marsh).}
#'   \item{SVIndex}{Sacramento Valley Water Year Index}
#'   \item{YearType}{Water year type based on Sacramento Valley Water Year Index
#'     (Critical, Dry, Below Normal, Above Normal, or Wet). The Fall season
#'     (Sept-Nov) straddles two water years, but we assigned it to the previous
#'     water year.}
#'   \item{Drought}{Drought/Wet/Neutral classification. Multiple Dry, Critical,
#'     and Below Normal years in a row are a drought (D), multiple Wet or Above
#'     Normal years in a row are a wet period (W), and years that are not on a
#'     streak are neutral (N).}
#'   \item{Temperature}{Water temperature in degrees Celsius from the
#'     `discretewq` data package. Data from EMP and FMWT collected in 2021 were
#'     provided directly from staff who collected the data and should be
#'     considered provisional.}
#'   \item{Salinity}{Salinity value on the Practical Salinity Scale from the
#'     `discretewq` data package. Data from EMP and FMWT collected in 2021 were
#'     provided directly from staff who collected the data and should be
#'     considered provisional.}
#'   \item{Secchi}{Secchi depth in centimeters from the `discretewq` data
#'     package. Data from EMP and FMWT collected in 2021 were provided directly
#'     from staff who collected the data and should be considered provisional.}
#' }
#'
#' @source
#' * Sacramento Valley Water Year Index: <https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST>
#' * `discretewq` data package (version 2.1.0): <https://github.com/sbashevkin/discretewq/tree/1af04438c961c6d51f46cc777cbb6a142c1e3b30>
"lt_regional"

#' @title Raw daily values for hydrology and LSZ metrics for 1975-2021
#' @description A data set containing raw daily values of the hydrology and Low
#'   Salinity Zone (LSZ) metrics for 1975-2021. Seasonal averages of this data
#'   are in the `lt_seasonal` data set. Used in the analyses for the Drought
#'   Synthesis.
#'
#' @format data frame with 17,137 rows and 6 columns
#' \describe{
#'   \item{YearAdj}{Adjusted calendar year - December-November, with December of
#'     the previous calendar year included with the following year. For example,
#'     December of 1974 is assigned year "1975" in this data set.}
#'   \item{Season}{Winter (Dec-Feb), Spring (Mar-May), Summer (Jun-Aug), or Fall
#'     (Sept-Nov)}
#'   \item{Date}{Calendar date}
#'   \item{Outflow}{Net Delta outflow in cubic feet per second (cfs) at Chipps
#'     Island from Dayflow. The values for October 2020 through October 2021 are
#'     estimated by data from the DTO station on CDEC.}
#'   \item{Export}{Total amount of water diverted from the Delta by the Federal
#'     and State governments to meet water agreements and contracts, expressed in
#'     cubic feet per second (cfs). From Dayflow. No data available for October
#'     2020 through October 2021 at this point.}
#'   \item{X2}{The distance from the Golden Gate Bridge to the point where daily
#'     average salinity is 2 parts per thousand at a depth of 1 meter off the
#'     bottom, expressed in kilometers. From Hutton et. al. for WY 1975-1996, from
#'     Dayflow for WY 1997-2020, and estimated using the Autoregressive Lag Model
#'     from Dayflow and Outflow from the DTO station on CDEC for October 2020
#'     through August 2021.}
#' }
#'
#' @source
#' * Dayflow: <https://data.cnra.ca.gov/dataset/dayflow>
#' * DTO data from CDEC: <https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=DTO>
"raw_hydro_1975_2021"

#' @title Raw values for water quality measurements for 1975-2021
#' @description A data set containing raw values of the water quality parameters
#'   (water temperature, salinity, secchi depth) for 1975-2021. Seasonal and
#'   Regional averages of this data are in the `lt_seasonal` and `lt_regional`
#'   data sets, respectively. This data set contains only one data point per
#'   `Station` and `Date` and excludes any data points that are more than 10
#'   standard deviations away from the mean of each `SubRegion`. Used in the
#'   analyses for the Drought Synthesis.
#'
#' @format data frame with 34,523 rows and 14 columns
#' \describe{
#'   \item{Source}{Name of the source dataset. Either EMP (DWR's Environmental
#'     Monitoring Program), FMWT (CDFW's Fall Midwater Trawl survey), or STN
#'     (CDFW's Summer Townet survey)}
#'   \item{Station}{Location where measurement was collected}
#'   \item{Latitude}{Latitude of `Station` in Decimal Degrees (WGS 84 Datum)}
#'   \item{Longitude}{Longitude of `Station` in Decimal Degrees (WGS 84 Datum)}
#'   \item{Region}{Region designation of `Station` within the larger Delta and
#'     Suisun Marsh/Bay (Confluence, North, SouthCentral, Suisun Bay, Suisun
#'     Marsh). The Drought Synthesis team categorized `SubRegion`s into broader
#'     `Region`s using [this crosswalk](https://github.com/mountaindboz/DroughtData/blob/master/data-raw/Rosies_regions.csv)
#'     .}
#'   \item{SubRegion}{Subregion designation of `Station` as defined by the
#'     `R_EDSM_Subregions_Mahardja_FLOAT` shapefile from the
#'     [`deltamapr`](https://github.com/InteragencyEcologicalProgram/deltamapr)
#'     package.}
#'   \item{YearAdj}{Adjusted calendar year - December-November, with December of
#'     the previous calendar year included with the following year. For example,
#'     December of 1974 is assigned year "1975" in this data set.}
#'   \item{Season}{Winter (Dec-Feb), Spring (Mar-May), Summer (Jun-Aug), or Fall
#'     (Sept-Nov)}
#'   \item{Month}{Integer representing the month}
#'   \item{Date}{Calendar date of the measurement}
#'   \item{Datetime}{Date and time of the measurement. Not available for all
#'     measurements.}
#'   \item{Temperature}{Water temperature in degrees Celsius from the
#'     `discretewq` data package. Data from EMP and FMWT collected in 2021 were
#'     provided directly from staff who collected the data and should be
#'     considered provisional.}
#'   \item{Salinity}{Salinity value on the Practical Salinity Scale from the
#'     `discretewq` data package. Data from EMP and FMWT collected in 2021 were
#'     provided directly from staff who collected the data and should be
#'     considered provisional.}
#'   \item{Secchi}{Secchi depth in centimeters from the `discretewq` data
#'     package. Data from EMP and FMWT collected in 2021 were provided directly
#'     from staff who collected the data and should be considered provisional.}
#' }
#'
#' @source
#' `discretewq` data package (version 2.1.0): <https://github.com/sbashevkin/discretewq/tree/1af04438c961c6d51f46cc777cbb6a142c1e3b30>
"raw_wq_1975_2021"
