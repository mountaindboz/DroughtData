#' @title Long-term seasonal averages
#' @description A long-term (1975-2021) integrated data set containing seasonal
#'   averages of various metrics for each year and across the entire Delta. Used
#'   in the analyses for the Drought Synthesis.
#'
#' @format data frame with 188 rows and 11 columns
#' \describe{
#'   \item{Year}{Adjusted calendar year - December-November, with December of
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
#'     Island from Dayflow. The values for water year 2021 are estimated by data
#'     from the DTO station on CDEC.}
#'   \item{X2}{The distance from the Golden Gate Bridge to the point where daily
#'     average salinity is 2 parts per thousand at a depth of 1 meter off the
#'     bottom, expressed in kilometers. From Dayflow for recent years (WY
#'     1997-2020), Hutton et. al. for earlier years.}
#'   \item{Export}{Total amount of water diverted from the Delta by the Federal
#'     and State governments to meet water agreements and contracts, expressed in
#'     cubic feet per second (cfs). From Dayflow.}
#'   \item{Temperature}{Water temperature in degrees Celsius from the
#'     `discretewq` data package}
#'   \item{Secchi}{Secchi depth in centimeters from the `discretewq` data
#'     package}
#'   \item{Salinity}{Salinity value on the Practical Salinity Scale from the
#'     `discretewq` data package}
#' }
#'
#' @source
#' * Sacramento Valley Water Year Index: <https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST>
#' * Dayflow: <https://data.cnra.ca.gov/dataset/dayflow>
#' * DTO data from CDEC: <https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=DTO>
#' * `discretewq` data package (version 2.0.0): <https://github.com/sbashevkin/discretewq/releases/tag/v2.0.0>
"lt_seasonal"

#' @title Long-term regional averages
#' @description A long-term (1975-2021) integrated data set containing regional
#'   averages of various metrics for each year. Used in the analyses for the
#'   Drought Synthesis.
#'
#' @format data frame with 235 rows and 8 columns
#' \describe{
#'   \item{Year}{Adjusted calendar year - December-November, with December of
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
#'     `discretewq` data package}
#'   \item{Secchi}{Secchi depth in centimeters from the `discretewq` data
#'     package}
#'   \item{Salinity}{Salinity value on the Practical Salinity Scale from the
#'     `discretewq` data package}
#' }
#'
#' @source
#' * Sacramento Valley Water Year Index: <https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST>
#' * `discretewq` data package (version 2.0.0): <https://github.com/sbashevkin/discretewq/releases/tag/v2.0.0>
"lt_regional"

#' @title Daily Delta outflow and X2 values for water year 2021
#' @description Daily values for Delta outflow and X2 for water year 2021 (Oct
#'   2020 - Oct 2021). Used in the analyses for the Drought Synthesis.
#'
#' @format data frame with 396 rows and 3 columns
#' \describe{
#'   \item{Date}{Calendar date}
#'   \item{Outflow}{Estimated net Delta outflow in cubic feet per second (cfs)
#'     from the DTO station on CDEC.}
#'   \item{X2}{The distance from the Golden Gate Bridge to the point where daily
#'     average salinity is 2 parts per thousand at a depth of 1 meter off the
#'     bottom, expressed in kilometers. Estimated using the Autoregressive Lag
#'     Model from Dayflow and Outflow from the DTO station on CDEC.}
#' }
#'
#' @source
#' * Dayflow: <https://data.cnra.ca.gov/dataset/dayflow>
#' * DTO data from CDEC: <https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=DTO>
"st_hydro_2021"

#' @title Short-term monthly averages for hydrology and LSZ metrics
#' @description A short-term (2011-2021) data set containing monthly averages of
#'   the hydrology and Low Salinity Zone (LSZ) metrics. Used in the analyses for
#'   the Drought Synthesis.
#'
#' @format data frame with 131 rows and 5 columns
#' \describe{
#'   \item{Year}{Adjusted calendar year - December-November, with December of
#'     the previous calendar year included with the following year. For example,
#'     December of 2019 is assigned year "2020" in this data set.}
#'   \item{Month}{Calendar month}
#'   \item{Outflow}{Net Delta outflow in cubic feet per second (cfs) at Chipps
#'     Island from Dayflow. The values for water years 2021 and 2022 are estimated
#'     by data from the DTO station on CDEC.}
#'   \item{Export}{Total amount of water diverted from the Delta by the Federal
#'     and State governments to meet water agreements and contracts, expressed in
#'     cubic feet per second (cfs). From Dayflow. No data available for WY 2021
#'     and 2022 at this point.}
#'   \item{X2}{The distance from the Golden Gate Bridge to the point where daily
#'     average salinity is 2 parts per thousand at a depth of 1 meter off the
#'     bottom, expressed in kilometers. From Dayflow for WY 2011-2020. Estimated
#'     using the Autoregressive Lag Model from Dayflow and Outflow from the DTO
#'     station on CDEC for WY 2021.}
#' }
#'
#' @source
#' * Dayflow: <https://data.cnra.ca.gov/dataset/dayflow>
#' * DTO data from CDEC: <https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=DTO>
"st_hydro_month"
