#' @title Long-term seasonal averages
#' @description A long-term (1975-2021) integrated data set containing seasonal
#'   averages of various metrics for each year and across the entire upper San
#'   Francisco Estuary (Delta). Used in the analyses for the Drought Synthesis.
#'
#' @format data frame with 188 rows and 11 columns
#' \describe{
#'   \item{YearAdj}{Adjusted calendar year - December-November, with December of
#'     the previous calendar year included with the following year. For example,
#'     December of 2020 is assigned year "2021" in this data set.}
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
#'     Island. Data from WY 1975-2021 is from Dayflow. The values for October
#'     through November 2021 are estimated by data from the USBR Delta Outflow
#'     Computation reports.}
#'   \item{Export}{Total amount of water diverted from the Delta by the Federal
#'     and State governments to meet water agreements and contracts, expressed in
#'     cubic feet per second (cfs). Data from WY 1975-2021 is from Dayflow. The
#'     values for October through November 2021 are estimated by data from the
#'     USBR Delta Outflow Computation reports.}
#'   \item{X2}{The distance from the Golden Gate Bridge to the point where daily
#'     average salinity is 2 parts per thousand at a depth of 1 meter off the
#'     bottom, expressed in kilometers. From Hutton et al. (2017) for WY
#'     1975-1996, from Dayflow for WY 1997-2021, and estimated using the
#'     Autoregressive Lag Model from Dayflow for October through November 2021.}
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
#' * Sacramento Valley Water Year Index:
#'   <https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST>
#' * Dayflow: <https://data.cnra.ca.gov/dataset/dayflow>
#' * USBR Delta Outflow Computation reports:
#'   <https://www.usbr.gov/mp/cvo/pmdoc.html>
#' * Hutton, P. H., J. S. Rath, and S. B Roy. 2017. Freshwater flow to the San
#'   Francisco Bay-Delta estuary over nine decades (Part 1): Trend evaluation.
#'   Hydrological Processes **31**(14): 2500-2515. <https://doi.org/10.1002/hyp.11201>.
#' * `discretewq` data package (version 2.3.1):
#'   <https://github.com/sbashevkin/discretewq/tree/v2.3.1>
"lt_seasonal"

#' @title Long-term regional averages
#' @description A long-term (1975-2021) integrated data set containing regional
#'   averages of various metrics for each year within the upper San Francisco
#'   Estuary (Delta). Used in the analyses for the Drought Synthesis.
#'
#' @format data frame with 235 rows and 8 columns
#' \describe{
#'   \item{YearAdj}{Adjusted calendar year - December-November, with December of
#'     the previous calendar year included with the following year. For example,
#'     December of 2020 is assigned year "2021" in this data set.}
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
#'     `discretewq` data package. Data collected in 2021 were provided directly
#'     from staff who collected the data and should be considered provisional.}
#'   \item{Salinity}{Salinity value on the Practical Salinity Scale from the
#'     `discretewq` data package. Data collected in 2021 were provided directly
#'     from staff who collected the data and should be considered provisional.}
#'   \item{Secchi}{Secchi depth in centimeters from the `discretewq` data
#'     package. Data collected in 2021 were provided directly from staff who
#'     collected the data and should be considered provisional.}
#' }
#'
#' @source
#' * Sacramento Valley Water Year Index:
#'   <https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST>
#' * `discretewq` data package (version 2.3.1):
#'   <https://github.com/sbashevkin/discretewq/tree/v2.3.1>
"lt_regional"

#' @title Raw daily values of hydrology and LSZ metrics for 1975-2021
#' @description A data set containing raw daily values of the hydrology and Low
#'   Salinity Zone (LSZ) metrics for 1975-2021 in the upper San Francisco
#'   Estuary (Delta). Seasonal averages of the `Outflow`, `Export`, and `X2`
#'   data are in the `lt_seasonal` data set. Used in the analyses for the
#'   Drought Synthesis.
#'
#' @format data frame with 17,167 rows and 12 columns
#' \describe{
#'   \item{YearAdj}{Adjusted calendar year - December-November, with December of
#'     the previous calendar year included with the following year. For example,
#'     December of 2020 is assigned year "2021" in this data set.}
#'   \item{Season}{Winter (Dec-Feb), Spring (Mar-May), Summer (Jun-Aug), or Fall
#'     (Sept-Nov)}
#'   \item{Date}{Calendar date of the value in yyyy-mm-dd}
#'   \item{InflowSacR}{Sacramento River inflow to the Delta in cubic feet per
#'     second (cfs). Data from WY 1975-2021 is from Dayflow.}
#'   \item{InflowYolo}{Yolo Bypass inflow to the Delta in cubic feet per
#'     second (cfs). Data from WY 1975-2021 is from Dayflow.}
#'   \item{InflowEast}{Eastern Delta inflow in cubic feet per second (cfs).
#'     Includes inflows from the San Joaquin, Cosumnes and Mokelumne Rivers, and
#'     miscellaneous streams from the northeast, east, and southeast. Data from WY
#'     1975-2021 is from Dayflow.}
#'   \item{InflowTotal}{Total Delta inflow in cubic feet per second (cfs). Data
#'     from WY 1975-2021 is from Dayflow.}
#'   \item{Outflow}{Net Delta outflow in cubic feet per second (cfs) at Chipps
#'     Island. Data from WY 1975-2021 is from Dayflow. The values for October
#'     through November 2021 are estimated by data from the USBR Delta Outflow
#'     Computation reports.}
#'   \item{Export}{Total amount of water diverted from the Delta by the Federal
#'     and State governments to meet water agreements and contracts, expressed in
#'     cubic feet per second (cfs). Data from WY 1975-2021 is from Dayflow. The
#'     values for October through November 2021 are estimated by data from the
#'     USBR Delta Outflow Computation reports.}
#'   \item{X2}{The distance from the Golden Gate Bridge to the point where daily
#'     average salinity is 2 parts per thousand at a depth of 1 meter off the
#'     bottom, expressed in kilometers. From Hutton et al. (2017) for WY
#'     1975-1996, from Dayflow for WY 1997-2021, and estimated using the
#'     Autoregressive Lag Model from Dayflow for October through November 2021.}
#'   \item{TotalUSGSOutflow}{Daily averages of combined tidally-filtered flow
#'     data in cubic feet per second (cfs) from four USGS stations: Sacramento
#'     River at Rio Vista CA (11455420), San Joaquin River at Jersey Point CA
#'     (11337190), Threemile Slough near Rio Vista CA (11337080), and Dutch Slough
#'     below Jersey Island Road at Jersey Island (11313433). Regarded to be an
#'     estimate of daily net Delta outflow. Data was obtained from the USGS
#'     National Water Information System. Some of the data is considered
#'     provisional.}
#'   \item{CacheFlow}{Daily averages of tidally-filtered flow data for Cache
#'     Slough in cubic feet per second (cfs). Data is from two USGS stations:
#'     Cache Slough at Ryer Island (11455350) from 3/6/2003 to 4/26/2019, and
#'     Cache Slough above Ryer Island Ferry near Rio Vista CA (11455385) from
#'     4/27/2019 to 11/30/2021. Data was obtained from the USGS National Water
#'     Information System. Some of the data is considered provisional.}
#' }
#'
#' @source
#' * Dayflow: <https://data.cnra.ca.gov/dataset/dayflow>
#' * USBR Delta Outflow Computation reports:
#'   <https://www.usbr.gov/mp/cvo/pmdoc.html>
#' * Hutton, P. H., J. S. Rath, and S. B Roy. 2017. Freshwater flow to the San
#'   Francisco Bay-Delta estuary over nine decades (Part 1): Trend evaluation.
#'   Hydrological Processes **31**(14): 2500-2515. <https://doi.org/10.1002/hyp.11201>.
#' * USGS National Water Information System (NWIS):
#'   <https://nwis.waterdata.usgs.gov/nwis>
"raw_hydro_1975_2021"

#' @title Raw values of water quality measurements for 1975-2021
#' @description A data set containing raw values of three water quality
#'   parameters (water temperature, salinity, and secchi depth) for 1975-2021
#'   within the upper San Francisco Estuary (Delta). Seasonal and Regional
#'   averages of this data are in the `lt_seasonal` and `lt_regional` data sets,
#'   respectively. Used in the analyses for the Drought Synthesis.
#'
#'   All data are from the `discretewq` data package. This data set contains
#'   only one data point per `Station` and `Date` and excludes any data points
#'   that are more than 15 standard deviations away from the mean of each
#'   `SubRegion`.
#'
#' @format data frame with 82,548 rows and 14 columns
#' \describe{
#'   \item{Source}{Name of the source dataset. Either 20mm (CDFW's 20mm survey),
#'     Baystudy (CDFW's Bay Study), DJFMP (USFWS's Delta Juvenile Fish Monitoring
#'     Program), EMP (DWR's Environmental Monitoring Program), FMWT (CDFW's Fall
#'     Midwater Trawl survey), SDO (DWR's Stockton Dissolved Oxygen Monitoring
#'     Program), SKT (CDFW's Spring Kodiak Trawl), STN (CDFW's Summer Townet
#'     survey), Suisun (UC Davis's Suisun Marsh Fish Study), or USGS_SFBS (USGS's
#'     San Francisco Bay Water Quality Survey)}
#'   \item{Station}{Location where measurement was collected}
#'   \item{Latitude}{Latitude of `Station` in Decimal Degrees (WGS 84 Datum)}
#'   \item{Longitude}{Longitude of `Station` in Decimal Degrees (WGS 84 Datum)}
#'   \item{Region}{Region designation of `Station` within the larger Delta and
#'     Suisun Marsh/Bay (Confluence, North, SouthCentral, Suisun Bay, Suisun
#'     Marsh). The Drought Synthesis team categorized `SubRegion`s into broader
#'     `Region`s using [this crosswalk](https://github.com/mountaindboz/DroughtData/blob/master/data-raw/Global/Rosies_regions.csv)
#'     .}
#'   \item{SubRegion}{Subregion designation of `Station` as defined by the
#'     `R_EDSM_Subregions_Mahardja_FLOAT` shapefile from the
#'     [`deltamapr`](https://github.com/InteragencyEcologicalProgram/deltamapr)
#'     package.}
#'   \item{YearAdj}{Adjusted calendar year - December-November, with December of
#'     the previous calendar year included with the following year. For example,
#'     December of 2020 is assigned year "2021" in this data set.}
#'   \item{Season}{Winter (Dec-Feb), Spring (Mar-May), Summer (Jun-Aug), or Fall
#'     (Sept-Nov)}
#'   \item{Month}{Integer representing the month}
#'   \item{Date}{Calendar date of the measurement in yyyy-mm-dd}
#'   \item{Datetime}{Date and time (yyyy-mm-dd HH:MM:SS) of the measurement in
#'     PST. Not available for all measurements.}
#'   \item{Temperature}{Water temperature in degrees Celsius}
#'   \item{Salinity}{Salinity value on the Practical Salinity Scale}
#'   \item{Secchi}{Secchi depth in centimeters}
#' }
#'
#' @source
#' `discretewq` data package (version 2.3.2.9000):
#'   <https://github.com/sbashevkin/discretewq/tree/dbb2638e800e637f3f2c70d47c9faad68f95db07>
"raw_wq_1975_2021"

#' @title Raw values of nutrient concentrations for 1975-2021
#' @description A long-term data set containing raw values of three nutrient
#'   parameters (Dissolved Ammonia, Dissolved Nitrate + Nitrite, and Dissolved
#'   Ortho-phosphate) for 1975-2021 within the upper San Francisco Estuary
#'   (Delta). Used in the analyses for the Drought Synthesis.
#'
#'   All data are from the `discretewq` data package. This data set contains
#'   only one data point per `Station` and `Date` and excludes any data points
#'   that have modified z-scores greater than 15 grouped by `SubRegion` with the
#'   exception of Dissolved Ammonia. The flagged outliers in the Dissolved
#'   Ammonia data set appeared to be accurate based upon best professional
#'   judgement.
#'
#' @format data frame with 10,523 rows and 17 columns
#' \describe{
#'   \item{Source}{Name of the source dataset. Either EMP (DWR's Environmental
#'     Monitoring Program), USGS_SFBS (USGS's San Francisco Bay Water Quality
#'     Survey), or USGS_CAWSC (USGS's California Water Science Center)}
#'   \item{Station}{Location where measurement was collected}
#'   \item{Latitude}{Latitude of `Station` in Decimal Degrees (WGS 84 Datum)}
#'   \item{Longitude}{Longitude of `Station` in Decimal Degrees (WGS 84 Datum)}
#'   \item{Region}{Region designation of `Station` within the larger Delta and
#'     Suisun Bay (Confluence, North, SouthCentral, Suisun Bay). The Drought
#'     Synthesis team categorized `SubRegion`s into broader `Region`s using [this
#'     crosswalk](https://github.com/mountaindboz/DroughtData/blob/master/data-raw/Global/Rosies_regions.csv)
#'     .}
#'   \item{SubRegion}{Subregion designation of `Station` as defined by the
#'     `R_EDSM_Subregions_Mahardja_FLOAT` shapefile from the
#'     [`deltamapr`](https://github.com/InteragencyEcologicalProgram/deltamapr)
#'     package.}
#'   \item{YearAdj}{Adjusted calendar year - December-November, with December of
#'     the previous calendar year included with the following year. For example,
#'     December of 2020 is assigned year "2021" in this data set.}
#'   \item{Season}{Winter (Dec-Feb), Spring (Mar-May), Summer (Jun-Aug), or Fall
#'     (Sept-Nov)}
#'   \item{Month}{Integer representing the month}
#'   \item{Date}{Calendar date of the measurement in yyyy-mm-dd}
#'   \item{Datetime}{Date and time (yyyy-mm-dd HH:MM:SS) of the measurement in
#'     PST.}
#'   \item{DissAmmonia_Sign}{A symbol representing whether the Dissolved Ammonia
#'     value is below the reporting limit or above it. Contains one of four
#'     symbols: "<", "< (estimated)", "=", or "~". See the section
#'     below for more information about what these symbols represent.}
#'   \item{DissAmmonia}{Dissolved Ammonia value in mg/L as N}
#'   \item{DissNitrateNitrite_Sign}{A symbol representing whether the Dissolved
#'     Nitrate + Nitrite value is below the reporting limit or above it. Contains
#'     one of four symbols: "<", "< (estimated)", "=", or "~". See
#'     the section below for more information about what these symbols represent.}
#'   \item{DissNitrateNitrite}{Dissolved Nitrate + Nitrite value in mg/L as N}
#'   \item{DissOrthophos_Sign}{A symbol representing whether the Dissolved
#'     Ortho-phosphate value is below the reporting limit or above it. Contains
#'     one of four symbols: "<", "< (estimated)", "=", or "~". See
#'     the section below for more information about what these symbols represent.}
#'   \item{DissOrthophos}{Dissolved Ortho-phosphate value in mg/L as P}
#' }
#'
#' @section More information about the `_Sign` variables:
#' For the variables that have the _Sign suffix, the symbols in these variables
#' represent four conditions of the value contained in its corresponding result
#' variable.
#' * "<" - The value is below the Reporting Limit (RL) with the value in the
#' corresponding result variable equal to the RL.
#' * "< (estimated)" - The value is below the RL, but the RL is unknown. The
#' value in the corresponding result variable is the estimated RL value.
#' * "=" - The value is above the RL with the value in the corresponding result
#' variable equal to the actual value measured by the laboratory. An `NA` value
#' in the corresponding result variable indicates that the value is missing or
#' wasn't collected.
#' * "~" - The value in the corresponding result variable was estimated.
#'
#' @source
#' `discretewq` data package (version 2.3.2.9000):
#'   <https://github.com/sbashevkin/discretewq/tree/dbb2638e800e637f3f2c70d47c9faad68f95db07>
"raw_nutr_1975_2021"

#' @title Raw values of nutrient concentrations for 2013-2021
#' @description A short-term data set containing raw values of three nutrient
#'   parameters (Dissolved Ammonia, Dissolved Nitrate + Nitrite, and Dissolved
#'   Ortho-phosphate) for 2013-2021 within the upper San Francisco Estuary
#'   (Delta). Used in the analyses for the Drought Synthesis.
#'
#'   Most of the nutrient data from EMP (DWR's Environmental Monitoring Program)
#'   and all of the nutrient data from USGS_SFBS (USGS's San Francisco Bay Water
#'   Quality Survey) are from the `discretewq` data package. Data from EMP
#'   collected in 2021 was provided directly from staff who collected the data
#'   and should be considered provisional. Data from USGS_CAWSC were downloaded
#'   directly from the Water Quality Portal using the `dataRetrieval` R package.
#'   Some of the data from USGS_CAWSC is also considered provisional.
#'
#'   This data set contains only one data point per `Station` and `Date` and
#'   excludes any data points that have modified z-scores greater than 15
#'   grouped by `SubRegion`. Additionally, samples collected in the Suisun Marsh
#'   `Region` were excluded from this data set because it only has recent data
#'   for 2017-2021.
#'
#' @format data frame with 3,179 rows and 17 columns
#' \describe{
#'   \item{Source}{Name of the source dataset. Either EMP (DWR's Environmental
#'     Monitoring Program), USGS_SFBS (USGS's San Francisco Bay Water Quality
#'     Survey), or USGS_CAWSC (USGS's California Water Science Center)}
#'   \item{Station}{Location where measurement was collected}
#'   \item{Latitude}{Latitude of `Station` in Decimal Degrees (WGS 84 Datum)}
#'   \item{Longitude}{Longitude of `Station` in Decimal Degrees (WGS 84 Datum)}
#'   \item{Region}{Region designation of `Station` within the larger Delta and
#'     Suisun Bay (Confluence, North, SouthCentral, Suisun Bay). The Drought
#'     Synthesis team categorized `SubRegion`s into broader `Region`s using [this
#'     crosswalk](https://github.com/mountaindboz/DroughtData/blob/master/data-raw/Global/Rosies_regions.csv)
#'     .}
#'   \item{SubRegion}{Subregion designation of `Station` as defined by the
#'     `R_EDSM_Subregions_Mahardja_FLOAT` shapefile from the
#'     [`deltamapr`](https://github.com/InteragencyEcologicalProgram/deltamapr)
#'     package.}
#'   \item{YearAdj}{Adjusted calendar year - December-November, with December of
#'     the previous calendar year included with the following year. For example,
#'     December of 2020 is assigned year "2021" in this data set.}
#'   \item{Season}{Winter (Dec-Feb), Spring (Mar-May), Summer (Jun-Aug), or Fall
#'     (Sept-Nov)}
#'   \item{Month}{Integer representing the month}
#'   \item{Date}{Calendar date of the measurement in yyyy-mm-dd}
#'   \item{Datetime}{Date and time (yyyy-mm-dd HH:MM:SS) of the measurement in
#'     PST.}
#'   \item{DissAmmonia_Sign}{A symbol representing whether the Dissolved Ammonia
#'     value is below the reporting limit or above it. Contains one of four
#'     symbols: "<", "< (estimated)", "=", or "= (unreliable)". See the section
#'     below for more information about what these symbols represent.}
#'   \item{DissAmmonia}{Dissolved Ammonia value in mg/L as N}
#'   \item{DissNitrateNitrite_Sign}{A symbol representing whether the Dissolved
#'     Nitrate + Nitrite value is below the reporting limit or above it. Contains
#'     one of four symbols: "<", "< (estimated)", "=", or "= (unreliable)". See
#'     the section below for more information about what these symbols represent.}
#'   \item{DissNitrateNitrite}{Dissolved Nitrate + Nitrite value in mg/L as N}
#'   \item{DissOrthophos_Sign}{A symbol representing whether the Dissolved
#'     Ortho-phosphate value is below the reporting limit or above it. Contains
#'     one of four symbols: "<", "< (estimated)", "=", or "= (unreliable)". See
#'     the section below for more information about what these symbols represent.}
#'   \item{DissOrthophos}{Dissolved Ortho-phosphate value in mg/L as P}
#' }
#'
#' @section More information about the `_Sign` variables:
#' For the variables that have the _Sign suffix, the symbols in these variables
#' represent four conditions of the value contained in its corresponding result
#' variable.
#' * "<" - The value is below the Reporting Limit (RL) with the value in the
#' corresponding result variable equal to the RL.
#' * "< (estimated)" - The value is below the RL, but the RL is unknown. The
#' value in the corresponding result variable is the estimated RL value.
#' * "=" - The value is above the RL with the value in the corresponding result
#' variable equal to the actual value measured by the laboratory. An `NA` value
#' in the corresponding result variable indicates that the value is missing or
#' wasn't collected.
#' * "= (unreliable)" - The value in the corresponding result variable was
#' determined to be an outlier and is excluded from the data set.
#'
#' @source
#' * `discretewq` data package (version 2.3.2):
#'   <https://github.com/sbashevkin/discretewq/tree/v2.3.2>
#' * Water Quality Portal hosted by the National Water Quality Monitoring
#'   Council: <https://www.waterqualitydata.us/>
"raw_nutr_2013_2021"

#' @title Daily averages of continuous dissolved oxygen data from 2014-2021
#' @description A data set containing daily average values of continuous
#'   dissolved oxygen data collected by USGS from 2014-2021 at a few locations
#'   in the upper San Francisco Estuary (Delta). The locations included are
#'   Sacramento River near Decker Island (DEC) and the Yolo Bypass Toe Drain
#'   (TOE). Used in the analyses for the Drought Synthesis.
#'
#'   All dissolved oxygen data were downloaded directly from the National Water
#'   Information System (NWIS) using the `dataRetrieval` R package. For the DEC
#'   location, data is from the Sacramento River at Decker Island station
#'   (11455478) from 2014 to 4/27/2021 and from the Sacramento River below
#'   Toland Landing station (11455485) from 4/29/2021 through the remainder of
#'   2021. For the TOE location, data is from the Toe Drain at Mallard Road
#'   station (11455139) from 2014 to 12/19/2016 and from the Toe Drain at
#'   Liberty Island station (11455140) from 5/23/2017 through the remainder of
#'   2021. Some of the data in 2021 is considered provisional.
#'
#' @format data frame with 5,305 rows and 4 columns
#' \describe{
#'   \item{Station}{Location where measurement was collected. Either DEC
#'     (Sacramento River near Decker Island) or TOE (Yolo Bypass Toe Drain).}
#'   \item{Year}{Calendar year of the value}
#'   \item{Date}{Calendar date of the value in yyyy-mm-dd}
#'   \item{AvgDO}{Daily average dissolved oxygen value in milligrams per liter}
#' }
#'
#' @source
#' USGS National Water Information System (NWIS):
#'   <https://nwis.waterdata.usgs.gov/nwis>
"cont_do_daily"

#' @title Raw values of discrete chlorophyll-a water concentrations for 1975-2021
#' @description A long-term data set containing raw concentration values of
#'   chlorophyll-a in water samples collected within the upper San Francisco
#'   Estuary (Delta) from 1975-2021. Used in the analyses for the Drought
#'   Synthesis.
#'
#'   All data are from the `discretewq` data package. This data set contains
#'   only one data point per `Station` and `Date`.
#'
#' @format data frame with 19,887 rows and 13 columns
#' \describe{
#'   \item{Source}{Name of the source dataset. Either EMP (DWR's Environmental
#'     Monitoring Program), USGS_SFBS (USGS's San Francisco Bay Water Quality
#'     Survey), or DWR_NCRO (DWR's North Central Region Office)}
#'   \item{Station}{Location where sample was collected}
#'   \item{Latitude}{Latitude of `Station` in Decimal Degrees (WGS 84 Datum)}
#'   \item{Longitude}{Longitude of `Station` in Decimal Degrees (WGS 84 Datum)}
#'   \item{Region}{Region designation of `Station` within the larger Delta and
#'     Suisun Bay (Confluence, North, SouthCentral, Suisun Bay). The
#'     Drought Synthesis team categorized `SubRegion`s into broader `Region`s
#'     using [this
#'     crosswalk](https://github.com/mountaindboz/DroughtData/blob/master/data-raw/Global/Rosies_regions.csv)
#'   .}
#'   \item{SubRegion}{Subregion designation of `Station` as defined by the
#'     `R_EDSM_Subregions_Mahardja_FLOAT` shapefile from the
#'     [`deltamapr`](https://github.com/InteragencyEcologicalProgram/deltamapr)
#'     package.}
#'   \item{YearAdj}{Adjusted calendar year - December-November, with December of
#'     the previous calendar year included with the following year. For example,
#'     December of 2020 is assigned year "2021" in this data set.}
#'   \item{Season}{Winter (Dec-Feb), Spring (Mar-May), Summer (Jun-Aug), or Fall
#'     (Sept-Nov)}
#'   \item{Month}{Integer representing the month}
#'   \item{Date}{Calendar date of the measurement in yyyy-mm-dd}
#'   \item{Datetime}{Date and time (yyyy-mm-dd HH:MM:SS) of the measurement in
#'     PST.}
#'   \item{Chlorophyll_Sign}{A symbol representing whether the chlorophyll-a
#'     value is below the reporting limit or above it. Contains one of two
#'     symbols: "<" or "=". "<" indicates that the value is below the Reporting
#'     Limit (RL) with the value in `Chlorophyll` equal to the RL. "=" indicates
#'     that the value was detected by the laboratory with the value in
#'     `Chlorophyll` equal to the actual value measured by the laboratory.}
#'   \item{Chlorophyll}{Chlorophyll-a concentration in micrograms per liter}
#' }
#'
#' @source
#' `discretewq` data package (version 2.3.2.9000):
#'   <https://github.com/sbashevkin/discretewq/tree/dbb2638e800e637f3f2c70d47c9faad68f95db07>
"raw_chla_1975_2021"

