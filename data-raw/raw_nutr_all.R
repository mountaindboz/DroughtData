# Code to prepare data sets of raw nutrient data:
  # 1) `raw_nutr_1975_2021` - raw values of long-term nutrient concentrations (DissAmmonia,
    # DissNitrateNitrite, and DissOrthophos) for 1975-2021 from the EMP, USGS-SFBP, and
    # USGS-CAWSC (only SR at Freeport) surveys
  # 2) raw_nutr_2013_2021` - raw values of short-term nutrient concentrations (DissAmmonia,
    # DissNitrateNitrite, and DissOrthophos) for 2013-2021 from the EMP, USGS-SFBP, and
    # USGS-CAWSC surveys

# Load packages
library(tidyverse)
library(lubridate)
library(hms)
library(readxl)
# Make sure we are using `discretewq` version 2.1.0, commit 78232cec30df1e2606431eeae3710ca25cf0c7f7
# devtools::install_github("sbashevkin/discretewq", ref = "78232cec30df1e2606431eeae3710ca25cf0c7f7")
library(discretewq)
library(dataRetrieval)
library(deltamapr)
library(sf)


# 1. Import Data ----------------------------------------------------------

# Define file path for raw nutrient data
fp_nutr <- "data-raw/Nutrients/"

# Import nutrient data from the discretewq package
# Select EMP and USGS since these are the only surveys that have collected nutrient data
df_nutr_package <- wq(Sources = c("EMP", "USGS"), End_year = 2021)

# Import additional EMP nutrient data for 2021 provided from personal data request
df_nutr_2021_emp <-
  read_csv(
    file.path(fp_nutr, "EMP_delta_water_quality_Oct20-Nov21.csv"),
    col_types = cols_only(
      Station = "c",
      Date = "c",
      Time = "t",
      NorthLat = "d",
      WestLong = "d",
      DissAmmonia = "c",
      DissNitrateNitrite = "c",
      DissOrthophos = "c"
    )
  )

# Import EMP station coordinates from EDI
df_coord_emp <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.4&entityid=827aa171ecae79731cc50ae0e590e5af")

# Import CAWSC station info
df_coord_cawsc <-
  read_excel(
    file.path(fp_nutr, "USGS_CAWSC_stations.xlsx"),
    col_types = c(rep("text", 3), rep("numeric", 2), rep("skip", 4))
  )

# Download and save local copies of the USGS-CAWSC nutrient data using the `dataRetrieval`
  # package since some of the data is provisional and may change

# Set download to TRUE if need to download and save USGS-CAWSC nutrient data
download <- FALSE

# Download and save DAYFLOW and DTO data if necessary
if (download == TRUE) {

  # Add "USGS-" as a prefix to siteNumbers to work correctly with the `readWQPqw` function
  df_coord_cawsc_tmp <- df_coord_cawsc %>% mutate(siteNumber = paste0("USGS-", siteNumber))

  # Define the parameter codes and site numbers for the USGS data download
  #00608 = Ammonium + Ammonia
  #00631 = Nitrate + Nitrite
  #00671 = Ortho-Phosphate
  params <- c("00608", "00631", "00671")
  site_numb <- df_coord_cawsc_tmp %>% pull(siteNumber)

  # Download USGS nutrient data collected by the CAWSC group using the `readWQPqw` function
  df_nutr_cawsc_tmp <-
    readWQPqw(
      siteNumbers = site_numb,
      parameterCd = params,
      startDate = "1974-12-01",
      endDate = "2021-10-01"
    )

  # Save data as a .csv file in the "data-raw/Nutrients" folder
  df_nutr_cawsc_tmp %>% write_csv(file.path(fp_nutr, "USGS_CAWSC_nutr_data_1979-2021.csv"))

  # Clean up
  rm(df_coord_cawsc_tmp, params, site_numb, df_nutr_cawsc_tmp)
}

# Import USGS nutrient data collected by the CAWSC group
df_nutr_cawsc <-
  read_csv(
    file.path(fp_nutr, "USGS_CAWSC_nutr_data_1979-2021.csv"),
    col_types = cols_only(
      MonitoringLocationIdentifier = "c",
      ActivityStartDate = "D",
      ActivityStartTime.Time = "t",
      ActivityStartTime.TimeZoneCode = "c",
      CharacteristicName = "c",
      ResultAnalyticalMethod.MethodName = "c",
      ResultMeasureValue = "d",
      ResultMeasure.MeasureUnitCode = "c",
      ResultDetectionConditionText = "c",
      DetectionQuantitationLimitMeasure.MeasureValue = "d",
      DetectionQuantitationLimitMeasure.MeasureUnitCode = "c",
      ResultStatusIdentifier = "c"
    )
  )

# Import region assignments
df_regions <- read_csv("data-raw/Rosies_regions.csv")

# Load Delta regions shapefile from Brian
sf_delta <- R_EDSM_Subregions_Mahardja_FLOAT %>%
  # Filter to regions of interest
  filter(SubRegion %in% unique(df_regions$SubRegion)) %>%
  select(SubRegion)


# 2. Clean and Combine Data -----------------------------------------------

# Create functions to clean up and standardize values below the detection limit and their reporting limits
# EMP data from the discretewq` package
fill_miss_rl_emp <- function(df, value_var, sign_var, rl_val) {
  df %>%
    mutate(
      # replace the _Sign variable with "< (estimated)" for the records below the detection limit
      # and unknown reporting limits
      "{{sign_var}}" := if_else(
        {{ sign_var }} == "<" & is.na({{ value_var }}),
        "< (estimated)",
        {{ sign_var }}
      ),
      # fill in reporting limits for values that are non-detect and the reporting limits are unknown
      # fill with value in rl_val
      "{{value_var}}" := if_else({{ sign_var }} == "< (estimated)", rl_val, {{ value_var }})
    )
}

# Prepare nutrient data from the discretewq package to be combined with EMP data from 2021
df_nutr_package_c <- df_nutr_package %>%
  mutate(
    # Convert Date variable to date object
    Date = date(Date),
    # Convert Datetime to PST
    Datetime = with_tz(Datetime, tzone = "Etc/GMT+8"),
    # Change Source for USGS data to "USGS-SFBP"
    Source = if_else(Source == "USGS", "USGS-SFBP", Source)
  ) %>%
  # Fill in "=" for the _Sign variables for the USGS-SFBP data for now
    # this assumes all provided values are above the RL
  mutate(across(ends_with("_Sign"), ~if_else(is.na(.x), "=", .x))) %>%
  # Fill in reporting limits for values that are non-detect and the reporting limits are unknown
    # We will use 0.01 for all of these for now
  fill_miss_rl_emp(DissOrthophos, DissOrthophos_Sign, 0.01) %>%
  fill_miss_rl_emp(DissAmmonia, DissAmmonia_Sign, 0.01) %>%
  fill_miss_rl_emp(DissNitrateNitrite, DissNitrateNitrite_Sign, 0.01) %>%
  # Select variables to keep
  select(
    Source,
    Station,
    Latitude,
    Longitude,
    Date,
    Datetime,
    starts_with("DissAmmonia"),
    starts_with("DissNitrateNitrite"),
    starts_with("DissOrthophos")
  )

# Prepare EMP station coordinates to be joined to 2021 EMP data
df_coord_emp_c <- df_coord_emp %>%
  select(Station, Latitude, Longitude) %>%
  drop_na()

# Prepare 2021 EMP data to be combined with the discretewq data
df_nutr_2021_emp_c <- df_nutr_2021_emp %>%
  # Parse Date and create Source and Datetime (as PST) variables
  mutate(
    Date = mdy(Date),
    Datetime = ymd_hms(paste(Date, Time), tz = "Etc/GMT+8"),
    Source = "EMP"
  ) %>%
  # Remove overlapping data
  filter(year(Date) > 2020) %>%
  # Add station coordinates
  left_join(df_coord_emp_c, by = "Station") %>%
  mutate(
    Latitude = if_else(is.na(Latitude), NorthLat, Latitude),
    Longitude = if_else(is.na(Longitude), WestLong, Longitude)
  ) %>%
  # Clean up data variables
  pivot_longer(
    cols = starts_with("Diss"),
    names_to = "Parameter",
    values_to = "Value"
  ) %>%
  mutate(
    # Keep the first value of a lab replicate pair
    Value = if_else(str_detect(Value, ","), str_extract(Value, ".+(?=,)"), Value),
    # Add a new variable to identify values below the reporting limit
    Sign = if_else(str_detect(Value, "^<"), "<", "="),
    # For the values below the reporting limit, make them equal to the reporting limit and convert to numeric
    Value = as.numeric(if_else(str_detect(Value, "^<"), str_remove(Value, "^<"), Value))
  ) %>%
  pivot_wider(
    names_from = Parameter,
    values_from = c(Value, Sign),
    names_glue = "{Parameter}_{.value}"
  ) %>%
  rename_with(~str_remove(.x, "_Value"), ends_with("_Value")) %>%
  # Select variables to keep
  select(
    Source,
    Station,
    Latitude,
    Longitude,
    Date,
    Datetime,
    starts_with("DissAmmonia"),
    starts_with("DissNitrateNitrite"),
    starts_with("DissOrthophos")
  )

# Prepare USGS-CAWSC station coordinates to be joined to USGS-CAWSC data
df_coord_cawsc_c <- df_coord_cawsc %>%
  # Fix one longitude value that should be negative
  mutate(Long = if_else(Long > 0, -Long, Long)) %>%
  select(
    Station = siteNumber,
    Latitude = Lat,
    Longitude = Long
  )

# Prepare the CAWSC nutrient data to be combined with the discretewq data
df_nutr_cawsc_c <- df_nutr_cawsc %>%
  # Rename variables so that they are easier to use
  select(
    Station = MonitoringLocationIdentifier,
    Date = ActivityStartDate,
    Time = ActivityStartTime.Time,
    TimeZone = ActivityStartTime.TimeZoneCode,
    Parameter = CharacteristicName,
    Method = ResultAnalyticalMethod.MethodName,
    Value = ResultMeasureValue,
    ValueDetectionQual = ResultDetectionConditionText,
    DetectionLimit = DetectionQuantitationLimitMeasure.MeasureValue
  ) %>%
  # Remove rows with NA values in Time variable
  filter(!is.na(Time)) %>%
  mutate(
    # Remove "USGS-" prefix from Station names
    Station = str_remove(Station, "^USGS-"),
    # Add Source variable
    Source = "USGS-CAWSC",
    # Change Parameter names to standardized names
    Parameter = case_when(
      str_detect(Parameter, "^Amm") ~ "DissAmmonia",
      str_detect(Parameter, "^Inorg") ~ "DissNitrateNitrite",
      str_detect(Parameter, "^Ortho") ~ "DissOrthophos"
    ),
    # Convert Time variable to PST
    Time = if_else(TimeZone == "PDT", hms(lubridate::hms(Time) - hours(1)), Time),
    # Create Datetime variable as PST
    Datetime = ymd_hms(paste(Date, Time), tz = "Etc/GMT+8"),
    # Calculate difference from noon for each data point for later filtering
    NoonDiff = abs(hms(hours = 12) - Time)
  ) %>%
  # Select only 1 data point per station and date, choose data closest to noon
  group_by(Station, Date, Parameter) %>%
  filter(NoonDiff == min(NoonDiff)) %>%
  # When points are equidistant from noon, select earlier point
  filter(Time == min(Time)) %>%
  ungroup() %>%
  # Clean up data below the detection limit
  mutate(
    # Add a new variable to identify values below the detection limit - NA's in Value are <DL
    Sign = case_when(
      !is.na(Value) ~ "=",
      ValueDetectionQual == "Not Detected" & !is.na(DetectionLimit) ~ "<",
      ValueDetectionQual == "Not Detected" & is.na(DetectionLimit) ~ "< (estimated)"
    ),
    # For the values below the detection limit, make them equal to the detection limit
      # If no detection limit is available, use the most common detection limit for the method
    Value = case_when(
      Sign == "=" ~ Value,
      Sign == "<" ~ DetectionLimit,
      Sign == "< (estimated)" & Method == "Ammonia, wf, DA sal/hypo (NWQL)" ~ 0.01,
      Sign == "< (estimated)" & Method == "NO3+NO2, wf, FCC,NaR, DA" ~ 0.04,
      Sign == "< (estimated)" & Method == "NO3+NO2, wf, FCC,NaR, DA, LL" ~ 0.01,
      Sign == "< (estimated)" & Method == "Ortho-PO4, wf, DA phos/mol(NWQL)" ~ 0.004,
    )
  ) %>%
  # Remove a few unnecessary variables and restructure to wide format
  select(-c(ValueDetectionQual, DetectionLimit, Method)) %>%
  pivot_wider(
    names_from = Parameter,
    values_from = c(Value, Sign),
    names_glue = "{Parameter}_{.value}"
  ) %>%
  rename_with(~str_remove(.x, "_Value"), ends_with("_Value")) %>%
  # Fill in "=" for the NA values in the _Sign variables
  mutate(across(ends_with("_Sign"), ~if_else(is.na(.x), "=", .x))) %>%
  # Add station coordinates
  left_join(df_coord_cawsc_c) %>%
  # Select variables to keep
  select(
    Source,
    Station,
    Latitude,
    Longitude,
    Date,
    Datetime,
    starts_with("DissAmmonia"),
    starts_with("DissNitrateNitrite"),
    starts_with("DissOrthophos")
  )

# Combine EMP data from 2021 and USGS-CAWSC data to discretewq data
df_nutr_all <- bind_rows(df_nutr_package_c, df_nutr_2021_emp_c, df_nutr_cawsc_c)


# 3. Clean All Raw Data ---------------------------------------------------

# Begin to clean all raw data

# Run a couple of QC checks:
# Are there cases where multiple samples were collected at the same station in one day?

# Remove data points that are more than 10 SDs away from the mean of each subregion

# Finish cleaning all raw data


# 4. Process Long-term Data -----------------------------------------------


# 5. Process Short-term Data ----------------------------------------------


# 6. Save and Export Data -------------------------------------------------


