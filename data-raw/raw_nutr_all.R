# Code to prepare data sets of raw nutrient data:
  # 1) `raw_nutr_1975_2021` - raw values of long-term nutrient concentrations (DissAmmonia,
    # DissNitrateNitrite, and DissOrthophos) for 1975-2021 from the EMP, USGS_SFBS, and
    # USGS_CAWSC (only SR at Freeport) surveys
  # 2) `raw_nutr_2013_2021` - raw values of short-term nutrient concentrations (DissAmmonia,
    # DissNitrateNitrite, and DissOrthophos) for 2013-2021 from the EMP, USGS_SFBS, and
    # USGS_CAWSC surveys

# Load packages
library(tidyverse)
library(lubridate)
library(hms)
library(readxl)
# Make sure we are using `discretewq` version 2.3.1
# install.packages("devtools")
# devtools::install_github("sbashevkin/discretewq", ref = "v2.3.1")
library(discretewq)
library(dataRetrieval)
# Make sure we are using `deltamapr` version 1.0.0, commit d0a6f9c22aa074f906176e99a0ed70f97f26fffd
# devtools::install_github("InteragencyEcologicalProgram/deltamapr", ref = "d0a6f9c22aa074f906176e99a0ed70f97f26fffd")
library(deltamapr)
library(sf)
library(rlang)
library(glue)


# 1. Import Data ----------------------------------------------------------

# Define file path for raw nutrient data
fp_nutr <- "data-raw/Nutrients/"

# Import discrete nutrient data from the discretewq package (v2.3.1)
# Select EMP and USGS_SFBS since these are the only surveys besides USGS_CAWSC
  # that have collected discrete nutrient data
# The USGS_CAWSC survey also has collected discrete nutrient data; however, we
  # will use data directly imported with the dataRetrieval R package since the
  # discretewq package does not provide preliminary data and RL values
df_nutr_package <- wq(Sources = c("EMP", "USGS_SFBS"), End_year = 2021)

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

# Download and save local copies of the USGS_CAWSC nutrient data using the `dataRetrieval`
  # package since some of the data is provisional and may change

# Set download to TRUE if need to download and save USGS_CAWSC nutrient data
download <- FALSE

# Download and save USGS_CAWSC data if necessary
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


# 2. Clean and Combine Data -----------------------------------------------

# 2.1 discretewq Data -----------------------------------------------------

# Create function to fill in missing reporting limits in the data from the discretewq` package
fill_miss_rl <- function(df, value_var, sign_var, rl_val) {
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

# Prepare nutrient data from the discretewq package to be combined with EMP data from 2021 and USGS_CAWSC data
df_nutr_package_c <- df_nutr_package %>%
  mutate(
    # Convert Date variable to date object
    Date = date(Date),
    # Convert Datetime to PST
    Datetime = with_tz(Datetime, tzone = "Etc/GMT+8")
  ) %>%
  # Fill in "=" for the _Sign variables for the USGS_SFBS data for now
    # this assumes all provided values are above the RL
  mutate(across(ends_with("_Sign"), ~if_else(is.na(.x), "=", .x))) %>%
  # Fill in reporting limits for values that are non-detect and the reporting limits are unknown in the EMP
    # data set- We will use 0.01 for all of these for now
  fill_miss_rl(DissAmmonia, DissAmmonia_Sign, 0.01) %>%
  fill_miss_rl(DissNitrateNitrite, DissNitrateNitrite_Sign, 0.01) %>%
  fill_miss_rl(DissOrthophos, DissOrthophos_Sign, 0.01) %>%
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

# For the USGS_SFBS, if at least one of the nutrient parameters has a value reported, then we
  # will assume that the other parameters were below the reporting limit for that station and day
df_nutr_sfbs_blw_rl <- df_nutr_package_c %>%
  filter(Source == "USGS_SFBS") %>%
  filter(!if_all(c(DissAmmonia, DissNitrateNitrite, DissOrthophos), is.na)) %>%
  filter(if_any(c(DissAmmonia, DissNitrateNitrite, DissOrthophos), is.na)) %>%
  mutate(
    DissAmmonia_Sign = if_else(is.na(DissAmmonia), "<", DissAmmonia_Sign),
    DissNitrateNitrite_Sign = if_else(is.na(DissNitrateNitrite), "<", DissNitrateNitrite_Sign),
    DissOrthophos_Sign = if_else(is.na(DissOrthophos), "<", DissOrthophos_Sign)
  ) %>%
  # These RL values were provided by USGS for 2006-present. We assumed these were constant throughout the
    # entire monitoring program including in years earlier than 2006.
  fill_miss_rl(DissAmmonia, DissAmmonia_Sign, 0.0007) %>%
  fill_miss_rl(DissNitrateNitrite, DissNitrateNitrite_Sign, 0.0007) %>%
  fill_miss_rl(DissOrthophos, DissOrthophos_Sign, 0.0015)

df_nutr_package_c1 <- df_nutr_package_c %>%
  anti_join(df_nutr_sfbs_blw_rl, by = c("Source", "Station", "Datetime")) %>%
  bind_rows(df_nutr_sfbs_blw_rl)

# 2.2 2021 EMP Data -------------------------------------------------------

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

# 2.3 USGS_CAWSC Data -----------------------------------------------------

# Prepare USGS_CAWSC station coordinates to be joined to USGS_CAWSC data
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
    Source = "USGS_CAWSC",
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

# 2.4 Combine All Data ----------------------------------------------------

# Combine EMP data from 2021 and USGS_CAWSC data to discretewq data
df_nutr_all <- bind_rows(df_nutr_package_c1, df_nutr_2021_emp_c, df_nutr_cawsc_c)


# 3. Clean All Raw Data ---------------------------------------------------

# Begin to clean all raw data
df_nutr_all_c <- df_nutr_all %>%
  # Remove records where all three nutrient variables are NA
  filter(!if_all(c(DissAmmonia, DissNitrateNitrite, DissOrthophos), is.na)) %>%
  # Remove records without latitude-longitude coordinates
  filter(!if_any(c(Latitude, Longitude), is.na)) %>%
  # Convert to sf object
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  # Change to crs of sf_delta
  st_transform(crs = st_crs(DroughtData:::sf_delta)) %>%
  # Add subregions
  st_join(DroughtData:::sf_delta, join = st_intersects) %>%
  # Remove any data outside our subregions of interest
  filter(!is.na(SubRegion)) %>%
  # Drop sf geometry column since it's no longer needed
  st_drop_geometry() %>%
  # Remove one DissAmmonia value equal to zero
  mutate(DissAmmonia = if_else(DissAmmonia == 0, NA_real_, DissAmmonia))

# Run a couple of QC checks:
# Create a vector of nutrient parameter names to check
vars_nutr <- str_subset(names(df_nutr_all_c), "^Diss.+[^_Sign]$")

# Are there cases where multiple samples were collected at the same station in one day?
for (var in vars_nutr) {
  print(var)
  df_nutr_all_c %>%
    filter(!is.na(.data[[var]])) %>%
    count(Source, Station, Date) %>%
    filter(n > 1) %>%
    print()
}
# NO- no need to filter out duplicates

# Look for and remove outliers from the data set:
# Create function to flag data points with modified z-scores greater than a specified threshold
flag_modzscore <- function(df, data_var, threshold) {
  mod_zscore_sym <- sym(glue("{as_name(enquo(data_var))}_mod_zscore"))
  mod_zscore_enquo <- enquo(mod_zscore_sym)

  df %>%
    mutate(
      tmp_median = median({{ data_var }}, na.rm = TRUE),
      tmp_mad = mad({{ data_var }}, na.rm = TRUE),
      !!mod_zscore_sym := if_else(
        tmp_mad == 0,
        NA_real_,
        abs(0.6745 * ({{ data_var }} - tmp_median) / tmp_mad)
      ),
      "{{data_var}}_mod_zscore_flag" := case_when(
        is.na(!!mod_zscore_enquo) ~ FALSE,
        !!mod_zscore_enquo > threshold ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    select(!starts_with("tmp_"))
}

# Create function to remove flagged data points and modify value in related _Sign variable to "= (unreliable)"
rm_flagged <- function(df, data_var) {
  sign_sym <- sym(glue("{as_name(enquo(data_var))}_Sign"))
  sign_enquo <- enquo(sign_sym)
  flag_sym <- sym(glue("{as_name(enquo(data_var))}_mod_zscore_flag"))
  flag_enquo <- enquo(flag_sym)

  df %>%
    mutate(
      !!sign_sym := if_else(!!flag_enquo == TRUE, "= (unreliable)", !!sign_enquo),
      "{{data_var}}" := if_else(!!flag_enquo == TRUE, NA_real_, {{ data_var }})
    )
}

# Flag data points that have modified z-scores greater than 15 grouped by subregion
df_nutr_all_c1 <- df_nutr_all_c %>%
  group_by(SubRegion) %>%
  flag_modzscore(DissAmmonia, 15) %>%
  flag_modzscore(DissNitrateNitrite, 15) %>%
  flag_modzscore(DissOrthophos, 15) %>%
  ungroup() %>%
  # Remove flags from all data points in the Lower Mokelumne River subregion since these probably
    # shouldn't be considered outliers
  mutate(
    DissAmmonia_mod_zscore_flag = if_else(
      SubRegion == "Lower Mokelumne River" & DissAmmonia_mod_zscore_flag == TRUE,
      FALSE,
      DissAmmonia_mod_zscore_flag
    ),
    DissNitrateNitrite_mod_zscore_flag = if_else(
      SubRegion == "Lower Mokelumne River" & DissNitrateNitrite_mod_zscore_flag == TRUE,
      FALSE,
      DissNitrateNitrite_mod_zscore_flag
    ),
    DissOrthophos_mod_zscore_flag = if_else(
      SubRegion == "Lower Mokelumne River" & DissOrthophos_mod_zscore_flag == TRUE,
      FALSE,
      DissOrthophos_mod_zscore_flag
    )
  ) %>%
  # Exclude remaining flagged data points
  rm_flagged(DissAmmonia) %>%
  rm_flagged(DissNitrateNitrite) %>%
  rm_flagged(DissOrthophos) %>%
  # Exclude flagged data points that are <RL values with high RL's
  # *** This filter removes some data that we would like to keep because the
    # data frame is in a wide format; however, this is how the data was prepared
    # for the Feb 2022 version of the Drought Synthesis report. We will fix this
    # in a later version.
  filter(
    !(str_detect(DissAmmonia_Sign, "^<") & DissAmmonia >= .2),
    !(str_detect(DissNitrateNitrite_Sign, "^<") & DissNitrateNitrite >= 3),
    !(str_detect(DissOrthophos_Sign, "^<") & DissOrthophos > .15)
  ) %>%
  # Remove mod z-score variables
  select(!contains("_mod_zscore"))

# Finish cleaning all raw data
df_nutr_all_c2 <- df_nutr_all_c1 %>%
  # Add variables for adjusted calendar year, month, and season
    # Adjusted calendar year: December-November, with December of the previous calendar year
    # included with the following year
  mutate(
    Month = month(Date),
    YearAdj = if_else(Month == 12, year(Date) + 1, year(Date)),
    Season = case_when(
      Month %in% 3:5 ~ "Spring",
      Month %in% 6:8 ~ "Summer",
      Month %in% 9:11 ~ "Fall",
      Month %in% c(12, 1, 2) ~ "Winter"
    )
  ) %>%
  # Restrict data to 1975-2021
  filter(YearAdj %in% 1975:2021) %>%
  # Add region designations
  left_join(DroughtData:::df_regions, by = c("Season", "SubRegion")) %>%
  # Remove data collected within the Suisun Marsh region since there is a large
    # gap in its long-term record and it only has recent data for 2017-2021
  filter(Region != "Suisun Marsh") %>%
  # Select variables to keep
  select(
    Source,
    Station,
    Latitude,
    Longitude,
    Region,
    SubRegion,
    YearAdj,
    Season,
    Month,
    Date,
    Datetime,
    starts_with("DissAmmonia"),
    starts_with("DissNitrateNitrite"),
    starts_with("DissOrthophos"),
    Long_term
  ) %>%
  # Arrange data
  arrange(Date, Source, Station)


# 4. Process Long-term Data -----------------------------------------------

raw_nutr_1975_2021 <- df_nutr_all_c2 %>%
  # Remove any data not chosen for the long-term analysis
  filter(Long_term) %>%
  # Only include data from the SR @ Freeport station (11447650) for the USGS_CAWSC survey
  filter(!(Source == "USGS_CAWSC" & Station != "11447650")) %>%
  # Remove the Long_term variable
  select(-Long_term)


# 5. Process Short-term Data ----------------------------------------------

raw_nutr_2013_2021 <- df_nutr_all_c2 %>%
  # Remove the Long_term variable
  select(-Long_term) %>%
  # Only include data from 2013-2021
  filter(YearAdj %in% 2013:2021)


# 6. Save and Export Data -------------------------------------------------

# Save final data sets of raw nutrient concentrations as csv files for easier diffing
write_csv(raw_nutr_1975_2021, "data-raw/Final/raw_nutr_1975_2021.csv")
write_csv(raw_nutr_2013_2021, "data-raw/Final/raw_nutr_2013_2021.csv")

# Save final data sets of raw nutrient concentrations as objects in the data package
usethis::use_data(raw_nutr_1975_2021, raw_nutr_2013_2021, overwrite = TRUE)

