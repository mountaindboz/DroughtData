# Code to prepare data set of raw water quality measurements:
  # `raw_wq_1975_2021` - raw values for water quality parameters (water temperature,
    # salinity, secchi) for 1975-2021

# Load packages
library(tidyverse)
library(lubridate)
library(hms)
library(discretewq)
library(deltamapr)
library(sf)
library(wql)


# 1. Import Data ----------------------------------------------------------

# Import WQ field measurement data from the discretewq package
  # Select the EMP, STN, and FMWT long-term surveys since these are the only
  # long-term surveys for which we have data for 2021
df_wq_package <- wq(Sources = c("EMP", "STN", "FMWT"), End_year = 2021)

# Import additional WQ field measurement data for 2021 provided from personal data requests
# EMP:
df_wq_2021_emp <-
  read_csv(
    "data-raw/WQ_Measurements/EMP_fielddata_1220-0921.csv",
    col_types = "ccc--ddddd",
    na = "N/A"
  )

# FMWT:
df_wq_2021_fmwt <-
  read_csv(
    "data-raw/WQ_Measurements/FMWT 1967-2021 Catch Matrix_updated.csv",
    col_types = cols_only(
      SampleDate = "c",
      StationCode = "c",
      StationLat = "d",
      StationLong = "d",
      SampleTimeStart = "c",
      WaterTemperature = "d",
      Secchi = "d",
      ConductivityTop = "d"
    )
  )

# Import EMP station coordinates from EDI
df_coord_emp <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.4&entityid=827aa171ecae79731cc50ae0e590e5af")

# Import region assignments
df_regions <- read_csv("data-raw/Rosies_regions.csv")

## Load Delta regions shapefile from Brian
sf_delta <- R_EDSM_Subregions_Mahardja_FLOAT %>%
  # Filter to regions of interest
  filter(SubRegion %in% unique(df_regions$SubRegion)) %>%
  select(SubRegion)


# 2. Clean and Combine Data -----------------------------------------------

# Prepare WQ field measurement data from the discretewq package to be combined with data from 2021
df_wq_package_c <- df_wq_package %>%
  mutate(
    # Convert Date variable to date object
    Date = date(Date),
    # Convert Datetime to PST
    Datetime = with_tz(Datetime, tzone = "Etc/GMT+8")
  ) %>%
  # Select variables to keep
  select(
    Source,
    Station,
    Latitude,
    Longitude,
    Date,
    Datetime,
    Temperature,
    Salinity,
    Secchi
  )

# Prepare EMP station coordinates to be joined to 2021 EMP data
df_coord_emp_c <- df_coord_emp %>%
  select(Station, Latitude, Longitude) %>%
  drop_na()

# Prepare 2021 EMP data
df_wq_2021_emp_c <- df_wq_2021_emp %>%
  # Rename variables so that they are easier to use
  rename(
    StationName = `Station Name`,
    StationNumber = `Station Number`,
    SampleDate = `Sample Date`,
    Latitude = `North Latitude Decimal Degrees GPS Location - [1]*`,
    Longitude = `West Longitude Decimal Degrees GPS Location - [1]*`,
    Secchi = `Secchi Depth Centimeters Secchi Depth - [1]*`,
    Temperature = `Water Temperature °C EPA 170.1 (Field) - [1]*`,
    SpCnd = `Specific Conductance uS/cm@25 °C EPA 120.1 (Field) - [1]*`
  ) %>%
  mutate(
    # Parse datetime as PST and create Date and Source variables
    Datetime = mdy_hm(SampleDate, tz = "Etc/GMT+8"),
    Date = date(Datetime),
    Source = "EMP",
    # Clean up station names
    Station = case_when(
      str_detect(StationName, "^SF Estuarine") ~ StationNumber,
      str_detect(StationName, "Hood") ~ "C3A",
      str_detect(StationName, "^NZ068") ~ "NZ068",
      TRUE ~ str_extract(StationName, "[:graph:]+(?=\\s-\\s)")
    )
  ) %>%
  # Remove overlapping data collected in Dec 2020
  filter(year(Date) != 2020) %>%
  # Add station coordinates
  left_join(df_coord_emp_c, by = "Station", suffix = c("_field", "")) %>%
  mutate(
    Latitude = if_else(is.na(Latitude), Latitude_field, Latitude),
    Longitude = if_else(is.na(Longitude), Longitude_field, Longitude)
  ) %>%
  # Convert SpCnd to Salinity
  mutate(Salinity = ec2pss(SpCnd/1000, t = 25)) %>%
  # Select variables to keep
  select(
    Source,
    Station,
    Latitude,
    Longitude,
    Date,
    Datetime,
    Temperature,
    Salinity,
    Secchi
  )

# Prepare 2021 FMWT data
df_wq_2021_fmwt_c <- df_wq_2021_fmwt %>%
  # Parse Date and create Source variable
  mutate(
    Date = mdy(SampleDate),
    Source = "FMWT"
  ) %>%
  # Remove overlapping data
  filter(year(Date) > 2020) %>%
  mutate(
    # Parse Time and create Datetime variable converted to PST
    Time = as_hms(as.numeric(SampleTimeStart)),
    Datetime = with_tz(ymd_hms(paste(Date, Time), tz = "America/Los_Angeles"), tzone = "Etc/GMT+8"),
    # Clean up station codes
    Station = case_when(
      StationCode == "72" ~ "072",
      StationCode == "73" ~ "073",
      TRUE ~ StationCode
    ),
    # Convert Secchi depth to centimeters
    Secchi = Secchi * 100,
    # Convert SpCnd to Salinity
    Salinity = ec2pss(ConductivityTop/1000, t = 25)
  ) %>%
  # Clean up variable names
  select(
    Source,
    Station,
    Latitude = StationLat,
    Longitude = StationLong,
    Date,
    Datetime,
    Temperature = WaterTemperature,
    Salinity,
    Secchi
  )

# Combine data from 2021 to all other data
df_wq_all <- bind_rows(df_wq_package_c, df_wq_2021_emp_c, df_wq_2021_fmwt_c)


# 3. Clean All Raw Data ---------------------------------------------------

# Begin to clean all raw data
df_wq_all_c <- df_wq_all %>%
  # Remove records where all three WQ variables are NA
  filter(!if_all(c(Temperature, Salinity, Secchi), is.na)) %>%
  # Remove records without latitude-longitude coordinates
  filter(!if_any(c(Latitude, Longitude), is.na)) %>%
  # Convert to sf object
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  # Change to crs of sf_delta
  st_transform(crs = st_crs(sf_delta)) %>%
  # Add subregions
  st_join(sf_delta, join = st_intersects) %>%
  # Remove any data outside our subregions of interest
  filter(!is.na(SubRegion)) %>%
  # Drop sf geometry column since it's no longer needed
  st_drop_geometry()

# Run a couple of QC checks:
# Are there cases where multiple samples were collected at the same station in one day?
df_wq_all_c %>%
  count(Source, Station, Date) %>%
  filter(n > 1)
# NO- no need to filter out duplicates

# Remove data points that are more than 10 SDs away from the mean of each subregion
df_wq_all_c1 <- df_wq_all_c %>%
  pivot_longer(
    cols = c(Temperature, Salinity, Secchi),
    names_to = "Parameter",
    values_to = "Value"
  ) %>%
  filter(!is.na(Value)) %>%
  # Calculate the number of SD's each data point exceeds the mean for each subregion
  group_by(SubRegion, Parameter) %>%
  mutate(ValueExceed = (Value - mean(Value))/sd(Value)) %>%
  ungroup() %>%
  filter(ValueExceed < 10) %>%
  select(-ValueExceed) %>%
  pivot_wider(names_from = Parameter, values_from = Value) %>%
  filter(!if_all(c(Temperature, Salinity, Secchi), is.na))

# Finish cleaning all raw data
raw_wq_1975_2021 <- df_wq_all_c1 %>%
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
  left_join(df_regions, by = c("Season", "SubRegion")) %>%
  # Remove any data not chosen for the long-term analysis
  filter(Long_term) %>%
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
    Temperature,
    Salinity,
    Secchi
  ) %>%
  # Arrange data
  arrange(Date, Source, Station)


# 4. Save and Export Data -------------------------------------------------

# Save final data set of raw water quality measurements as csv file for easier diffing
write_csv(raw_wq_1975_2021, "data-raw/Final/raw_wq_1975_2021.csv")

# Save final data set of raw water quality measurements as object in the data package
usethis::use_data(raw_wq_1975_2021, overwrite = TRUE)

