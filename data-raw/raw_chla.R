# Code to prepare data set of raw discrete chlorophyll-a data:
  # `raw_chla_1975_2021` - raw values of discrete chlorophyll-a concentrations for
    # 1975-2021 from the EMP and USGS_SFBS surveys

# Load packages
library(tidyverse)
library(lubridate)
library(readxl)
# Make sure we are using `discretewq` version 2.3.2
# install.packages("devtools")
# devtools::install_github("sbashevkin/discretewq", ref = "v2.3.2")
library(discretewq)
# Make sure we are using `deltamapr` version 1.0.0, commit d0a6f9c22aa074f906176e99a0ed70f97f26fffd
# devtools::install_github("InteragencyEcologicalProgram/deltamapr", ref = "d0a6f9c22aa074f906176e99a0ed70f97f26fffd")
library(deltamapr)
library(sf)
library(here)

i_am("data-raw/raw_chla.R")


# 1. Import Data ----------------------------------------------------------

# Import discrete chlorophyll-a data from the discretewq package (v2.3.2)
# Select EMP and USGS_SFBS since these are the only surveys that have collected
  # long-term (1975-current) discrete chlorophyll-a data
df_package <- wq(Sources = c("EMP", "USGS_SFBS"), End_year = 2021)

# Import additional EMP discrete chlorophyll-a data for 2021 - provided from
  # personal data request
df_emp_2021 <-
  read_excel(
    path = here("data-raw/Chlorophyll/EMP_water_quality_Dec2020-Dec2021.xlsx"),
    range = "A2:AA288",
    col_types = "text",
    na = c("", "N/A")
  )

# Import additional EMP field data for 2021 - this includes EZ station
  # coordinates - provided from personal data request
df_emp_field_2021 <-
  read_excel(
    path = here("data-raw/Chlorophyll/EMP_water_quality_Dec2020-Dec2021.xlsx"),
    range = "A290:R576",
    col_types = "text",
    na = c("", "N/A")
  )

# Import EMP station coordinates from EDI
df_emp_coord <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.4&entityid=827aa171ecae79731cc50ae0e590e5af")

# Modify sf_delta to include an additional subregion - Grant Line Canal and Old River
sf_delta_chla <- R_EDSM_Subregions_Mahardja_FLOAT %>%
  # Filter to regions of interest
  filter(SubRegion %in% c(unique(DroughtData:::df_regions$SubRegion), "Grant Line Canal and Old River")) %>%
  select(SubRegion)


# 2. Clean and Combine Data -----------------------------------------------

# discretewq data
df_package_c <- df_package %>%
  transmute(
    Source,
    Station,
    Latitude,
    Longitude,
    # Convert Date variable to date object
    Date = date(Date),
    # Convert Datetime to PST
    Datetime = with_tz(Datetime, tzone = "Etc/GMT+8"),
    Chlorophyll,
    # No Chla values appear to be <RL - make all records "=" in Chlorophyll_Sign
    Chlorophyll_Sign = "="
  ) %>%
  # Remove records without Chlorophyll-a data
  drop_na(Chlorophyll)

# Prepare EMP station coordinates to be joined to 2021 EMP data
df_emp_coord_c <- df_emp_coord %>%
  select(Station, Latitude, Longitude) %>%
  drop_na()

# Prepare DWR_EMP EZ station coordinates for 2021 to be joined to 2021 DWR_EMP data
df_emp_coord_ez21 <- df_emp_field_2021 %>%
  select(
    SampleCode = `Sample Code`,
    Latitude_field = contains("Latitude"),
    Longitude_field = contains("Longitude")
  ) %>%
  drop_na() %>%
  mutate(across(ends_with("_field"), as.numeric))

# 2021 EMP Data
df_emp_2021_c <- df_emp_2021 %>%
  # Select and standardize variable names
  select(
    Station = `Station Name`,
    StationNumber = `Station Number`,
    SampleCode = `Sample Code`,
    Datetime = `Sample Date`,
    Chlorophyll = contains("Chlorophyll")
  ) %>%
  # Parse Datetime (as PST) and create Date variable
  mutate(
    Datetime = mdy_hm(Datetime, tz = "Etc/GMT+8"),
    Date = date(Datetime)
  ) %>%
  # Remove overlapping data
  filter(year(Date) > 2020) %>%
  # Remove records without Chlorophyll-a data
  drop_na(Chlorophyll) %>%
  mutate(
    # Create Source variable
    Source = "EMP",
    # Standardize Stations
    Station = case_when(
      str_detect(Station, "^SF Estuarine") ~ StationNumber,
      str_detect(Station, "Hood\\s-\\sC3A$") ~ "C3A",
      str_detect(Station, "^NZ068") ~ "NZ068",
      TRUE ~ str_extract(Station, ".+(?=\\s-\\s)")
    ),
    # Add a new variable to identify Chlorophyll-a values below the reporting limit
    Chlorophyll_Sign = if_else(str_detect(Chlorophyll, "^<"), "<", "="),
    # For the Chlorophyll-a values below the reporting limit, make them equal to
      # the reporting limit and convert to numeric
    Chlorophyll = as.numeric(
      if_else(
        Chlorophyll_Sign == "<",
        str_remove(Chlorophyll, "^<"),
        Chlorophyll
      )
    )
  ) %>%
  # Add station coordinates
  left_join(df_emp_coord_c, by = "Station") %>%
  left_join(df_emp_coord_ez21, by = "SampleCode") %>%
  mutate(
    Latitude = if_else(is.na(Latitude), Latitude_field, Latitude),
    Longitude = if_else(is.na(Longitude), Longitude_field, Longitude)
  ) %>%
  # Select variable order
  select(
    Source,
    Station,
    Latitude,
    Longitude,
    Date,
    Datetime,
    starts_with("Chlorophyll")
  )

# Combine all discrete chlorophyll-a data
df_all <- bind_rows(df_package_c, df_emp_2021_c)


# 3. Clean All Raw Data ---------------------------------------------------

# Begin to clean all raw data
df_all_c <- df_all %>%
  # Remove records without latitude-longitude coordinates
  drop_na(Latitude, Longitude) %>%
  # Convert to sf object
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  # Change to crs of sf_delta_chla
  st_transform(crs = st_crs(sf_delta_chla)) %>%
  # Add subregions
  st_join(sf_delta_chla, join = st_intersects) %>%
  # Remove any data outside our subregions of interest
  filter(!is.na(SubRegion)) %>%
  # Drop sf geometry column since it's no longer needed
  st_drop_geometry()

# Run a couple of QC checks:
# Are there cases where multiple samples were collected at the same station in one day?
df_all_c %>%
  count(Source, Station, Date) %>%
  filter(n > 1)
# NO- no need to filter out duplicates

# Look for and remove outliers from the data set:
# Flag data points that have modified z-scores greater than 15 grouped by subregion
df_all_c %>%
  group_by(SubRegion) %>%
  mutate(
    Chla_median = median(Chlorophyll),
    Chla_mad = mad(Chlorophyll),
    Chla_mod_zscore = if_else(
      Chla_mad == 0,
      NA_real_,
      abs(0.6745 * (Chlorophyll - Chla_median) / Chla_mad)
    ),
    Chla_mod_zscore_flag = case_when(
      is.na(Chla_mod_zscore) ~ FALSE,
      Chla_mod_zscore > 15 ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  ungroup()
# We won't remove these flagged outliers because all the higher values appeared
  # to be real based on best professional judgment

# Finish cleaning all raw data
raw_chla_1975_2021 <- df_all_c %>%
  # Add variables for adjusted calendar year, month, and season
  # Adjusted calendar year: December-November, with December of the previous
    # calendar year included with the following year
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
  # Assign SouthCentral Region to the Grant Line Canal and Old River subregion
  mutate(Region = if_else(SubRegion == "Grant Line Canal and Old River", "SouthCentral", Region)) %>%
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
    Chlorophyll,
    Chlorophyll_Sign
  ) %>%
  # Arrange data
  arrange(Date, Source, Station)


# 4. Save and Export Data -------------------------------------------------

# Save final data set of raw discrete chlorophyll-a concentrations as csv file
  # for easier diffing
raw_chla_1975_2021 %>%
  # Convert Datetime to character so that it isn't converted to UTC upon export
  mutate(Datetime = as.character(Datetime)) %>%
  write_csv(here("data-raw/Final/raw_chla_1975_2021.csv"))

# Save final data set of raw discrete chlorophyll-a concentrations as object in
  # the data package
usethis::use_data(raw_chla_1975_2021, overwrite = TRUE)

