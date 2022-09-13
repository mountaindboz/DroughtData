# Code to prepare data set of long-term raw discrete chlorophyll-a data:
  # `raw_chla_1975_2021` - raw values of discrete chlorophyll-a concentrations for
    # 1975-2021 from the EMP, USGS_SFBS, and NCRO surveys

# Load packages
library(dplyr)
library(tidyr)
library(readr)
library(dtplyr)
library(lubridate)
library(hms)
# Make sure we are using `discretewq` version 2.3.2.9000, commit dbb2638e800e637f3f2c70d47c9faad68f95db07
# install.packages("devtools")
# devtools::install_github("sbashevkin/discretewq", ref = "dbb2638e800e637f3f2c70d47c9faad68f95db07")
library(discretewq)
library(sf)
library(here)

i_am("data-raw/raw_chla.R")


# 1. Import Data ----------------------------------------------------------

# Import discrete chlorophyll-a data from the discretewq package
# Select all surveys available that have at least 20 years of data. This
  # includes EMP, USGS_SFBS, and NCRO.
df_chla <- wq(Sources = c("EMP", "USGS_SFBS", "NCRO"), End_year = 2021)


# 2. Clean Data -----------------------------------------------

# Prepare discrete chlorophyll-a data from the discretewq package
df_chla_c <- df_chla %>%
  transmute(
    Source,
    Station,
    Latitude,
    Longitude,
    # Convert Date variable to date object
    Date = date(Date),
    # Convert Datetime to PST
    Datetime = with_tz(Datetime, tzone = "Etc/GMT+8"),
    # Fill in "=" for the NA values in Chlorophyll_Sign
    Chlorophyll_Sign = if_else(is.na(Chlorophyll_Sign), "=", Chlorophyll_Sign),
    Chlorophyll
  ) %>%
  # Remove records without Chlorophyll-a data
  drop_na(Chlorophyll) %>%
  # Remove records without latitude-longitude coordinates
  drop_na(Latitude, Longitude) %>%
  # Assign SubRegions to the stations
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = st_crs(DroughtData:::sf_delta)) %>%
  st_join(DroughtData:::sf_delta, join = st_intersects) %>%
  # Remove any data outside our subregions of interest
  filter(!is.na(SubRegion)) %>%
  st_drop_geometry() %>%
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
  filter(YearAdj %in% 1975:2021)

# Are there cases where multiple samples were collected at the same station in one day?
df_chla_c %>%
  count(Source, Station, Date) %>%
  filter(n > 1)
# YES- we need to filter data so that there is only one sample per station-day

# Create function to filter data so that there is only one sample per
  # station-day by choosing the data point closest to noon
filt_daily_dups <- function(df, param) {
  # Look for any instances when more than 1 data point was collected at a station-day
  df_dups <- df %>%
    count(Source, Station, Date) %>%
    filter(n > 1) %>%
    select(-n)

  # Fix duplicates
  df_dups_fixed <- df %>%
    inner_join(df_dups, by = c("Source", "Station", "Date")) %>%
    drop_na(Datetime) %>%
    mutate(
      # Create variable for time
      Time = as_hms(Datetime),
      # Calculate difference from noon for each data point for later filtering
      Noon_diff = abs(hms(hours = 12) - Time)
    ) %>%
    # Use dtplyr to speed up operations
    lazy_dt() %>%
    group_by(Station, Date) %>%
    # Select only 1 data point per station and date, choose data closest to noon
    filter(Noon_diff == min(Noon_diff)) %>%
    # When points are equidistant from noon, select earlier point
    filter(Time == min(Time)) %>%
    ungroup() %>%
    # End dtplyr operation
    as_tibble() %>%
    select(-c(Time, Noon_diff))

  # Add back fixed duplicates and format data frame
  df %>%
    anti_join(df_dups, by = c("Source", "Station", "Date")) %>%
    bind_rows(df_dups_fixed)
}

# Filter daily duplicates
df_chla_c2 <- filt_daily_dups(df_chla_c, "Chlorophyll")

# Make sure there is only one sample per station-day for each parameter
df_chla_c2 %>%
  count(Source, Station, Date) %>%
  filter(n > 1)
# No more duplicates now


# 3. Filter Data Temporally and Spatially ---------------------------------

# Not all of the subregions were sampled consistently from 1975-2021. To make
  # sure that we only include the subregions that were sampled adequately, we will
  # require that a subregion needs to have data for at least 75% of the 47 years
  # between 1975 to 2021 (35 years) for each season.
df_subreg_seas <- df_chla_c2 %>%
  distinct(SubRegion, YearAdj, Season) %>%
  count(SubRegion, Season, name = "NumYears") %>%
  group_by(SubRegion) %>%
  filter(min(NumYears) >= 35) %>%
  ungroup()

df_chla_filt_seas <- df_chla_c2 %>% filter(SubRegion %in% unique(df_subreg_seas$SubRegion))


# 4. Remove Outliers ------------------------------------------------------

# Create function to flag data points with modified z-scores greater than a
  # specified threshold
flag_modzscore <- function(df, param, threshold) {
  df %>%
    mutate(
      tmp_median = median(.data[[param]]),
      tmp_mad = mad(.data[[param]]),
      ModZscore = if_else(
        tmp_mad == 0,
        NA_real_,
        abs(0.6745 * (.data[[param]] - tmp_median) / tmp_mad)
      ),
      ModZscore_flag = case_when(
        is.na(ModZscore) ~ FALSE,
        ModZscore > threshold ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    select(!starts_with("tmp_"))
}

# Flag data points that have modified z-scores greater than 15 grouped by subregion
df_chla_flag_mzc <- df_chla_filt_seas %>%
  group_by(SubRegion) %>%
  flag_modzscore("Chlorophyll", threshold = 15) %>%
  ungroup()
# We won't remove these flagged outliers because all the higher values appeared
  # to be real based on best professional judgment


# 5. Save and Export Data -------------------------------------------------

# Finish cleaning all raw data
raw_chla_1975_2021 <- df_chla_filt_seas %>%
  left_join(DroughtData:::df_regions %>% distinct(SubRegion, Region)) %>%
  # Define variable order
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
    Chlorophyll_Sign,
    Chlorophyll
  ) %>%
  # Arrange data
  arrange(Date, Source, Station)

# Save final data set of raw discrete chlorophyll-a concentrations as csv file
  # for easier diffing
raw_chla_1975_2021 %>%
  # Convert Datetime to character so that it isn't converted to UTC upon export
  mutate(Datetime = as.character(Datetime)) %>%
  write_csv(here("data-raw/Final/raw_chla_1975_2021.csv"))

# Save final data set of raw discrete chlorophyll-a concentrations as object in
  # the data package
usethis::use_data(raw_chla_1975_2021, overwrite = TRUE)

