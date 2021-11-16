# Code to prepare long-term (1975-2021) integrated data sets:
  # 1) `lt_seasonal` - Seasonal averages for each year for the entire Delta
  # 2) `lt_regional` - Regional averages for each year


# 1. Global Code ----------------------------------------------------------

# Load packages
library(tidyverse)
library(lubridate)
library(hms)
library(readxl)
library(magrittr)
library(deltamapr)
library(sf)

# Import region assignments
df_regions <- read_csv("data-raw/Rosies_regions.csv")

## Load Delta regions shapefile from Brian
sf_delta <- R_EDSM_Subregions_Mahardja_FLOAT %>%
  # Filter to regions of interest
  filter(SubRegion %in% unique(df_regions$SubRegion)) %>%
  select(SubRegion)


# 2. Water Quality --------------------------------------------------------

# 2.1 Hydrology -----------------------------------------------------------

# If it hasn't been done already, download and save local copies of the DAYFLOW data from
  # the CNRA data portal and Delta Outflow data (DTO) from CDEC since the DAYFLOW files may
  # change and the DTO data is provisional

# Set download to TRUE if need to download and save DAYFLOW and DTO data
download <- FALSE

# Download and save DAYFLOW and DTO data if necessary
if (download) {source("data-raw/download_hydro.R")}

# Define file path for raw hydrology data
fp_hydro <- "data-raw/Hydrology/"

# Import DAYFLOW data for 1970-2020
df_dayflow_1970_1983 <- read_csv(file.path(fp_hydro, "dayflow_1970-1983.csv"))
df_dayflow_1984_1996 <- read_csv(file.path(fp_hydro, "dayflow_1984-1996.csv"))
df_dayflow_1997_2020 <- read_csv(file.path(fp_hydro, "dayflow_1997-2020.csv"))

# Rename "EXPORTS" to "EXPORT" in the 1997-2020 data to match other data sets
df_dayflow_1997_2020 %<>% rename(EXPORT = EXPORTS)

# Combine DAYFLOW data and start with some basic cleaning
df_dayflow_v1 <-
  bind_rows(df_dayflow_1970_1983, df_dayflow_1984_1996, df_dayflow_1997_2020) %>%
  select(Date, Outflow = OUT, Export = EXPORT, X2) %>%
  # convert date column to date
  mutate(Date = mdy(Date))

# Add X2 for earlier years based on Hutton et al. paper
hutton_x2 <-
  read_excel(
    file.path(fp_hydro, "supplemental_data_wr.1943-5452.0000617_hutton3.xlsx"),
    sheet = "Daily"
  ) %>%
  mutate(Date = as_date(Date)) %>%
  select(Date, X2Hutton = SacX2)

df_dayflow_v2 <- df_dayflow_v1 %>%
  left_join(hutton_x2, by = "Date") %>%
  mutate(X2 = if_else(is.na(X2), X2Hutton, X2)) %>%
  select(-X2Hutton)

# Import Delta Outflow (DTO) from CDEC for WY 2021 until DAYFLOW data is available
df_dto_2021 <- read_csv(file.path(fp_hydro, "dto_2021.csv")) %>%
  mutate(Date = date(DateTime)) %>%
  select(Date, Outflow = Value)

# Add DTO data to the dayflow data
df_dayflow_v3 <- bind_rows(df_dayflow_v2, df_dto_2021)

# Calculate X2 for WY 2021 based on DAYFLOW documentation:
  # The 1994 Bay-Delta agreement established standards for salinity in the estuary.
  # Specifically, the standards determine the degree to which salinity is allowed
  # to penetrate up-estuary, with salinity to be controlled through delta outflow.
  # The basis for the standards is a series of relationships between the salinity
  # pattern and the abundance or survival of various species of fish and
  # invertebrates. These relationships have been expressed in terms of X2,
  # the distance from the Golden Gate to the point where daily average salinity is
  # 2 parts per thousand at 1 meter off the bottom (Jassby et. al. 1995).
  # In Dayflow, X2 is estimated using the Autoregressive Lag Model:

  # X2(t) = 10.16 + 0.945*X2(t-1) - 1.487log(QOUT(t))
  # NOTE: It seems like the log in the DAYFLOW notation is referring to Log10 (i.e., not ln)

# >>>>> On 9/2/2021 DTO outflow was a negative number which won't work in the equation to
  # calculate X2 (the log of a negative number is not defined); therefore, we will only use
  # the equation to fill in X2 from 10/1/2020 - 9/1/2021. We will need to wait until the
  # DAYFLOW program publishes their WY 2021 results to have X2 values past this point.

# Fill in X2 data for most current WY data (from 10/1/2020 to 9/1/2021)
for (i in which(df_dayflow_v3$Date == "2020-10-01"):which(df_dayflow_v3$Date == "2021-09-01")) {
  df_dayflow_v3$X2[i] = 10.16 + 0.945*df_dayflow_v3$X2[i-1] - 1.487*log10(df_dayflow_v3$Outflow[i])
}

# Summarize per Rosemary's instructions:
# Adjusted calendar year, December-November, with December of the previous calendar year included
# with the following year (so December of 2019 is the first month of “2020” in our data set)

df_dayflow_f <- df_dayflow_v3 %>%
  mutate(
    Month = month(Date),
    Year = if_else(Month == 12, year(Date) + 1, year(Date)),
    # Seasons: Winter (Dec-Feb), Spring (Mar-May), Summer (Jun-Aug) or fall (Sept-Nov)
    Season = case_when(
      Month %in% c(12,1,2) ~ "Winter",
      Month %in% c(3:5) ~ "Spring",
      Month %in% c(6:8) ~ "Summer",
      Month %in% c(9:11) ~ "Fall"
    )
  ) %>%
  # Only include years after 1975
  filter(Year >= 1975) %>%
  # Calculate seasonal averages
  group_by(Year, Season) %>%
  summarize(across(c("Outflow", "Export", "X2"), mean, na.rm = TRUE)) %>%
  ungroup() %>%
  # Convert NaN values to NA values
  mutate(across(c("Outflow", "Export", "X2"), ~if_else(is.nan(.x), NA_real_, .x))) %>%
  # Don't include Fall 2021 for X2 since data only available for 9/1/2021
  mutate(X2 = if_else(Year == 2021 & Season == "Fall", NA_real_, X2))

## WARNING!: Hutton et al. had missing X2 data and summarized seasonal X2 may be skewed as a result.

# Clean up
rm(
  df_dayflow_1970_1983,
  df_dayflow_1984_1996,
  df_dayflow_1997_2020,
  df_dayflow_v1,
  df_dayflow_v2,
  df_dayflow_v3,
  df_dto_2021,
  hutton_x2,
  download,
  fp_hydro,
  i
)

# 2.2 WQ Field measurements -----------------------------------------------

# Load specific packages
# devtools::install_github("sbashevkin/discretewq", ref = "v2.0.0") # make sure we are using v2.0.0
library(discretewq) # water quality data https://github.com/sbashevkin/discretewq
library(rlang)
library(dtplyr)

# Import WQ field measurement data from the discretewq package
# Select all long-term surveys (excluding EDSM and the USBR Sacramento ship channel study)
df_wq <-
  wq(
    End_year = 2021,
    Sources = c("EMP", "STN", "FMWT", "DJFMP", "SKT", "20mm", "Suisun", "Baystudy", "USGS")
  )

# Create function to summarize WQ field measurement data
WQindices <- function(variable, type = "season", month.na = "strict") {

  if(!month.na %in% c("strict", "relaxed")) {
    stop("month.na must be either 'strict' or 'relaxed'")
  }

  if(!type %in% c("season", "year")) {
    stop("type must be either 'season' or 'year'")
  }

  vardata <- df_wq %>%
    # Remove any rows with NAs in our key variables
    filter(!is.na(.data[[variable]]) & !is.na(Latitude) & !is.na(Datetime) & !is.na(Longitude) & !is.na(Date)) %>%
    mutate(
      # Convert to a timezone without daylight savings time
      Datetime = with_tz(Datetime, tzone = "America/Phoenix"),
      Date = with_tz(Date, tzone = "America/Phoenix"),
      Station = paste(Source, Station),
      # Create variable for time-of-day, not date
      Time = as_hms(Datetime),
      # Calculate difference from noon for each data point for later filtering
      Noon_diff = abs(hms(hours = 12) - Time)
    ) %>%
    # Use dtplyr to speed up operations
    lazy_dt() %>%
    group_by(Station, Source, Date) %>%
    # Select only 1 data point per station and date, choose data closest to noon
    filter(Noon_diff == min(Noon_diff)) %>%
    # When points are equidistant from noon, select earlier point
    filter(Time == min(Time))%>%
    ungroup() %>%
    # End dtplyr operation
    as_tibble() %>%
    # Convert to sf object
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
    # Change to crs of sf_delta
    st_transform(crs = st_crs(sf_delta)) %>%
    # Add subregions
    st_join(sf_delta, join = st_intersects) %>%
    # Remove any data outside our subregions of interest
    filter(!is.na(SubRegion)) %>%
    # Drop sf geometry column since it's no longer needed
    st_drop_geometry() %>%
    # Use dtplyr to speed up operations
    lazy_dt() %>%
    group_by(SubRegion) %>%
    # Calculate variable SD for each subregion, then the number of SD's each data point exceeds the mean
    mutate(
      var_sd = sd(.data[[variable]]),
      var_ext = (.data[[variable]] - mean(.data[[variable]]))/var_sd
    ) %>%
    ungroup() %>%
    # End dtplyr operation
    as_tibble() %>%
    # Filter out any data points that are more than 10 SDs away from the mean of each subregion
    filter(var_ext < 10) %>%
    mutate(
      # Move Decembers to the following year
      Year = if_else(Month == 12, Year + 1, Year),
      # Create seasonal variables
      Season = case_when(
        Month %in% 3:5 ~ "Spring",
        Month %in% 6:8 ~ "Summer",
        Month %in% 9:11 ~ "Fall",
        Month %in% c(12, 1, 2) ~ "Winter",
        TRUE ~ NA_character_
      )
    ) %>%
    # Add region designations
    left_join(df_regions, by = c("Season", "SubRegion")) %>%
    # Remove any data not chosen for the long-term analysis
    filter(Long_term) %>%
    # Use dtplyr to speed up operations
    lazy_dt() %>%
    group_by(Month, Season, Region, Year) %>%
    # Calculate monthly mean for each region and sample size
    summarize(
      var_month_mean = mean(.data[[variable]]),
      N = n()
    ) %>%
    ungroup() %>%
    # End dtplyr operation
    as_tibble() %>%
    {if (month.na == "strict") {
      # Fill in NAs for variable (and 0 for N) for any missing month, Region, year combinations to make sure all months are represented in each season
      complete(., nesting(Month, Season), Region, Year, fill = list(N = 0))
    } else {
      # Fill in NAs for variable (and 0 for N) for any missing season, Region, year combinations to make sure all seasons and regions are represented when averaging
      complete(., Season, Region, Year, fill = list(N = 0))
    }} %>%
    # Use dtplyr to speed up operations
    lazy_dt() %>%
    group_by(Season, Region, Year) %>%
    # Calculate seasonal mean variable for each region and total seasonal sample size
    summarize(
      var_mean = mean(var_month_mean),
      N = sum(N)
    ) %>%
    ungroup() %>%
    # End dtplyr operation
    as_tibble()


  variable2 <- sym(variable)

  out <- vardata %>%
    filter(Year %in% 1975:2021) %>%
    {if (type == "season") {
      # Group by season in order to calculate seasonal averages for entire Delta
      group_by(., Season, Year)
    } else {
      # Group by region in order to calculate regional averages for each year
      group_by(., Region, Year)
    }} %>%
    summarize(
      {{variable}} := mean(var_mean),
      "N_{{variable2}}" := sum(N),
      .groups = "drop"
    )

  cat(paste("\nFinished", variable, "\n"))

  return(out)
}

# Define WQ variables to summarize
vars <- c("Temperature", "Secchi", "Salinity")

# Seasonal averages for each year - relaxing the requirement of 3 months being present in all years
lst_wq_season <- map(vars, WQindices, month.na = "relaxed", type = "season")

df_wq_season_f <- reduce(lst_wq_season, left_join) %>%
  select(Year, Season, Temperature, Secchi, Salinity) %>%
  arrange(Year, Season)

# Yearly averages for each region - relaxing the requirement of 3 months being present in all years
lst_wq_region <- map(vars, WQindices, month.na = "relaxed", type = "year")

df_wq_region_f <- reduce(lst_wq_region, left_join) %>%
  select(Year, Region, Temperature, Secchi, Salinity) %>%
  arrange(Year, Region)

# Clean up
rm(df_wq, lst_wq_season, lst_wq_region, vars, WQindices)


# 3. Primary Producers ----------------------------------------------------




# 4. Lower Trophic --------------------------------------------------------




# 5. Fish -----------------------------------------------------------------




# 6. Integrate data sets --------------------------------------------------

# Create data frames that contain all possible combinations of year, season, and region
lt_yrs <- c(1975:2021)
df_yr <- tibble(Year = lt_yrs)
df_yr_season <- expand_grid(Year = lt_yrs, Season = c("Winter", "Spring", "Summer", "Fall"))
df_yr_region <- expand_grid(Year = lt_yrs, Region = unique(df_regions$Region))

# Import year assignments
df_yr_type <- read_csv("data-raw/Year_assignments.csv")

# Integrate data sets with seasonal averages for each year for the entire Delta
lst_seasonal <- lst(
  df_yr_season,
  df_yr_type,
  df_dayflow_f,
  df_wq_season_f
)

lt_seasonal <- reduce(lst_seasonal, left_join)

# Integrate data sets with regional averages for each year
lst_regional <- lst(
  df_yr_region,
  df_yr_type,
  df_wq_region_f
)

lt_regional <- reduce(lst_regional, left_join)

# Save final long-term integrated data sets as csv files for easier diffing
write_csv(lt_seasonal, "data-raw/Final/lt_seasonal.csv")
write_csv(lt_regional, "data-raw/Final/lt_regional.csv")

# Save final long-term integrated data sets as objects in the data package
usethis::use_data(lt_seasonal, lt_regional, overwrite = TRUE)

