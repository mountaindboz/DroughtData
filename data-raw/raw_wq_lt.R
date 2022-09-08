# Code to prepare data set of long-term raw water quality measurements:
  # `raw_wq_1975_2021` - raw values for water quality parameters (water temperature,
    # salinity, secchi) for 1975-2021

# Load packages
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readr)
library(dtplyr)
library(lubridate)
library(hms)
# Make sure we are using `discretewq` version 2.3.2.9000, commit c910fa0f00504cb1120741ab6c4630518aba36b3
# install.packages("devtools")
# devtools::install_github("sbashevkin/discretewq", ref = "c910fa0f00504cb1120741ab6c4630518aba36b3")
library(discretewq)
library(sf)


# 1. Import Data ----------------------------------------------------------

# Import WQ field measurement data from the discretewq package
# Select all surveys available that have at least 20 years of data. This
  # excludes SLS, USBR, and EDSM.
df_dwq <-
  wq(
    Sources = c(
      "EMP",
      "STN",
      "FMWT",
      "DJFMP",
      "SDO",
      "SKT",
      "20mm",
      "Suisun",
      "Baystudy",
      "USGS_SFBS",
      "YBFMP",
      "USGS_CAWSC",
      "NCRO"
    ),
    End_year = 2021
  )


# 2. Clean Data -----------------------------------------------

# Prepare WQ field measurement data from the discretewq package
df_dwq_c <- df_dwq %>%
  transmute(
    Source,
    Station,
    Latitude,
    Longitude,
    Date = date(Date),
    # Convert Datetime to PST
    Datetime = with_tz(Datetime, tzone = "Etc/GMT+8"),
    Temperature,
    Salinity,
    Secchi
  ) %>%
  # Remove records with NA values for all WQ parameters
  filter(!if_all(c(Temperature, Salinity, Secchi), is.na)) %>%
  # Remove records without lat-long coordinates
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

# Create a vector of WQ parameter names
vars_wq <- c("Temperature", "Salinity", "Secchi")

# Create a nested data frame to run parameter-specific functions on
ndf_dwq <-
  tibble(
    Parameter = vars_wq,
    df_data = rep(list(df_dwq_c), 3)
  ) %>%
  mutate(
    df_data = map2(
      df_data,
      Parameter,
      ~ drop_na(.x, all_of(.y)) %>%
        select(
          Source,
          Station,
          Latitude,
          Longitude,
          SubRegion,
          YearAdj,
          Season,
          Month,
          Date,
          Datetime,
          contains(.y)
        )
    )
  )

# Are there cases where multiple samples were collected at the same station in one day?
ndf_dwq %>%
  transmute(
    df_count = map(
      df_data,
      ~ count(.x, Source, Station, Date) %>%
        filter(n > 1)
    )
  ) %>%
  pull(df_count)
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

# Filter daily duplicates for each parameter
ndf_dwq_c <- ndf_dwq %>% mutate(df_data = map2(df_data, Parameter, .f = filt_daily_dups))


# 3. Filter Data Temporally and Spatially ---------------------------------

# Some of the stations from the Suisun Marsh survey are located in small
  # backwater channels and dead-end sloughs which represent a much different
  # habitat than the sampling locations from the other surveys which tend to be in
  # larger, open water channel habitat. We’ll keep the stations located in Suisun,
  # Montezuma, and Nurse Sloughs from the Suisun Marsh survey, since they seem to
  # be in the larger channels in the area.

# In addition, there is only salinity data available from DJFMP for 2019-2021,
  # so we won’t include salinity data from this survey
ndf_dwq_filt <- ndf_dwq_c %>%
  mutate(
    df_data_filt = map(df_data, ~ filter(.x, !(Source == "Suisun" & !str_detect(Station, "^SU|^MZ|^NS")))),
    df_data_filt = if_else(
      Parameter == "Salinity",
      modify_depth(df_data_filt, 1, ~ filter(.x, Source != "DJFMP")),
      df_data_filt
    )
  )

# Not all of the subregions were sampled consistently from 1975-2021. To make
  # sure that we only include the subregions that were sampled adequately, we will
  # require that a subregion needs to have data for at least 75% of the 47 years
  # between 1975 to 2021 (35 years) for each season.
ndf_dwq_filt <- ndf_dwq_filt %>%
  mutate(
    df_subreg_seas = map(
      df_data_filt,
      ~ distinct(.x, SubRegion, YearAdj, Season) %>%
        count(SubRegion, Season, name = "NumYears") %>%
        group_by(SubRegion) %>%
        filter(min(NumYears) >= 35) %>%
        ungroup()
    ),
    df_data_filt_seas = map2(
      df_data_filt, df_subreg_seas,
      ~ filter(.x, SubRegion %in% unique(.y$SubRegion))
    )
  )


# 4. Remove Outliers ------------------------------------------------------

# Create function to flag data points with Z-scores greater than a specified
  # threshold
flag_zscore <- function(df, param, threshold) {
  df %>%
    mutate(
      tmp_mean = mean(.data[[param]]),
      tmp_sd = sd(.data[[param]]),
      Zscore = if_else(
        tmp_sd == 0,
        NA_real_,
        abs((.data[[param]] - tmp_mean) / tmp_sd)
      ),
      Zscore_flag = case_when(
        is.na(Zscore) ~ FALSE,
        Zscore > threshold ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    select(!starts_with("tmp_"))
}

# Remove data points that are more than 15 SDs away from the mean of each subregion
ndf_dwq_filt2 <- ndf_dwq_filt %>%
  transmute(
    Parameter,
    df_data_flag = map2(
      df_data_filt_seas,
      Parameter,
      ~ group_by(.x, SubRegion) %>%
        flag_zscore(.y, threshold = 15) %>%
        ungroup()
    ),
    df_data_filt = map(
      df_data_flag,
      ~ filter(.x, !Zscore_flag) %>%
        select(!starts_with("Zscore"))
    )
  )


# 5. Save and Export Data -------------------------------------------------

# Finish cleaning all raw data
raw_wq_1975_2021 <-
  # Combine WQ data back together
  reduce(ndf_dwq_filt2$df_data_filt, full_join) %>%
  # Add region designations
  left_join(DroughtData:::df_regions %>% distinct(SubRegion, Region)) %>%
  relocate(Region, .before = SubRegion) %>%
  # Arrange data
  arrange(Date, Source, Station)

# Make sure there is only one sample per station-day for each parameter
for (var in vars_wq) {
  print(var)
  raw_wq_1975_2021 %>%
    filter(!is.na(.data[[var]])) %>%
    count(Source, Station, Date) %>%
    filter(n > 1) %>%
    print()
}
# No more duplicates now

# Save final data set of raw water quality measurements as csv file for easier diffing
write_csv(raw_wq_1975_2021, "data-raw/Final/raw_wq_1975_2021.csv")

# Save final data set of raw water quality measurements as object in the data package
usethis::use_data(raw_wq_1975_2021, overwrite = TRUE)

