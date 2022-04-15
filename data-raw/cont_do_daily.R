# Code to prepare data set of continuous dissolved oxygen data:
  # `cont_do_daily` - daily average dissolved oxygen values for 2014-2021
    # from Sacramento River near Decker Island and the Yolo Bypass Toe Drain

# Load packages
library(dplyr)
library(readr)
library(lubridate)
library(purrr)
library(stringr)
library(tidyr)
library(dataRetrieval)
library(here)

# Check if we are in the correct working directory
i_am("data-raw/daily_avg_diss_oxy.R")

# Set System Timezone as "Etc/GMT+8" (PST) to make it consistent with all data frames
Sys.setenv(TZ = "Etc/GMT+8")


# 1. Import Data ----------------------------------------------------------

# Define parameter codes and site numbers for data downloads:
# 00300 = Dissolved Oxygen in mg/L
params <- "00300"
# Two sites located on the Sacramento River near Decker Island:
# 11455478 - Sacramento River at Decker Island near Rio Vista CA (DEC)
# 11455485 - Sacramento River below Toland Landing Near Rio Vista CA (TOL)
site_numb_dec_tol <- c("11455478", "11455485")
# Two sites located on the Yolo Bypass Toe Drain:
# 11455139 - Toe Drain at Mallard Road near Courtland CA
# 11455140 - Toe Drain at Liberty Island near Courtland CA
site_numb_toe <- c("11455139", "11455140")

# If it hasn't been done already, download and save local copies of the
  # 15-minute continuous dissolved oxygen data collected by USGS at a few stations
  # (DEC, TOE) in 2021 using the `dataRetrieval` package since some of the data is
  # provisional and may change

# Set download to TRUE if need to download 2021 USGS continuous dissolved oxygen data
download <- FALSE

# Download 2021 USGS continuous dissolved data if necessary
if (download == TRUE) {
  # Create vectors for start and end dates
  start_date <- "2021-01-01"
  end_date <- "2021-12-31"

  # Download continuous dissolved oxygen data for all sites
  DEC_TOL <- readNWISuv(site_numb_dec_tol, params, start_date, end_date, tz = "Etc/GMT+8")
  TOE <- readNWISuv(site_numb_toe, params, start_date, end_date, tz = "Etc/GMT+8")

  # Export raw data as .csv files for each location
  lst(DEC_TOL, TOE) %>%
    map(as_tibble) %>%
    map(~ mutate(.x, dateTime = as.character(dateTime))) %>%
    iwalk(
      .f = ~ write_csv(
        .x,
        file = here("data-raw/Cont_Diss_Oxygen/", paste0(.y, "_DO_2021.csv")),
        na = ""
      )
    )

  # Clean up
  rm(start_date, end_date, DEC_TOL, TOE)
}

# Create a vector of file paths for the 2021 continuous dissolved oxygen data
fp_do_2021 <- sort(dir(here("data-raw/Cont_Diss_Oxygen"), pattern = "\\.csv$", full.names = TRUE))

# Import 2021 continuous dissolved oxygen data into a nested dataframe
ndf_do_2021 <-
  tibble(
    Station = map_chr(fp_do_2021, ~ str_extract(.x, "(?<=Cont_Diss_Oxygen/)[:upper:]{3}")),
    df_data = map(fp_do_2021, read_csv)
  )

# Download 15-minute continuous dissolved oxygen data collected by USGS at a few
  # stations (DEC, TOE) in 2014-2020 using the `dataRetrieval` package. All of
  # this data is approved and shouldn't change through time.

# Create vectors for start and end dates
start_date <- "2014-01-01"
end_date <- "2020-12-31"

# Download 2014-2020 continuous dissolved oxygen data for all sites
df_do_dec_tol <- readNWISuv(site_numb_dec_tol, params, start_date, end_date, tz = "Etc/GMT+8")
df_do_toe <- readNWISuv(site_numb_toe, params, start_date, end_date, tz = "Etc/GMT+8")


# 2. Clean and Integrate Data ---------------------------------------------

# Add 2014-2020 continuous dissolved oxygen data to the 2021 data
df_do_comb <- ndf_do_2021 %>%
  # Convert site_no to character and tz of dateTime to PST so that all data
    # frames are compatible
  mutate(
    df_data = map(
      df_data,
      ~ mutate(
        .x,
        site_no = as.character(site_no),
        dateTime = force_tz(dateTime, tzone = "Etc/GMT+8")
      )
    )
  ) %>%
  add_row(
    Station = c("DEC", "TOE"),
    df_data = list(df_do_dec_tol, df_do_toe)
  ) %>%
  unnest(df_data) %>%
  rename(DO = X_00300_00000)

# Run a few quality checks on the 15-minute data before calculating daily averages

# Look for duplicated time stamps
# df_do_comb %>%
#   mutate(dateTime = round_date(dateTime, unit = "15 minute")) %>%
#   count(Station, dateTime) %>%
#   filter(n > 1)
# No duplicated time stamps present in data set

# Look at min and max values for each station
qc_min_max <- df_do_comb %>%
  mutate(Year = year(dateTime)) %>%
  group_by(Station, Year) %>%
  summarize(
    min_value = min(DO),
    max_value = max(DO)
  ) %>%
  ungroup()

# View(qc_min_max)
# There were some dissolved oxygen values less than 1 in 2019 and 2021. Upon
  # further inspection, they seem to be true low DO events, so we will keep these
  # values in the data set.


# 3. Aggregate Values -----------------------------------------------------

# Calculate daily means of continuous dissolved oxygen data for each station
cont_do_daily <- df_do_comb %>%
  mutate(Date = date(dateTime)) %>%
  group_by(Station, Date) %>%
  summarize(AvgDO = mean(DO)) %>%
  ungroup() %>%
  # Add a variable for year
  mutate(Year = year(Date)) %>%
  # Reorder columns
  relocate(Year, .before = Date) %>%
  # Arrange data
  arrange(Date, Station)

# Save final data set containing continuous dissolved oxygen data as csv file
  # for easier diffing
cont_do_daily %>% write_csv(here("data-raw/Final/cont_do_daily.csv"))

# Save final data set containing continuous dissolved oxygen data as object in
  # the data package
usethis::use_data(cont_do_daily, overwrite = TRUE)

