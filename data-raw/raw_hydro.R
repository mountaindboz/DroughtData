# Code to prepare data set of raw hydrology and LSZ metrics:
  # `raw_hydro_1975_2021` - raw daily values for hydrology and LSZ metrics (Delta Outflow,
    # Delta Exports, X2) for 1975-2021

# Load packages
library(tidyverse)
library(lubridate)
library(cder)
library(readxl)


# 1. Import Data ----------------------------------------------------------

# If it hasn't been done already, download and save local copies of the DAYFLOW data from
  # the CNRA data portal and Delta Outflow data (DTO) from CDEC since the DAYFLOW files may
  # change and the DTO data is provisional

# Set download to TRUE if need to download and save DAYFLOW and DTO data
download <- FALSE

# Download and save DAYFLOW and DTO data if necessary
if (download) {
  # Define directory for raw hydrology data
  dir_hydro <- "data-raw/Hydrology/"

  # Download DAYFLOW Data from CNRA portal: https://data.cnra.ca.gov/dataset/dayflow
  dayflow_1970_1983 <- read_csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/a0a46a1d-bec5-4db9-b331-655e306860ba/download/dayflow-results-1970-1983.csv")
  dayflow_1984_1996 <- read_csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/cb04e626-9729-4105-af81-f6e5a37f116a/download/dayflow-results-1984-1996.csv")
  dayflow_1997_2020 <- read_csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/21c377fe-53b8-4bd6-9e1f-2025221be095/download/dayflow-results-1997-2020.csv")

  # Download and save Delta Outflow (DTO) from CDEC for WY 2021 until DAYFLOW data is available
  dto_2021 <-
    cdec_query(
      stations = "DTO",
      sensors = 23,
      durations = "D",
      start.date = "2020-10-01",
      end.date = "2021-10-31"
    )

  # Save data as .csv files in the "data-raw/Hydrology" folder
  dayflow_1970_1983 %>% write_csv(file.path(dir_hydro, "dayflow_1970-1983.csv"))
  dayflow_1984_1996 %>% write_csv(file.path(dir_hydro, "dayflow_1984-1996.csv"))
  dayflow_1997_2020 %>% write_csv(file.path(dir_hydro, "dayflow_1997-2020.csv"))
  dto_2021 %>% write_csv(file.path(dir_hydro, "dto_2021.csv"))
}

# Define file path for raw hydrology data
fp_hydro <- "data-raw/Hydrology/"

# Import DAYFLOW data for 1970-2020
df_dayflow_1970_1983 <- read_csv(file.path(fp_hydro, "dayflow_1970-1983.csv"))
df_dayflow_1984_1996 <- read_csv(file.path(fp_hydro, "dayflow_1984-1996.csv"))
df_dayflow_1997_2020 <- read_csv(file.path(fp_hydro, "dayflow_1997-2020.csv"))

# Import Delta Outflow (DTO) from CDEC for WY 2021 until DAYFLOW data is available
df_dto_2021 <- read_csv(file.path(fp_hydro, "dto_2021.csv"))

# Import estimated X2 values for earlier years based on Hutton et al. paper
df_hutton_x2 <-
  read_excel(
    file.path(fp_hydro, "supplemental_data_wr.1943-5452.0000617_hutton3.xlsx"),
    sheet = "Daily"
  )


# 2. Clean and Combine Data -----------------------------------------------

# Rename "EXPORTS" to "EXPORT" in the 1997-2020 data to match other data sets
df_dayflow_1997_2020_c <- df_dayflow_1997_2020 %>% rename(EXPORT = EXPORTS)

# Combine DAYFLOW data and start with some basic cleaning
df_dayflow_v1 <-
  bind_rows(df_dayflow_1970_1983, df_dayflow_1984_1996, df_dayflow_1997_2020_c) %>%
  select(Date, Outflow = OUT, Export = EXPORT, X2) %>%
  # convert date column to date
  mutate(Date = mdy(Date))

# Prepare X2 data from Hutton et al. paper to be joined with DAYFLOW data
df_hutton_x2_c <- df_hutton_x2 %>%
  mutate(Date = as_date(Date)) %>%
  select(Date, X2Hutton = SacX2)

# Add X2 for earlier years based on Hutton et al. paper
df_dayflow_v2 <- df_dayflow_v1 %>%
  left_join(df_hutton_x2_c, by = "Date") %>%
  mutate(X2 = if_else(is.na(X2), X2Hutton, X2)) %>%
  select(-X2Hutton)

# Prepare DTO data to be combined with DAYFLOW data
df_dto_2021_c <- df_dto_2021 %>%
  mutate(Date = date(DateTime)) %>%
  select(Date, Outflow = Value)

# Add DTO data to the DAYFLOW data
df_dayflow_v3 <- bind_rows(df_dayflow_v2, df_dto_2021_c)

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

# Add a variable for adjusted calendar year and restrict data to 1975-2021
  # Adjusted calendar year: December-November, with December of the previous calendar year
  # included with the following year
raw_hydro_1975_2021 <- df_dayflow_v3 %>%
  mutate(YearAdj = if_else(month(Date) == 12, year(Date) + 1, year(Date))
  ) %>%
  # Restrict data to 1975-2021
  filter(YearAdj >= 1975) %>%
  relocate(YearAdj)


# 3. Save and Export Data -------------------------------------------------

# Save final raw hydrology and LSZ data set as csv file for easier diffing
write_csv(raw_hydro_1975_2021, "data-raw/Final/raw_hydro_1975_2021.csv")

# Save final raw hydrology and LSZ data set as object in the data package
usethis::use_data(raw_hydro_1975_2021, overwrite = TRUE)

