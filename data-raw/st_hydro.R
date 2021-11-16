# Code to prepare short-term (2011-2021) hydrology and LSZ data sets:
  # 1) `st_hydro_2021` - daily values for hydrology and LSZ metrics for Oct 2020 - Oct 2021
  # 1) `st_hydro_month` - monthly averages of hydrology and LSZ metrics for 2011-2021


# Load packages
library(tidyverse)
library(lubridate)

# 1. Import data ----------------------------------------------------------

# If it hasn't been done already, download and save local copies of the DAYFLOW data from
  # the CNRA data portal and Delta Outflow data (DTO) from CDEC since the DAYFLOW files may
  # change and the DTO data is provisional

# Set download to TRUE if need to download and save DAYFLOW and DTO data
download <- FALSE

# Download and save DAYFLOW and DTO data if necessary
if (download) {source("data-raw/download_hydro.R")}

# Define file path for raw hydrology data
fp_hydro <- "data-raw/Hydrology/"

# Import DAYFLOW data for 1997-2020
df_dayflow_1997_2020 <- read_csv(file.path(fp_hydro, "dayflow_1997-2020.csv"))

# Import Delta Outflow (DTO) from CDEC for WY 2021 until DAYFLOW data is available
df_dto_2021 <- read_csv(file.path(fp_hydro, "dto_2021.csv"))


# 2. Clean and Combine data -----------------------------------------------

# Prepare DAYFLOW and DTO data to be combined
df_dayflow_clean <- df_dayflow_1997_2020 %>%
  select(Date, Outflow = OUT, Export = EXPORTS, X2) %>%
  # convert date column to date
  mutate(Date = mdy(Date))

df_dto_clean <- df_dto_2021 %>%
  mutate(Date = date(DateTime)) %>%
  select(Date, Outflow = Value)

# Combine DAYFLOW and DTO data
df_hydro <- bind_rows(df_dayflow_clean, df_dto_clean)

# Calculate X2 for WY2021 based on DAYFLOW documentation:
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
for (i in which(df_hydro$Date == "2020-10-01"):which(df_hydro$Date == "2021-09-01")) {
  df_hydro$X2[i] = 10.16 + 0.945*df_hydro$X2[i-1] - 1.487*log10(df_hydro$Outflow[i])
}


# 3. Summarize and Export data --------------------------------------------

# Calculate monthly averages for 2011-2021 (using adjusted calendar year - December-November, with
  # December of the previous calendar year included with the following year)
st_hydro_month <- df_hydro %>%
  # Add month and adjusted calendar year variables
  mutate(
    Month = month(Date, label = TRUE, abbr = TRUE),
    Year = if_else(Month == "Dec", year(Date) + 1, year(Date))
  ) %>%
  group_by(Year, Month) %>%
  summarize(across(c("Outflow", "Export", "X2"), mean, na.rm = TRUE)) %>%
  ungroup() %>%
  # Convert NaN values to NA values
  mutate(across(c("Outflow", "Export", "X2"), ~if_else(is.nan(.x), NA_real_, .x))) %>%
  # Don't include Sept 2021 for X2 since data only available for 9/1/2021
  mutate(X2 = if_else(Year == 2021 & Month == "Sep", NA_real_, X2)) %>%
  # Only include 2011-2021
  filter(Year >= 2011) %>%
  # Rearrange factor order for month so that December is first to correspond with adjusted calendar year
  mutate(Month = fct_shift(Month, -1)) %>%
  # Arrange by Year and Month
  arrange(Year, Month) %>%
  # Convert Month to a character vector
  mutate(Month = as.character(Month))

# Prepare daily hydrology data for 2021
st_hydro_2021 <- df_hydro %>%
  # Only include Oct 2020 - Oct 2021 for daily hydrology data
  filter(Date >= "2020-10-01") %>%
  # Remove Export variable since it is all NA values
  select(-Export)

# Save final short-term hydrology and LSZ data sets as csv files for easier diffing
write_csv(st_hydro_2021, "data-raw/Final/st_hydro_2021.csv")
write_csv(st_hydro_month, "data-raw/Final/st_hydro_month.csv")

# Save final short-term hydrology and LSZ data sets as objects in the data package
usethis::use_data(st_hydro_2021, st_hydro_month, overwrite = TRUE)

