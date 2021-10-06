# Code to prepare long-term (1975-2021) integrated data sets:
  # 1) Seasonal averages for each year for the entire Delta
  # 2) Regional averages for each year

library(tidyverse)
library(lubridate)
library(readxl)
library(magrittr)


# 1. Water Quality --------------------------------------------------------

# 1.1 Hydrology -----------------------------------------------------------

# Define file path for raw hydrology data
fp_hydro <- "data-raw/Hydrology/"

# Import DAYFLOW Data: https://data.cnra.ca.gov/dataset/dayflow
df_dayflow_1970_1983 <- read_csv(file.path(fp_hydro, "dayflow-results-1970-1983.csv"))
df_dayflow_1984_1996 <- read_csv(file.path(fp_hydro, "dayflow-results-1984-1996.csv"))
df_dayflow_1997_2020 <- read_csv(file.path(fp_hydro, "dayflow-results-1997-2020.csv"))

# Rename "EXPORTS" to "EXPORT" in the 1997-2020 data to match other data sets
df_dayflow_1997_2020 %<>% rename(EXPORT = EXPORTS)

# Combine DAYFLOW data and start with some basic cleaning
df_dayflow_v1 <-
  bind_rows(df_dayflow_1970_1983, df_dayflow_1984_1996, df_dayflow_1997_2020) %>%
  select(Date, OUT, EXPORT, X2) %>%
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

# Add future outflow (DTO) and X2 until DAYFLOW is available: https://cdec.water.ca.gov/dynamicapp/wsSensorData
dto_2021 <- read_csv(file.path(fp_hydro, "DTO_23.csv")) %>%
  mutate(Date = ymd(`OBS DATE`)) %>%
  select(Date, OUT = VALUE)

# Add DTO data to the dayflow data
df_dayflow_v3 <- bind_rows(df_dayflow_v2, dto_2021)

##################### Calculate future X2 based on DAYFLOW documentation:
###
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

# >>>>> On 7/22/2021 DTO outflow was a negative number which won't work in the equation to
# calculate X2 (the log of a negative number is not defined); therefore, we will only use
# the equation to fill in X2 from 10/1/2020 - 7/21/2021. We will need to wait until the
# DAYFLOW program publishes their WY 2021 results to have X2 values past this point.

# Fill in X2 data for most current WY data (from 10/1/2020 to 7/21/2021)
for (i in which(df_dayflow_v3$Date == "2020-10-01"):which(df_dayflow_v3$Date == "2021-07-21")) {
  df_dayflow_v3$X2[i] = 10.16 + 0.945*df_dayflow_v3$X2[i-1] - 1.487*log10(df_dayflow_v3$OUT[i])
}

##################### Summarize per Rosemary's instructions
# Adjusted calendar year, December-November, with December of the previous calendar year included
# with the following year (so December of 2019 is the first month of “2020” in our data set)

df_dayflow_f <- df_dayflow_v3 %>%
  mutate(
    Month = month(Date),
    Year_adjusted = if_else(Month == 12, year(Date) + 1, year(Date)),
    # Seasons: Winter (Dec-Feb), Spring (Mar-May), Summer (Jun-Aug) or fall (Sept-Nov)
    Season = case_when(
      Month %in% c(12,1,2) ~ "Winter",
      Month %in% c(3:5) ~ "Spring",
      Month %in% c(6:8) ~ "Summer",
      Month %in% c(9:11) ~ "Fall"
    )
  ) %>%
  # Only include years after 1975
  filter(Year_adjusted >= 1975) %>%
  # Calculate seasonal averages
  group_by(Year_adjusted, Season) %>%
  summarize(
    Outflow = mean(OUT, na.rm = TRUE),
    X2 = mean(X2, na.rm = TRUE),
    Export = mean(EXPORT, na.rm = TRUE)
  ) %>%
  ungroup()

## WARNING!: Hutton et al. had missing X2 data and summarized seasonal X2 may be skewed as a result.

# Clean up
rm(
  df_dayflow_1970_1983,
  df_dayflow_1984_1996,
  df_dayflow_1997_2020,
  df_dayflow_v1,
  df_dayflow_v2,
  df_dayflow_v3,
  dto_2021,
  hutton_x2,
  fp_hydro,
  i
)

# 2. Primary Producers ----------------------------------------------------




# 3. Lower Trophic --------------------------------------------------------




# 4. Fish -----------------------------------------------------------------




#usethis::use_data(lt_integrated, overwrite = TRUE)
