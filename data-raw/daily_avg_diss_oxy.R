## code to prepare `daily_avg_diss_oxy` dataset goes here

# Load packages
library(dplyr)
library(readr)
library(lubridate)
library(dataRetrieval)

# Set System Timezone as "Etc/GMT+8" (PST) to make it consistent with all data frames
Sys.setenv(TZ = "Etc/GMT+8")


# If it hasn't been done already, download and save local copies of the
# 15-minute continuous dissolved oxygen data collected by USGS at a few stations
# (DEC, TOE) in 2021 using the `dataRetrieval` package since some of the data is
# provisional and may change

# Set download to TRUE if need to download 2021 USGS continuous dissolved oxygen data
download <- FALSE

# Download 2021 USGS continuous dissolved data if necessary
if (download == TRUE) {
  # Create vectors for parameters, start and end dates
  start_date <- "2021-01-01"
  end_date <- "2021-12-31"
  params <- "00300"  # Dissolved Oxygen in mg/L

  # Download continuous dissolved oxygen data for sites in the Sacramento River
    # near Decker Island:
  # 11455478 - Sacramento River at Decker Island near Rio Vista CA (DEC)
  # 11455485 - Sacramento River below Toland Landing Near Rio Vista CA (TOL)
  site_numb_dec_tol <- c("11455478", "11455485")
  DEC_TOL <- readNWISuv(site_numb_dec_tol, params, start_date, end_date, tz = "Etc/GMT+8")

  # Export raw data as .csv files for each site
  lst(MDM, SJJ) %>%
    map(as_tibble) %>%
    map(~ mutate(.x, dateTime = as.character(dateTime))) %>%
    iwalk(
      .f = ~ write_csv(
        .x,
        file = here("data-raw/Cont_chla_data/", paste0(.y, "_Chlor_2020-2021.csv")),
        na = ""
      )
    )
}


usethis::use_data(daily_avg_diss_oxy, overwrite = TRUE)
