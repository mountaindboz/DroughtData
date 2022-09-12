# Code to prepare long-term (1975-2021) averaged data sets:
  # 1) `lt_avg_hydro` - Seasonal averages for each year for 3 hydrology metrics
    # (Outflow, Exports, X2)
  # 2) `lt_avg_wq` - Seasonal-regional averages for each year for 3 water
    # quality parameters (Water Temperature, Salinity, Secchi Depth)


# 1. Global Code ----------------------------------------------------------

# Load packages
library(dplyr)
library(tidyr)
library(purrr)
library(readr)

# Create data frames that contain all possible combinations of year, season, and region
lt_yrs <- c(1975:2021)
seasons <- c("Winter", "Spring", "Summer", "Fall")
df_yr_season <- expand_grid(YearAdj = lt_yrs, Season = seasons)
df_yr_seas_reg <- expand_grid(
  YearAdj = lt_yrs,
  Season = seasons,
  Region = unique(DroughtData:::df_regions$Region)
)


# 2. Hydrology -----------------------------------------------------------

# Calculate seasonal averages for each year
df_hydro_season <- DroughtData::raw_hydro_1975_2021 %>%
  group_by(YearAdj, Season) %>%
  summarize(across(c("Outflow", "Export", "X2"), mean, na.rm = TRUE)) %>%
  ungroup()

# Make sure each Year-Season combination is represented and add Year Type info
lt_avg_hydro <- reduce(
  list(
    df_yr_season,
    DroughtData:::df_yr_type,
    df_hydro_season
  ),
  left_join
)

## WARNING!: Hutton et al. had missing X2 data and summarized seasonal X2 may be
  # skewed as a result.


# 3. WQ Field measurements -----------------------------------------------

# Define WQ variables to summarize
vars_wq <- c("Temperature", "Salinity", "Secchi")

# Seasonal-regional averages for each year - relaxing the requirement of 3
  # months being present in all years
df_wq_seas_reg <-
  map2(
    rep(list(DroughtData::raw_wq_1975_2021), 3),
    vars_wq,
    .f = DroughtData::drt_avg_data,
    avg_type = "both",
    month_na = "relaxed",
    .quote = TRUE
  ) %>%
  reduce(full_join)

# Make sure each Year-Season-Region combination is represented and add Year Type info
lt_avg_wq <- reduce(
  list(
    df_yr_seas_reg,
    DroughtData:::df_yr_type,
    df_wq_seas_reg
  ),
  left_join
)


# 4. Save and Export Data --------------------------------------------------

# Save final long-term averaged data sets as csv files for easier diffing
write_csv(lt_avg_hydro, "data-raw/Final/lt_avg_hydro.csv")
write_csv(lt_avg_wq, "data-raw/Final/lt_avg_wq.csv")

# Save final long-term averaged data sets as objects in the data package
usethis::use_data(lt_avg_hydro, lt_avg_wq, overwrite = TRUE)

