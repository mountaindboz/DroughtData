# Code to prepare long-term (1975-2021) integrated data sets:
  # 1) `lt_seasonal` - Seasonal averages for each year for the entire Delta
  # 2) `lt_regional` - Regional averages for each year


# 1. Global Code ----------------------------------------------------------

# Load packages
library(tidyverse)
library(dtplyr)


# 2. Water Quality --------------------------------------------------------

# 2.1 Hydrology -----------------------------------------------------------

# Load raw hydrology data from 1975-2021
load("data/raw_hydro_1975_2021.rda")

# Calculate seasonal averages for each year
df_hydro_season <- raw_hydro_1975_2021 %>%
  group_by(YearAdj, Season) %>%
  summarize(across(c("Outflow", "Export", "X2"), mean, na.rm = TRUE)) %>%
  ungroup() %>%
  # Convert any NaN values to NA values
  mutate(across(c("Outflow", "Export", "X2"), ~if_else(is.nan(.x), NA_real_, .x)))

## WARNING!: Hutton et al. had missing X2 data and summarized seasonal X2 may be skewed as a result.

# 2.2 WQ Field measurements -----------------------------------------------

# Load raw WQ measurement data from 1975-2021
load("data/raw_wq_1975_2021.rda")

# Create function to summarize WQ field measurement data
WQindices <- function(df, variable, type = c("season", "region"), month.na = c("strict", "relaxed")) {
  # Argument checking
  type <- match.arg(type, c("season", "region"))
  month.na <- match.arg(month.na, c("strict", "relaxed"))

  # Calculate seasonal averages for each region
  vardata <- df %>%
    # Remove any rows with NAs in the variable to summarize
    filter(!is.na(.data[[variable]])) %>%
    # Use dtplyr to speed up operations
    lazy_dt() %>%
    # Calculate monthly mean for each region
    group_by(Month, Season, Region, YearAdj) %>%
    summarize(var_month_mean = mean(.data[[variable]])) %>%
    ungroup() %>%
    # End dtplyr operation
    as_tibble() %>%
    {if (month.na == "strict") {
      # Fill in NAs for variable for any missing Month, Region, YearAdj
        # combinations to make sure all months are represented in each season
      complete(., nesting(Month, Season), Region, YearAdj)
    } else {
      # Fill in NAs for variable for any missing Season, Region, YearAdj
        # combinations to make sure all seasons and regions are represented when
        # averaging
      complete(., Season, Region, YearAdj)
    }} %>%
    # Use dtplyr to speed up operations
    lazy_dt() %>%
    # Calculate seasonal mean for each region
    group_by(Season, Region, YearAdj) %>%
    summarize(var_mean = mean(var_month_mean)) %>%
    ungroup() %>%
    # End dtplyr operation
    as_tibble()

  # Calculate either the overall seasonal or regional averages
  out <- vardata %>%
    {if (type == "season") {
      # Group by season in order to calculate seasonal averages for entire Delta
      group_by(., Season, YearAdj)
    } else {
      # Group by region in order to calculate regional averages for each year
      group_by(., Region, YearAdj)
    }} %>%
    summarize(
      {{variable}} := mean(var_mean),
      .groups = "drop"
    )

  cat(paste("\nFinished", variable, "\n"))

  return(out)
}

# Define WQ variables to summarize
vars_wq <- c("Temperature", "Salinity", "Secchi")

# Seasonal averages for each year - relaxing the requirement of 3 months being present in all years
df_wq_season <-
  map2(
    rep(list(raw_wq_1975_2021), 3),
    vars_wq,
    .f = WQindices,
    type = "season",
    month.na = "relaxed"
  ) %>%
  reduce(left_join) %>%
  select(YearAdj, Season, Temperature, Salinity, Secchi) %>%
  arrange(YearAdj, Season)

# Yearly averages for each region - relaxing the requirement of 3 months being present in all years
df_wq_region <-
  map2(
    rep(list(raw_wq_1975_2021), 3),
    vars_wq,
    .f = WQindices,
    type = "region",
    month.na = "relaxed"
  ) %>%
  reduce(left_join) %>%
  select(YearAdj, Region, Temperature, Salinity, Secchi) %>%
  arrange(YearAdj, Region)

# 2.3 Nutrients -----------------------------------------------------------




# 3. Primary Producers ----------------------------------------------------




# 4. Lower Trophic --------------------------------------------------------




# 5. Fish -----------------------------------------------------------------




# 6. Integrate data sets --------------------------------------------------

# Create data frames that contain all possible combinations of year, season, and region
lt_yrs <- c(1975:2021)
df_yr <- tibble(Year = lt_yrs)
df_yr_season <- expand_grid(YearAdj = lt_yrs, Season = c("Winter", "Spring", "Summer", "Fall"))
df_yr_region <- expand_grid(YearAdj = lt_yrs, Region = unique(DroughtData:::df_regions$Region))

# Integrate data sets with seasonal averages for each year for the entire Delta
lst_seasonal <- lst(
  df_yr_season,
  DroughtData:::df_yr_type,
  df_hydro_season,
  df_wq_season
)

lt_seasonal <- reduce(lst_seasonal, left_join)

# Integrate data sets with regional averages for each year
lst_regional <- lst(
  df_yr_region,
  DroughtData:::df_yr_type,
  df_wq_region
)

lt_regional <- reduce(lst_regional, left_join)

# Save final long-term integrated data sets as csv files for easier diffing
write_csv(lt_seasonal, "data-raw/Final/lt_seasonal.csv")
write_csv(lt_regional, "data-raw/Final/lt_regional.csv")

# Save final long-term integrated data sets as objects in the data package
usethis::use_data(lt_seasonal, lt_regional, overwrite = TRUE)

