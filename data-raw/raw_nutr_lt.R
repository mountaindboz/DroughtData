# Code to prepare data set of long-term raw nutrient data:
  # `raw_nutr_1975_2021` - raw values of long-term nutrient concentrations (DissAmmonia,
    # DissNitrateNitrite, and DissOrthophos) for 1975-2021 from the EMP, USGS_SFBS, and
    # USGS_CAWSC (only SR at Freeport) surveys

# Load packages
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readr)
library(dtplyr)
library(lubridate)
library(hms)
# Make sure we are using `discretewq` version 2.3.2.9000, commit dbb2638e800e637f3f2c70d47c9faad68f95db07
# install.packages("devtools")
# devtools::install_github("sbashevkin/discretewq", ref = "dbb2638e800e637f3f2c70d47c9faad68f95db07")
library(discretewq)
library(sf)
library(rlang)


# 1. Import Data ----------------------------------------------------------

# Import discrete nutrient data from the discretewq package
# Select EMP, USGS_SFBS, and USGS_CAWSC since these are the only surveys that
  # have collected discrete nutrient data
df_nutr <- wq(Sources = c("EMP", "USGS_SFBS", "USGS_CAWSC"), End_year = 2021)


# 2. Clean Data -----------------------------------------------

# Prepare nutrient data from the discretewq package
df_nutr_c <- df_nutr %>%
  transmute(
    Source,
    Station,
    Latitude,
    Longitude,
    # Convert Date variable to date object
    Date = date(Date),
    # Convert Datetime to PST
    Datetime = with_tz(Datetime, tzone = "Etc/GMT+8"),
    # The EMP data set has a few non-detect values without reporting limits -
      # we'll fill in 0.01 for the reporting limits for these values for now as
      # suggested by Sarah Perry. Also, we'll fill in "=" for the _Sign variables
      # for the USGS_SFBS data for now since they are all NA.
    DissAmmonia_Sign = case_when(
      is.na(DissAmmonia_Sign) ~ "=",
      DissAmmonia_Sign == "<" & is.na(DissAmmonia) ~ "< (estimated)",
      TRUE ~ DissAmmonia_Sign
    ),
    DissAmmonia = if_else(DissAmmonia_Sign == "< (estimated)", 0.01, DissAmmonia),
    DissNitrateNitrite_Sign = case_when(
      is.na(DissNitrateNitrite_Sign) ~ "=",
      DissNitrateNitrite_Sign == "<" & is.na(DissNitrateNitrite) ~ "< (estimated)",
      TRUE ~ DissNitrateNitrite_Sign
    ),
    DissNitrateNitrite = if_else(DissNitrateNitrite_Sign == "< (estimated)", 0.01, DissNitrateNitrite),
    DissOrthophos_Sign = case_when(
      is.na(DissOrthophos_Sign) ~ "=",
      DissOrthophos_Sign == "<" & is.na(DissOrthophos) ~ "< (estimated)",
      TRUE ~ DissOrthophos_Sign
    ),
    DissOrthophos = if_else(DissOrthophos_Sign == "< (estimated)", 0.01, DissOrthophos)
  ) %>%
  # Remove records with NA values for all nutrient parameters
  filter(!if_all(c(DissAmmonia, DissNitrateNitrite, DissOrthophos), is.na)) %>%
  # Remove one DissAmmonia value from the USGS_CAWSC survey that is equal to zero
  mutate(DissAmmonia = if_else(DissAmmonia == 0, NA_real_, DissAmmonia))

# For the USGS_SFBS survey, if at least one of the nutrient parameters has a
  # value reported, then we will assume that the other parameters were below the
  # reporting limit for that station and day. We'll use RL values provided by USGS
  # for 2006-present. We assumed these were constant throughout the entire
  # monitoring program including in years earlier than 2006.
df_nutr_sfbs_blw_rl <- df_nutr_c %>%
  filter(Source == "USGS_SFBS") %>%
  filter(if_any(c(DissAmmonia, DissNitrateNitrite, DissOrthophos), is.na)) %>%
  mutate(
    DissAmmonia_Sign = if_else(is.na(DissAmmonia), "< (estimated)", DissAmmonia_Sign),
    DissAmmonia = if_else(DissAmmonia_Sign == "< (estimated)", 0.0007, DissAmmonia),
    DissNitrateNitrite_Sign = if_else(is.na(DissNitrateNitrite), "< (estimated)", DissNitrateNitrite_Sign),
    DissNitrateNitrite = if_else(DissNitrateNitrite_Sign == "< (estimated)", 0.0007, DissNitrateNitrite),
    DissOrthophos_Sign = if_else(is.na(DissOrthophos), "< (estimated)", DissOrthophos_Sign),
    DissOrthophos = if_else(DissOrthophos_Sign == "< (estimated)", 0.0015, DissOrthophos)
  )

# Add back the USGS_SFBS data and continue preparing the nutrient data
df_nutr_c1 <- df_nutr_c %>%
  anti_join(df_nutr_sfbs_blw_rl, by = c("Source", "Station", "Datetime")) %>%
  bind_rows(df_nutr_sfbs_blw_rl) %>%
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

# Create a vector of nutrient parameter names
vars_nutr <- str_subset(names(df_nutr_c1), "^Diss.+[^_Sign]$")

# Create a nested data frame to run parameter-specific functions on
ndf_nutr <-
  tibble(
    Parameter = vars_nutr,
    df_data = rep(list(df_nutr_c1), 3)
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
ndf_nutr %>%
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

  # Add back fixed duplicates
  df %>%
    anti_join(df_dups, by = c("Source", "Station", "Date")) %>%
    bind_rows(df_dups_fixed)
}

# Filter daily duplicates for each parameter
ndf_nutr_c <- ndf_nutr %>% mutate(df_data = map2(df_data, Parameter, .f = filt_daily_dups))


# 3. Filter Data Temporally and Spatially ---------------------------------

# For the USGS_CAWSC survey, nutrients were only sampled on a long-term basis at
  # one station (11447650 - Sacramento River at Freeport), so we'll only include
  # data for this one station.
ndf_nutr_filt <- ndf_nutr_c %>%
  mutate(
    df_data_filt = map(
      df_data,
      ~ filter(.x, !(Source == "USGS_CAWSC" & Station != "USGS-11447650"))
    )
  )

# Not all of the subregions were sampled consistently from 1975-2021. To make
  # sure that we only include the subregions that were sampled adequately, we will
  # require that a subregion needs to have data for at least 75% of the 47 years
  # between 1975 to 2021 (35 years) for each season.
ndf_nutr_filt <- ndf_nutr_filt %>%
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

# Create function to flag the <RL values with high reporting limits (greater
  # than a specified percentile of the data)
flag_high_rl <- function(df, param, perc_thresh) {
  sign_sym <- sym(paste0(param, "_Sign"))
  sign_enquo <- enquo(sign_sym)

  threshold <- df %>%
    summarize(quant = quantile(.data[[param]], probs = perc_thresh)) %>%
    pull(quant)

  df %>%
    mutate(
      HighRL_flag = if_else(
        str_detect(!!sign_enquo, "^<") & .data[[param]] > threshold,
        TRUE,
        FALSE
      )
    )
}

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

ndf_nutr_filt2 <- ndf_nutr_filt %>%
  transmute(
    Parameter,
    # Flag the <RL values with high RL's (> 75th percentile) in the DWR_EMP data set
    df_data_flag_rl = map2(df_data_filt_seas, Parameter, .f = flag_high_rl, perc_thresh = 0.75),
    # Remove these flagged values
    df_data_filt = map(df_data_flag_rl, ~ filter(.x, !HighRL_flag) %>% select(-HighRL_flag)),
    # Flag data points that have modified z-scores greater than 15 grouped by subregion
    df_data_flag_mzc = map2(
      df_data_filt,
      Parameter,
      ~ group_by(.x, SubRegion) %>%
        flag_modzscore(.y, threshold = 15) %>%
        ungroup()
    ),
    # Remove the flagged outliers except for those in the DissAmmonia data set
      # since they appeared to be okay based on best professional judgment
    df_data_filt2 = if_else(
      Parameter != "DissAmmonia",
      modify_depth(df_data_flag_mzc, 1, ~ filter(.x, !ModZscore_flag)),
      df_data_flag_mzc
    ),
    df_data_filt2 = map(df_data_filt2, ~ select(.x, !starts_with("ModZscore")))
  )


# 5. Save and Export Data -------------------------------------------------

# Finish cleaning all raw data
raw_nutr_1975_2021 <-
  # Combine nutrient data back together
  reduce(ndf_nutr_filt2$df_data_filt2, full_join) %>%
  # Add region designations
  left_join(DroughtData:::df_regions %>% distinct(SubRegion, Region)) %>%
  relocate(Region, .before = SubRegion) %>%
  # Fill in "=" for the NA values in the _Sign variables
  mutate(across(ends_with("_Sign"), ~ if_else(is.na(.x), "=", .x))) %>%
  # Arrange data
  arrange(Date, Source, Station)

# Make sure there is only one sample per station-day for each parameter
for (var in vars_nutr) {
  print(var)
  raw_nutr_1975_2021 %>%
    filter(!is.na(.data[[var]])) %>%
    count(Source, Station, Date) %>%
    filter(n > 1) %>%
    print()
}
# No more duplicates now

# Save final data set of raw nutrient concentrations as csv file for easier diffing
raw_nutr_1975_2021 %>%
  # Convert Datetime to character so that it isn't converted to UTC upon export
  mutate(Datetime = as.character(Datetime)) %>%
  write_csv("data-raw/Final/raw_nutr_1975_2021.csv")

# Save final data set of raw nutrient concentrations as object in the data package
usethis::use_data(raw_nutr_1975_2021, overwrite = TRUE)

