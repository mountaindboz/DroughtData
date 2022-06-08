# Code to prepare data set of raw hydrology and LSZ metrics:
  # `raw_hydro_1975_2021` - raw daily values for hydrology and LSZ metrics
    # (Delta Inflow and Outflow from DAYFLOW, Delta Exports, X2, Combined USGS
    # Outflow, Cache Slough Flow) for 1975-2021

# Load packages
library(tidyverse)
library(lubridate)
library(readxl)
library(pdftools)
library(glue)
library(dataRetrieval)


# 1. Import Data ----------------------------------------------------------

# If it hasn't been done already, download and save local copies of the DAYFLOW
  # data from the CNRA data portal since the DAYFLOW files may change

# Set download_dayflow to TRUE if need to download and save DAYFLOW data
download_dayflow <- FALSE

# Download and save DAYFLOW data if necessary
if (download_dayflow == TRUE) {
  # Define directory for raw hydrology data
  dir_hydro <- "data-raw/Hydrology/"

  # Download DAYFLOW Data from CNRA portal: https://data.cnra.ca.gov/dataset/dayflow
  dayflow_1970_1983 <- read_csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/a0a46a1d-bec5-4db9-b331-655e306860ba/download/dayflow-results-1970-1983.csv")
  dayflow_1984_1996 <- read_csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/cb04e626-9729-4105-af81-f6e5a37f116a/download/dayflow-results-1984-1996.csv")
  dayflow_1997_2020 <- read_csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/21c377fe-53b8-4bd6-9e1f-2025221be095/download/dayflow-results-1997-2020.csv")
  dayflow_2021 <- read_csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/83122ce7-e7f5-4ad1-b5e9-6a7032cb1117/download/dayflowcalculations2021.csv")


  # Save data as .csv files in the "data-raw/Hydrology" folder
  dayflow_1970_1983 %>% write_csv(file.path(dir_hydro, "dayflow_1970-1983.csv"))
  dayflow_1984_1996 %>% write_csv(file.path(dir_hydro, "dayflow_1984-1996.csv"))
  dayflow_1997_2020 %>% write_csv(file.path(dir_hydro, "dayflow_1997-2020.csv"))
  dayflow_2021 %>% write_csv(file.path(dir_hydro, "dayflow_2021.csv"))

  # Clean up
  rm(dir_hydro, dayflow_1970_1983, dayflow_1984_1996, dayflow_1997_2020, dayflow_2021)
}

# If it hasn't been done already, download and save local copies of the Total
  # Delta Exports and NDOI daily Outflow Index for Oct - Nov 2021 from the
  # USBR Delta Outflow Computation reports since some of this data is preliminary

# Set download_usbr to TRUE if need to download and save USBR data
download_usbr <- FALSE

# Download and save Delta Export and Outflow data from the USBR Delta Outflow
  # Computation reports if necessary
if (download_usbr == TRUE) {
  # Function to download Delta Outflow Computation reports from the USBR website
    # to the temporary R directory
    # USBR website: https://www.usbr.gov/mp/cvo/pmdoc.html
  dnld_usbr_rpt <- function(month_year) {
    usbr_rpt_url <- glue("https://www.usbr.gov/mp/cvo/vungvari/dout{month_year}.pdf")
    saved_file <- file.path(tempdir(), glue("Delta_Outflow{month_year}.pdf"))
    download.file(usbr_rpt_url, saved_file, mode = "wb")
  }

  # Create vector of months-years for the URL's of the pdf files
  vec_mo_yr <- c("1021", "1121")

  # Download Delta Outflow Computation reports for Oct - Nov 2021
  for (i in vec_mo_yr) {dnld_usbr_rpt(i)}

  # Create a vector of all file paths for the Delta Outflow Computation reports
  fp_usbr_rpt <- dir(path = tempdir(), pattern = "\\.pdf$", full.names = TRUE)

  # Function to convert pdf_text for the Delta Outflow Computation reports to a
  # tibble with the daily Total Exports and Outflows
  conv_pdf2tibb <- function(pdf_txt) {
    vec_pdf_txt <- read_lines(pdf_txt)

    # Keep "rows" 12-42 in the data and convert to a matrix with 23 columns
    mat_pdf_txt <- vec_pdf_txt[12:42] %>%
      str_squish() %>%
      str_split_fixed(pattern = " ", n = 23)

    # Keep the columns for Date, Export, and Outflow and rename them
    mat_pdf_txt2 <- mat_pdf_txt[,c(1, 16, 18)]
    colnames(mat_pdf_txt2) <- c("Date", "Export", "Outflow")

    # Convert matrix to a tibble
    as_tibble(mat_pdf_txt2)
  }

  # Import the Delta Outflow Computation reports and convert data to a tibble with
    # the daily Total Exports and Outflows
  df_usbr_rpt <-
    map(fp_usbr_rpt, pdf_text) %>%
    map(conv_pdf2tibb) %>%
    bind_rows() %>%
    # Remove blank rows
    filter(Date != "-") %>%
    # Remove commas from Export and Outflow data
    mutate(across(c(Export, Outflow), ~str_remove(.x, ",")))

  # Save data as a .csv file in the "data-raw/Hydrology" folder
  df_usbr_rpt %>% write_csv("data-raw/Hydrology/usbr_export_outflow_2021.csv")

  # Clean up
  rm(dnld_usbr_rpt, vec_mo_yr, i, fp_usbr_rpt, conv_pdf2tibb, df_usbr_rpt)
}

# If it hasn't been done already, download and save local copies of the daily
  # average tidally-filtered flow data collected by USGS using the `dataRetrieval`
  # package since some of the data is provisional and may change

# Set download_usgs to TRUE if need to download and save USGS flow data
download_usgs <- FALSE

# Download and save USGS flow data if necessary
if (download_usgs == TRUE) {
  # Define site numbers for the USGS data download:
  # 11455420 - Sacramento River at Rio Vista CA (RVB)
  # 11337190 - San Joaquin River at Jersey Point CA (SJJ)
  # 11337080 - Threemile Slough near Rio Vista CA (TSL)
  # 11455350 - Cache Slough at Ryer Island (RYI)
  # 11313433 - Dutch Slough below Jersey Island Road at Jersey Island (DSJ)
  # 11455385 - Cache Slough above Ryer Island Ferry near Rio Vista CA (RYF)
  site_numb <- c(
    "11455420",
    "11337190",
    "11337080",
    "11313433",
    "11455350",
    "11455385"
  )

  # Download daily average tidally-filtered flow data in cfs (#72137) collected
    # by USGS using the `readNWISdv` function. Download data for entire period of
    # record up to 11/30/2021.
  df_tfq_dv_tmp <- readNWISdv(site_numb, "72137", endDate = "2021-11-30")

  # Save data as a .csv file in the "data-raw/Hydrology" folder
  df_tfq_dv_tmp %>% write_csv("data-raw/Hydrology/usgs_daily_avg_tf_flow_1994-2021.csv")

  # Clean up
  rm(site_numb, df_tfq_dv_tmp)
}

# Define file path for raw hydrology data
fp_hydro <- "data-raw/Hydrology/"

# Import DAYFLOW data for 1970-2021
df_dayflow_1970_1983 <-
  read_csv(
    file.path(fp_hydro, "dayflow_1970-1983.csv"),
    col_types = cols_only(
      Date = "c",
      SAC = "d",
      YOLO = "d",
      EAST = "d",
      TOT = "d",
      EXPORT = "d",
      OUT = "d"
    )
  )

df_dayflow_1984_1996 <-
  read_csv(
    file.path(fp_hydro, "dayflow_1984-1996.csv"),
    col_types = cols_only(
      Date = "c",
      SAC = "d",
      YOLO = "d",
      EAST = "d",
      TOT = "d",
      EXPORT = "d",
      OUT = "d"
    )
  )

df_dayflow_1997_2020 <-
  read_csv(
    file.path(fp_hydro, "dayflow_1997-2020.csv"),
    col_types = cols_only(
      Date = "c",
      SAC = "d",
      YOLO = "d",
      EAST = "d",
      TOT = "d",
      EXPORTS = "d",
      OUT = "d",
      X2 = "d"
    )
  )

df_dayflow_2021 <-
  read_csv(
    file.path(fp_hydro, "dayflow_2021.csv"),
    col_types = cols_only(
      Date = "c",
      SAC = "d",
      YOLO = "d",
      EAST = "d",
      TOT = "d",
      EXPORTS = "d",
      OUT = "d",
      X2 = "d"
    )
  )

# Import Delta Export and Outflow data from the USBR Delta Outflow Computation
  # reports for Oct - Nov 2021 until DAYFLOW data is available
df_usbr_2021 <- read_csv(file.path(fp_hydro, "usbr_export_outflow_2021.csv"), col_types = "cdd")

# Import estimated X2 values for earlier years based on Hutton et al. paper
df_hutton_x2 <-
  read_excel(
    file.path(fp_hydro, "supplemental_data_wr.1943-5452.0000617_hutton3.xlsx"),
    sheet = "Daily"
  )

# Import daily average tidally-filtered flow data collected by USGS
df_usgs <- read_csv(file.path(fp_hydro, "usgs_daily_avg_tf_flow_1994-2021.csv"))


# 2. Clean and Combine Data -----------------------------------------------

# Rename "EXPORTS" to "EXPORT" in the 1997-2021 data to match other data sets
df_dayflow_1997_2020_c <- df_dayflow_1997_2020 %>% rename(EXPORT = EXPORTS)
df_dayflow_2021_c <- df_dayflow_2021 %>% rename(EXPORT = EXPORTS)

# Combine DAYFLOW data and start with some basic cleaning
df_dayflow_v1 <-
  bind_rows(
    df_dayflow_1970_1983,
    df_dayflow_1984_1996,
    df_dayflow_1997_2020_c,
    df_dayflow_2021_c
  ) %>%
  rename(
    InflowSacR = SAC,
    InflowYolo = YOLO,
    InflowEast = EAST,
    InflowTotal = TOT,
    Outflow = OUT,
    Export = EXPORT
  ) %>%
  # convert date column to date
  mutate(Date = parse_date_time(Date, c("mdY", "Ymd")))

# Prepare X2 data from Hutton et al. paper to be joined with DAYFLOW data
df_hutton_x2_c <- df_hutton_x2 %>%
  mutate(Date = date(Date)) %>%
  select(Date, X2Hutton = SacX2)

# Add X2 for earlier years based on Hutton et al. paper
df_dayflow_v2 <- df_dayflow_v1 %>%
  left_join(df_hutton_x2_c, by = "Date") %>%
  mutate(X2 = if_else(is.na(X2), X2Hutton, X2)) %>%
  select(-X2Hutton)

# Prepare USBR Delta Export and Outflow data to be combined with DAYFLOW data
df_usbr_2021_c <- df_usbr_2021 %>% mutate(Date = mdy(Date))

# Add USBR data to the DAYFLOW data
df_dayflow_v3 <- bind_rows(df_dayflow_v2, df_usbr_2021_c) %>% arrange(Date)

# Calculate X2 for Oct - Nov 2021 based on DAYFLOW documentation:
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

# Fill in X2 data for Oct - Nov 2021
for (i in which(df_dayflow_v3$Date == as_date("2021-10-01")):which(df_dayflow_v3$Date == as_date("2021-11-30"))) {
  df_dayflow_v3$X2[i] = 10.16 + 0.945*df_dayflow_v3$X2[i-1] - 1.487*log10(df_dayflow_v3$Outflow[i])
}

# Prepare daily average tidally-filtered flow data collected by USGS to be
  # combined with other hydrology data
# Start with some general cleaning
df_usgs_c <- df_usgs %>% select(site_no, Date, DailyAvgFlow = X_72137_00003)

# Calculate combined USGS outflow from RVB, SJJ, TSL, and DSJ
df_usgs_comb_outf <- df_usgs_c %>%
  # Remove Cache Slough data
  filter(!site_no %in% c("11455350", "11455385")) %>%
  # Fill in NA values to complete period of record to make sure data for all
    # stations is included in the total outflow
  complete(site_no, Date) %>%
  group_by(Date) %>%
  summarize(TotalUSGSOutflow = sum(DailyAvgFlow)) %>%
  filter(!is.na(TotalUSGSOutflow))

# Combine Cache Slough data from two stations (RYI and RYF) so that it only has
  # one value per day
df_usgs_cache <- df_usgs_c %>%
  # Only keep Cache Slough data
  filter(site_no %in% c("11455350", "11455385")) %>%
  # Data for RYI (11455350) goes through 2019-04-26 - We'll use data from RYI
    # through its period of record, then RYF (11455385) for the time period
    # afterwards, so we'll remove data from RYF where data from the two stations
    # overlap
  filter(!(site_no == "11455385" & Date < "2019-04-27")) %>%
  # Remove erroneous record from 2003-02-19
  filter(Date != "2003-02-19") %>%
  select(Date, CacheFlow = DailyAvgFlow)

# Create a data frame that contains all possible dates from 12/1/1974 - 11/30/2021
df_date <- tibble(Date = seq.Date(as_date("1974-12-01"), as_date("2021-11-30"), by = 1))

# Integrate data sets of raw hydrology data
lst_hydro <- list(df_date, df_dayflow_v3, df_usgs_comb_outf, df_usgs_cache)

raw_hydro_1975_2021_v1 <- reduce(lst_hydro, left_join)

# Finish cleaning raw hydrology data
raw_hydro_1975_2021 <- raw_hydro_1975_2021_v1 %>%
# Add variables for adjusted calendar year and season
  # Adjusted calendar year: December-November, with December of the previous calendar year
  # included with the following year
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
  # Select variables to keep
  select(
    YearAdj,
    Season,
    Date,
    InflowSacR,
    InflowYolo,
    InflowEast,
    InflowTotal,
    Outflow,
    Export,
    X2,
    TotalUSGSOutflow,
    CacheFlow
  ) %>%
  arrange(Date)


# 3. Save and Export Data -------------------------------------------------

# Save final raw hydrology and LSZ data set as csv file for easier diffing
write_csv(raw_hydro_1975_2021, "data-raw/Final/raw_hydro_1975_2021.csv")

# Save final raw hydrology and LSZ data set as object in the data package
usethis::use_data(raw_hydro_1975_2021, overwrite = TRUE)

