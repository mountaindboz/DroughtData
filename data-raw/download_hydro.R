# Download and save local copies of the DAYFLOW data from the CNRA data portal and Delta Outflow data (DTO)
  # from CDEC since the DAYFLOW files may change and the DTO data is provisional

library(readr)
library(cder)

# Define file path for raw hydrology data
fp_hydro <- "data-raw/Hydrology/"

# Download DAYFLOW Data from CNRA portal: https://data.cnra.ca.gov/dataset/dayflow
df_dayflow_1970_1983 <- read_csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/a0a46a1d-bec5-4db9-b331-655e306860ba/download/dayflow-results-1970-1983.csv")
df_dayflow_1984_1996 <- read_csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/cb04e626-9729-4105-af81-f6e5a37f116a/download/dayflow-results-1984-1996.csv")
df_dayflow_1997_2020 <- read_csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/21c377fe-53b8-4bd6-9e1f-2025221be095/download/dayflow-results-1997-2020.csv")

# Download and save Delta Outflow (DTO) from CDEC for WY 2021 until DAYFLOW data is available
df_dto_2021 <-
  cdec_query(
    stations = "DTO",
    sensors = 23,
    durations = "D",
    start.date = "2020-10-01",
    end.date = "2021-10-31"
  )

# Save data as .csv files in the "data-raw/Hydrology" folder
df_dayflow_1970_1983 %>% write_csv(file.path(fp_hydro, "dayflow_1970-1983.csv"))
df_dayflow_1984_1996 %>% write_csv(file.path(fp_hydro, "dayflow_1984-1996.csv"))
df_dayflow_1997_2020 %>% write_csv(file.path(fp_hydro, "dayflow_1997-2020.csv"))
df_dto_2021 %>% write_csv(file.path(fp_hydro, "dto_2021.csv"))

# Remove all objects
rm(
  fp_hydro,
  df_dayflow_1970_1983,
  df_dayflow_1984_1996,
  df_dayflow_1997_2020,
  df_dto_2021
)

