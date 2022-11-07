library(dataRetrieval)
library(dplyr)
library(tidyr)
library(readr)

# Identify stations of interest
siteNumbers<-c(
  'USGS-11455385',
  'USGS-11337190',
  'USGS-11313405',
  'USGS-11312676')

# Retrieve lat/long coordinates and attach to data
lat_long <- whatWQPsites(siteid=siteNumbers)%>%
  select(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure)

lat_long$MonitoringLocationIdentifier <- gsub('USGS-', '',lat_long$MonitoringLocationIdentifier)

lat_long$station <- lat_long$MonitoringLocationIdentifier

lat_long$station <- recode(lat_long$station,
                        '11313405' = 'Old',
                        '11312676'='Middle',
                        '11455385'='Cache',
                        '11337190'='Jersey')

write_csv(lat_long, glue("data-raw/Hydrology/vel_coord.csv"))
