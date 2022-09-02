# Code to prepare internal data for functions and data used globally in the package:
  # `df_regions` - Region assignments for the Drought Synthesis
  # `sf_delta` - Shapefile containing subregions for the Drought Synthesis
  # `df_yr_type` - Year assignment info for the Drought Synthesis

# Load packages
library(readr)
library(dplyr)
# Make sure we are using `deltamapr` version 1.0.0, commit d0a6f9c22aa074f906176e99a0ed70f97f26fffd
# install.packages("devtools")
# devtools::install_github("InteragencyEcologicalProgram/deltamapr", ref = "d0a6f9c22aa074f906176e99a0ed70f97f26fffd")
library(deltamapr)
library(sf)

# Import region assignments
df_regions <- read_csv("data-raw/Global/Rosies_regions.csv")

# Load Delta regions shapefile from Brian
sf_delta <- R_EDSM_Subregions_Mahardja_FLOAT %>%
  # Filter to regions of interest
  filter(SubRegion %in% unique(df_regions$SubRegion)) %>%
  select(SubRegion)

# Import year assignments
df_yr_type <- read_csv("data-raw/Global/Year_assignments.csv") %>% rename(YearAdj = Year)

# Save internal data sets as internal objects in the package
usethis::use_data(df_regions, sf_delta, df_yr_type, internal = TRUE, overwrite = TRUE)

