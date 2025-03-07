# Script Title: 02_count_claims_by_census_geography.R
# Author: Michael Branion-Calles
# Date: October 2024
# Description: Count crashes from claims data by geographic unit

# Load required libraries
library(sf)
library(readxl)
library(tidyverse)

# Get the current working directory
wd <-  getwd()

# Source functions from other script
source(file = paste0(wd, "/R/functions.R"))

# Load census geography
bc_da <-
  st_read(paste0(wd, "/Processed Data/da_v2_2021.gpkg"))
# 'bc_da' contains the census data for the province of British Columbia.

da_buff <-
  st_read(paste0(wd, "/Processed Data/da_buffer_2021.gpkg"))
# 'da_buff' contains buffer data around the geographic units, used for spatial joins.


# Load insurance claims data
claims <-
  claims <- read_csv(file = paste0(wd, "/Data/icbc_reported_crashes_2019_2023.csv")) %>%
  filter(!is.na(Latitude)) %>% # Remove rows where Latitude is missing
  uncount(`Total Crashes`) %>% 
  mutate(id = row_number()) %>%          # Add a unique ID for each row
  select(id, `Cyclist Flag`, `Pedestrian Flag`, `Crash Severity`, Longitude, Latitude)
# 'claims' now contains cleaned and selected columns from the insurance claims data.

# Convert claims data to an 'sf' object for spatial analysis
claims_sf <- st_as_sf(claims,
                      coords = c("Longitude", "Latitude"),  # Specify columns with coordinate data
                      crs = 4326                            # Set the coordinate reference system (WGS 84)
) %>%
  st_transform(crs = 3005)                # Transform to a different CRS (NAD83 / BC Environment Albers)


# Spatially join insurance claims with buffer areas
claims_da_join <- st_join(claims_sf, da_buff, join = st_intersects) %>%
  as_tibble()  

# Identify and analyze duplicate spatial joins (if any)
claims_da_duplicated <- claims_da_join %>%
  group_by(id) %>%                      # Group by the unique ID
  summarise(n_DAs = n()) %>%            # Count the number of distinct areas (DAs) each claim intersects
  group_by(n_DAs) %>%                   # Group by the count of intersected areas
  tally() %>%                           # Count how many claims have the same number of intersections
  mutate(p = n / sum(n) * 100)          # Calculate the percentage of claims for each intersection count

claims_da_duplicated

# Randomly assign claims to unique DAs to handle duplicates
set.seed(222)                           # Set a random seed for reproducibility
claims_rnd_assignment_duplicates <- claims_da_join %>%
  group_by(id) %>%                     # Group by unique ID
  slice_sample(n = 1) %>%              # Randomly sample one record per ID
  ungroup()                            # Remove grouping

# Aggregate total crashes by geographic unit (DA)
total_crashes_by_da <- claims_rnd_assignment_duplicates %>%
  group_by(dauid) %>%                 # Group by the geographic unit ID
  summarise(
    n_claims = n(),                    # Total number of claims
    n_casualty_claims = sum(`Crash Severity` == "CASUALTY CRASH"), # Number of casualty claims (casualty means injury here)
    n_cyclist_claims = sum(`Cyclist Flag` == "Yes"),         # Number of cyclist claims
    n_cyclist_casualty_claims = sum(`Cyclist Flag` == "Yes" & `Crash Severity` == "CASUALTY CRASH"), # Number of cyclist casualty claims
    n_pedestrian_claims = sum(`Pedestrian Flag` == "Yes"),   # Number of pedestrian claims
    n_pedestrian_casualty_claims = sum(`Pedestrian Flag` == "Yes" & `Crash Severity` == "CASUALTY CRASH") # Number of pedestrian casualty claims
  )


# Join the aggregated crash data back to the original census geography data
bc_da <- bc_da %>%
  left_join(total_crashes_by_da, by = "dauid") %>%  # Add crash data to the census data
  replace_na(
    list(
      n_claims = 0,
      n_casualty_claims = 0,
      n_cyclist_claims = 0,
      n_cyclist_casualty_claims = 0,
      n_pedestrian_claims = 0,
      n_pedestrian_casualty_claims = 0
    )
  )  # Replace any NA values with 0

# Save the updated census data with crash statistics to a new file
st_write(bc_da, dsn = paste0(wd, "/Processed Data/da_v3_2021b.gpkg"))
