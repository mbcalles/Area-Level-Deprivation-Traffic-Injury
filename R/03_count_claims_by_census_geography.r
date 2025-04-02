# Script Title: 02_count_claims_by_census_geography.R
# Author: Michael Branion-Calles
# Date: March 2025
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

aggregate_claims <- function(data, filter_expr,seed = 222,intersection_csv=NA) {
  # Evaluate the filter expression using rlang to allow flexible filtering
  filter_quo <- rlang::enquo(filter_expr)
  
  # Filter claims and create intersection identifiers
  # Each crash at an intersection of multiple DAs gets a hyphenated string of DA IDs
  crash_intersect <- claims_da_join %>% # data %>% 
  #   dplyr::filter(!!filter_quo) %>%  # Apply the provided filter expression
    filter(`Crash Severity` == "CASUALTY CRASH") %>% 
    arrange(id) %>% 
    group_by(id) %>% 
    summarise(
      n_das = n(),                               # Count DAs bordering this crash
      dauid_intersection = paste0(dauid, collapse = "-")  # Create hyphenated string of DA IDs
    )
  

  # Process intersections to distribute claims among component DAs
  da_claims <- crash_intersect %>% 
    # Count claims per unique intersection
    group_by(dauid_intersection) %>% 
    summarise(n_claims = n()) %>% 
    # Split intersection string into component DA IDs (allowing up to 5 DAs per intersection)
    separate(dauid_intersection, sep = "-", into = c("dauid1", "dauid2", "dauid3", "dauid4", "dauid5"), remove = FALSE) %>% 
    # Convert to long format with one row per DA in each intersection
    pivot_longer(
      cols = c("dauid1", "dauid2", "dauid3", "dauid4", "dauid5"),
      names_to = "dauid_num",
      values_to = "dauid",
      values_drop_na = TRUE     # Remove empty DA slots
    ) %>% 
    # For each intersection, calculate crash distribution
    group_by(dauid_intersection) %>% 
    mutate(
      n_intersections = n(),                          # Count DAs in this intersection
      base_claims = floor(n_claims / n_intersections),  # Evenly distribute claims (integer division)
      remainder = n_claims %% n_intersections        # Track leftover claims for random assignment
    )
  
  # Sum up base claims per DA (the evenly distributed portion)
  base_dauid <- da_claims %>% 
    group_by(dauid) %>% 
    summarise(base_claims = sum(base_claims))

  
  set.seed(seed)
  # Randomly assign remainder claims to DAs in each intersection
  remainder_dauid <- da_claims %>% 
    filter(remainder > 0) %>%
    group_by(dauid_intersection) %>% 
    group_modify(~ {
      n_sample <- first(.x$remainder)
      slice_sample(.x, n = n_sample)
    }) %>%
    group_by(dauid) %>% 
    summarise(remainder_claims = n())
  
  
  if(!is.na(intersection_csv)){
    write_csv(da_claims,paste0(wd, "/Processed Data/intersection_base_claims",intersection_csv))
    write_csv(remainder_dauid,paste0(wd, "/Processed Data/intersection_remainder_assignment",intersection_csv))
    
  }
  
  # Combine base and remainder claims for total per DA
  total_claims <- left_join(base_dauid, remainder_dauid) %>% 
    replace_na(list(remainder_claims = 0)) %>%  # Set remainder to 0 for DAs with none
    mutate(total_claims = base_claims + remainder_claims)  # Calculate final total
  
  return(total_claims)
}

# Aggregate casualty crash claims and distribute them among dissemination areas
casualty_claims <- aggregate_claims(
  claims_da_join,
  filter_expr = `Crash Severity` == "CASUALTY CRASH",
  intersection_csv = "casualty_crash.csv"
) %>%
  rename(n_casualty_claims = total_claims)
# Verify that all casualty crashes are accounted for
sum(casualty_claims$n_casualty_claims) == nrow(claims_sf %>% filter(`Crash Severity` == "CASUALTY CRASH"))

# Aggregate cyclist casualty crash claims
cyclist_casualty_claims <- aggregate_claims(
  claims_da_join,
  filter_expr = `Crash Severity` == "CASUALTY CRASH" &
    `Cyclist Flag` == "Yes") %>%
  rename(n_cyclist_casualty_claims = total_claims)
# Verify that all cyclist casualty crashes are accounted for
sum(cyclist_casualty_claims$n_cyclist_casualty_claims) == nrow(claims_sf %>% filter(`Crash Severity` == "CASUALTY CRASH" &
                                                                                      `Cyclist Flag` == "Yes"))
# Aggregate pedestrian casualty crash claims

pedestrian_casualty_claims <- aggregate_claims(
  claims_da_join,
  filter_expr = `Crash Severity` == "CASUALTY CRASH" &
    `Pedestrian Flag` == "Yes") %>%
  rename(n_pedestrian_casualty_claims = total_claims)
# Verify that all pedestrian casualty crashes are accounted for

sum(pedestrian_casualty_claims$n_pedestrian_casualty_claims) == nrow(claims_sf %>% filter(`Crash Severity` == "CASUALTY CRASH" &
                                                                                            `Pedestrian Flag` == "Yes"))
# Combine all claim types into a single dataset

total_claims_by_da <- casualty_claims %>% select(dauid, n_casualty_claims) %>%
  full_join(cyclist_casualty_claims %>% select(dauid, n_cyclist_casualty_claims)) %>%
  full_join(pedestrian_casualty_claims %>% select(dauid, n_pedestrian_casualty_claims))

# Join the aggregated crash data back to the original census geography data
bc_da <- bc_da %>%
  left_join(total_claims_by_da, by = "dauid") %>%  # Add crash data to the census data
  replace_na(
    list(
      n_casualty_claims = 0,
      n_cyclist_casualty_claims = 0,
      n_pedestrian_casualty_claims = 0
    )
  )  # Replace any NA values with 0

# Save the updated census data with crash statistics to a new file
st_write(bc_da, dsn = paste0(wd, "/Processed Data/da_v3_2021c.gpkg"))


iterate_casualty_claims <- lapply(222:271,function(x) aggregate_claims(claims_da_join,filter_expr = `Crash Severity` == "CASUALTY CRASH",seed = x))
iterate_cyclist_casualty_claims <- lapply(222:271,function(x) aggregate_claims(claims_da_join,filter_expr = `Crash Severity` == "CASUALTY CRASH" & `Cyclist Flag` == "Yes",seed = x))
iterate_pedestrian_casualty_claims <- lapply(222:271,function(x) aggregate_claims(claims_da_join,filter_expr = `Crash Severity` == "CASUALTY CRASH" & `Pedestrian Flag` == "Yes",seed = x))


saveRDS(iterate_casualty_claims, file =  paste0(wd, "/Processed Data/iterate_casualty_claims.rds"))
saveRDS(iterate_cyclist_casualty_claims, file =  paste0(wd, "/Processed Data/iterate_cyclist_casualty_claims.rds"))
saveRDS(iterate_pedestrian_casualty_claims, file =  paste0(wd, "/Processed Data/iterate_pedestrian_casualty_claims.rds"))





























# Identify and analyze duplicate spatial joins (if any)
claims_da_duplicated <- claims_da_join %>%
  group_by(id) %>%                      # Group by the unique ID
  summarise(n_DAs = n()) %>%            # Count the number of distinct areas (DAs) each claim intersects
  group_by(n_DAs) %>%                   # Group by the count of intersected areas
  tally() %>%                           # Count how many claims have the same number of intersections
  mutate(p = n / sum(n) * 100)          # Calculate the percentage of claims for each intersection count

claims_da_duplicated

# Randomly assign claims to unique DAs to handle duplicates
set.seed(222)     

# Set a random seed for reproducibility
claims_rnd_assignment_duplicates <- claims_da_join %>%
  group_by(id) %>%                     # Group by unique ID
  slice_sample(n = 1) %>%              # Randomly sample one record per ID
  ungroup()      

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


# Create a function that performs all operations with a given seed
process_iteration <- function(seed_val) {
  # Set the seed
  set.seed(seed_val)
  
  # Random sampling
  claims_rnd_assignment <- claims_da_join %>%
    group_by(id) %>%
    slice_sample(n = 1) %>%
    ungroup()
  
  # Aggregate total crashes by DA
  total_crashes_by_da <- claims_rnd_assignment %>%
    group_by(dauid) %>%
    summarise(
      n_claims = n(),
      n_casualty_claims = sum(`Crash Severity` == "CASUALTY CRASH"),
      n_cyclist_claims = sum(`Cyclist Flag` == "Yes"),
      n_cyclist_casualty_claims = sum(`Cyclist Flag` == "Yes" & `Crash Severity` == "CASUALTY CRASH"),
      n_pedestrian_claims = sum(`Pedestrian Flag` == "Yes"),
      n_pedestrian_casualty_claims = sum(`Pedestrian Flag` == "Yes" & `Crash Severity` == "CASUALTY CRASH")
    )
  
  # Join and replace NAs
  bc_da_iteration <- bc_da %>%
    st_drop_geometry() %>% 
    left_join(total_crashes_by_da, by = "dauid") %>%
    replace_na(
      list(
        n_claims = 0,
        n_casualty_claims = 0,
        n_cyclist_claims = 0,
        n_cyclist_casualty_claims = 0,
        n_pedestrian_claims = 0,
        n_pedestrian_casualty_claims = 0
      )
    )
  
  # Add the seed/iteration identifier
  bc_da_iteration %>% mutate(seed = seed_val)
}

# Apply the function to 50 different seeds and store the results
# Store as a list of 50 separate dataframes
results_list <- map(222:272, process_iteration)


