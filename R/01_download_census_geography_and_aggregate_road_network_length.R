# Script Title: 01_download_census_geography_and_aggregate_road_network_data.R
# Author: Michael Branion-Calles
# Date: September 2024
# Description: Load Census Geography for province of British Columbia and Aggregate Length of Roads within each
start_time <- Sys.time()
# Load required libraries
library(sf)
library(tidyverse)
library(cancensus)
library(readxl)
library(purrr)

# Get the current working directory and source a file containing R functions
wd <- getwd()
source(file = paste0(wd, "/R/functions.R"))


# Load census dissemination area geographies ------------------------------

bc_da <-
  get_census(
    dataset = "CA21",
    regions = list(PR = "59"),
    level = "DA",
    use_cache = FALSE,
    geo_format = "sf",
    quiet = FALSE
  ) %>%
  # Transform to bc albers projection
  st_transform(crs = 3005)

str(bc_da)

# Load previously downloaded bc digital road atlas ------------------------------

dra <- #road network polyline
  st_read(dsn = paste0(wd, "/Data/dgtl_road_atlas.gdb"),
          layer = "TRANSPORT_LINE")

type <- #data dictionary
  st_read(dsn = paste0(wd, "/Data/dgtl_road_atlas.gdb"),
          layer = "TRANSPORT_LINE_TYPE_CODE")

dra_sub <- dra %>% # Subset road network by road types
  left_join(type, by = "TRANSPORT_LINE_TYPE_CODE") %>%
  filter(
    str_detect(DESCRIPTION, "arterial") |
      str_detect(DESCRIPTION, "collector") |
      str_detect(DESCRIPTION, "freeway") |
      str_detect(DESCRIPTION, "highway") |
      str_detect(DESCRIPTION, "local") |
      str_detect(DESCRIPTION, "ramp") |
      str_detect(DESCRIPTION, "strata")
  )

dra_highway <- # highway only
  dra_sub %>%
  filter(str_detect(DESCRIPTION, "freeway|highway|ramp"))

dra_arterial <- # arterial only
  dra_sub %>%
  filter(str_detect(DESCRIPTION, "arterial"))

dra_collector <- # collector only
  dra_sub %>%
  filter(str_detect(DESCRIPTION, "collector"))

dra_local <- # local only
  dra_sub %>%
  filter(str_detect(DESCRIPTION, "local|strata"))

roads <- # Create list of road networks by classification
  list(
    highway = dra_highway,
    arterial = dra_arterial,
    collector = dra_collector,
    local = dra_local
  )

# Split lines by the boundary of the census geography  ------------------

overlap <-
  10 # size in m of overlap of boundaries for counting roads and crashes

roads_clip_da <-
  lapply(1:length(roads), function(x) {
    clip_linestring_by_poly(
      linestring = roads[[x]],
      clipping_polygon = bc_da,
      clipping_polygon_buffer_size = overlap,
      n_grid_cells = 1000
    )
  })

bc_da_buff <- st_buffer(bc_da, dist = overlap) %>%
  select(GeoUID)

st_write(bc_da_buff, dsn = paste0(wd, "/Processed Data/da_buffer_2021.gpkg"))

roads_agg_da <- roads_clip_da %>%
  map(
    .,
    ~ st_join(., bc_da_buff, join = st_intersects) %>% # assign the dauid that overlaps with a given line segment
      st_drop_geometry() %>%
      group_by(GeoUID) %>%
      summarise(length_m = sum(length_m))
  ) %>%
  bind_rows(.id = "stratification") %>%
  mutate(
    stratification = case_when(
      stratification == "1" ~ "highway_m",
      stratification == "2" ~ "arterial_m",
      stratification == "3" ~ "collector_m",
      stratification == "4" ~ "local_m"
    ),
    length_m = as.numeric(length_m)
  ) %>%
  pivot_wider(names_from = stratification,
              values_from = length_m,
              values_fill = 0) %>%
  mutate(total_roads_m = highway_m + arterial_m + collector_m + local_m)

bc_da <- bc_da %>%
  left_join(roads_agg_da, by = "GeoUID") %>%
  replace_na(list(
    highway_m = 0,
    arterial_m = 0,
    collector_m = 0,
    local_m = 0,
    total_roads_m = 0
  ))

st_write(bc_da, dsn = paste0(wd, "/Processed Data/da_v1_2021.gpkg"))

end_time <- Sys.time()
end_time - start_time
