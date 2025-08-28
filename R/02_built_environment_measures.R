# Script Title: 02_built_environment_measures.R
# Author: Michael Branion-Calles
# Date: October 2024
# Description: Assign built environment measures

# Load required libraries
library(sf)
library(tidyverse)
library(janitor)

# Get the current working directory
wd <-  getwd()

# Source functions from other script
source(file = paste0(wd, "/R/functions.R"))


# Load census geography
bc_da <-
  st_read(paste0(wd, "/Processed Data/da_v1_2021.gpkg"))
# 'bc_da' contains the census data for the province of British Columbia.

######### Intersections

bc_int <- st_read(dsn = paste0(wd,"/Data/DMTI Spatial/2021/RoadJunctionPoint.shp")) %>% 
  janitor::clean_names()%>% 
  filter(prov == "BC") %>% 
  st_transform(crs = 3005) %>%  
  filter(type == "INTERSECTION")


bc_int_da <- st_join(bc_int %>% st_buffer(dist = 5),bc_da %>% select(dauid))


no_int_by_da <-   bc_int_da %>% 
  st_drop_geometry() %>%
  group_by(dauid) %>% 
  tally(name = "number_intersections")


######### Building Points


bc_building <- st_read(dsn = paste0(wd,"/Data/DMTI Spatial/2021/BuildingPoint.shp")) %>% 
  janitor::clean_names()%>% 
  filter(prov == "BC") %>% 
  st_transform(crs = 3005)


bc_building_da <- st_join(bc_building %>% st_buffer(dist = 5),bc_da %>% select(dauid))


no_building_by_da <-   bc_building_da %>% 
  st_drop_geometry() %>%
  group_by(dauid) %>% 
  tally(name = "number_buildings")

######### Transportation Stops


bc_transit_stop <- st_read(dsn = paste0(wd,"/Data/DMTI Spatial/2021/TransportationStopsPoint.shp")) %>% 
  janitor::clean_names()%>% 
  filter(prov == "BC") %>% 
  st_transform(crs = 3005)


bc_transit_stop_da <- st_join(bc_transit_stop %>% st_buffer(dist = 5),bc_da %>% select(dauid))


no_transit_stop_by_da <-   bc_transit_stop_da %>% 
  st_drop_geometry() %>%
  group_by(dauid) %>% 
  tally(name = "number_transit_stops")


######### Canadian Bikeway Comfort and Safety Index - Aggregated to DA previously using DA centroid buffers
bics <- read_csv(paste0(wd,"/Data/nhbic_ava_21.csv")) %>% 
  mutate(dauid = as.character(nhbic21_01)) %>% 
  select(-province,-postalcode21,-nhbic21_01) %>% 
  distinct() %>% 
  rename(bike_to_work_rate = nhbic21_04,
         sust_transport_to_work_rate = nhbic21_05,
         km_high_comfort_bike_infra = nhbic21_06,
         km_med_comfort_bike_infra = nhbic21_07,
         km_low_comfort_bike_infra = nhbic21_08,
         canbics_index = nhbic21_09,
         canbics_class = nhbic21_10
  ) %>% 
  mutate(bike_infra = ifelse(km_high_comfort_bike_infra+km_med_comfort_bike_infra+km_low_comfort_bike_infra>0,1,0))


######### Canadian Bikeway Comfort and Safety Index - Raw data aggregated to DA

canbics <- #CanBICS polyline January 1 2022
  st_read(dsn = paste0(wd, "/Data/canbics_2022/OSM_CAN_BICS_latest.shp")) %>% 
  filter(PRNAME == "British Columbia / Colombie-Britannique") %>% 
  st_transform(st_crs(bc_da)) %>% 
  clean_names()



canbics_hc <- # high comfort
  canbics %>%
  filter(cbics_comf=="1. High Comfort")

canbics_mc <- # medium comfort
  canbics %>%
  filter(cbics_comf=="2. Medium Comfort")


canbics_lc <- # low comfort bikewaysw
  canbics %>%
  filter(cbics_comf=="3. Low Comfort")



bike_infra <- # Create list of road networks by classification
  list(
    canbics_high_comfort = canbics_hc,  
    canbics_med_comfort = canbics_mc,
    canbics_low_comfort = canbics_lc
  )


# Split lines by the boundary of the census geography  ------------------

overlap <-
  5 # size in m of overlap of boundaries for counting roads and crashes

bike_infra_clip_da <-
  lapply(1:length(bike_infra), function(x) {
    clip_linestring_by_poly(
      linestring = bike_infra[[x]],
      clipping_polygon = bc_da,
      clipping_polygon_buffer_size = overlap,
      n_grid_cells = 1
    )
  })

bc_da_buff <- st_buffer(bc_da, dist = overlap,endCapStyle = "FLAT",joinStyle = "BEVEL") %>%
  select(dauid)

bike_infra_agg_da <- bike_infra_clip_da %>%
  map(~mutate(.,bike_infra_id = row_number())) %>% #assign unique id for each new Road Segment fragment
  map(.,
      ~ st_join(., bc_da_buff, join = st_intersects) %>% # assign the dauid that overlaps with a given line segment
        st_drop_geometry() %>%
        group_by(bike_infra_id,osm_id) %>%
        mutate(intersecting_geo_units = n_distinct(dauid) ) %>%  # Count the number of distinct dauids that overlap with a bike infrastructure segment fragment 
        group_by(dauid) %>% 
        summarise(length_m = sum(length_m) %>% as.numeric()
        )
  ) %>%
  bind_rows(.id = "stratification") %>%
  mutate(
    stratification = case_when(
      stratification == "1" ~ "bike_infra_high_comfort",
      stratification == "2" ~ "bike_infra_medium_comfort",
      stratification == "3" ~ "bike_infra_low_comfort",
    )
  ) %>%
  pivot_wider(names_from = stratification,
              names_glue = "{stratification}_{.value}",
              values_from = length_m,
              values_fill = 0) %>%
  mutate(total_bike_infra_m = bike_infra_high_comfort_length_m + bike_infra_medium_comfort_length_m + bike_infra_low_comfort_length_m,
  )



######## Join Data to Census Geographies

bc_da_be <- bc_da %>%
  left_join(no_int_by_da, by = "dauid") %>%
  left_join(no_building_by_da, by = "dauid") %>%
  left_join(no_transit_stop_by_da, by = "dauid") %>%
  left_join(bics, by = "dauid") %>%
  left_join(bike_infra_agg_da, by = "dauid") %>% 
  replace_na(
    list(
      number_intersections = 0,
      number_buildings = 0,
      number_transit_stops = 0,
      km_high_comfort_bike_infra = 0,
      km_med_comfort_bike_infra = 0,
      km_low_comfort_bike_infra = 0,
      bike_infra = 0,
      bike_infra_high_comfort_length_m = 0,
      bike_infra_medium_comfort_length_m  = 0,
      bike_infra_low_comfort_length_m  = 0,
      total_bike_infra_m = 0
    )
  ) %>%
  mutate(
    building_density = number_buildings / landarea,
    intersection_density = number_intersections / landarea,
    transit_stop_density = number_transit_stops / landarea,
    any_bike_infra = ifelse(total_bike_infra_m > 0, "Yes", "No") %>%
      factor(., levels = c("No", "Yes"))
  )



st_write(bc_da_be, dsn = paste0(wd, "/Processed Data/da_v2_2021.gpkg"))



