# Script Title: 01_download_census_geography_and_aggregate_road_network_data.R
# Author: Michael Branion-Calles
# Date: October 2024
# Description: Load Census Geography for province of British Columbia and Aggregate Length of lane within each
start_time <- Sys.time()
# Load required libraries
library(sf)
library(tidyverse)
# library(cancensus)
library(readxl)
library(purrr)

# Get the current working directory and source a file containing R functions
wd <- getwd()
source(file = paste0(wd, "/R/functions.R"))


# Load census dissemination area geographies ------------------------------
# 
# bc_da <-
#   get_census(
#     dataset = "CA21",
#     regions = list(PR = "59"),
#     level = "DA",
#     use_cache = FALSE,
#     geo_format = "sf",
#     quiet = FALSE
#   ) %>%
#   # Transform to bc albers projection
#   st_transform(crs = 3005) %>% 
# rename(GeoUID=dauid)

# https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?Year=21

bc_da <- st_read(dsn = paste0(wd,"/Data/census geography/census_da_geo")) %>% 
  janitor::clean_names() %>% 
  filter(pruid=="59") %>% 
  st_transform(crs = 3005)

plot(bc_da["landarea"])

# Load previously downloaded bc digital road atlas ------------------------------

dra <- #road network polyline
  st_read(dsn = paste0(wd, "/Data/dgtl_road_atlas.gdb"),
          layer = "TRANSPORT_LINE")

type <- #data dictionary
  st_read(dsn = paste0(wd, "/Data/dgtl_road_atlas.gdb"),
          layer = "TRANSPORT_LINE_TYPE_CODE")


# type %>% 
# filter(
#   str_detect(DESCRIPTION, "arterial") |
#     str_detect(DESCRIPTION, "collector") |
#     str_detect(DESCRIPTION, "freeway") |
#     str_detect(DESCRIPTION, "highway") |
#     str_detect(DESCRIPTION, "local") |
#     str_detect(DESCRIPTION, "ramp") |
#     str_detect(DESCRIPTION, "strata")
# )
# 
# type %>% 
#   filter(!(
#     str_detect(DESCRIPTION, "arterial") |
#       str_detect(DESCRIPTION, "collector") |
#       str_detect(DESCRIPTION, "freeway") |
#       str_detect(DESCRIPTION, "highway") |
#       str_detect(DESCRIPTION, "local") |
#       str_detect(DESCRIPTION, "ramp") |
#       str_detect(DESCRIPTION, "strata")
#   )
#   )

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

st_write(dra_sub, dsn = paste0(wd, "/Processed Data/dra_subset.gpkg"))


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
  5 # size in m of overlap of boundaries for counting roads and crashes

roads_clip_da <-
  lapply(1:length(roads), function(x) {
    clip_linestring_by_poly(
      linestring = roads[[x]],
      clipping_polygon = bc_da,
      clipping_polygon_buffer_size = overlap,
      n_grid_cells = 1
    )
  })

bc_da_buff <- st_buffer(bc_da, dist = overlap,endCapStyle = "FLAT",joinStyle = "BEVEL") %>%
  select(dauid)


st_write(bc_da_buff, dsn = paste0(wd, "/Processed Data/da_buffer_2021.gpkg"))


# library(mapview)
# 
# # Example methodology
# 
# example_road <- roads_clip_da[[2]]  %>% filter(TRANSPORT_LINE_ID=="577") #four lane arterial road 
# 
# sel_sgbp <- st_intersects(x = bc_da_buff, y = example_road) #intersecting DAs for example road
# sel_logical <- lengths(sel_sgbp) > 0
# example_da_buff <- bc_da_buff[sel_logical, ]
# 
# example_da <- bc_da %>% filter(dauid %in%example_da_buff$dauid)
# 
# mapview(example_da_buff, z = "dauid") + mapview(example_da) + mapview::mapview(example_road ,color="red") # view one road segment cut by boundary for DAs
# 
# 
# 
#  example_road %>% 
#   group_by(TRANSPORT_LINE_ID) %>% 
#   mutate(road_id = row_number()) %>% 
#   st_join(., example_da_buff, join = st_intersects) %>% 
#   st_drop_geometry() %>% 
#   group_by(road_id,TRANSPORT_LINE_ID) %>% 
#   mutate(
#          intersecting_geo_units = n_distinct(dauid),
#          total_lane_length_m = length_m*TOTAL_NUMBER_OF_LANES,
#          adjusted_lane_length_m = total_lane_length_m/intersecting_geo_units
#          ) %>% 
#   select(TRANSPORT_LINE_ID,road_id,dauid,intersecting_geo_units,contains("LANE"),contains("length")) %>% 
#   arrange(TRANSPORT_LINE_ID ) %>% 
#   filter(TRANSPORT_LINE_ID=="577") %>% 
#   group_by(dauid) %>% 
#   summarise(length_m = sum(length_m),
#             total_lane_length_m = sum(total_lane_length_m),
#             adjusted_lane_length_m = sum(adjusted_lane_length_m)
#   )
# 
#  #one 4 lane arterial road is on the boundary of two DAs. 
#  # We calculate the total length of lanes by multiplying the length of the raod segment by the number of lanes. We then assign each DA an equal proportion of the lane lengths
#  # Here the length of the road segment is 49.92 m over 4 lanes is ~200m of lane length. Since it straddles two DAs we assign each one 2 lane lengths worth of road - 99.8m

 
roads_agg_da <- roads_clip_da %>%
  map(~mutate(.,road_id = row_number())) %>% #assign unique id for each new Road Segment fragment
  map(.,
    ~ st_join(., bc_da_buff, join = st_intersects) %>% # assign the dauid that overlaps with a given line segment
      st_drop_geometry() %>%
      group_by(road_id,TRANSPORT_LINE_ID) %>%
      mutate(intersecting_geo_units = n_distinct(dauid),# Count the number of distinct dauids that overlap with a road segment fragment 
             total_lane_length_m = length_m*TOTAL_NUMBER_OF_LANES, # Count road lengths for each lane int he road
             adjusted_lane_length_m = total_lane_length_m / intersecting_geo_units # Divide lane lengths equally for each overlapping dauid
             ) %>% 
      group_by(dauid) %>% 
      summarise(length_m = sum(length_m) %>% as.numeric(),
                total_lane_length_m = sum(total_lane_length_m) %>% as.numeric(),
                adjusted_lane_length_m = sum(adjusted_lane_length_m) %>% as.numeric()
                )
  ) %>%
  bind_rows(.id = "stratification") %>%
  mutate(
    stratification = case_when(
      stratification == "1" ~ "highway",
      stratification == "2" ~ "arterial",
      stratification == "3" ~ "collector",
      stratification == "4" ~ "local"
    )
  ) %>%
  pivot_wider(names_from = stratification,
              values_from = c(length_m,total_lane_length_m,adjusted_lane_length_m),
              values_fill = 0) %>%
  mutate(total_roads_m = length_m_highway + length_m_arterial+ length_m_collector + length_m_local,
         total_lane_length_m =  total_lane_length_m_highway +  total_lane_length_m_arterial+  total_lane_length_m_collector +  total_lane_length_m_local,
         adjusted_lane_length_m =  adjusted_lane_length_m_highway +  adjusted_lane_length_m_arterial+  adjusted_lane_length_m_collector +  adjusted_lane_length_m_local
         )


bc_da_roads <- bc_da %>%
  left_join(roads_agg_da, by = "dauid") %>%
  replace_na(list(
    length_m_highway = 0,
    length_m_arterial = 0,
    length_m_collector = 0,
    length_m_local = 0,
    total_roads_m = 0,
    
    total_lane_length_m_highway = 0,
    total_lane_length_m_arterial = 0,
    total_lane_length_m_collector = 0,
    total_lane_length_m_local = 0,
    total_lane_length_m = 0,
    
    adjusted_lane_length_m_highway = 0,
    adjusted_lane_length_m_arterial = 0,
    adjusted_lane_length_m_collector = 0,
    adjusted_lane_length_m_local = 0,
    adjusted_lane_length_m = 0
    
    
  ))


st_write(bc_da_roads, dsn = paste0(wd, "/Processed Data/da_v1_2021.gpkg"))

end_time <- Sys.time()
end_time - start_time
