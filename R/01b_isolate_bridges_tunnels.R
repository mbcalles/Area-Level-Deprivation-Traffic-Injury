library(sf)
library(tidyverse)
# Read spatial data for dissemination areas in British Columbia
bc_boundary <- st_read(
  "C:/Users/micha/Documents/GitHub/Area-Level-Deprivation-Traffic-Injury/Processed Data/da_v4_2021.gpkg"
) %>% 
  st_union()
# Read in road network data, focusing on bridges
dra <- #road network polyline
  st_read(dsn = "C:/Users/micha/Documents/GitHub/Area-Level-Deprivation-Traffic-Injury/Processed Data/dra_subset.gpkg")
# Filter road network for bridges and tunnels
bridges <- dra %>% filter(TRANSPORT_LINE_STRUCTURE_CODE=="B"|TRANSPORT_LINE_STRUCTURE_CODE=="T")
# Keep bridges outside BC boundary
bridges_sub <- st_difference(bridges, bc_boundary)

st_write(bridges_sub, dsn = "C:/Users/micha/Documents/GitHub/Area-Level-Deprivation-Traffic-Injury/Processed Data/dra_briges_tunnels.gpkg")
