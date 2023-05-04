# -----------------------------------------------------------------------------
# Script Name: 1B_Data_Processing_Dissemination_Area_Geography.R
# Description: Join CANUE Data to DA Geography
# Author: Michael Branion-Calles
# Date Created: 2022-11-01
# Last Modified: 2023-04-28
# -----------------------------------------------------------------------------

# Load required libraries

library(sf)
library(readxl)
library(tidyverse)

wd <- getwd()

bc_da <- st_read(paste0(wd,"/Processed Data/bc_da.gpkg"))


#Canadian Marginalization Index
cmg <- read_xlsx(paste0(wd,"/Data/cmg_da_16.xlsx"),sheet = 5) %>% 
  mutate(GeoUID = as.character(DA16UID)) %>% 
  filter(Province=="British Columbia") %>% 
  rename_all(~ gsub("_DA16", "", .)) %>% 
  select(GeoUID,households_dwellings,material_resources,age_labourforce,immigration_vismin) %>% 
  rename_with(~ paste0("cmg_", .), -GeoUID) %>% 
  mutate(cmg_households_dwellings_quintile = ntile(cmg_households_dwellings ,5),
         cmg_material_resources_quintile = ntile(cmg_material_resources ,5),
         cmg_age_labourforce_quintile = ntile(cmg_age_labourforce ,5),
         cmg_immigration_vismin_quintile = ntile(cmg_immigration_vismin ,5),
         
  )

bc_da <- bc_da %>% left_join(cmg,by="GeoUID")

#Canadian Active Living Environment - Can-ALE

ale <- read_csv(paste0(wd,"/Data/ale_a_16.csv")) %>% 
  mutate(GeoUID = as.character(ale16_01)) %>% 
  select(-province,-postalcode16,-ale16_01) %>% 
  distinct() %>% 
  rename(
         int_density = ale16_02,
         dwelling_density = ale16_03,
         int_density_z = ale16_04,
         dwelling_density_z = ale16_05,
         ale_index = ale16_06,
         ale_class = ale16_07,
         n_poi = ale16_08,
         n_poi_z = ale16_09,
         n_transit_stops = ale16_10,
         n_transit_stops_z = ale16_11,
         ale_transit_index = ale16_12,
         ale_transit_class = ale16_13)
         
bc_da <- bc_da %>% left_join(ale,by="GeoUID")

#Canadian Active Living Environment - Can-ALE


bics <- read_csv(paste0(wd,"/Data/nhbic_ava_21.csv")) %>% 
  mutate(GeoUID = as.character(nhbic21_01)) %>% 
  select(-province,-postalcode21,-nhbic21_01) %>% 
  distinct() %>% 
  rename(bike_to_work_rate = nhbic21_04,
         sust_transport_to_work_rate = nhbic21_05,
         km_high_comfort_bike_infra = nhbic21_06,
         km_med_comfort_bike_infra = nhbic21_07,
         km_low_comfort_bike_infra = nhbic21_08,
         canbics_index = nhbic21_09,
         canbics_class = nhbic21_10
         )

bc_da <- bc_da %>% left_join(bics,by="GeoUID")

st_write(bc_da,dsn = paste0(wd,"/Processed Data/bc_da.gpkg"),append = FALSE)


