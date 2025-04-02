# Script Title: 03_assign_deprivation_measures.R
# Author: Michael Branion-Calles
# Date: March 2025
# Description: This script loads Census Geography for British Columbia, calculates various deprivation measures, 
# and merges these with existing geographic data.

# Load required libraries
library(tidyverse)   # For data manipulation and visualization
library(cancensus)   # For accessing Canadian census data
library(sf)          # For handling spatial data
library(readxl)      # For reading Excel files
library(janitor)
# Get the current working directory
wd <- getwd()

# Load previously processed Census geography data
bc_da <- st_read(paste0(wd, "/Processed Data/da_v3_2021c.gpkg")) 

bc_da_census <- read_csv(paste0(wd, "/Processed Data/census_2021.csv")) %>% 
  rename(dauid = geo_name) %>% 
  mutate(dauid = as.character(dauid))

bc_gaf <- read_csv(paste0(wd, "/Data/census geography/census_gaf/2021_92-151_X_utf.csv")) %>% 
  clean_names() %>% 
  filter(pruid_pridu==59) %>% 
  select(dauid_adidu,ctuid_sridu,cdname_drnom, cduid_dridu,csdname_sdrnom,csduid_sdridu,
         cmaname_rmrnom,cmauid_rmridu, cmatype_rmrgenre
         ) %>% 
  rename_with(~ sub("_.*", "", .)) %>%   # Remove everything after and including the underscore
  distinct() %>% 
  mutate(
    dauid = as.character(dauid),
    cmatype_name = case_when(
    cmatype == 'B' ~ 'Census metropolitan area (CMA)',
    cmatype == 'D' ~ 'Census agglomeration (CA) that is not tracted',
    cmatype == 'G' ~ 'Strong metropolitan influenced zone',
    cmatype == 'H' ~ 'Moderate metropolitan influenced zone',
    cmatype == 'I' ~ 'Weak metropolitan influenced zone',
    cmatype == 'J' ~ 'No metropolitan influenced zone',
    cmatype == 'K' ~ 'Census agglomeration (CA) that is tracted'))



# Load Deprivation Indices, Remoteness Index and Calculate VANDIX from Census Variables
cmdi <- st_read(paste0(wd, "/Data/multiple_deprivation_2021.gdb"), layer = "multiple_deprivation_DA" ) %>% 
  clean_names() %>% 
  st_drop_geometry() %>% 
  select(dauid, contains("Scores"), contains("Quintiles"))

msdi <- # Load Social and Material Deprivation Index
  read_xlsx(path = paste0(wd, "/Data/MSDI_2021.xlsx"), sheet = 1) %>%
  clean_names() %>% 
  rename(dauid = da) %>%
  mutate(dauid = as.character(dauid)) %>%
  select(dauid, contains("score"), contains("quint"))



# # Remoteness Index
# ri <-
#   read_csv(paste0(wd, "/Data/RI/2021IR_DATABASE.csv")) %>% 
#   filter(Pruid == 59) %>% 
#   mutate(CSD_UID = as.character(CSDuid),
#          remote_index = as.numeric(Index_of_remoteness),
#          CSDpop2016 = as.numeric(CSDpop2021)) %>% 
#   select(CSD_UID, remote_index,CSDpop2021) %>% 
#   ggplot2::remove_missing() %>% 
#   rename(csd_pop_2021=CSDpop2021) %>% 
#   clean_names()
# 
# # Different numbers of clusters, iterations and seed replacement methods are available 
# #  in the k-means clustering algorithm. Therefore, to find the most suitable model for a given dataset, 
# #  different k‑means clustering methods were applied to develop 3 to 10 clusters with one to five iterations, 
# # with both full and random seed replacement methods. 
# #  The most suitable model yielded five clusters when five iterations 
# #  and a full seed replacement method were selected (Table 5).
# library(factoextra)
# library(cluster)
# 
# fviz_nbclust(ri[,"remote_index"], kmeans, method = "wss")
# 
# #make this example reproducible
# set.seed(1)
# 
# 
# #perform k-means clustering with k = 3 clusters
# km3 <- kmeans(ri[,"remote_index"], centers = 3, nstart = 25)
# ri$cluster3 <- km3$cluster
# ri_class3 <- ri %>% 
#   group_by(cluster3) %>% 
#   summarise(n = n(),
#             mean_ri = mean(remote_index),
#             mean_pop = mean(csd_pop_2021,na.rm=TRUE)) %>% 
#   arrange(mean_ri) %>% 
#   mutate(remote_index_class3 = row_number())
# ri_class3 
# 
# ri_class3 <- ri_class3%>% 
#   select(cluster3,remote_index_class3)
# 
# 
# 
# #perform k-means clustering with k = 5 clusters
# km5 <- kmeans(ri[,"remote_index"], centers = 5, nstart = 25)
# ri$cluster5 <- km5$cluster
# ri_class5 <- ri %>% 
#   group_by(cluster5) %>% 
#   summarise(n = n(),
#             mean_ri = mean(remote_index),
#             mean_pop = mean(csd_pop_2021,na.rm=TRUE)) %>% 
#   arrange(mean_ri) %>% 
#   mutate(remote_index_class5 = row_number())
# ri_class5 
# ri_class5 <- ri_class5%>% 
#   select(cluster5,remote_index_class5)
# 
# ri <- ri %>% left_join(ri_class5) %>% 
#   mutate(remote_index_class5 = case_when(remote_index_class5 == 1 ~ "1 - Easily Accessible",
#                                         remote_index_class5 == 2 ~ "2 - Accessible",
#                                         remote_index_class5 == 3 ~ "3 - Less Accessible",
#                                         remote_index_class5 == 4 ~ "4 - Remote",
#                                         remote_index_class5 == 5 ~ "5 - Very Remote",
#                                         )) %>% 
#   left_join(ri_class3) %>% 
#   mutate(remote_index_class3 = case_when(remote_index_class3 == 1 ~ "1 - Accessible",
#                                         remote_index_class3 == 2 ~ "2 - Less Accessible",
#                                         remote_index_class3 == 3 ~ "3 - Remote")
#   ) %>% 
#   select(csd_uid,remote_index,remote_index_class3,remote_index_class5)
# 


bc_da <- bc_da %>%
  left_join(bc_gaf, by = "dauid") %>% 
  left_join(bc_da_census, by = "dauid") %>% 
  left_join(cmdi, by = "dauid") %>% #Join Deprivation Indices to DA geography
  left_join(msdi, by = "dauid") 


st_write(bc_da, dsn = paste0(wd, "/Processed Data/da_v4_2021c.gpkg"))
