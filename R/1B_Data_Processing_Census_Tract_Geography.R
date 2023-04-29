# -----------------------------------------------------------------------------
# Script Name: 1B_Data_Processing_Dissemination_Area_Geography.R
# Description: Load Census Variables at CT Level Geography, Calculate Deprivation Indices, Aggregate Road Network Data, Aggregate Claims Data 
# Author: Michael Branion-Calles
# Date Created: 2023-04-27
# Last Modified: 2023-04-28
# -----------------------------------------------------------------------------

# Load required libraries

library(sf)
library(tidyverse)
library(cancensus)
library(readxl)
library(purrr)
library(mapview)

wd <- getwd()
source(file = paste0(wd,"/R/functions.R"))


# Dissemination Area ------------------------------------------------------

########### Load Census Geography and Relevant Census Variables

bc_ct <- get_census(dataset='CA16', regions=list(PR="59"),vectors = c("v_CA16_2570",
                                                                      "v_CA16_4986", #average income,
                                                                      "v_CA16_4837", #home owners
                                                                      "v_CA16_4838", #renters
                                                                      "v_CA16_4836", #total private households by tenure
                                                                      "v_CA16_3957","v_CA16_3954", #vis minority
                                                                      "v_CA16_3432","v_CA16_3405", #recent immigrant
                                                                      "v_CA16_3855","v_CA16_3852", #Aboriginal identity
                                                                      "v_CA16_5054","v_CA16_5060","v_CA16_5051", #highschool diploma
                                                                      "v_CA16_5618", #unemployment rate
                                                                      "v_CA16_5612", #participation rate
                                                                      "v_CA16_488","v_CA16_484", #single parent families
                                                                      "v_CA16_475","v_CA16_472","v_CA16_469","v_CA16_451",#marital status
                                                                      "v_CA16_419","v_CA16_418" #% living alone  
),
level='DA', use_cache = FALSE,
geo_format = "sf",
quiet=TRUE) %>% 
  st_transform(crs = 3005) %>% #BC Albers
  rename(lico_at = `v_CA16_2570: Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)`,
         hh_avg_income = `v_CA16_4986: Average after-tax income of households in 2015 ($)`,
         home_owner = `v_CA16_4837: Owner`,
         renter = `v_CA16_4838: Renter`,
         home_owner_renter_denom = `v_CA16_4836: Total - Private households by tenure - 25% sample data`,
         vis_min = `v_CA16_3957: Total visible minority population`,
         vis_min_denom = `v_CA16_3954: Total - Visible minority for the population in private households - 25% sample data`,
         recent_imm = `v_CA16_3432: 2011 to 2016`,
         recent_imm_denom = `v_CA16_3405: Total - Immigrant status and period of immigration for the population in private households - 25% sample data`,
         aboriginal_id = `v_CA16_3855: Aboriginal identity`,
         aboriginal_id_denom = `v_CA16_3852: Total - Aboriginal identity for the population in private households - 25% sample data`,
         no_highschool = `v_CA16_5054: No certificate, diploma or degree`,
         university_degree = `v_CA16_5060: Postsecondary certificate, diploma or degree`,
         no_highschool_denom = `v_CA16_5051: Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households - 25% sample data`,
         unemployment_rate = `v_CA16_5618: Unemployment rate`,
         participation_rate = `v_CA16_5612: Participation rate`,
         lone_parent_fam = `v_CA16_488: Total lone-parent families by sex of parent`,
         lone_parent_fam_denom = `v_CA16_484: Total number of census families in private households - 100% data`,
         seperated = `v_CA16_469: Separated`,
         divorced = `v_CA16_472: Divorced`,
         widowed = `v_CA16_475: Widowed`,
         sep_div_wid_denom = `v_CA16_451: Total - Marital status for the population aged 15 years and over - 100% data`,
         living_alone = `v_CA16_419: 1 person`,
         living_alone_denom = `v_CA16_418: Private households by household size`) %>% 
  mutate(pop_den_per_10k = Population/`Shape Area`/10000,
         urban_rural = ifelse(is.na(CMA_UID),"Rural","Urban"),
         lico_at_quintile = ntile(lico_at,5),
         hh_avg_income_quintile = rev(ntile(hh_avg_income,5)),#reverse coded so 5th quintile is lowest income
         home_owner_prevalence = home_owner/home_owner_renter_denom,
         home_owner_quintile = ntile(home_owner_prevalence,5),
         vis_min_prevalance = vis_min/vis_min_denom,
         vis_min_quintile = ntile(vis_min_prevalance,5),
         recent_imm_prevalance = recent_imm/recent_imm_denom,
         recent_imm_quintile =  ntile(recent_imm_prevalance,5),
         aboriginal_id_prevalance = aboriginal_id/aboriginal_id_denom,
         aboriginal_id_quintile = ntile(aboriginal_id_prevalance,5),      
         no_highschool_prevalance = no_highschool/no_highschool_denom,
         no_highschool_quintile = ntile(no_highschool_prevalance,5),
         university_degree_prevalance = university_degree/no_highschool_denom,
         university_degree_quintile = ntile(university_degree_prevalance,5),
         unemployment_rate_quintile = ntile(unemployment_rate,5),
         lone_parent_fam_prevalence = lone_parent_fam/lone_parent_fam_denom,
         lone_parent_fam_quintile = ntile(lone_parent_fam_prevalence,5),
         sep_div_wid_prevalence = (seperated + divorced + widowed)/sep_div_wid_denom,
         sep_div_wid_quintile = ntile(sep_div_wid_prevalence,5),
         living_alone_prevalence = living_alone/living_alone_denom,
         living_alone_quintile = ntile(living_alone_prevalence,5))

########### Load Deprivation Indices, Remoteness Index and Calculate VANDIX from Census Variables

ri <- read_csv(paste0(wd,"/Data/remoteness_index.csv")) %>% #Remoteness Index
  mutate(CSD_UID = as.character(CSDuid))

bc_ct <- bc_ct %>% #Calculate VanDIX score
  mutate(z_no_highschool_prevalance_w = scale(no_highschool_prevalance),
         z_university_degree_prevalance_w = scale(university_degree_prevalance*-1),
         z_unemployment_rate_w = scale(unemployment_rate),
         z_lone_parent_fam_prevalence_w = scale(lone_parent_fam_prevalence),
         z_hh_avg_income_w = scale(hh_avg_income*-1),
         z_home_owner_prevalence_w = scale(home_owner_prevalence*-1),
         z_participation_rate_w = scale(participation_rate*-1),
         #VANDIX - Area Level Deprivation Index
         bc_dix = as.numeric(z_hh_avg_income_w*0.089 + 
           z_home_owner_prevalence_w*0.089 + 
           z_lone_parent_fam_prevalence_w*0.143 + 
           z_no_highschool_prevalance_w*0.25 + z_university_degree_prevalance_w*0.179 + 
           z_unemployment_rate_w*0.214 + z_participation_rate_w*0.036))

bc_ct <- left_join(bc_ct,ri,by = "CSD_UID")


########### Load BC Digital Road Atlas (Road network)

dra <- st_read(dsn = paste0(wd,"/Data/dgtl_road_atlas.gdb"),layer = "TRANSPORT_LINE")
type <- st_read(dsn = paste0(wd,"/Data/dgtl_road_atlas.gdb"),layer = "TRANSPORT_LINE_TYPE_CODE")

dra_sub <- dra %>%  #subset road network
  left_join(type,by = "TRANSPORT_LINE_TYPE_CODE") %>% 
  filter(str_detect(DESCRIPTION ,"arterial")|
           str_detect(DESCRIPTION ,"collector")|
           str_detect(DESCRIPTION ,"freeway")|
           str_detect(DESCRIPTION ,"highway")|
           str_detect(DESCRIPTION ,"local")|
           str_detect(DESCRIPTION ,"ramp")|
           str_detect(DESCRIPTION ,"strata")) 

dra_highway <- dra_sub %>% 
  filter(str_detect(DESCRIPTION ,"freeway|highway|ramp")) 

dra_arterial <- dra_sub %>% 
  filter(str_detect(DESCRIPTION ,"arterial"))

dra_collector <- dra_sub %>% 
  filter(str_detect(DESCRIPTION ,"collector"))

dra_local <- dra_sub %>% 
  filter(str_detect(DESCRIPTION ,"local|strata"))

#Create list of road networks of a specific classification

roads <- list(highway = dra_highway,
              arterial = dra_arterial,
              collector = dra_collector,
              local = dra_local
              )


########### Split lines by the boundary of the census geography - use a 5 m buffer to enable double counting of roads that split boundaries of DAs
ptm <- proc.time()

overlap <- 10 #size in m of overlap of CT Boundaries for counting roads and crashes

roads_clip_ct <- lapply(1:length(roads), function(x) clip_linestring_by_poly(linestring = roads[[x]],
                                                                          clipping_polygon = bc_ct,
                                                                          clipping_polygon_buffer_size = overlap,
                                                                          n_grid_cells = 1000)
                     )

proc.time() - ptm

########### Sum up the length of roads that intersect with DA boundary geography


bc_ct_buff <- st_buffer(bc_ct,dist = overlap) %>%
  select(GeoUID)

roads_agg_ct <- roads_clip_ct %>% 
  map(.,~st_join(.,bc_ct_buff, join = st_intersects) %>% #assign the DAUID that overlaps with a given line segment 
        st_drop_geometry() %>% 
        group_by(GeoUID) %>% 
        summarise(length_m = sum(length_m))
  )  %>% 
  bind_rows(.id = "stratification") %>% 
  mutate(stratification = case_when(stratification=="1"~"highway_m",
                            stratification=="2"~"arterial_m",
                            stratification=="3"~"collector_m",
                            stratification=="4"~"local_m"),
         length_m = as.numeric(length_m)) %>% 
  pivot_wider(names_from = stratification,
              values_from = length_m,
              values_fill = 0) %>% 
  mutate(total_roads_m = highway_m + arterial_m + collector_m + local_m)

bc_ct <- bc_ct %>% 
  left_join(roads_agg_ct,by="GeoUID") %>% 
  replace_na(list(highway_m = 0, arterial_m = 0, collector_m = 0, local_m = 0,total_roads_m=0))

########### Load Claims Crash Data - Aggregate to Census Geography

constant <- 1000

claims <- read_csv(file = paste0(wd,"/Data/ICBC_reported_crashes_Full_Data_data.csv")) %>% 
  filter(!is.na(Latitude)) %>% 
  mutate(id = row_number())

claims_sf <- st_as_sf(claims,coords = c("Longitude","Latitude"),crs = 4326) %>% 
  st_transform(crs = 3005)



claims_ct_join <- st_join(claims_sf,bc_ct_buff,join = st_intersects) %>% as_tibble()

claims_ct_duplicated <- claims_ct_join %>% 
  group_by(id) %>% 
  summarise(n_cts = n()) %>% 
  mutate(crash_weight = 1/n_cts) #assign weight to crash that occurs on border of DAs


claims_ct_duplicated_casualty <- claims_ct_join %>% 
  filter(`Crash Severity` == "CASUALTY") %>% 
  group_by(id) %>% 
  summarise(n_cts = n()) %>% 
  mutate(casualty_crash_weight = 1/n_cts) #crash weight for injury crashes

claims_ct_duplicated <- left_join(claims_ct_duplicated,claims_ct_duplicated_casualty,by=c("id","n_cts")) %>% 
  replace_na(list(casualty_crash_weight = 0))

claims_ct_join <- claims_ct_join %>% left_join(claims_ct_duplicated,by = "id") %>% 
  mutate(weighted_crashes = `Crash Count`*crash_weight,
         weighted_casualty_crashes =  `Crash Count`*casualty_crash_weight)


total_crashes_by_ct <- claims_ct_join %>% 
  group_by(GeoUID) %>% 
  summarise(n_claims = sum(weighted_crashes),
            n_casualty_claims = sum(weighted_casualty_crashes)
  ) 

total_bicycle_crashes_by_ct <- claims_ct_join %>% 
  filter(`Cyclist Flag`=="Yes") %>% 
  group_by(GeoUID) %>% 
  summarise(n_cyclist_claims = sum(weighted_crashes),
            n_cyclist_casualty_claims = sum(weighted_casualty_crashes)
  ) 

total_pedestrian_crashes_by_ct <- claims_ct_join %>% 
  filter(`Pedestrian Flag`=="Yes") %>% 
  group_by(GeoUID) %>% 
  summarise(n_ped_claims = sum(weighted_crashes),
            n_ped_casualty_claims = sum(weighted_casualty_crashes)
  ) 

wide_crashes_ct <- left_join(total_crashes_by_ct,total_bicycle_crashes_by_ct,by="GeoUID",) %>%
  left_join(total_pedestrian_crashes_by_ct,by="GeoUID")

bc_ct <- bc_ct %>% 
  left_join(wide_crashes_ct, by="GeoUID")%>% 
  replace_na(list(n_claims = 0, n_cyclist_claims = 0, n_ped_claims = 0,
                  n_casualty_claims = 0, n_cyclist_casualty_claims = 0, n_ped_casualty_claims = 0)) %>% 
  mutate(pop_inc_rate_overall = n_claims/Population*constant,
         pop_inc_rate_cyclists = ceiling(n_cyclist_claims)/Population*constant,
         pop_inc_rate_peds = ceiling(n_ped_claims)/Population*constant,
         pop_casualty_inc_rate_overall = n_casualty_claims/Population*constant,
         pop_casualty_inc_rate_cyclists = ceiling(n_cyclist_casualty_claims)/Population*constant,
         pop_casualty_inc_rate_peds = ceiling(n_ped_casualty_claims)/Population*constant,
         ntwrk_inc_rate_overall = n_claims/total_roads_m*constant,
         ntwrk_inc_rate_cyclists = ceiling(total_roads_m)/total_roads_m*constant,
         ntwrk_inc_rate_peds = ceiling(n_ped_claims)/total_roads_m*constant,
         ntwrk_casualty_inc_rate_overall = n_casualty_claims/total_roads_m*constant,
         ntwrk_casualty_inc_rate_cyclists = ceiling(n_cyclist_casualty_claims)/total_roads_m*constant,
         ntwrk_casualty_inc_rate_peds = ceiling(n_ped_casualty_claims)/total_roads_m*constant
  )





st_write(bc_ct,dsn = paste0(wd,"/Processed Data/bc_ct.gpkg"))

