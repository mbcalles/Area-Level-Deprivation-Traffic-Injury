

library(sf)
library(tidyverse)
library(cancensus)
library(mapview)

wd <- getwd()


source(file = paste0(wd,"/R/functions.R"))

# Dissemination Area ------------------------------------------------------


bc_da <- get_census(dataset='CA16', regions=list(PR="59"),vectors = c("v_CA16_2570",
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

### Deprivation Indices at DA Level

bc_da <- bc_da %>% 
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



summary(bc_da$bc_dix)

mapview::mapview(bc_da,z="bc_dix")

dra <- st_read(dsn = paste0(wd,"/Data/dgtl_road_atlas.gdb"),layer = "TRANSPORT_LINE")
type <- st_read(dsn = paste0(wd,"/Data/dgtl_road_atlas.gdb"),layer = "TRANSPORT_LINE_TYPE_CODE")

dra_sub <- dra %>%  
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





roads <- list(highway = dra_highway,
              arterial = dra_arterial,
              collector = dra_collector,
              local = dra_local
              )

ptm <- proc.time()

roads_clip_da <- lapply(1:length(roads), function(x) clip_linestring_by_poly(linestring = roads[[x]],
                                                                          clipping_polygon = bc_da,
                                                                          clipping_polygon_buffer_size = 5,
                                                                          n_grid_cells = 1000)
                     )

proc.time() - ptm

library(purrr)

roads_agg_da <- roads_clip_da %>% 
  map(.,~st_join(.,bc_da, join = st_intersects) %>% 
        st_drop_geometry() %>% 
        group_by(GeoUID) %>% 
        summarise(length_m = sum(length_m))
  )  %>% 
  bind_rows(.id = "strata") %>% 
  mutate(strata = case_when(strata=="1"~"highway_m",
                            strata=="2"~"arterial_m",
                            strata=="3"~"collector_m",
                            strata=="4"~"local_m"),
         length_m = as.numeric(length_m)) %>% 
  pivot_wider(names_from = strata,
              values_from = length_m,
              values_fill = 0) %>% 
  mutate(total_roads_m = highway_m + arterial_m + collector_m + local_m)

bc_da <- bc_da %>% 
  left_join(roads_agg_da,by="GeoUID") %>% 
  replace_na(list(highway_m = 0, arterial_m = 0, collector_m = 0, local_m = 0,total_roads_m=0))


st_write(bc_da,dsn = paste0(wd,"/Processed Data/bc_da.gpkg"))



# Census Tract ------------------------------------------------------------



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
level='CT', use_cache = FALSE,
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

bc_ct <- bc_ct %>% 
  mutate(z_no_highschool_prevalance_w = scale(no_highschool_prevalance),
         z_university_degree_prevalance_w = scale(university_degree_prevalance*-1),
         z_unemployment_rate_w = scale(unemployment_rate),
         z_lone_parent_fam_prevalence_w = scale(lone_parent_fam_prevalence),
         z_hh_avg_income_w = scale(hh_avg_income*-1),
         z_home_owner_prevalence_w = scale(home_owner_prevalence*-1),
         z_participation_rate_w = scale(participation_rate*-1),
         bc_dix = as.numeric(z_hh_avg_income_w*0.089 + 
                               z_home_owner_prevalence_w*0.089 + 
                               z_lone_parent_fam_prevalence_w*0.143 + 
                               z_no_highschool_prevalance_w*0.25 + z_university_degree_prevalance_w*0.179 + 
                               z_unemployment_rate_w*0.214 + z_participation_rate_w*0.036))

bc_ct %>% 
  st_drop_geometry() %>% 
  group_by(CSD_UID,`Region Name`) %>% 
  summarise(n = n(),
            nas = sum(is.na(bc_dix)),
            mean_bc_dix = mean(bc_dix,na.rm=TRUE)
  ) %>% 
  arrange(desc(mean_bc_dix)) %>% View()

summary(bc_ct$bc_dix)

bc_ct %>% filter(GeoUID=="9330251.02")

mapview::mapview(bc_ct,z="bc_dix")

ptm <- proc.time()

roads_clip_ct <- lapply(1:length(roads), function(x) clip_linestring_by_poly(linestring = roads[[x]],
                                                                             clipping_polygon = bc_ct,
                                                                             clipping_polygon_buffer_size = 5,
                                                                             n_grid_cells = 1000)
)

proc.time() - ptm

library(purrr)

roads_agg_ct<- roads_clip_ct %>% 
  map(.,~st_join(.,bc_ct, join = st_intersects) %>% 
        st_drop_geometry() %>% 
        group_by(GeoUID) %>% 
        summarise(length_m = sum(length_m))
  )  %>% 
  bind_rows(.id = "strata") %>% 
  mutate(strata = case_when(strata=="1"~"highway_m",
                            strata=="2"~"arterial_m",
                            strata=="3"~"collector_m",
                            strata=="4"~"local_m"),
         length_m = as.numeric(length_m)) %>% 
  pivot_wider(names_from = strata,
              values_from = length_m,
              values_fill = 0) %>% 
  mutate(total_roads_m = highway_m + arterial_m + collector_m + local_m)

bc_ct <- bc_ct %>% 
  left_join(roads_agg_da,by="GeoUID") %>% 
  replace_na(list(highway_m = 0, arterial_m = 0, collector_m = 0, local_m = 0,total_roads_m=0))



st_write(bc_ct,dsn = paste0(wd,"/Processed Data/bc_da.gpkg"))
