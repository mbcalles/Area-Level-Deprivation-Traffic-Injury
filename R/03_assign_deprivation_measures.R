# Script Title: 03_assign_deprivation_measures.R
# Author: Michael Branion-Calles
# Date: September 2024
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
bc_da <- st_read(paste0(wd, "/Processed Data/da_v2_2021.gpkg")) %>% 
  clean_names()

# Retrieve census data for British Columbia (Province code: 59) for various socio-economic variables
da_vars <- get_census(
  dataset = 'CA16',                  # Use the 2016 Census dataset
  regions = list(PR = "59"),         # Specify the region (British Columbia)
  vectors = c(
    "v_CA16_2570", "v_CA16_4986", "v_CA16_4837", "v_CA16_4838", 
    "v_CA16_4836", "v_CA16_3957", "v_CA16_3954", "v_CA16_3432", 
    "v_CA16_3405", "v_CA16_3855", "v_CA16_3852", "v_CA16_5054", 
    "v_CA16_5060", "v_CA16_5051", "v_CA16_5618", "v_CA16_5612", 
    "v_CA16_488", "v_CA16_484", "v_CA16_475", "v_CA16_472", 
    "v_CA16_469", "v_CA16_451", "v_CA16_419", "v_CA16_418", 
    "v_CA16_5792", "v_CA16_5795", "v_CA16_5798", "v_CA16_5801", 
    "v_CA16_5804", "v_CA16_5807", "v_CA16_5810"
  ),
  level = 'DA',                        # Data at Dissemination Area level
  use_cache = FALSE,                  # Do not use cached data
  geo_format = NA,                    # Use default geographical format
  quiet = TRUE                        # Suppress output messages
) %>%
  rename(
    lico_at = `v_CA16_2570: Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)`,
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
    living_alone_denom = `v_CA16_418: Private households by household size`,
    jtw_total = `v_CA16_5792: Total - Main mode of commuting for the employed labour force aged 15 years and over in private households with a usual place of work or no fixed workplace address - 25% sample data`,
    jtw_driver = `v_CA16_5795: Car, truck, van - as a driver`,
    jtw_passenger = `v_CA16_5798: Car, truck, van - as a passenger`,
    jtw_transit = `v_CA16_5801: Public transit`,
    jtw_walk = `v_CA16_5804: Walked`,
    jtw_bicycle = `v_CA16_5807: Bicycle`,
    jtw_other = `v_CA16_5810: Other method`
  ) %>%
  # Compute various metrics and categorize into quintiles
  mutate(
    urban_rural = ifelse(CMA_UID == "", "Rural", "Urban"), # Determine if the area is urban or rural
    lico_at_quintile = ntile(lico_at, 5), # Quintile rank for low-income prevalence
    hh_avg_income_quintile = ntile(hh_avg_income * -1, 5), # Quintile rank for average income (inverted)
    home_owner_prevalence = home_owner / home_owner_renter_denom, # Homeownership prevalence
    home_owner_quintile = ntile(home_owner_prevalence * -1, 5), # Quintile rank for homeownership prevalence (inverted)
    vis_min_prevalance = vis_min / vis_min_denom, # Visible minority prevalence
    vis_min_quintile = ntile(vis_min_prevalance, 5), # Quintile rank for visible minority prevalence
    recent_imm_prevalance = recent_imm / recent_imm_denom, # Recent immigrant prevalence
    recent_imm_quintile = ntile(recent_imm_prevalance, 5), # Quintile rank for recent immigrant prevalence
    aboriginal_id_prevalance = aboriginal_id / aboriginal_id_denom, # Aboriginal identity prevalence
    aboriginal_id_quintile = ntile(aboriginal_id_prevalance, 5), # Quintile rank for Aboriginal identity prevalence
    no_highschool_prevalance = no_highschool / no_highschool_denom, # No high school diploma prevalence
    no_highschool_quintile = ntile(no_highschool_prevalance, 5), # Quintile rank for no high school diploma prevalence
    university_degree_prevalance = university_degree / no_highschool_denom, # University degree prevalence
    university_degree_quintile = ntile(university_degree_prevalance * -1, 5), # Quintile rank for university degree prevalence (inverted)
    unemployment_rate_quintile = ntile(unemployment_rate, 5), # Quintile rank for unemployment rate
    participation_rate_quintile = ntile(participation_rate * -1, 5), # Quintile rank for participation rate (inverted)
    lone_parent_fam_prevalence = lone_parent_fam / lone_parent_fam_denom, # Lone-parent family prevalence
    lone_parent_fam_quintile = ntile(lone_parent_fam_prevalence, 5), # Quintile rank for lone-parent family prevalence
    sep_div_wid_prevalence = (seperated + divorced + widowed) / sep_div_wid_denom, # Separated, divorced, and widowed prevalence
    sep_div_wid_quintile = ntile(sep_div_wid_prevalence, 5), # Quintile rank for separated, divorced, and widowed prevalence
    living_alone_prevalence = living_alone / living_alone_denom, # Living alone prevalence
    living_alone_quintile = ntile(living_alone_prevalence, 5), # Quintile rank for living alone prevalence
    # Calculate standardized (z) scores for VanDIX score
    z_no_highschool_prevalance_w = scale(no_highschool_prevalance),
    z_university_degree_prevalance_w = scale(university_degree_prevalance * -1),
    z_unemployment_rate_w = scale(unemployment_rate),
    z_lone_parent_fam_prevalence_w = scale(lone_parent_fam_prevalence),
    z_hh_avg_income_w = scale(hh_avg_income * -1),
    z_home_owner_prevalence_w = scale(home_owner_prevalence * -1),
    z_participation_rate_w = scale(participation_rate * -1),
    # Compute VanDIX (Area Level Deprivation Index) score
    vandix = as.numeric(
      z_hh_avg_income_w * 0.089 +
        z_home_owner_prevalence_w * 0.089 +
        z_lone_parent_fam_prevalence_w * 0.143 +
        z_no_highschool_prevalance_w * 0.25 +
        z_university_degree_prevalance_w * 0.179 +
        z_unemployment_rate_w * 0.214 +
        z_participation_rate_w * 0.036
    ),
    vandix_quintile = ntile(vandix, 5) # Quintile rank for VanDIX score
  ) %>%
  select(-Type, -Households, -CD_UID, -Dwellings, -Population, -CSD_UID, -CMA_UID, -CT_UID) %>% # Drop unnecessary columns
  clean_names()


cd <- cancensus::get_census("CA16",regions = list(PR=59),level = "CD") %>% 
  select(GeoUID,`Region Name`) %>% 
  rename(CD_UID = GeoUID,
         "Census Division" =  `Region Name`) %>% 
  clean_names()


csd <- cancensus::get_census("CA16",regions = list(PR=59),level = "CSD") %>% 
  select(GeoUID,`Region Name`) %>% 
  rename(CSD_UID = GeoUID,
         "Census Subdivision" =  `Region Name`) %>% 
  clean_names()



# Load Deprivation Indices, Remoteness Index and Calculate VANDIX from Census Variables
cmdi <- st_read(paste0(wd, "/Data/multiple_deprivation_2021.gdb"), layer = "multiple_deprivation_DA" ) %>% 
  clean_names() %>% 
  rename(geo_uid = dauid) %>% 
  st_drop_geometry() %>% 
  select(geo_uid, contains("Scores"), contains("Quintiles"))

msdi <- # Load Social and Material Deprivation Index
  read_xlsx(path = paste0(wd, "/Data/MSDI_2021.xlsx"), sheet = 1) %>%
  clean_names() %>% 
  rename(geo_uid = da) %>%
  mutate(geo_uid = as.character(geo_uid)) %>%
  select(geo_uid, contains("score"), contains("quint"))



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
    ale_transit_class = ale16_13) %>% 
  mutate(across(where(is.numeric), ~na_if(.x, -9999.00))) %>% 
  clean_names()

# Canadian Active Living Environment - Can-ALE
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
  ) %>% 
  clean_names()

# Remoteness Index
ri <-
  read_csv(paste0(wd, "/Data/RI/2021IR_DATABASE.csv")) %>% 
  filter(Pruid == 59) %>% 
  mutate(CSD_UID = as.character(CSDuid),
         remote_index = as.numeric(Index_of_remoteness),
         CSDpop2016 = as.numeric(CSDpop2021)) %>% 
  select(CSD_UID, remote_index,CSDpop2021) %>% 
  ggplot2::remove_missing() %>% 
  rename(csd_pop_2021=CSDpop2021) %>% 
  clean_names()

# Different numbers of clusters, iterations and seed replacement methods are available 
#  in the k-means clustering algorithm. Therefore, to find the most suitable model for a given dataset, 
#  different k‑means clustering methods were applied to develop 3 to 10 clusters with one to five iterations, 
# with both full and random seed replacement methods. 
#  The most suitable model yielded five clusters when five iterations 
#  and a full seed replacement method were selected (Table 5).
library(factoextra)
library(cluster)

fviz_nbclust(ri[,"remote_index"], kmeans, method = "wss")

#make this example reproducible
set.seed(1)


#perform k-means clustering with k = 3 clusters
km3 <- kmeans(ri[,"remote_index"], centers = 3, nstart = 25)
ri$cluster3 <- km3$cluster
ri_class3 <- ri %>% 
  group_by(cluster3) %>% 
  summarise(n = n(),
            mean_ri = mean(remote_index),
            mean_pop = mean(csd_pop_2021,na.rm=TRUE)) %>% 
  arrange(mean_ri) %>% 
  mutate(remote_index_class3 = row_number())
ri_class3 

ri_class3 <- ri_class3%>% 
  select(cluster3,remote_index_class3)



#perform k-means clustering with k = 5 clusters
km5 <- kmeans(ri[,"remote_index"], centers = 5, nstart = 25)
ri$cluster5 <- km5$cluster
ri_class5 <- ri %>% 
  group_by(cluster5) %>% 
  summarise(n = n(),
            mean_ri = mean(remote_index),
            mean_pop = mean(csd_pop_2021,na.rm=TRUE)) %>% 
  arrange(mean_ri) %>% 
  mutate(remote_index_class5 = row_number())
ri_class5 
ri_class5 <- ri_class5%>% 
  select(cluster5,remote_index_class5)

ri <- ri %>% left_join(ri_class5) %>% 
  mutate(remote_index_class5 = case_when(remote_index_class5 == 1 ~ "1 - Easily Accessible",
                                        remote_index_class5 == 2 ~ "2 - Accessible",
                                        remote_index_class5 == 3 ~ "3 - Less Accessible",
                                        remote_index_class5 == 4 ~ "4 - Remote",
                                        remote_index_class5 == 5 ~ "5 - Very Remote",
                                        )) %>% 
  left_join(ri_class3) %>% 
  mutate(remote_index_class3 = case_when(remote_index_class3 == 1 ~ "1 - Accessible",
                                        remote_index_class3 == 2 ~ "2 - Less Accessible",
                                        remote_index_class3 == 3 ~ "3 - Remote")
  ) %>% 
  select(csd_uid,remote_index,remote_index_class3,remote_index_class5)



# Calculate VanDIX score and join with other deprivation indices
bc_da <- bc_da %>%
  left_join(csd, by = "csd_uid") %>% 
  left_join(ri, by = "csd_uid") %>% 
  left_join(cd, by = "cd_uid") %>% 
  left_join(da_vars, by = "geo_uid") %>%
  left_join(cmdi, by = "geo_uid") %>% #Join Deprivation Indices to DA geography
  left_join(msdi, by = "geo_uid") %>% 
  left_join(ale, by = "geo_uid") %>% 
  left_join(bics, by = "geo_uid")


st_write(bc_da, dsn = paste0(wd, "/Processed Data/da_v3_2021.gpkg"))
