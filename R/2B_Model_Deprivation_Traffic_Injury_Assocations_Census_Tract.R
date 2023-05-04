# -----------------------------------------------------------------------------
# Script Name: 2_Model_Deprivation_Traffic_Injury_Associations.R
# Description: Load Census Variables at DA Level Geography, Calculate Deprivation Indices, Aggregate Road Network Data, Aggregate Claims Data 
# Author: Michael Branion-Calles
# Date Created: 2022-11-01
# Last Modified: 2023-04-28
# -----------------------------------------------------------------------------

# Load required libraries

# install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(tidyverse)
library(sp)
library(spdep)
library(INLA)
library(cancensus)

search_census_vectors("bicycle","CA16")

jtw_vectors <- cancensus::child_census_vectors(vector_list = "v_CA16_5792",keep_parent = TRUE)

biketowork <- cancensus::child_census_vectors(vector_list = "v_CA16_5792",keep_parent = TRUE) %>% pull(vector)

jtw <- get_census("CA16",regions = list(PR="59"),vectors = jtw_vectors$vector,level = "DA",labels = "short") %>% 
  rename(jtw_total = `v_CA16_5792`,
         jtw_driver = v_CA16_5795,
         jtw_passenger = v_CA16_5798,
         jtw_transit = v_CA16_5801,
         jtw_walk = v_CA16_5804,
         jtw_bicycle = v_CA16_5807,
         jtw_other = v_CA16_5810
         ) %>% 
  select(GeoUID,starts_with("jtw")) %>% 
  mutate(jtw_prop_mv = (jtw_driver+jtw_passenger)/jtw_total*100,
         jtw_prop_pedestrian = jtw_walk/jtw_total*100,
         jtw_prop_bicycle = jtw_bicycle/jtw_total*100,
         jtw_prop_other = (jtw_transit+jtw_other)/jtw_total*100,
         jtw_total_100 = jtw_total/100
  )

         
wd <- getwd()

bc_da <- st_read(paste0(wd,"/Processed Data/bc_da.gpkg")) %>% 
  left_join(jtw,by = "GeoUID")

random_round <- function(x) {
  floor_value <- floor(x)
  round_prob <- x - floor_value
  rounded_value <- ifelse(runif(length(x)) < round_prob, ceiling(x), floor_value)
  return(rounded_value)
}


set.seed(1)

bc_da <- bc_da %>% 
  filter(total_roads_m>0) %>% 
  mutate(
    n_claims_r = random_round(n_claims),
    n_casualty_claims_r = random_round(n_casualty_claims),
    n_cyclist_claims_r = random_round(n_cyclist_claims),
    n_cyclist_casualty_claims_r = random_round(n_cyclist_casualty_claims),
    n_ped_claims_r = random_round(n_ped_claims),
    n_ped_casualty_claims_r = random_round(n_ped_casualty_claims),  
    
    highway_km = highway_m/1000,
    arterial_km = arterial_m/1000,
    collector_km = collector_m/1000,
    local_km = local_m/1000,
    roads_km = total_roads_m/1000,
    ln_roads_km = log(total_roads_m),
    roads_prop_highway = highway_km/roads_km*100,
    roads_prop_arterial = arterial_km/roads_km*100,
    roads_prop_highway_arterial = (highway_km+arterial_km)/roads_km*100,
    roads_prop_collector = collector_km/roads_km*100,
    roads_prop_local = local_km/roads_km*100,
    
    population_100 = Population/100,
    vandix_quintile = case_when(vandix_quintile    ==1~"1 - Least",
                                vandix_quintile    ==5~"5 - Most",
                                TRUE ~ as.character(vandix_quintile)
    ) %>% as.factor(),
    
    cmg_material_resources_quintile = case_when(cmg_material_resources_quintile    ==1~"1 - Least",
                                                cmg_material_resources_quintile    ==5~"5 - Most",
                                TRUE ~ as.character(cmg_material_resources_quintile)
    ) %>% as.factor(),
    cmg_households_dwellings_quintile = case_when(cmg_households_dwellings_quintile    ==1~"1 - Least",
                                                cmg_households_dwellings_quintile    ==5~"5 - Most",
                                                TRUE ~ as.character(cmg_households_dwellings_quintile)
    ) %>% as.factor(),
    cmg_age_labourforce_quintile = case_when(cmg_age_labourforce_quintile    ==1~"1 - Least",
                                             cmg_age_labourforce_quintile    ==5~"5 - Most",
                                                  TRUE ~ as.character(cmg_age_labourforce_quintile)
    ) %>% as.factor(),
    cmg_immigration_vismin_quintile = case_when(cmg_immigration_vismin_quintile    ==1~"1 - Least",
                                                cmg_immigration_vismin_quintile    ==5~"5 - Most",
                                             TRUE ~ as.character(cmg_immigration_vismin_quintile)
    ) %>% as.factor(),
    
    
    canbics_class = case_when(canbics_class    ==1~"1 - Lowest Infrastructure",
                              canbics_class    ==5~"5 - Highest Infrastructure",
                                TRUE ~ as.character(canbics_class)
    ) %>% as.factor(),
    
   ale_class = case_when(ale_class    ==1~"1 - Very Low",
                         ale_class    ==5~"5 - Very High",
                         ale_class == -9999 ~ NA,
                              TRUE ~ as.character(ale_class)
    ) %>% as.factor(),
    
    remote_index = as.numeric(Index_of_remoteness),
    remote_index_quintile = ntile(Index_of_remoteness,n=5) %>% as.character()
    
  )



# Descriptive Statistics --------------------------------------------------

# Create a descriptive statistics table using summarytools
descr <- bc_da %>%
  st_drop_geometry() %>% 
  select(contains("casualty_claims_r"),
         contains("_km"),
         -(contains("ln")),
         ale_class,
         canbics_class,
         jtw_total,
         starts_with("jtw_prop"),
         remote_index_quintile
         ) %>% 
  summarytools::dfSummary(
    varnumbers = FALSE, # Exclude variable numbers from the table
    plain.ascii = TRUE, # Use plain ASCII characters for table borders
    valid.col = FALSE,  # Exclude the 'Valid' column from the table
    caption = "Descriptive Statistics Table" # Add a caption to the table
  )

# Print the descriptive statistics table
print(descr)


# Spatial Models ----------------------------------------------------------

bc_da_sp <- as(bc_da,"Spatial")

centroids <- coordinates(bc_da_sp)

# Define the number of nearest neighbors (k)
# Replace 'your_k' with a suitable number of nearest neighbors for your data
k_neighbors <- 10

# Create k-nearest neighbors object
nb <- knn2nb(knearneigh(centroids, k = k_neighbors), row.names = row.names(bc_da_sp))
nb <- make.sym.nb(nb)

# Convert the k-nearest neighbors object to a spatial weights matrix

nb2INLA("bc_da_sp.adj", nb)
g <- inla.read.graph(filename = "bc_da_sp.adj")

bc_da_sp$ui <- 1:nrow(bc_da_sp@data)
bc_da_sp$vi <- 1:nrow(bc_da_sp@data)


bc_da_sp@data <- bc_da_sp@data %>% 
  mutate_if(is.numeric, ~if_else(is.na(.), mean(., na.rm = TRUE), .)) %>%
  mutate_if(is.factor, ~if_else(is.na(.), DescTools::Mode(., na.rm = TRUE)[1], .))

summary(bc_da_sp@data$ale_class)


# All Injury Crashes ------------------------------------------------------

########### Material Resources

formula <- n_casualty_claims_r ~ cmg_material_resources_quintile + 
  ln_roads_km + roads_prop_highway_arterial + 
  ale_class + canbics_class + 
  jtw_total_100 +  jtw_prop_pedestrian + jtw_prop_bicycle + 
  remote_index_quintile


material_bym2  <- inla(update(formula, . ~. +   f(ui, model = "bym2", graph = g)),
                     verbose = FALSE,
                     family = "poisson", 
                     data = bc_da_sp@data, 
                     control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                     control.predictor = list(compute = TRUE)
)

round(exp(material_bym2$summary.fixed),3)
round(material_bym2$summary.fixed,3)
# Extract fixed effects estimates and their credible intervals
material_results <- material_bym2$summary.fixed %>% 
  as_tibble(rownames = "term") %>% 
  filter(str_detect(string = term, pattern = "cmg_material_resources_quintile")) %>% 
  mutate(term = str_remove(term,"cmg_material_resources_quintile")) %>% 
  add_row(term = "1 - Least",mean=0) %>% 
  mutate(Variable = "Material Deprivation") %>% 
  arrange(term)

mapview::mapview(bc_da_sp,z="cmg_material_resources_quintile")

########### Household Dwellings

formula <- n_casualty_claims_r ~ cmg_households_dwellings_quintile  + 
  ln_roads_km + roads_prop_highway_arterial + 
  ale_class + canbics_class + 
  jtw_total_100 +  jtw_prop_pedestrian + jtw_prop_bicycle + 
  remote_index_quintile

household_bym2  <- inla(update(formula, . ~. +   f(ui, model = "bym2", graph = g)),
                     verbose = FALSE,
                     family = "poisson", 
                     data = bc_da_sp@data, 
                     control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                     control.predictor = list(compute = TRUE)
)

# Extract fixed effects estimates and their credible intervals
household_results <- household_bym2$summary.fixed %>% 
  as_tibble(rownames = "term") %>% 
  filter(str_detect(string = term, pattern = "cmg_households_dwellings_quintile")) %>% 
  mutate(term = str_remove(term,"cmg_households_dwellings_quintile")) %>% 
  add_row(term = "1 - Least",mean=0) %>% 
  mutate(Variable = "Household Dwelling Insecurity") %>% 
  arrange(term)

########### Age/Labour force

formula <- n_casualty_claims_r ~ cmg_age_labourforce_quintile + 
  ln_roads_km + roads_prop_highway_arterial + 
  ale_class + canbics_class + 
  jtw_total_100 +  jtw_prop_pedestrian + jtw_prop_bicycle + 
  remote_index_quintile
  
age_bym2  <- inla(update(formula, . ~. +   f(ui, model = "bym2", graph = g)),
                        verbose = FALSE,
                        family = "poisson", 
                        data = bc_da_sp@data, 
                        control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                        control.predictor = list(compute = TRUE)
)

# Extract fixed effects estimates and their credible intervals
age_results <- age_bym2$summary.fixed %>% 
  as_tibble(rownames = "term") %>% 
  filter(str_detect(string = term, pattern = "cmg_age_labourforce_quintile")) %>% 
  mutate(term = str_remove(term,"cmg_age_labourforce_quintile")) %>% 
  add_row(term = "1 - Least",mean=0) %>% 
  mutate(Variable = "Dependent Population") %>% 
  arrange(term)

########### Immigration/Visible Minority

formula <- n_casualty_claims_r ~ cmg_immigration_vismin_quintile + 
  ln_roads_km + roads_prop_highway_arterial + 
  ale_class + canbics_class + 
  jtw_total_100 +  jtw_prop_pedestrian + jtw_prop_bicycle + 
  remote_index_quintile

imm_vis_bym2  <- inla(update(formula, . ~. +   f(ui, model = "bym2", graph = g)),
                        verbose = FALSE,
                        family = "poisson", 
                        data = bc_da_sp@data, 
                        control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                        control.predictor = list(compute = TRUE)
)

# Extract fixed effects estimates and their credible intervals
imm_vis_results <- imm_vis_bym2$summary.fixed %>% 
  as_tibble(rownames = "term") %>% 
  filter(str_detect(string = term, pattern = "cmg_immigration_vismin_quintile")) %>% 
  mutate(term = str_remove(term,"cmg_immigration_vismin_quintile")) %>% 
  add_row(term = "1 - Least",mean=0) %>% 
  mutate(Variable = "Immigrants/Visible Minority Population") %>% 
  arrange(term)

all_injury_crashes <- bind_rows(material_results,household_results,age_results,imm_vis_results) %>%
  mutate(Variable = factor(Variable,levels = c("Material Deprivation",
                                               "Household Dwelling Insecurity",
                                               "Dependent Population",
                                               "Immigrants/Visible Minority Population"))) %>% 
  mutate(exp_mean = exp(mean),
         exp_lci = exp(`0.025quant`),
         exp_uci = exp(`0.975quant`)
  ) 

all_injury_crashes %>%
  mutate(IRR =
           ifelse(
             term != "1 - Least",
             paste0(
               round(exp_mean, 2),
               " (",
               round(exp_lci, 2),
               " - ",
               round(exp_uci, 2),
               ")"
             ),
             "1"
           )) %>%
  select(Variable, term, IRR) %>%
  pivot_wider(names_from = Variable,values_from = IRR) %>% 
  knitr::kable()


all_plot <- all_injury_crashes %>% 
  ggplot(aes(x=fct_rev(term),y=exp(mean))) +
  geom_hline(yintercept=1, lty=2, size =1,colour="grey") +  # add a dotted line at x=0 after flip
  geom_point() +
  geom_errorbar(aes(ymin=exp(`0.025quant`), ymax=exp(`0.975quant`)), width=0.25)+  
  theme_bw()+
  scale_y_log10(limits = c(0.70, 6)) +  #
  labs(y="5-year Adjusted Traffic Injury Incidence Rate Ratio",x="Quintiles")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))   + 
  ggtitle("All Traffic Injury Crashes")+
  facet_wrap(~Variable,scales = "free_y",ncol=1)+
  coord_flip()




# Bicycle-Motor vehicle Injury Crashes ------------------------------------------------------


########### Material Resources

formula <- n_cyclist_casualty_claims_r ~ cmg_material_resources_quintile +   
  ln_roads_km + roads_prop_highway_arterial + 
  ale_class + canbics_class + 
  jtw_total_100 +  jtw_prop_pedestrian + jtw_prop_bicycle + 
  remote_index_quintile


material_bym2_cyclist  <- inla(update(formula, . ~. +   f(ui, model = "bym2", graph = g)),
                       verbose = FALSE,
                       family = "poisson", 
                       data = bc_da_sp@data, 
                       control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                       control.predictor = list(compute = TRUE)
)

# Extract fixed effects estimates and their credible intervals
material_results_cyclist <- material_bym2_cyclist$summary.fixed %>% 
  as_tibble(rownames = "term") %>% 
  filter(str_detect(string = term, pattern = "cmg_material_resources_quintile")) %>% 
  mutate(term = str_remove(term,"cmg_material_resources_quintile")) %>% 
  add_row(term = "1 - Least",mean=0) %>% 
  mutate(Variable = "Material Deprivation") %>% 
  arrange(term)

########### Household Dwellings

formula <- n_cyclist_casualty_claims_r ~ cmg_households_dwellings_quintile +
  ln_roads_km + roads_prop_highway_arterial + 
  ale_class + canbics_class + 
  jtw_total_100 +  jtw_prop_pedestrian + jtw_prop_bicycle + 
  remote_index_quintile

household_bym2_cyclist  <- inla(update(formula, . ~. +   f(ui, model = "bym2", graph = g)),
                        verbose = FALSE,
                        family = "poisson", 
                        data = bc_da_sp@data, 
                        control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                        control.predictor = list(compute = TRUE)
)

# Extract fixed effects estimates and their credible intervals
household_results_cyclist <- household_bym2_cyclist$summary.fixed %>% 
  as_tibble(rownames = "term") %>% 
  filter(str_detect(string = term, pattern = "cmg_households_dwellings_quintile")) %>% 
  mutate(term = str_remove(term,"cmg_households_dwellings_quintile")) %>% 
  add_row(term = "1 - Least",mean=0) %>% 
  mutate(Variable = "Household Dwelling Insecurity") %>% 
  arrange(term)

########### Age/Labour force

formula <- n_cyclist_casualty_claims_r ~ cmg_age_labourforce_quintile + 
  ln_roads_km + roads_prop_highway_arterial + 
  ale_class + canbics_class + 
  jtw_total_100 +  jtw_prop_pedestrian + jtw_prop_bicycle + 
  remote_index_quintile


age_bym2_cyclist  <- inla(update(formula, . ~. +   f(ui, model = "bym2", graph = g)),
                  verbose = FALSE,
                  family = "poisson", 
                  data = bc_da_sp@data, 
                  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                  control.predictor = list(compute = TRUE)
)

# Extract fixed effects estimates and their credible intervals
age_results_cyclist <- age_bym2_cyclist$summary.fixed %>% 
  as_tibble(rownames = "term") %>% 
  filter(str_detect(string = term, pattern = "cmg_age_labourforce_quintile")) %>% 
  mutate(term = str_remove(term,"cmg_age_labourforce_quintile")) %>% 
  add_row(term = "1 - Least",mean=0) %>% 
  mutate(Variable = "Dependent Population") %>% 
  arrange(term)

########### Immigration/Visible Minority

formula <- n_cyclist_casualty_claims_r ~ cmg_immigration_vismin_quintile + 
  ln_roads_km + roads_prop_highway_arterial + 
  ale_class + canbics_class + 
  jtw_total_100 +  jtw_prop_pedestrian + jtw_prop_bicycle + 
  remote_index_quintile

imm_vis_bym2_cyclist  <- inla(update(formula, . ~. +   f(ui, model = "bym2", graph = g)),
                      verbose = FALSE,
                      family = "poisson", 
                      data = bc_da_sp@data, 
                      control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                      control.predictor = list(compute = TRUE)
)

# Extract fixed effects estimates and their credible intervals
imm_vis_results_cyclist <- imm_vis_bym2_cyclist$summary.fixed %>% 
  as_tibble(rownames = "term") %>% 
  filter(str_detect(string = term, pattern = "cmg_immigration_vismin_quintile")) %>% 
  mutate(term = str_remove(term,"cmg_immigration_vismin_quintile")) %>% 
  add_row(term = "1 - Least",mean=0) %>% 
  mutate(Variable = "Immigrants/Visible Minority Population") %>% 
  arrange(term)


bicycle_motorvehicle_injury_crashes <- bind_rows(material_results_cyclist,household_results_cyclist,age_results_cyclist,imm_vis_results_cyclist) %>%
  mutate(Variable = factor(Variable,levels = c("Material Deprivation",
                                               "Household Dwelling Insecurity",
                                               "Dependent Population",
                                               "Immigrants/Visible Minority Population"))) %>% 
  mutate(exp_mean = exp(mean),
         exp_lci = exp(`0.025quant`),
         exp_uci = exp(`0.975quant`)
         ) 

bicycle_motorvehicle_injury_crashes %>%
  mutate(IRR =
           ifelse(
             term != "1 - Least",
             paste0(
               round(exp_mean, 2),
               " (",
               round(exp_lci, 2),
               " - ",
               round(exp_uci, 2),
               ")"
             ),
             "1"
           )) %>%
  select(Variable, term, IRR) %>%
  pivot_wider(names_from = Variable,values_from = IRR) %>% 
  knitr::kable()


bike_plot <- bicycle_motorvehicle_injury_crashes %>% 
  ggplot(aes(x=fct_rev(term),y=exp(mean))) +
  geom_hline(yintercept=1, lty=2, size =1,colour="grey") +  # add a dotted line at x=0 after flip
  geom_point() +
  geom_errorbar(aes(ymin=exp(`0.025quant`), ymax=exp(`0.975quant`)), width=0.25)+  
  theme_bw()+
  scale_y_log10(limits = c(0.70, 6)) +  #
  labs(y="5-year Adjusted Traffic Injury Incidence Rate Ratio",x="Quintiles")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))   + 
  ggtitle("Bicyclist-Motor Vehicle Injury Crashes")+
  facet_wrap(~Variable,scales = "free_y",ncol=1)+
  coord_flip()

bike_plot

# Pedestrian-Motor vehicle Injury Crashes ------------------------------------------------------
  
  
########### Material Resources
  
formula <- n_ped_casualty_claims_r ~ cmg_material_resources_quintile + 
  ln_roads_km + roads_prop_highway_arterial + 
  ale_class + canbics_class + 
  jtw_total_100 +  jtw_prop_pedestrian + jtw_prop_bicycle + 
  remote_index_quintile


material_bym2_ped  <- inla(update(formula, . ~. +   f(ui, model = "bym2", graph = g)),
                               verbose = FALSE,
                               family = "poisson", 
                               data = bc_da_sp@data, 
                               control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                               control.predictor = list(compute = TRUE)
)

# Extract fixed effects estimates and their credible intervals
material_results_ped <- material_bym2_ped$summary.fixed %>% 
  as_tibble(rownames = "term") %>% 
  filter(str_detect(string = term, pattern = "cmg_material_resources_quintile")) %>% 
  mutate(term = str_remove(term,"cmg_material_resources_quintile")) %>% 
  add_row(term = "1 - Least",mean=0) %>% 
  mutate(Variable = "Material Deprivation") %>% 
  arrange(term)

########### Household Dwellings

formula <- n_ped_casualty_claims_r ~ cmg_households_dwellings_quintile + 
  ln_roads_km + roads_prop_highway_arterial + 
  ale_class + canbics_class + 
  jtw_total_100 +  jtw_prop_pedestrian + jtw_prop_bicycle + 
  remote_index_quintile  
  
  
household_bym2_ped  <- inla(update(formula, . ~. +   f(ui, model = "bym2", graph = g)),
                                verbose = FALSE,
                                family = "poisson", 
                                data = bc_da_sp@data, 
                                control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                                control.predictor = list(compute = TRUE)
)

# Extract fixed effects estimates and their credible intervals
household_results_ped <- household_bym2_ped$summary.fixed %>% 
  as_tibble(rownames = "term") %>% 
  filter(str_detect(string = term, pattern = "cmg_households_dwellings_quintile")) %>% 
  mutate(term = str_remove(term,"cmg_households_dwellings_quintile")) %>% 
  add_row(term = "1 - Least",mean=0) %>% 
  mutate(Variable = "Household Dwelling Insecurity") %>% 
  arrange(term)

########### Age/Labour force

formula <- n_ped_casualty_claims_r ~ cmg_age_labourforce_quintile + 
  ln_roads_km + roads_prop_highway_arterial + 
  ale_class + canbics_class + 
  jtw_total_100 +  jtw_prop_pedestrian + jtw_prop_bicycle + 
  remote_index_quintile

age_bym2_ped  <- inla(update(formula, . ~. +   f(ui, model = "bym2", graph = g)),
                          verbose = FALSE,
                          family = "poisson", 
                          data = bc_da_sp@data, 
                          control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                          control.predictor = list(compute = TRUE)
)

# Extract fixed effects estimates and their credible intervals
age_results_ped <- age_bym2_ped$summary.fixed %>% 
  as_tibble(rownames = "term") %>% 
  filter(str_detect(string = term, pattern = "cmg_age_labourforce_quintile")) %>% 
  mutate(term = str_remove(term,"cmg_age_labourforce_quintile")) %>% 
  add_row(term = "1 - Least",mean=0) %>% 
  mutate(Variable = "Dependent Population") %>% 
  arrange(term)

########### Immigration/Visible Minority

formula <- n_ped_casualty_claims_r ~ cmg_immigration_vismin_quintile + 
  ln_roads_km + roads_prop_highway_arterial + 
  ale_class + canbics_class + 
  jtw_total_100 +  jtw_prop_pedestrian + jtw_prop_bicycle + 
  remote_index_quintile  


imm_vis_bym2_ped  <- inla(update(formula, . ~. +   f(ui, model = "bym2", graph = g)),
                              verbose = FALSE,
                              family = "poisson", 
                              data = bc_da_sp@data, 
                              control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                              control.predictor = list(compute = TRUE)
)

# Extract fixed effects estimates and their credible intervals
imm_vis_results_ped <- imm_vis_bym2_ped$summary.fixed %>% 
  as_tibble(rownames = "term") %>% 
  filter(str_detect(string = term, pattern = "cmg_immigration_vismin_quintile")) %>% 
  mutate(term = str_remove(term,"cmg_immigration_vismin_quintile")) %>% 
  add_row(term = "1 - Least",mean=0) %>% 
  mutate(Variable = "Immigrants/Visible Minority Population") %>% 
  arrange(term)


ped_motorvehicle_injury_crashes <- bind_rows(material_results_ped,household_results_ped,age_results_ped,imm_vis_results_ped) %>%
  mutate(Variable = factor(Variable,levels = c("Material Deprivation",
                                               "Household Dwelling Insecurity",
                                               "Dependent Population",
                                               "Immigrants/Visible Minority Population"))) %>% 
  mutate(exp_mean = exp(mean),
         exp_lci = exp(`0.025quant`),
         exp_uci = exp(`0.975quant`)
  ) 

ped_motorvehicle_injury_crashes %>%
  mutate(IRR =
           ifelse(
             term != "1 - Least",
             paste0(
               round(exp_mean, 2),
               " (",
               round(exp_lci, 2),
               " - ",
               round(exp_uci, 2),
               ")"
             ),
             "1"
           )) %>%
  select(Variable, term, IRR) %>%
  pivot_wider(names_from = Variable,values_from = IRR) %>% 
  knitr::kable()

ped_plot <- ped_motorvehicle_injury_crashes %>% 
  ggplot(aes(x=fct_rev(term),y=exp(mean))) +
  geom_hline(yintercept=1, lty=2, size =1,colour="grey") +  # add a dotted line at x=0 after flip
  geom_point() +
  geom_errorbar(aes(ymin=exp(`0.025quant`), ymax=exp(`0.975quant`)), width=0.25)+  
  theme_bw()+
  scale_y_log10(limits = c(0.70, 6)) +  #
  labs(y="5-year Adjusted Traffic Injury Incidence Rate Ratio",x="Quintiles")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))   + 
  ggtitle("Pedestrian-Motor Vehicle Injury Crashes")+
  facet_wrap(~Variable,scales = "free_y",ncol=1)+
  coord_flip()

ped_plot



library(cowplot)

plot_grid(all_plot,bike_plot,ped_plot,ncol = 3)

bind_rows(all_injury_crashes %>% 
  mutate(subset = "All"),
bicycle_motorvehicle_injury_crashes %>% 
  mutate(subset = "Bicycle-Motor vehicle"),
ped_motorvehicle_injury_crashes %>% 
  mutate(subset = "Pedestrian-Motor vehicle")) %>% 
  ggplot(aes(x=fct_rev(term),y=exp(mean))) +
  geom_hline(yintercept=1, lty=2, size =1,colour="grey") +  # add a dotted line at x=0 after flip
  geom_point() +
  geom_errorbar(aes(ymin=exp(`0.025quant`), ymax=exp(`0.975quant`)), width=0.25)+  
  theme_bw()+
  labs(y="5-year Adjusted Traffic Injury Incidence Rate Ratio",x="Quintiles")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))   + 
  facet_grid(subset~Variable,scales = "free_y")+
  coord_flip()


