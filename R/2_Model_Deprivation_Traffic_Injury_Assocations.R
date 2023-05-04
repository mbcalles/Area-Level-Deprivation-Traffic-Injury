# -----------------------------------------------------------------------------
# Script Name: 2_Model_Deprivation_Traffic_Injury_Associations.R
# Description: Load Census Variables at DA Level Geography, Calculate Deprivation Indices, Aggregate Road Network Data, Aggregate Claims Data 
# Author: Michael Branion-Calles
# Date Created: 2022-11-01
# Last Modified: 2023-05-04
# -----------------------------------------------------------------------------

# Load required libraries

# install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(tidyverse)
library(sp)
library(spdep)
library(rgeos)
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
  filter(total_roads_m>10) %>% 
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
    
    cmg_material_resources_z = scale(cmg_material_resources),
    
    cmg_households_dwellings_z = scale(cmg_households_dwellings),
    
    cmg_age_labourforce_z = scale(cmg_age_labourforce),
    
    cmg_immigration_vismin_z = scale(cmg_immigration_vismin),
    
    canbics_class = case_when(canbics_class    ==1~"1 - Lowest Infrastructure",
                              canbics_class    ==5~"5 - Highest Infrastructure",
                                TRUE ~ as.character(canbics_class)
    ) %>% as.factor(),
    canbics_index_center = scale(canbics_index,scale = FALSE),
    
   ale_index = ifelse(ale_index==-9999,NA,ale_index),
   ale_index_center = scale(ale_index,scale = FALSE),
   ale_class = case_when(ale_class    ==1~"1 - Very Low",
                         ale_class    ==5~"5 - Very High",
                         ale_class == -9999 ~ NA,
                              TRUE ~ as.character(ale_class)
    ) %>% as.factor(),
   
    
    remote_index = as.numeric(Index_of_remoteness),
    remote_index_center = scale(remote_index,scale = FALSE),
    remote_index_quintile = ntile(Index_of_remoteness,n=5) %>% as.character()
    
  )



# Descriptive Statistics --------------------------------------------------

# Create a descriptive statistics table using summarytools
descr <- bc_da %>%
  st_drop_geometry() %>% 
  select(contains("casualty_claims_r"),
         contains("_km"),
         -(contains("ln")),
         roads_prop_highway_arterial,
         ale_index,
         canbics_index,
         vandix,
         cmg_material_resources_z,
         cmg_households_dwellings_z,
         cmg_age_labourforce_z,
         cmg_immigration_vismin_z,
         jtw_total,
         starts_with("jtw_prop"),
         remote_index,
         urban_rural
         )

# Generate the Table 1
table1 <- descr %>% 
  select(-urban_rural) %>% 
  # group_by(urban_rural) %>% 
  skimr::skim()

options(scipen = 999)

table1 %>% as_tibble() %>% 
  mutate(numeric.mean = round(numeric.mean,2)) %>% 
  print(n=100)

print(table1,digits =3)

#################################################################################### 
##################### Define Spatial Neighbourhoods ################################
#################################################################################### 

bc_da_sp <- as(bc_da,"Spatial")

nb <- poly2nb(bc_da,queen = FALSE)
nb
nb_second_order <- nblag(nb, 2)
nb <- nblag_cumul(nb_second_order)
nb

#manually assign the regions with no links the nearest polygon as aneighbour
centroids <- gCentroid(bc_da_sp, byid = TRUE)

# Calculate the distance matrix between all pairs of centroids
dist_matrix <- gDistance(centroids, centroids, byid = TRUE)

# Identify the islands (polygons with no neighbors)
islands <- which(lapply(nb, sum) == 0) %>% as.integer()

# Update the nb object with the nearest neighbor for each island
for (i in islands) {
  # Find the nearest neighbor by removing the diagonal (distance to itself) and getting the index of the minimum distance
  nearest_neighbor <- names(which.min(dist_matrix[i,-i])) %>% as.integer()
  
  # Update the nb object
  nb[[i]] <- c(nearest_neighbor)
}

# Check the updated nb object
nb
nb <- make.sym.nb(nb)


# Convert the k-nearest neighbors object to a spatial weights matrix
nb2INLA("bc_da_sp.adj", nb)
g <- inla.read.graph(filename = "bc_da_sp.adj")

bc_da_sp$ui <- 1:nrow(bc_da_sp@data)

# bc_da_sp@data <- bc_da_sp@data %>% 
#   mutate_if(is.numeric, ~if_else(is.na(.), mean(., na.rm = TRUE), .)) %>%
#   mutate_if(is.factor, ~if_else(is.na(.), DescTools::Mode(., na.rm = TRUE)[1], .))


# Fit BYM2 Models  -------------------------------------------------

tidy_inla <- function(inla_model, exponentiate = FALSE, digits = NULL) {
  fixed_effects <- inla_model$summary.fixed
  
  # Create a tidy data frame
  tidy_results <- fixed_effects %>%
    as.data.frame() %>%
    rownames_to_column(var = "term") %>%
    dplyr::rename(estimate = 'mean',
                  std.error = 'sd',
                  lower_ci = '0.025quant',
                  upper_ci = '0.975quant')
  
  if (exponentiate) {
    tidy_results <- tidy_results %>%
      mutate(estimate = exp(estimate),
             lower_ci = exp(lower_ci),
             upper_ci = exp(upper_ci))
  }
  
  if (!is.null(digits)) {
    tidy_results <- tidy_results %>%
      mutate(estimate = round(estimate, digits),
             std.error = round(std.error, digits),
             lower_ci = round(lower_ci, digits),
             upper_ci = round(upper_ci, digits))
  }
  
  return(tidy_results)
} # function to extract fixed effects from INLA models


fit_bym2 <- function(outcome_var, interest_var, formula_terms = NULL,exponentiate =FALSE,digits = NULL) {
  
  # Construct the INLA formula
  if (is.null(formula_terms)) {
    inla_formula <- as.formula(paste(outcome_var, "~", interest_var, "+ f(ui, model = 'bym2', graph = g,hyper = prior)"))
  } else {
    inla_formula <- as.formula(paste(outcome_var, "~", interest_var, "+", formula_terms, "+ f(ui, model = 'bym2', graph = g, hyper = prior)"))
  }
  

  prior <- list(
    prec = list(
      prior = "pc.prec",
      param = c(0.5 / 0.31, 0.01)),
    phi = list(
      prior = "pc",
      param = c(0.5, 2 / 3))
  )
  
  # Fit the INLA model with BYM2 prior
  fit <- inla(inla_formula,
              data = bc_da_sp@data,
              family = "poisson",
              control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
              control.predictor = list(compute = TRUE),
              verbose = FALSE)
  
  results <- tidy_inla(fit,exponentiate = exponentiate,digits = digits)
  
  adjustment <- ifelse(is.null(formula_terms),FALSE,TRUE)
  
  ret <- tibble(outcome = outcome_var,exposure = interest_var,adjustment = adjustment,model = list(fit),fixed_effects = list(results))
  
  gc(reset = TRUE)
  
  # Return the INLA model object
  return(ret)
} #function to run BYM2 INLA model and store results in a tiblle


#define adjustment variables
adjustment_vars <- "ln_roads_km + roads_prop_highway_arterial + ale_class + canbics_class + jtw_total_100 + jtw_prop_pedestrian + jtw_prop_bicycle + remote_index_quintile"

# Define the different outcomes, exposure variables, and adjustments
outcomes <- c("n_casualty_claims_r", "n_cyclist_casualty_claims_r","n_ped_casualty_claims_r")
exposure_variables <- c("cmg_material_resources_z", "cmg_households_dwellings_z", "cmg_immigration_vismin_z", "cmg_age_labourforce_z","vandix")
adjustments <- list(NULL, adjustment_vars)

# Create a data frame with all combinations of outcomes, exposure variables, and adjustments
combinations <- expand.grid(outcome = outcomes,
                            exposure = exposure_variables,
                            adjustment = adjustments,
                            stringsAsFactors = FALSE) %>% 
  arrange(desc(outcome),adjustment_vars)


# Run the fit_bym2() function for each combination and store the results in a tibble


results_tibble <- combinations %>%
  pmap_dfr(function(outcome, exposure, adjustment) {
    fit_bym2(outcome_var = outcome,
             interest_var = exposure,
             formula_terms = adjustment,
             exponentiate = TRUE,
             digits = 3)
  })

.# Output Results  -------------------------------------------------

saveRDS(results_tibble, file = paste0(wd,"/Outputs/model_results.Rds")


# Extract the fixed_effects data from the results_tibble
fixed_effects_tibble <- results_tibble %>%
  select(outcome, exposure, adjustment, fixed_effects) %>%
  unnest(cols = fixed_effects)

# Filter the rows to keep only the exposure variables of interest
exposure_rows <- fixed_effects_tibble %>%
  filter(term == exposure)

# Create a table with exposure variables as rows and adjusted/unadjusted as columns
comparison_table <- exposure_rows %>%
  mutate_if(is.numeric, ~ sprintf(paste0("%.", 2, "f"), .)) %>% 
  mutate(adjustment = ifelse(adjustment, "adjusted", "unadjusted"),
         IRR = paste0(estimate, " (",lower_ci,", ",upper_ci,")")
         ) %>%
  select(outcome,exposure, adjustment, IRR) %>%
  pivot_wider(names_from = c(outcome,adjustment),values_from = IRR) %>% 
  select(exposure,contains("n_casualty"),contains("n_cyclist"),contains("n_ped")) 

print(comparison_table)





