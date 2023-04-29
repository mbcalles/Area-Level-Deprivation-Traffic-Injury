# -----------------------------------------------------------------------------
# Script Name: 2_Model_Deprivation_Traffic_Injury_Associations.R
# Description: Load Census Variables at DA Level Geography, Calculate Deprivation Indices, Aggregate Road Network Data, Aggregate Claims Data 
# Author: Michael Branion-Calles
# Date Created: 2022-11-01
# Last Modified: 2023-04-28
# -----------------------------------------------------------------------------

# Load required libraries
library(tidyverse)
library(spdep)
library(INLA)

wd <- wd()

bc_da <- st_read(paste0(wd,"/Processed Data/bc_da.gpkg"))


bc_da <- bc_da %>% 
  mutate(
    # n_claims = round(n_claims),
    # n_casualty_claims = round(n_casualty_claims),
    highway_km = highway_m/1000,
    arterial_km = arterial_m/1000,
    collector_km = collector_m/1000,
    local_km = local_m/1000,
    roads_km = total_roads_m/1000,
    ln_roads_km =  log(total_roads_m+0.0001),
    ln_highway_km = log(highway_km+0.0001),
    ln_arterial_km = log(arterial_km+0.0001),
    ln_collector_km = log(collector_km+0.0001),
    ln_local_km = log(local_km+0.0001),
    population_100 = Population/100,
    ethno_cultural_s = `Ethno-cultural composition Scores` %>% as.character() %>% as.numeric(),
    ethno_cultural_q = case_when(`Ethno-cultural composition Quintiles`==1~"1 - Lowest",
                                 `Ethno-cultural composition Quintiles`==5~"5 - Highest",
                                 TRUE ~ as.character(`Ethno-cultural composition Quintiles`)
    ) %>% as.factor(),
    situ_vuln_s = `Situational vulnerability Scores` %>% as.character() %>% as.numeric(),
    situ_vuln_q = case_when(`Situational vulnerability Quintiles`==1~"1 - Lowest",
                            `Situational vulnerability Quintiles`==5~"5 - Highest",
                            TRUE ~ as.character(`Situational vulnerability Quintiles`)
    ) %>% as.factor(),
    econ_dep_s = `Economic dependency Scores` %>% as.character() %>% as.numeric(),
    econ_dep_q = case_when(`Economic dependency Quintiles`==1~"1 - Lowest",
                           `Economic dependency Quintiles`==5~"5 - Highest",
                           TRUE ~ as.character(`Economic dependency Quintiles`)
    ) %>% as.factor(),
    res_insta_s = `Residential instability Scores` %>% as.character() %>% as.numeric(),
    res_insta_q = case_when(`Residential instability Quintiles`==1~"1 - Lowest",
                            `Residential instability Quintiles`==5~"5 - Highest",
                            TRUE ~ as.character(`Residential instability Quintiles`)
    ) %>% as.factor(),
    material_dep_s = SCOREMAT, 
    material_dep_q = case_when(QUINTMATCR==1~"1 - Lowest",
                               QUINTMATCR==5~"5 - Highest",
                               TRUE ~ as.character(QUINTMATCR)
    ) %>% as.factor(),
    social_dep_s = SCORESOC,
    social_dep_q = case_when(QUINTSOCCR==1~"1 - Lowest",
                             QUINTSOCCR==5~"5 - Highest",
                             TRUE ~ as.character(QUINTSOCCR)
    ) %>% as.factor(),
    lico_quintile = case_when(lico_at_quintile==1~"1 - Least Deprived",
                              lico_at_quintile==5~"5 - Most Deprived",
                              TRUE ~ as.character(lico_at_quintile)
    ) %>% as.factor(),
    unemployment_rate_quintile     = case_when(unemployment_rate_quintile    ==1~"1 - Least Deprived",
                                               unemployment_rate_quintile    ==5~"5 - Most Deprived",
                                               TRUE ~ as.character(unemployment_rate_quintile    )
    ) %>% as.factor(),
    hh_avg_income_quintile     = case_when(hh_avg_income_quintile    ==1~"1 - Least Deprived",
                                           hh_avg_income_quintile    ==5~"5 - Most Deprived",
                                           TRUE ~ as.character(hh_avg_income_quintile)
    ) %>% as.factor(),
    vis_min_quintile= case_when(vis_min_quintile    ==1~"1 - Least Deprived",
                                vis_min_quintile    ==5~"5 - Most Deprived",
                                TRUE ~ as.character(vis_min_quintile)
    ) %>% as.factor(),
    recent_imm_quintile= case_when(recent_imm_quintile    ==1~"1 - Least Deprived",
                                   recent_imm_quintile    ==5~"5 - Most Deprived",
                                   TRUE ~ as.character(recent_imm_quintile)
    ) %>% as.factor(),
    lone_parent_fam_quintile= case_when(lone_parent_fam_quintile    ==1~"1 - Least Deprived",
                                        lone_parent_fam_quintile    ==5~"5 - Most Deprived",
                                        TRUE ~ as.character(lone_parent_fam_quintile)
    ) %>% as.factor(),
    no_highschool_quintile= case_when(no_highschool_quintile    ==1~"1 - Least Deprived",
                                      no_highschool_quintile    ==5~"5 - Most Deprived",
                                      TRUE ~ as.character(no_highschool_quintile)
    ) %>% as.factor(),
    sep_div_wid_quintile= case_when(sep_div_wid_quintile    ==1~"1 - Least Deprived",
                                    sep_div_wid_quintile    ==5~"5 - Most Deprived",
                                    TRUE ~ as.character(sep_div_wid_quintile)
    ) %>% as.factor(),
    living_alone_quintile= case_when(living_alone_quintile    ==1~"1 - Least Deprived",
                                     living_alone_quintile    ==5~"5 - Most Deprived",
                                     TRUE ~ as.character(living_alone_quintile)
    ) %>% as.factor(),
    bc_dix_quintile = ntile(bc_dix,5),
    bc_dix_quintile = case_when(bc_dix_quintile    ==1~"1 - Least Deprived",
                                bc_dix_quintile    ==5~"5 - Most Deprived",
                                TRUE ~ as.character(bc_dix_quintile)
    ) %>% as.factor(),
    
    remote_index = as.numeric(Index_of_remoteness),
    remote_index_quintile = ntile(Index_of_remoteness,n=5) %>% as.character()
    
  ) 




# Spatial Models ----------------------------------------------------------

bc_da_sp <- as(bc_da,"Spatial")

nb <- poly2nb(bc_da_sp)

nb2INLA("bc_da_sp.adj", nb)
g <- inla.read.graph(filename = "bc_da_sp.adj")

bc_da_sp$ui <- 1:nrow(bc_da_sp@data)
bc_da_sp$vi <- 1:nrow(bc_da_sp@data)

############### VANDIX ############### 

formula <- n_casualty_claims_r ~ bc_dix + highway_km + arterial_km + collector_km + local_km + population_100 + remoteness_index

bym_bcdix  <- inla(update(formula, . ~. + f(ui, model = "bym",graph = g)),
                    family = "poisson", 
                    data = bc_da_sp@data, 
                    control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                    control.predictor = list(compute = TRUE)
)

# Extract the fixed effects (regression coefficients)
fixed_effects <- fit_bym$summary.fixed

# Calculate the incidence rate ratios (IRRs) and their 95% credible intervals
fixed_effects_IRR <- fixed_effects[, c("mean", "0.025quant", "0.975quant")]
fixed_effects_IRR <- exp(fixed_effects_IRR)

# Rename the columns for clarity
colnames(fixed_effects_IRR) <- c("IRR", "Lower 95% CI", "Upper 95% CI")

# Display the IRRs and their 95% credible intervals in a table
cat("Incidence Rate Ratios (IRRs) and 95% Credible Intervals:\n")
print(fixed_effects_IRR)

