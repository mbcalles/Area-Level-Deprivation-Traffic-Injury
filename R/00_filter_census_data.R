# Script Title: 02_clean_census_data.R
# Author: Michael Branion-Calles
# Date: October 2024
# Description: This script loads Census Data for British Columbia, selects relevant measures and puts in tidy format

library(janitor)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)

# Get the current working directory
wd <- getwd()

# https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/download-telecharger.cfm?Lang=E

# Load Statistics Canada data
c <- read_csv(paste0(wd,"/Data/census geography/census_2021/98-401-X2021006_English_CSV_data_BritishColumbia.csv")) %>% 
  clean_names()

da <- c %>% 
  filter(geo_level=="Dissemination area")


# 1	Population, 2021 (1)
# 
# 251	Total - Income statistics for private households - 25% sample data (27)
# 252	  Average total income of household in 2020 ($)
# 
# 
# 1414	Total - Private households by tenure - 25% sample data (50)
# 1415	  Owner
# 1416	  Renter
# 1417	  Dwelling provided by the local government, First Nation or Indian band
# 
# 
# 1998	Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households - 25% sample data (165)
# 1999	  No certificate, diploma or degree
# 2000	  High (secondary) school diploma or equivalency certificate (167)
# 2001	  Postsecondary certificate, diploma or degree
# 
# 78	Total number of census families in private households - 100% data
# 79	  Total couple families
# 86	  Total one-parent families
# 
# 
# 2223	Total - Population aged 15 years and over by labour force status - 25% sample data (184)
# 2228	Participation rate
# 2230	Unemployment rate



da_wide <- da %>%
  filter(
    characteristic_name == "Population, 2021" |
      characteristic_name == "Total - Income statistics for private households - 25% sample data" |
      characteristic_name == "Average total income of household in 2020 ($)" |
      characteristic_name ==  "Total - Private households by tenure - 25% sample data" |
      characteristic_name == "Owner" |
      characteristic_name == "Renter" |
      characteristic_name == "Dwelling provided by the local government, First Nation or Indian band" |
      characteristic_name == "Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households - 25% sample data"|
      (characteristic_name == "No certificate, diploma or degree" & characteristic_id == 1999) |
      (characteristic_name == "High (secondary) school diploma or equivalency certificate" & characteristic_id == 2000) |
      (characteristic_name == "Postsecondary certificate, diploma or degree" & characteristic_id == 2001)|
      characteristic_name ==	"Total number of census families in private households - 100% data"|
    characteristic_name ==	  "Total couple families"|
    characteristic_name ==	  "Total one-parent families"|
      
      characteristic_name ==	"Total - Population aged 15 years and over by labour force status - 25% sample data"|
    characteristic_name ==	"Participation rate"|
    characteristic_name ==	"Unemployment rate"
      ) %>%
  select(geo_name, characteristic_name, c1_count_total) %>%
  pivot_wider(names_from = characteristic_name, values_from = c1_count_total) %>%
  clean_names()

write_csv(da_wide, paste0(wd, "/Processed Data/census_2021.csv"))

