# Evaluating regional variation in neighbourhood socioeconomic inequalities in motor-vehicle injury collisions

*Note: This is a work in progress. Scripts and documentation are being actively developed and updated.*

## Overview
This repository contains the analysis code and supplementary materials for a study examining the relationship between socioeconomic status (deprivation) and traffic injury crash incidence across British Columbia, Canada. The analysis focuses on spatial variations at the dissemination area level, employing Bayesian spatial modeling techniques.

## Quick Start
For users interested in running the statistical analysis only, the following files are provided:
- `da_v4_2021.gpkg`: Main dataset aggregated to dissemination area level
- `dra_bridges_tunnels.gpkg`: Infrastructure dataset
- `05_modeling_neighbourhood_ses.R`: Statistical modeling script

### Required R Packages
```r
# Primary dependencies
library(INLA)        
library(tidyverse)   
library(sf)          
library(spdep)       

# Additional required packages
library(flextable)   
library(RColorBrewer)
library(rcartocolor) 
library(cowplot)     
library(janitor)     
library(broom)       
library(ggspatial)   
```

## Project Structure
```
├── Figures/           # Generated visualizations and maps
├── Tables/           # Generated tables for statistical models
├── Processed Data/    # Cleaned and processed datasets
├── R/                # R scripts for analysis
│   ├── 00_filter_census_data.R              # Initial census data processing
│   ├── 01_download_census_geography_and_aggregat*.R  # Geographic data preparation
│   ├── 01b_isolate_bridges_tunnels.R        # Infrastructure filtering
│   ├── 02_built_environment_measures.R       # Built environment variable creation
│   ├── 03_count_claims_by_census_geography.R # Crash counting by geography
│   ├── 04_assign_deprivation_measures.R      # SES measure assignment
│   ├── 05_modeling_neighbourhood_ses.R       # Statistical modeling
│   └── functions.R                           # Helper functions
├── Supplementary Material/ # Additional documentation and analysis
├── .gitignore        # Git ignore file
├── bc.adj           # Adjacency matrix for spatial analysis
└── README.md        # This file
```

## Data Files
### Available Data
- `da_v4_2021c.gpkg`: Final aggregated dataset at dissemination area level
- `dra_bridges_tunnels.gpkg`: Infrastructure dataset for bridges and tunnels

### Data Processing Pipeline
The scripts 00-04 document the complete data processing workflow but require access to the raw data sources which are not publicly available due to privacy considerations. These scripts are provided for methodological transparency.

## Reproducible Analysis
To run the final statistical analysis:

1. Ensure you have required R packages installed
2. Load the provided datasets:
   - `da_v4_2021c.gpkg`
   - `dra_bridges_tunnels.gpkg`
3. Run `05_modeling_neighbourhood_ses.R`

## Methods
The study employs spatial statistical modeling to analyze the relationship between neighborhood deprivation and traffic injury risk. Key methodological components include:

- Spatial unit of analysis: Dissemination areas
- Statistical approach: Bayesian spatial modeling using R-INLA
- Model specification: Besag-York-Mollie (BYM2) models
- Analysis categories:
  - All traffic crashes
  - Cyclist-involved crashes
  - Pedestrian-involved crashes

## Results

Estimated a socieconomic gradient for each crash type in most regions. Region-specific associations between Vancouver Area Deprivation Index and traffic injury crashes in British Columbia (2019-2023) are shown below. Incidence Rate Ratios show crash risk change per standard deviation increase in deprivation from BYM2 Poisson models: unadjusted (no covariates), minimally adjusted (road length), and adjusted (full built environment). Results shown for all injury crashes, crashes involving cyclists, and crashes involving pedestrians, with 95% credible intervals

![all injuries irr](Figures/vandix_injury_irr_forest_plot.jpg)

*Note: Results and supplementary materials are being actively updated as the analysis progresses.*

## Citation
Citation information coming soon.
