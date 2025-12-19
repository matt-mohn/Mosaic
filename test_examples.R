#### Example outside of RMD

## Load necessary packages
library(sf)
library(tidyverse)
library(igraph)
library(sf)
library(readr)
library(ggplot2)
library(dplyr)
library(shadowtext)
library(scales)


## Load mosaic files
source("timing_functions.R")
source("scoring_functions.R")
source("tree_functions.R")
source("partition_functions.R")
source("recom_functions.R")
source("graph_functions.R")
source("output_functions.R")
source("chain_runner.R")
#source("cleanup_globals.R") This is killed
source("graphics.R")
source("config.R")

SHAPEFILE <- "shapefiles/North_Carolina_Simplified.shp"

# Example One
results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  seed=123456
)

# Example Two
results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  seed=12345,
  county_bias=10,
  weight_county_splits=15,
)

# Example Three
results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  seed=123456,
  county_bias=2,
  weight_county_splits=5,
  target_efficiency_gap=0.5,
  weight_efficiency_gap=100,
  target_mean_median=0.15,
  weight_mean_median=50,
  num_steps=3000,
  target_dem_seats=2,
  weight_dem_seats=250
)

# Example Four
results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  seed=123456,
  county_bias=2,
  weight_county_splits=5,
  target_efficiency_gap=-0.3,
  weight_efficiency_gap=100,
  target_mean_median=-0.1,
  weight_mean_median=50,
  num_steps=3000,
  target_dem_seats=11,
  weight_dem_seats=250,
  verbose_console=TRUE ## FALSE when production 
)

# Example Five
results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 50,
  seed=123456,
  county_bias=5,
  weight_county_splits=10,
  target_mean_median=0,
  weight_mean_median=150,
  target_efficiency_gap=0,
  weight_efficiency_gap=100,
  weight_competitiveness=5,
  num_steps=5000,
  verbose_console = TRUE ## FALSE when production 
)

# Example Six
NC_incumbent_list <- c(
  1000,              # Weight: heavy penalty for splitting
  1,                 # Exponent: linear penalty
  "3707900BEAR", #Davis
  "37183001-27", #Ross
  "3714701504A", #Murphy
  "3713500000H", #Foushee
  "37011000002", #Foxx
  "37059000013", #McDowell
  "37129000W25", #Rouzer
  "37179000014", #Harris
  "37125000SSP", #Hudson
  "37035000013", #Harrigan
  "370890000FR", #Edwards
  "37119000011", #Adams
  "37183001-36", #Knott
  "3704500KM-N"  #Moore
)

results <- run_chain(
  shapefile_path = SHAPEFILE,
  num_districts = 14,
  seed=123456,
  county_bias=3,
  weight_county_splits=15,
  target_mean_median=0,
  weight_mean_median=500,
  target_efficiency_gap = 0,
  weight_efficiency_gap = 100,
  num_steps=2500,
  verbose_console = FALSE,
  verbose_initialization = FALSE,
  bunking_lists = create_bunking_lists(
    anti_bunking  = list(NC_incumbent_list) 
  )
)


