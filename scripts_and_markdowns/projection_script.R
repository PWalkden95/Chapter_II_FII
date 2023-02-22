### SCRIPT

## Development: Patrick Alexander Walkden

## description: run the FII projetcion function and analyise the results



rm(list = ls())


require(terra)
require(rgdal)
require(magrittr)
require(tidyverse)
require(raster)
require(sf)


outdir = "outputs"
alpha_model_path = "outputs/alpha_diversity_rao_model.rds"
alpha_data_path = "outputs/alpha_modelling_dataframe.rds"
forest_similarity_model_path = "outputs/functional_similarity_forest_model.rds"
forest_similarity_data_path = "outputs/functional_similarity_forest_dataframe.rds"
non_forest_similarity_model_path = "outputs/functional_similarity_non_forest_model.rds"
non_forest_similarity_data_path = "outputs/functional_similarity_non_forest_dataframe.rds"
luh = "1"
resolution = "10km"
land_use_to_raster = list(
  `Primary forest_Minimal use` = list("primary forest_minimal", 1),
  `Primary forest_Light use` = list(c("primary forest_light"), 1),
  `Primary forest_Intense use` = list(c("primary forest_intense"), 1),
  `Primary non-forest_Minimal use` = list("primary non-forest_minimal", 1),
  `Primary non-forest_Light use` = list(c("primary non-forest_light"), 1),
  `Primary non-forest_Intense use` = list(c("primary non-forest_intense"), 1),
  `Secondary vegetation_Minimal use` = list("secondary_minimal", 1),
  `Secondary vegetation_Light use` = list("secondary_light", 1),
  `Secondary vegetation_Intense use` = list("secondary_intense", 1),
  `Plantation forest_Minimal use` = list("plantation_minimal", 1),
  `Plantation forest_Light use` = list("plantation_light", 1),
  `Plantation forest_Intense use` = list("plantation_intense", 1),
  `Pasture_Minimal use` = list("pasture_minimal", 1),
  `Pasture_Light use` = list("pasture_light", 1),
  `Pasture_Intense use` = list("pasture_intense", 1),
  `Cropland_Minimal use` = list("cropland_minimal", 1),
  `Cropland_Light use` = list("cropland_light", 1),
  `Cropland_Intense use` = list("cropland_intense", 1),
  `Urban_Minimal use` = list("urban_minimal", 1),
  `Urban_Light use` = list("urban_light", 1),
  `Urban_Intense use` = list("urban_intense", 1)
)




    raster_dir = "data/projection_rasters/2000-rasters-for-projections/10km"
    year = 2000
    forest_biome_path = "data/projection_rasters/2000-rasters-for-projections/10km/10kmforest_biomes.tif"
    non_forest_biome_path = "data/projection_rasters/2000-rasters-for-projections/10km/10kmnon_forest_biomes.tif"
    
    source("functions/projection_function.R")
    
    raster_dir = "data/projection_rasters/2020-rasters-for-projections/10km"
    year = 2020
    forest_biome_path = "data/projection_rasters/2020-rasters-for-projections/10km/10kmforest_biomes.tif"
    non_forest_biome_path = "data/projection_rasters/2020-rasters-for-projections/10km/10kmnon_forest_biomes.tif"
  

    
    source("functions/projection_function.R")
    
       