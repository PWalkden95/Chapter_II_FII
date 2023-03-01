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


test <- readRDS(alpha_model_path)

summary(test)

outdir = "outputs"
alpha_model_path = "outputs/alpha_diversity_rao_model.rds"
alpha_data_path = "outputs/alpha_modelling_dataframe.rds"
forest_similarity_model_path = "outputs/functional_similarity_forest_model.rds"
forest_similarity_data_path = "outputs/functional_similarity_forest_dataframe.rds"
non_forest_similarity_model_path = "outputs/functional_similarity_non_forest_model.rds"
non_forest_similarity_data_path = "outputs/functional_similarity_non_forest_dataframe.rds"
resolution = "10km"



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
    
       