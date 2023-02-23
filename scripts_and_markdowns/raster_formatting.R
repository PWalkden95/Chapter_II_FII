## SCRIPT

## Code development: Patrick Alexander Walkden

## Description: Script that takes the 1km pressure and luh2 rasters from valentine network drive at the nhm and converts them to
## coarser resolution to facilitate modelling, thse resolutions will be 10km and 0.25 degree. Additionally, some combination of
## rasters that will then match the coarser land use categories I have done modellung with currently.

## first things first lets just itterate through the rasters and change there resolution

#########################################
#########################################

rm(list = ls())

require(terra)
require(tidyverse)
require(doParallel)

## LOAD IN FORMATTING FUNCTION

source("functions/raster_projection_functions.R")

## get the relevant raster file names

raster_files <-
  list.files(
    "data/projection_rasters/2000-rasters-for-projections/unformatted_rasters/",
    full.names = FALSE
  )

## create outdirs

#dir.create("data/projection_rasters/2020-rasters-for-projections/0.25_degree")
#dir.create("data/projection_rasters/2000-rasters-for-projections/10km")
#dir.create("data/projection_rasters/2020-rasters-for-projections/1km")


## loop

registerDoParallel(cores = detectCores() - 1)

foreach(raster = raster_files,
        .packages = c("terra", "tidyverse")) %dopar% {
         
           format_raster(
            data_dir = "data/projection_rasters/2000-rasters-for-projections/unformatted_rasters",
            raster_file = raster,
            outdir = "data/projection_rasters/2000-rasters-for-projections/10km",
            target_resolution = 10,
            units = "km",
            target_extent = c(-180,180,-60,90),
            keep = FALSE
          )
          
          
        
            # format_raster(
          #   data_dir = "data/projection_rasters/2020-rasters-for-projections/unformatted_rasters",
          #   raster_file = raster,
          #   outdir = "data/projection_rasters/2020-rasters-for-projections/0.25_degree",
          #   target_resolution = 0.25,
          #   units = "deg",
          #   target_extent = c(-180,180,-60,90),
          #   keep = FALSE
          # )
          # 
        }




registerDoSEQ()
closeAllConnections()



###############
##############

## Next becasue I am using the coarser resolution land use classifications I will have to combine some of the finer luh2 classes
## to create a broad apporximation of the classes I have used.

## the combinations are:

## Primary vegetation: primf + primn

## Secondary vegetation: secdf + secdn

## Plantation forest: c3per + c4per

## Pasture: past + range

## Cropland: c3ann + c4ann + nfx

## Urban: urban




## intensity can be added as a vector for which classifications should be split into intensity



luh2_to_luh1 <- function(data_dir, outdir, intensity = NA) {
  
  directory_files <- list.files(data_dir)
  directory_files <- directory_files[grepl(directory_files, pattern = ".tif")]
  
  luh1_list <- list(
    `primary forest` = c("primf"),
    `primary non-forest` = c("primn"),
    secondary = c("secdf", "secdn"),
    plantation = c("c3per", "c4per"),
    pasture = c("range","pastr"),
    cropland = c("c3ann", "c4ann", "c3nfx"),
    urban = c("urban")
  )
  
  for(land_use in names(luh1_list)){
  
    if(land_use %in% intensity){
      for(intense in c("minimal","light","intense")){
        luh1_list[[paste(land_use,intense,sep = "_")]] <- paste(luh1_list[[land_use]], intense,sep = "_")
      }
      luh1_list <- luh1_list[names(luh1_list) != land_use]
    }  else {
      luh1_list[[land_use]] <- paste("LU-",luh1_list[[land_use]], sep = "")
    } 
    
    
  
  }
  
  
  
  ### check whether the data_directory has all of the relevant rasters
  
  for (i in 1:length(luh1_list)) {
    for (j in 1:length(luh1_list[[i]])) {
      missing_rasters <- c()
      
      if (!any(
        grepl(directory_files, pattern = luh1_list[[i]][j])
      )) {
        missing_rasters <- c(missing_rasters, luh1_list[[i]][j])
      }
    }
    
    if (!is_null(missing_rasters)) {
      print(
        glue::glue(
          "luh2 rasters are missing from the data directory please find rasters for {missing_rasters}"
        )
      )
      stop()
    }
    
  }

    
###### nested function to combine the rasters 
  


  combine_luh2 <- function(luh2_rasters){
    
  
    
    luh1_raster <- terra::rast(paste(data_dir, grep(directory_files,pattern =  luh2_rasters[1],value = TRUE),sep = "/" )) 
    
    if(length(luh2_rasters) == 1){
      return(luh1_raster)
    }
    
    for(i in 2:length(luh2_rasters)){
      
      i_raster <- terra::rast(paste(data_dir, grep(directory_files,pattern =  luh2_rasters[i],value = TRUE),sep = "/" ))
      
      luh1_raster <- luh1_raster + i_raster
    
    }
    
    return(luh1_raster)
  }
  
  luh1_rasters <- lapply(X = luh1_list, FUN = combine_luh2)
  
  
  for(i in 1:length(luh1_rasters)){
  
    writeRaster(
      luh1_rasters[[i]],
      filename = paste(outdir,"/",names(luh1_rasters)[i],"_luh1",".tif",sep = ""),
      overwrite = TRUE,
      gdal = c("COMPRESS=NONE", "TFW=YES"),
      datatype = 'FLT8S'
    )
    
  }
  
}




luh2_to_luh1(data_dir = "data/projection_rasters/2020-rasters-for-projections/10km",
             outdir = "data/projection_rasters/2020-rasters-for-projections/10km", intensity = c("primary forest",
                                                                                                 "primary non-forest",
                                                                                                 "secondary",
                                                                                                 "plantation",
                                                                                                 "pasture",
                                                                                                 "cropland",
                                                                                                 "urban"))



forest_biomes <- terra::rast("data/projection_rasters/2000-rasters-for-projections/10km/10kmforest_biomes.tif")



terra::values(forest_biomes)[which(terra::values(forest_biomes) > 0)] <- 1


writeRaster(
  forest_biomes,
  filename = "data/projection_rasters/2000-rasters-for-projections/10km/10kmforest_biomes.tif",
  overwrite = TRUE,
  gdal = c("COMPRESS=NONE", "TFW=YES"),
  datatype = 'FLT8S'
)


writeRaster(
  forest_biomes,
  filename = "data/projection_rasters/2020-rasters-for-projections/10km/10kmforest_biomes.tif",
  overwrite = TRUE,
  gdal = c("COMPRESS=NONE", "TFW=YES"),
  datatype = 'FLT8S'
)

nonforest_biomes <- terra::rast("data/projection_rasters/2000-rasters-for-projections/10km/10kmnon_forest_biomes.tif")



terra::values(nonforest_biomes)[which(terra::values(nonforest_biomes) > 0)] <- 1


writeRaster(
  nonforest_biomes,
  filename = "data/projection_rasters/2000-rasters-for-projections/10km/10kmnon_forest_biomes.tif",
  overwrite = TRUE,
  gdal = c("COMPRESS=NONE", "TFW=YES"),
  datatype = 'FLT8S'
)


writeRaster(
  nonforest_biomes,
  filename = "data/projection_rasters/2020-rasters-for-projections/10km/10kmnon_forest_biomes.tif",
  overwrite = TRUE,
  gdal = c("COMPRESS=NONE", "TFW=YES"),
  datatype = 'FLT8S'
)

