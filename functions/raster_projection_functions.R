### FUNCTION

# Code development: Patrick Alexander Walkden November 2022


## Description: This is a quick function that inputs a raster at a given resolution which is then aggreated/dissaggregate
## to a given resolution.
## Arguments include the data directory, raster name, out directory and whether the output resolution in given in km or degrees.


## INPUT: data directory; raster file name; out directory; target resolution; units; target extent; whether we ant to keep it or not
## OUTPUT: a saved raster in the desired extent and raster

require(terra)
require(tidyverse)


format_raster <-
  function(data_dir,
           raster_file,
           outdir,
           target_resolution = NA,
           units = c("km", "deg"),
           target_extent = NA,
           keep = FALSE) {
    ## load in raster
    
    raster <-
      terra::rast(paste(data_dir, raster_file, sep = "/"))
    
    ## if the resolution needs to be changed
    
    if (!is.na(target_resolution)) {
      ## extract starting resolution and convert to km if needed
      
      resolution_change <-
        paste(target_resolution, units, sep = "")
      
      print(glue::glue(
        "raster resolution being changed to {target_resolution}{units}"
      ))
      
      starting_resolution <-
        ifelse(units == "deg",
               terra::res(raster)[1] ,
               round(terra::res(raster)[1] *
                       120))
      
      print(glue::glue("starting resolution: {starting_resolution}{units}"))
      
      ## determine the factor of aggregation to get to desired resolution
      
      factor <- round(target_resolution / starting_resolution)
      
      
      ## if factor is above 1 then raster needs to be aggregated, otherwise raster needs to be disaggregated
      
      
      if (factor == 1) {
        print("raster resolution is already {target_resolution} no aggregation required.")
        new_res_raster <- raster
        
      } else {
        if (factor > 1) {
          print(glue::glue("raster is being aggregated by a factor of {factor}"))
          ## aggregate
          
          new_res_raster <-
            terra::aggregate(raster, fact = factor, na.rm = TRUE)
          
          
        } else {
          print(
            glue::glue(
              "Target resolution is finer than starting raster resolution so dissagregation is required...."
            )
          )
          
          disaggregate_factor <-
            starting_resolution / target_resolution
          
          print(
            glue::glue(
              "raster is being disaggregated by a factor of {disaggregate_factor}"
            )
          )
          
          new_res_raster <-
            terra::disagg(raster, fact = disaggregate_factor)
        }
        ## newly resolved raster name
        
        
        
        
      }
      
      
    }
    
    
    if (any(!is.na(target_extent))) {
      print(glue::glue("extent being cropped..."))
      
      extent <-  terra::ext(raster)@ptr$vector
      
      
      
      print(
        glue::glue(
          "starting extent: xmin: {extent[1]}, xmax: {extent[2]}, ymin: {extent[3]}, ymax: {extent[4]}"
        )
      )
      
      if (any(!is.na(target_resolution))) {
        new_res_raster <-
          terra::crop(new_res_raster, terra::ext(c(target_extent)))
      } else {
        new_res_raster <- terra::crop(raster, terra::ext(c(target_extent)))
      }
      
      
      print(
        glue::glue(
          "extent changed to xmin: {target_extent[1]}, xmax: {target_extent[2]}, ymin: {target_extent[3]}, ymax: {target_extent[4]}"
        )
      )
      
    }
    
    ## filepath to outdir
    
    filepath <-
      paste(outdir, paste(resolution_change, raster_file, sep = ""), sep = "/")
    
    ## write
    
    writeRaster(
      new_res_raster,
      filename = filepath,
      overwrite = TRUE,
      gdal = c("COMPRESS=NONE", "TFW=YES"),
      datatype = 'FLT8S'
    )
    
    print(glue::glue("newly resolved raster save at: {filepath}"))
    
    if (keep) {
      print(glue::glue("raster outputted to the environment"))
      return(new_res_raster)
    }
    
  }


###################################
###################################

# FUNCTION 

## Description: a couple of functions to combine luh2 raster classification to align with the PREDICTS classification used in the modelling. 
## the first function will hceck whetehr the directory has the relevant rasters and output a list that directs the combinations.
## the second function will combine the rasters and save in teh out directory 

## INPUT: data directory; land use which we need intensity rasters for. 
## OUTPUT: list that shows how the luh2 rasters are combined to the PREDICTS classifcation and intensities. 

luh2_to_predicts <- function(data_dir, intensity = NA) {
  
  ## list tif files in the data directory 
  
  directory_files <- list.files(data_dir)
  directory_files <- directory_files[grepl(directory_files, pattern = ".tif")]
  
  ## create a list with how the rasters need to be combined 
  
  predicts_list <- list(
    `primary forest` = c("primf"),
    `primary non-forest` = c("primn"),
    secondary = c("secdf", "secdn"),
    plantation = c("c3per", "c4per"),
    pasture = c("range","pastr"),
    cropland = c("c3ann", "c4ann", "c3nfx"),
    urban = c("urban")
  )
  
  ## if we need the intensity ratsers for each land use, add those elements into the list 
  
  for(land_use in names(predicts_list)){
    
    if(land_use %in% intensity){
      for(intense in c("minimal","light","intense")){
        ## append intensity classes to land uses 
        predicts_list[[paste(land_use,intense,sep = "_")]] <- paste(predicts_list[[land_use]], intense,sep = "_")
      }
      ## remove the land use
      predicts_list <- predicts_list[names(predicts_list) != land_use]
    }  else {
      ## keep the land use if we dont want intesnity
      predicts_list[[land_use]] <- paste("LU-",predicts_list[[land_use]], sep = "")
    } 
    
    
    
  }
  
  
  
  ### check whether the data_directory has all of the relevant rasters
  
  for (i in 1:length(predicts_list)) {
    for (j in 1:length(predicts_list[[i]])) {
      missing_rasters <- c()
      
      if (!any(
        grepl(directory_files, pattern = predicts_list[[i]][j])
      )) {
        missing_rasters <- c(missing_rasters, predicts_list[[i]][j])
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
  
  return(predicts_list)
}


## This function could be combined but I have left seperate as I can use this in parallel to speed things up 

### INPUT: an element of a list that links to the luh2 rasters which to combine
### OUTPUT: the combined raster


combine_to_predicts <- function(predicts_list,predicts_lu,data_dir, outdir){
  
  directory_files <- list.files(data_dir)
  directory_files <- directory_files[grepl(directory_files, pattern = ".tif")]
  
  ## load in teh first luh2 raster
  
  predicts_raster <- terra::rast(paste(data_dir, grep(directory_files,pattern =  predicts_list[1],value = TRUE),sep = "/" )) 
  
  if(length(predicts_list) == 1){
    ## if doesn't need to be combined just return the raster
    return(predicts_raster)
  }
  
  ## if needs to be combined then load in other ratsers iteratively and add together.
  
  for(i in 2:length(predicts_list)){
    
    i_raster <- terra::rast(paste(data_dir, grep(directory_files,pattern =  predicts_list[i],value = TRUE),sep = "/" ))
    
    predicts_raster <- predicts_raster + i_raster
    
  }
  
  ## save 
  
  
  
  save_location <- paste(outdir,"/",predicts_lu,"_luh1",".tif",sep = "")
  
  writeRaster(
    predicts_raster,
    filename = save_location,
    overwrite = TRUE,
    gdal = c("COMPRESS=NONE", "TFW=YES"),
    datatype = 'FLT8S'
  )
  
}

