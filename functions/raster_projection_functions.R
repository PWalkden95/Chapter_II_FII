### FUNCTION

# Code development: Patrick Alexander Walkden November 2022


## Description: This is a quick function that inputs a raster at a given resolution which is then aggreated/dissaggregate
## to a given resolution.
## Arguments include the data directory, raster name, out directory and whether the output resolution in given in km or degrees.

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
