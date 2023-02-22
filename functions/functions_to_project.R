### SCRIPT

## Development: Patrick Alexander Walkden

## Description: This is my projection script that will take functional richness and similarity models and project them spatially.


## Inputs: data directory pointing to the folder with required rasters, out directory where the projection raster will be save,
##        functional richness and similarity models that variable values will be extracted from, the dataframes that the models
##        were run on to extract any scaling factors that will need to be applied to rasters.



require(tidyverse)
require(terra)
require(future.apply)
require(doParallel)
require(parallel)
require(stringr)
require(raster)

source("functions/Permuted_model_simplification.R")

### this function gets the scaling attributes from the dataframe upon which the models are based and scales
### a given object by these variables

scale_object <- function(values, by_column) {
  return(values - attr(by_column, "scale.center")) / attr(by_column, "scale.scale")
  
}


## inverse logit function that will be used to untransform functional similarity output

inv_logit <- function(f, a) {
  a <- (1 - 2 * a)
  (a * (1 + exp(f)) + (exp(f) - 1)) / (2 * a * (1 + exp(f)))
}



##### fucntion to get luh_rasters and weight



luh_classes <- function(luh = c("both", "1", "2"),
                        intensity = NA) {
  all <- c(
    "primary forest",
    "primary non-forest",
    "secondary",
    "plantation",
    "pasture",
    "cropland",
    "urban_luh1",
    "primf",
    "primn",
    "secdf",
    "secdn",
    "c3ann",
    "c4ann",
    "c3per",
    "c4per",
    "c3nfx",
    "pastr",
    "range",
    "LU-urban"
  )
  
  if (luh == "both") {
    classes <- all
  }
  
  if (luh == "1") {
    classes <- all[c(1:7)]
  }
  
  if (luh == "2") {
    classes <- all[-c(1:7)]
  }
  
  if (any(!is.na(intensity))) {
    for (intense in intensity) {
      classes <- classes[classes != intense]
      classes <-
        c(classes, paste(intense, c("minimal", "light", "intense"), sep = "_"))
    }
  }
  
  if("urban" %in% intensity){
    classes <- classes[!classes == "urban_luh1"]
  }
  
  return(classes)
  
}

#################
## compare rasters in the directory to a character vector

raster_comparisons <-
  function(raster_dir,
           comparators,
           fuzzy = FALSE,
           dist = NA) {
    
    
    available_rasters <- list.files(raster_dir)
    
    available_rasters <- available_rasters[grepl(available_rasters, pattern = ".tif")]
    
    
    combos <- expand.grid(available_rasters, comparators)
    
    
    if (!fuzzy) {
      return(as.character(unique(combos[apply(
        combos,
        MARGIN = 1,
        FUN = function(x)
          grepl(x[2], x[1], ignore.case = TRUE)
      ), 1])))
    } else {
      return(as.character(unique(combos[apply(
        combos,
        MARGIN = 1,
        FUN = function(x)
          agrepl(
            x = x[1],
            x[2],
            max.distance = dist ,
            ignore.case = TRUE
          )
      ), 1])))
    }
    
    
  }



####################



get_and_weight_lu_rasters <-
  function(raster_dir,
           luh,
           model_levels,
           match_land_use,
           intensity = NA) {
    
    print(glue::glue("luh{luh} land use classification used"))
    
    
    
    luh_rasters <-
      raster_comparisons(raster_dir = raster_dir, luh_classes(luh = luh, intensity = intensity))
    
    luh_rasters <- luh_rasters[!grepl(luh_rasters, pattern = "10km")]
    
    
    land_use_rasters <- purrr::map(c(luh_rasters), function(x)
      terra::rast(paste(raster_dir, "/", x, sep = "")))
    names(land_use_rasters) <- luh_classes(luh = luh, intensity = intensity)
    
    
    
    weighted_land_use_rasters <- list()
    
    for(i in 1:length(match_land_use)){
      
      
      for(j in 1:length(match_land_use[[i]][[1]])){
        if(j == 1){
          weighted_rast <- land_use_rasters[[match_land_use[[i]][[1]][[j]]]] * match_land_use[[i]][[2]][[j]]
        } else {
          weighted_rast <- weighted_rast + (land_use_rasters[[match_land_use[[i]][[j]]]] * match_land_use[[i]][[2]][[j]])
        }
      }
      
      weighted_land_use_rasters[[names(match_land_use)[i]]] <- weighted_rast
      
      
    }
    
    return(weighted_land_use_rasters)
    
  }


###### function to retrieve and scale the rasters for the explanatory variables


get_and_scale_model_rasters <-
  function(raster_dir, model_dataframe, exp_columns) {
    
    
    explanatory_variables <-
      colnames(model_dataframe)[exp_columns]
    
    ########
    ## define the land use classification that you are using
    
    
    
    
    ### get the variables that are present in the explanatory variables that could possibly be matched to a
    ### projection rasters
    
    split_variabless <- function(variables) {
      s_vars <-
        data.frame(vars = unlist(str_split(variables, pattern = "_"))) %>%
        dplyr::filter(!(
          vars %in% c(
            "site2",
            "diff",
            "1km",
            "SS",
            "log",
            "land",
            "use",
            "combination",
            "sw",
            "rao",
            "similarity"
          )
        ))
      
      return(s_vars)
    }
    
    split_vars <-
      split_variabless(explanatory_variables)
    
    
    ### fuzzy match some rasters and filter out the land use rasters
    exp_rasts <-
      data.frame(exp = raster_comparisons(
        raster_dir = raster_dir,
        comparators = split_vars$vars,
        fuzzy = TRUE,
        dist = 1
      )) %>%
      dplyr::filter(!(
        exp %in% raster_comparisons(raster_dir = raster_dir, luh_classes(luh = "both",intensity = "urban"))
      )) %>% pull()
    
    
    ### load rasters in and change there names
    prediction_rasters <-
      purrr::map(c(exp_rasts), function(x)
        terra::rast(paste(raster_dir, "/", x, sep = "")))
    names(prediction_rasters) <-
      c(gsub(exp_rasts, pattern = "\\.tif", replacement =  ""))
    
    
    
    
    #####################################
    ####################################
    
    
    #### once you get the rasters for a particular model scale them if necessecary
    
    
    for (var in explanatory_variables) {
      ###
      s_v <- split_variabless(var)
      
      
      
      
      combo <- expand.grid(names(prediction_rasters), s_v$vars)
      
      
      exp_rast <-
        unique(as.character(combo[apply(
          combo,
          MARGIN = 1,
          FUN =  function(x)
            agrepl(
              x = x[1],
              pattern = x[2],
              max.distance = 1
            )
        ), 1]))
      
      
      exp_rast <- exp_rast[!(exp_rast %in% luh_classes(luh = "both"))]
      
      if (length(exp_rast) > 1) {
        print(glue::glue("more than one raster flagged for {var}"))
        stop()
      }
      
      
      e_raster <- prediction_rasters[[exp_rast]]
      if (grepl(exp_rast, pattern = "T30") & !(grepl(exp_rast, pattern = "log"))) {
        e_raster <- log(terra::classify(e_raster, cbind(NA, 0)) + 1)
      }
      
      e_raster <- scale_object(e_raster, model_dataframe[, var])
      names(e_raster) <- var
      
      
      if (var == explanatory_variables[1]) {
        pred_rasts <- e_raster
      } else {
        pred_rasts <- c(pred_rasts, e_raster)
      }
      
      
      
    }
    
    
      ### if any of the rasters has NaN in a cell all ratsers have NA
    
    terra::values(pred_rasts)[apply(is.nan(terra::values(pred_rasts)), MARGIN = 1, function(x)
      any(x)), ] <- NA
    
    return(pred_rasts)
    
  }

##################################

## alpha projection function that limits the bounds of the prediciton rasters for the main effects to those
## values that are observed in the data so that the model isn't predicting outside of the bounds seen in the 
## data



land_use_projection_function <- function(land_use, projection_rasters, controls = c("hpd","roads"), data, model,
                                         land_use_rasters, transformation = c("log","logit","none"),
                                         alpha_beta = c("alpha","betafor","betanfor")){
  
  
  print(glue::glue("projecting for {land_use}"))
  
  
  if(alpha_beta == "alpha"){
    
    constants <-
      data.frame(
        LUI = land_use,
        control_hpd = scale_object(0, data$control_hpd)
        #control_roads = scale_object(0, data$control_roads)
      )
  } else {
    constants <-
      data.frame(
        land_use_combination = land_use,
        control_hpd = scale_object(0, data$control_hpd),
        control_roads = scale_object(0, data$control_roads),
        log_environmental_distance = scale_object(0, data$log_environmental_distance),
        log_geographic_distance = scale_object(0, data$log_geographic_distance)
      )
  }
  
  ### chnage the projection raster so that the model isn't projecting outside of the bounds of the observed 
  ### data
  
  model_variables <- simplification_test_variables(model)
  
  
  
  for(col in names(projection_rasters)){  
    
    if(grepl(col, pattern = "control")){
      next()
    }
    
    if(grepl(grep(model_variables, pattern = col, value = TRUE),pattern = ":")){
      
      
      if(alpha_beta == "alpha"){  
        col_range <- data %>% dplyr::filter(LUI == land_use) %>% dplyr::select(all_of(col)) %>% range()
      } else {
        col_range <- data %>% dplyr::filter(land_use_combination == land_use) %>% dplyr::select(all_of(col)) %>% range()
      }
      
      
      terra::values(projection_rasters[[col]]) <- ifelse(terra::values(projection_rasters[[col]]) <=
                                                           col_range[1], col_range[1],terra::values(projection_rasters[[col]]) )
      
      terra::values(projection_rasters[[col]]) <- ifelse(terra::values(projection_rasters[[col]]) >=
                                                           col_range[2], col_range[2],terra::values(projection_rasters[[col]]) )
      
      
    } else {
      col_range <- data %>%  dplyr::select(all_of(col)) %>% range()
      terra::values(projection_rasters[[col]]) <- ifelse(terra::values(projection_rasters[[col]]) <=
                                                           col_range[1], col_range[1],terra::values(projection_rasters[[col]]) )
      
      terra::values(projection_rasters[[col]]) <- ifelse(terra::values(projection_rasters[[col]]) >=
                                                           col_range[2], col_range[2],terra::values(projection_rasters[[col]]) )
      
    }
  }
  
  
  
  if(transformation == "log") {
    land_use_raster <- 
      exp(terra::predict(
        projection_rasters,
        model,
        const = constants,
        re.form  = NA))
  }
  
  if(transformation == "logit") {
    land_use_raster <- 
      inv_logit(terra::predict(
        projection_rasters,
        model,
        const = constants,
        re.form  = NA),a = 0.001)
  }
  
  if(transformation == "none") {
    land_use_raster <- 
      terra::predict(
        projection_rasters,
        model,
        const = constants,
        re.form  = NA)
  }
  
  
  
  
  
  terra::ext(land_use_raster) <- c(-180,180,-60,90)
  
  if(alpha_beta == "alpha"){
    terra::ext(land_use_rasters[[land_use]]) <- c(-180,180,-60,90)
  } else {
    if(alpha_beta == "betafor"){
      terra::ext(land_use_rasters[[gsub(x = land_use, pattern = "Primary forest_Minimal use - ", replacement = "")]]) <- c(-180,180,-60,90)
    } else {
      terra::ext(land_use_rasters[[gsub(x = land_use, pattern = "Primary non-forest_Low use - ", replacement = "")]]) <- c(-180,180,-60,90)
    }
  }
  
  if(alpha_beta == "alpha"){
    land_use_raster <-
      land_use_raster * land_use_rasters[[land_use]]
  } else {
    if(alpha_beta == "betafor"){
      land_use_raster <-  land_use_raster * land_use_rasters[[gsub(x = land_use, pattern = "Primary forest_Minimal use - ", replacement = "")]] 
    } else {
      land_use_raster <-  land_use_raster * land_use_rasters[[gsub(x = land_use, pattern = "Primary non-forest_Low use - ", replacement = "")]]
    }
  }
  
  
  
  
  return(land_use_raster)
  
}



