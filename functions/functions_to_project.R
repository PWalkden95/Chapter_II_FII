### FUCNTIONS

## Development: Patrick Alexander Walkden

## Description: This contains all the necessary functions to facilitate the projection of the alpha and similarity models
## including gathering the relevant rasters from the data directory, scaling/weighting them if necessary and
## back transform our predicted values.


## Inputs: data directory pointing to the folder with required rasters, out directory where the projection raster will be save,
##        functional richness and similarity models that variable values will be extracted from, the dataframes that the models
##        were run on to extract any scaling factors that will need to be applied to rasters.



### load in necessary packages

require(tidyverse)
require(terra)
require(future.apply)
require(doParallel)
require(parallel)
require(stringr)
require(raster)

## this function  extracts the main effects and interactions
## from a model. -- taken from function script to test similarity models

##INPUT: model

## OUTPUT: main effects and interactions


simplification_test_variables <- function(model){
  
  model_variables <- function(model){
    
    call <- as.character(model@call)
    
    variables <- unlist(str_split(call, "~"))[3]
    variables <- gsub(variables,pattern = "\n", replacement = "")
    variables <- str_trim(unlist(str_split(variables, "\\+")))
    variables <- variables[!grepl(variables, pattern = "\\|")]
    
    
    interactions <- variables[grepl(variables, pattern = "\\:")]
    fixed_effects <- variables[!grepl(variables, pattern = "\\:")]
    
    
    model_variables <- list(fixed_effects =  fixed_effects,interactions = interactions)
    
    return(model_variables)
    
  }
  
  model_vars <- model_variables(model)
  
  tested_variables <- model_vars$interactions
  split_test <- unlist(str_split(tested_variables,pattern = "\\:"))
  fix_test <- which(!(model_vars$fixed_effects %in% split_test))
  
  if(!is_empty(fix_test)){
    tested_variables <- c(tested_variables,model_vars$fixed_effects[fix_test])
  }
  return(tested_variables)
  
}

### this function gets the scaling attributes from the dataframe upon which the models are based and scales
### a given object by these variables

##INPUT: values that need to be scaled; the column that contains the attributes
##       to unscale.

## OUTPUT: scaled values

scale_object <- function(values, by_column) {
  return(values - attr(by_column, "scale.center")) / attr(by_column, "scale.scale")
  
}


## inverse logit function that will be used to untransform functional similarity output

##INPUT: f = values that need to be back-transformed; a = adjustment in the original logit function

## OUTPUT: back-transformed values

inv_logit <- function(f, a) {
  a <- (1 - 2 * a)
  (a * (1 + exp(f)) + (exp(f) - 1)) / (2 * a * (1 + exp(f)))
}



##### function to get a vector of the predicts land use classes and intensities
## if specified

## INPUT: Just the predicts land use classes that you want the intesnites of

##OUTPUT; vector of the land use classes and intensities



luh_classes <- function(intensity = NA) {
  ## the predicts land use classes
  
  classes <- c(
    "primary forest",
    "primary non-forest",
    "secondary",
    "plantation",
    "pasture",
    "cropland",
    "urban_predicts"
  )
  
  ## if intensities are specificed append the name with intensities and
  ## remove the original element
  
  if (any(!is.na(intensity))) {
    for (intense in intensity) {
      classes <- classes[classes != intense]
      classes <-
        c(classes, paste(intense, c("minimal", "light", "intense"), sep = "_"))
    }
  }
  
  ## this is necessary because the luh2 raster is also named urban
  
  if ("urban" %in% intensity) {
    classes <- classes[!classes == "urban_predicts"]
  }
  
  return(classes)
  
}

#################
## compare rasters in the directory to a character vector to ensure that the
## necessary rasters are available for use

## INPUT: the data directory to check; rasters that you want to check are available;
##        whether you want fuzzy matching; how fuzzy of a match you want

## OUTPUT: a vector of the




raster_comparisons <-
  function(raster_dir,
           comparators,
           fuzzy = FALSE,
           dist = NA) {
    
    ## rasters in the directory
    
    available_rasters <- list.files(raster_dir)
    
    ### just get the rasters 
    
    available_rasters <-
      available_rasters[grepl(available_rasters, pattern = ".tif")]
    
    ## look at the combinations of rasters and our matching elements
    
    combos <- expand.grid(available_rasters, comparators)
    
    
    ## test whether the combos match and if so return just the raster name 
    ## from the data directory
    
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


## FUNCTION: get the land use rasters from the data directory and weight if 
##           necessary (this step was done before there was intensity rasters)


## INPUT: raster data directory; a pre-defined list that conatins the predicts
##        raster name and the degree that it needs to be weighted; intensities
##        require for the predicts land use classes.

## OUTPUT: a list containing the rasters for all the land use classifications

get_and_weight_lu_rasters <-
  function(raster_dir,
           intensity = NA) {
    
    
    ## get the land use rasters form teh data directory
    
    luh_rasters <-
      raster_comparisons(raster_dir = raster_dir, luh_classes(intensity = intensity))
    
    
    luh_rasters
    
    ## I can't remember why this was necessary 
    
    luh_rasters <-
      luh_rasters[!grepl(luh_rasters, pattern = "10km")]
    
    
    ### load in all the land use rasters from the directory 
    
    land_use_rasters <- purrr::map(c(luh_rasters), function(x)
      terra::rast(paste(raster_dir, "/", x, sep = "")))
    names(land_use_rasters) <-
      luh_classes(intensity = intensity)
    
    
    ## if weighting is necessary then multiple the raster by the require weight
    ## NOW WE HAVE THE INTENSITY RASTERS AVAILABLE THIS IS NOW REDUNDANT.
    
    ## empty list to store the weighted rasters 
    
    # weighted_land_use_rasters <- list()
    # 
    # for (i in 1:length(match_land_use)) {
    #   for (j in 1:length(match_land_use[[i]][[1]])) {
    #     if (j == 1) {
    #       weighted_rast <-
    #         land_use_rasters[[match_land_use[[i]][[1]][[j]]]] * match_land_use[[i]][[2]][[j]]
    #     } else {
    #       weighted_rast <-
    #         weighted_rast + (land_use_rasters[[match_land_use[[i]][[j]]]] * match_land_use[[i]][[2]][[j]])
    #     }
    #   }
    #   
    #   weighted_land_use_rasters[[names(match_land_use)[i]]] <-
    #     weighted_rast
    #   
    #   
    # }
    
    ## return the rasters list
    
    return(land_use_rasters)
    
  }


###### function to retrieve and scale the rasters for the explanatory variables

## INPUT: the rasters data directory; the model datafame that will contain the 
##        attributes to unscale the explanatory varaibles 
##        the columns in the dataframe that has the columns for 
##        explanatory variables

## OUTPUT: A raster stack with all the continuous variables rasters. 

get_and_scale_model_rasters <-
  function(raster_dir, model_dataframe, exp_columns) {
   
    ## get the names of the explanatory variables from teh dataframe
    
     explanatory_variables <-
      colnames(model_dataframe)[exp_columns]
    
    
    
    ### get the variables that are present in the explanatory variables that could possibly be matched to a
    ### projection rasters, additionally remove terms that can be matched to 
     ## multiple rasters or fuzzy match to others, essentially just extracting
     ## the meaningful value that enables matching.
    
     
     ##INPUT: variable names
     
     ##OUTPUT: names split into parts that can then be compared to the data
     ##       directory
     
    split_variables <- function(variables) {
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
    
    
    ## perform function
    
    split_vars <-
      split_variables(explanatory_variables)
    
    
    ### fuzzy match some rasters and filter out the land use rasters
    
    exp_rasts <-
      data.frame(exp = raster_comparisons(
        raster_dir = raster_dir,
        comparators = split_vars$vars,
        fuzzy = TRUE,
        dist = 1
      )) %>%
      dplyr::filter(!(
        exp %in% raster_comparisons(raster_dir = raster_dir, luh_classes(intensity = "urban"))
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
      
      
      ### split for just a single variable
      
      s_v <- split_variables(var)
      
      ## combos between the loaded in rasters and teh variables to match
      
      combo <- expand.grid(names(prediction_rasters), s_v$vars)
      
      ## get the loaded in raster that matchs the required varaibles
      
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
      
      ## remove ones that match to land uses 
      
      exp_rast <-
        exp_rast[!(exp_rast %in% luh_classes())]
      
      ## if there is more than one stop and flag 
      
      if (length(exp_rast) > 1) {
        print(glue::glue("more than one raster flagged for {var}"))
        stop()
      }
      
      
      
      ### time since thirty raster used to come unlogged so I would
      ### log it to match the data used in the model but now the raster is
      ### pre-logged.
      
      e_raster <- prediction_rasters[[exp_rast]]
      if (grepl(exp_rast, pattern = "T30") &
          !(grepl(exp_rast, pattern = "log"))) {
        e_raster <- log(terra::classify(e_raster, cbind(NA, 0)) + 1)
      }
      
      ## scale the raster so that it matches the data used in the model.
      
      e_raster <- scale_object(e_raster, model_dataframe[, var])
      names(e_raster) <- var
      
      ### combine the rasters together 
      
      if (var == explanatory_variables[1]) {
        pred_rasts <- e_raster
      } else {
        pred_rasts <- c(pred_rasts, e_raster)
      }
      
      
      
    }
    
    
    ### if any of the rasters has NaN in a cell all ratsers have NA
    
    terra::values(pred_rasts)[apply(is.nan(terra::values(pred_rasts)), MARGIN = 1, function(x)
      any(x)),] <- NA
    
    ### return the rasters 
    
    return(pred_rasts)
    
  }

##################################

## function to predict the model spatially. It also limits the bounds of the prediciton rasters for the main effects to those
## values that are observed in the data so that the model isn't predicting outside of the bounds seen in the
## data. The function needs a few tweaks to make it work for alpha and similarity models and some slight
## changes depending on the variables present in the model -- should really make it automatic 

## INPUT: the land use/land use combination that we want to predict spatially;
##        the relevant explanaory variable rasters; the control varaibles;
## the dataframe that the model is based on; the model itself to predict; 
## the transformation needed after predicting; and what model we are predicting.

##OUTPUT: the projection of the model for each land use - these will be combined
##        post. 

land_use_projection_function <-
  function(land_use,
           projection_rasters,
           controls = c("hpd", "roads"),
           data,
           model,
           land_use_rasters,
           transformation = c("log", "logit", "none"),
           alpha_beta = c("alpha", "betafor", "betanfor")) {
    
    print(glue::glue("projecting for {land_use}"))
    
    
    ### there are going to be constants gloablly so let's define them,
    ## this is we the land use and then setting the control roads, hp, 
    ## environmentalk distance and geographic distance to zero 
    
    
    ##alpha model doesn't contain geogrpahic and environmental distance
    
    if (alpha_beta == "alpha") {
      
      constants <-
        data.frame(LUI = land_use)
      
      if("roads" %in% controls){
        constants <- data.frame(constants,control_roads = scale_object(0, data$control_roads))
      }
      
      if("hpd" %in% controls){
        constants <- data.frame(constants,control_hpd = scale_object(0, data$control_hpd))
      }
      
      
    } else {
      constants <-
        data.frame(
          land_use_combination = land_use,
          log_environmental_distance = scale_object(0, data$log_environmental_distance),
          log_geographic_distance = scale_object(0, data$log_geographic_distance)
        )
      
      if("roads" %in% controls){
        constants <- data.frame(constants,control_roads = scale_object(0, data$control_roads))
      }
      
      if("hpd" %in% controls){
        constants <- data.frame(constants,control_hpd = scale_object(0, data$control_hpd))
      }
    }
    
    ### change the projection raster so that the model isn't projecting outside of the bounds of the observed
    ### data
    
    
    ## get the model variables so we know which are in an interaction so needs 
    ## to be bound within just that land use class and without an interaction
    ## its bound by the limits of all the data.
    
    model_variables <- simplification_test_variables(model)
    
    
    
    for (col in names(projection_rasters)) {
      
      # don't need to bound control varaibles as they're set to 0
      
      if (grepl(col, pattern = "control")) {
        next()
      }
      
      ## if the varaible is in an interaction filter the data for land use being
      ## projected and extract the range of values  
      
      if (grepl(grep(model_variables, pattern = col, value = TRUE), pattern = ":")) {
        if (alpha_beta == "alpha") {
          col_range <-
            data %>% dplyr::filter(LUI == land_use) %>% dplyr::select(all_of(col)) %>% range()
        } else {
          col_range <-
            data %>% dplyr::filter(land_use_combination == land_use) %>% dplyr::select(all_of(col)) %>% range()
        } } 
      ### else if not in an interaction just get the range of the whole data.
      else {
          col_range <- data %>%  dplyr::select(all_of(col)) %>% range()
        }
        
        ### if values exceed the limits of the range of values set to the maximum
        ### or minimum values 
        
        terra::values(projection_rasters[[col]]) <-
          ifelse(
            terra::values(projection_rasters[[col]]) <=
              col_range[1],
            col_range[1],
            terra::values(projection_rasters[[col]])
          )
        
        terra::values(projection_rasters[[col]]) <-
          ifelse(
            terra::values(projection_rasters[[col]]) >=
              col_range[2],
            col_range[2],
            terra::values(projection_rasters[[col]])
          )
    }
        
      

  ### predict the model with the projection rasters and constants, then 
  ### back transform if necessary
    
    if (transformation == "log") {
      land_use_raster <-
        exp(terra::predict(
          projection_rasters,
          model,
          const = constants,
          re.form  = NA
        ))
    }
    
    if (transformation == "logit") {
      land_use_raster <-
        inv_logit(terra::predict(
          projection_rasters,
          model,
          const = constants,
          re.form  = NA
        ),
        a = 0.001)
    }
    
    if (transformation == "none") {
      land_use_raster <-
        terra::predict(projection_rasters,
                       model,
                       const = constants,
                       re.form  = NA)
    }
    
    
    
    ## there have been some small extent discrepancy issues so this just makes sure the extents 
    ## match -- selecting the land use for the similarity models requires 
    ## losting some text 
    
    terra::ext(land_use_raster) <- c(-180, 180, -60, 90)
    
    if (alpha_beta == "alpha") {
      terra::ext(land_use_rasters[[land_use]]) <- c(-180, 180, -60, 90)
    } else {
      if (alpha_beta == "betafor") {
        terra::ext(land_use_rasters[[gsub(x = land_use,
                                          pattern = "Primary forest_Minimal use - ",
                                          replacement = "")]]) <- c(-180, 180, -60, 90)
      } else {
        terra::ext(land_use_rasters[[gsub(x = land_use,
                                          pattern = "Primary non-forest_Low use - ",
                                          replacement = "")]]) <- c(-180, 180, -60, 90)
      }
    }
    
    
    #### multiply the projetced raster by th actual proportion of each grid cell
    ### which is made up of the land use.
    
    if (alpha_beta == "alpha") {
      land_use_raster <-
        land_use_raster * land_use_rasters[[land_use]]
    } else {
      if (alpha_beta == "betafor") {
        land_use_raster <-
          land_use_raster * land_use_rasters[[gsub(x = land_use,
                                                   pattern = "Primary forest_Minimal use - ",
                                                   replacement = "")]]
      } else {
        land_use_raster <-
          land_use_raster * land_use_rasters[[gsub(x = land_use,
                                                   pattern = "Primary non-forest_Low use - ",
                                                   replacement = "")]]
      }
    }
    
    
    ### return the land use raster 
    
    return(land_use_raster)
    
  }
