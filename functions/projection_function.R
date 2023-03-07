options(future.globals.maxSize= 10000000000000000000000000)

source("functions/functions_to_project.R")

#### begin main function

# FII_projection_function <-
#   function(raster_dir,
#            ## where the prediction rasters are held
#            outdir,
#            ## where the projection rasters are to be export to
#            alpha_model_path,
#            ## alpha model path
#            alpha_data_path,
#            ## alpha dataframe
#            forest_similarity_model_path,
#            ## similarity model path
#            forest_similarity_data_path,
#            ## similarity dataframe path
#            non_forest_similarity_model_path,
#            ## similarity model path
#            non_forest_similarity_data_path,
#            luh = c("1", "2"),
#            year = c("2000","2020"),
#            resolution = NA,
#            land_use_to_raster = list(),
#            forest_biome_path,
#            non_forest_biome_path
#            )
# 
# ##  whether the land use classifications are based in luh 1 or 2
#   {
## first we need to load in the model and the data it is based on, the dataframe is necessary to
## extract scaling variables used pre-modelling

alpha_model <- readRDS(alpha_model_path)
class(alpha_model) <- "lmerMod"

alpha_data <- data.frame(readRDS(alpha_data_path))
alpha_data <- alpha_data[, colnames(alpha_model@frame)]


print("alpha diversity model and dataframe loaded...")


#### load in data for the forest similarity baseline

forest_similarity_model <- readRDS(forest_similarity_model_path)
class(forest_similarity_model) <- "lmerMod"

forest_similarity_data <- data.frame(readRDS(forest_similarity_data_path))
forest_similarity_data  <-
  forest_similarity_data[, colnames(forest_similarity_model@frame)]

print("forest similarity diversity model and dataframe loaded...")



#### load in data for the non_forest similarity baseline

non_forest_similarity_model <- readRDS(non_forest_similarity_model_path)
class(non_forest_similarity_model) <- "lmerMod"

non_forest_similarity_data <- data.frame(readRDS(non_forest_similarity_data_path))
non_forest_similarity_data  <-
  non_forest_similarity_data[, colnames(non_forest_similarity_model@frame)]

print("non_forest similarity diversity model and dataframe loaded...")


forest_biome <- terra::rast(forest_biome_path)

terra::values(forest_biome) <- ifelse(terra::values(forest_biome) > 0, 1, NA)


non_forest_biome <- terra::rast(non_forest_biome_path)

terra::values(non_forest_biome) <- ifelse(terra::values(non_forest_biome) > 0, 1, NA)



######




alpha_prediction_rasters <-
  get_and_scale_model_rasters(
    raster_dir = raster_dir,
    model_dataframe = alpha_data,
    exp_columns = c(3:4)
  )


print(glue::glue("gathered alpha prediction rasters"))
############### weights so with my model as I only split primary minimal the wieghts are mostly the same with primary split 0.4
############### primary minimal 0.6 the rest


## NEED TO INPUT THE INTENSITY RASTERS HERE 




weighted_lu <-
  get_and_weight_lu_rasters(
    raster_dir = raster_dir,
    intensity = c("primary forest", "primary non-forest",
                                                       "secondary","plantation","pasture",
                                                       "cropland","urban"))


names(weighted_lu) <- c("Primary forest_Minimal use",
                        "Primary forest_Light use",
                        "Primary forest_Intense use",
                        "Primary non-forest_Minimal use",
                        "Primary non-forest_Light use",
                        "Primary non-forest_Intense use",
                        "Secondary vegetation_Minimal use",
                        "Secondary vegetation_Light use",
                        "Secondary vegetation_Intense use",
                        "Plantation forest_Minimal use",
                        "Plantation forest_Light use",
                        "Plantation forest_Intense use",
                        "Pasture_Minimal use",
                        "Pasture_Light use",
                        "Pasture_Intense use",
                        "Cropland_Minimal use",
                        "Cropland_Light use",
                        "Cropland_Intense use",
                        "Urban_Minimal use",
                        "Urban_Light use",
                        "Urban_Intense use")


#### now we will have to iteratively project the model for each land use



print(glue::glue("alpha: starting projecting for each land use class"))



weighted_lu[["Primary non-forest_Low use"]] <- sum(weighted_lu[["Primary non-forest_Minimal use"]],
                                                   weighted_lu[["Primary non-forest_Light use"]], 
                                                   na.rm = TRUE)

weighted_lu[["Primary non-forest"]] <- sum(weighted_lu[["Primary non-forest_Minimal use"]],
                                           weighted_lu[["Primary non-forest_Light use"]], 
                                           weighted_lu[["Primary non-forest_Intense use"]], 
                                           na.rm = TRUE)

weighted_lu[["Cropland_High use"]] <- sum(weighted_lu[["Cropland_Intense use"]],
                                          weighted_lu[["Cropland_Light use"]], 
                                          na.rm = TRUE)

weighted_lu[["Cropland"]] <- sum(weighted_lu[["Cropland_Intense use"]],
                                 weighted_lu[["Cropland_Light use"]], 
                                 weighted_lu[["Cropland_Minimal use"]], 
                                 na.rm = TRUE)

weighted_lu[["Pasture_High use"]] <- sum(weighted_lu[["Pasture_Intense use"]],
                                         weighted_lu[["Pasture_Light use"]], 
                                         na.rm = TRUE)


weighted_lu[["Urban_High use"]] <- sum(weighted_lu[["Urban_Intense use"]],
                                       weighted_lu[["Urban_Light use"]], 
                                       na.rm = TRUE)

weighted_lu[["Urban"]] <- sum(weighted_lu[["Urban_Intense use"]],
                              weighted_lu[["Urban_Light use"]], 
                              weighted_lu[["Urban_Minimal use"]], 
                              na.rm = TRUE)

weighted_lu[["Secondary vegetation_High use"]] <- sum(weighted_lu[["Secondary vegetation_Intense use"]],
                                                      weighted_lu[["Secondary vegetation_Light use"]], 
                                                      na.rm = TRUE)

weighted_lu[["Secondary vegetation"]] <- sum(weighted_lu[["Secondary vegetation_Intense use"]],
                                             weighted_lu[["Secondary vegetation_Light use"]], 
                                             weighted_lu[["Secondary vegetation_Minimal use"]], 
                                             na.rm = TRUE)

weighted_lu[["Plantation forest"]] <- sum(weighted_lu[["Plantation forest_Intense use"]],
                                          weighted_lu[["Plantation forest_Light use"]], 
                                          weighted_lu[["Plantation forest_Minimal use"]], 
                                          na.rm = TRUE)


plan(multicore(workers = detectCores() - 1))


land_use_levels <- levels(alpha_data$LUI)



alpha_projection_raster <-
  future.apply::future_lapply(X = land_use_levels,
                              FUN =  function(x) land_use_projection_function(land_use = x,
                                                                              projection_rasters = alpha_prediction_rasters,
                                                                              data = alpha_data,
                                                                              model = alpha_model,
                                                                              land_use_rasters = weighted_lu,controls = c("hpd"),
                                                                              transformation = "log",
                                                                              alpha_beta = "alpha"
                              ))



names(alpha_projection_raster) <- land_use_levels


print(glue::glue("alpha: all land use classes projected"))

alpha_combined_rast <- Reduce(alpha_projection_raster, f =  "+")





print(glue::glue("alpha: getting baseline projection for primary forest"))


forest_constants <- data.frame(
  LUI = "Primary forest_Minimal use",
  control_hpd = scale_object(0, alpha_data$control_hpd),
  log_hpd_1km = scale_object(0, alpha_data$log_hpd_1km),
  nat_hab_sw = scale_object(1, alpha_data$nat_hab_sw)
  #log_T30 = scale_object(0, alpha_data$log_T30),
  # control_roads = scale_object(0, alpha_data$control_roads)
)

forest_alpha_baseline_rast <-
  exp(terra::predict(
    alpha_prediction_rasters,
    alpha_model,
    re.form = NA,
    const = forest_constants))




forest_alpha_baseline_rast <- forest_alpha_baseline_rast * forest_biome




print(glue::glue("alpha: baseline projection for primary forest done"))



print(glue::glue("alpha: getting baseline projection for non-primary forest"))


non_forest_constants <- data.frame(
  LUI = "Primary non-forest_Minimal use",
  control_hpd = scale_object(0, alpha_data$control_hpd),
  log_hpd_1km = scale_object(0, alpha_data$log_hpd_1km),
  nat_hab_sw = scale_object(1, alpha_data$nat_hab_sw)
  # log_T30 = scale_object(0, alpha_data$log_T30),
 #  control_roads = scale_object(0, alpha_data$control_roads)
)

non_forest_alpha_baseline_rast <-
  exp(terra::predict(
    alpha_prediction_rasters,
    alpha_model,
    re.form = NA,
    const = non_forest_constants)
  )



non_forest_alpha_baseline_rast <- non_forest_alpha_baseline_rast * non_forest_biome




print(glue::glue("alpha: baseline projection for non-primary forest done"))

alpha_baseline_rast <- sum(non_forest_alpha_baseline_rast, forest_alpha_baseline_rast, na.rm = TRUE)





FII_alpha_diversity <- alpha_combined_rast / alpha_baseline_rast


FII_alpha_diversity <-
  terra::clamp(
    FII_alpha_diversity,
    lower = 0,
    upper = 1,
    values = TRUE
  )


plot(FII_alpha_diversity)

save_location <- paste(outdir,"/",resolution,"FII_alpha_diversity",year,".tif",sep = "")

writeRaster(
  FII_alpha_diversity,
  filename = save_location,
  overwrite = TRUE,
  gdal = c("COMPRESS=NONE", "TFW=YES"),
  datatype = 'FLT8S'
)


print(glue::glue("alpha FII projection saved at {save_location}"))




################################################
################################################
##### Functional similarity model projections###
################################################


print(glue::glue("beginning forest similarity projections"))

forest_similarity_prediction_rasters <- get_and_scale_model_rasters(raster_dir = raster_dir,model_dataframe = forest_similarity_data,
                                                                    exp_columns = c(3:10))



print(glue::glue("gathered forest similarity rasters"))
########################


land_use_levels <- levels(forest_similarity_data$land_use_combination)



forest_similarity_projection_raster <-
  future.apply::future_lapply(
    X = land_use_levels,
    FUN =  function(x)
      land_use_projection_function(
        land_use = x,
        projection_rasters = forest_similarity_prediction_rasters,
        data = forest_similarity_data,
        model = forest_similarity_model,
        land_use_rasters = weighted_lu,
        transformation = "logit",
        alpha_beta = "betafor"
      )
  )


print(glue::glue("similarity forest: all land use classes projected"))

forest_similarity_complete_rast <-
  Reduce(forest_similarity_projection_raster, f =  "+")



print(glue::glue("similarity forest: getting baseline projection"))

forest_constants <-
  data.frame(
    land_use_combination = land_use_levels[1],
    control_hpd = scale_object(0, forest_similarity_data$control_hpd),
    site2_log_hpd = scale_object(0, forest_similarity_data$site2_log_hpd),
    site2_nat_hab_sw = scale_object(1, forest_similarity_data$site2_nat_hab_sw),
    site2_log_T30 = scale_object(0, forest_similarity_data$site2_log_T30),
    control_roads = scale_object(0, forest_similarity_data$control_roads),
    log_environmental_distance = scale_object(log(0 + 1), forest_similarity_data$log_environmental_distance),
    log_geographic_distance = scale_object(log(0 + 1), forest_similarity_data$log_geographic_distance),
    log_hpd_diff = scale_object(0, forest_similarity_data$log_hpd_diff),
    log_T30_diff = scale_object(0, forest_similarity_data$log_T30_diff),
    nat_hab_diff = scale_object(0, forest_similarity_data$nat_hab_diff)
  )

forest_similarity_baseline_rast <-
  inv_logit(
    terra::predict(
      forest_similarity_prediction_rasters,
      forest_similarity_model,
      re.form = NA,
      const = forest_constants
    ),
    a = 0.0001
  )


print(glue::glue("similarity forest: baseline projection done"))

forest_similarity_relative_raster <-
  forest_similarity_complete_rast / forest_similarity_baseline_rast


forest_similarity_relative_raster <-
  terra::clamp(
    forest_similarity_relative_raster,
    lower = 0,
    upper = 1,
    values = TRUE
  )


forest_similarity_relative_raster <- forest_similarity_relative_raster * forest_biome



save_location <- paste(outdir,"/",resolution,"FII_forest_similarity",year,".tif",sep = "")


forest_similarity_relative_raster <- terra::rast(save_location)

writeRaster(
  forest_similarity_relative_raster,
  filename = save_location,
  overwrite = TRUE,
  gdal = c("COMPRESS=NONE", "TFW=YES"),
  datatype = 'FLT8S'
)
print(glue::glue("forest similarity FII projection saved at {save_location}"))


############# non-forest 

non_forest_similarity_prediction_rasters <- get_and_scale_model_rasters(raster_dir = raster_dir,
                                                                        model_dataframe = non_forest_similarity_data,
                                                                        exp_columns = c(3:7))


land_use_levels <- levels(non_forest_similarity_data$land_use_combination)


non_forest_similarity_projection_raster <-
  future.apply::future_lapply(
    X = land_use_levels,
    FUN =  function(x)
      land_use_projection_function(
        land_use = x,
        projection_rasters = non_forest_similarity_prediction_rasters,
        data = non_forest_similarity_data,
        model = non_forest_similarity_model,
        land_use_rasters = weighted_lu,
        transformation = "logit",
        alpha_beta = "betanfor"
      )
  )




print(glue::glue("similarity non_forest: all land use classes projected"))

non_forest_similarity_complete_rast <-
  Reduce(non_forest_similarity_projection_raster, f =  "+")


print(glue::glue("similarity non_forest: getting baseline projection"))

non_forest_constants <-
  data.frame(
    land_use_combination = "Primary non-forest_Low use - Primary non-forest_Low use",
    control_hpd = scale_object(0, non_forest_similarity_data$control_hpd),
    site2_log_hpd = scale_object(0, non_forest_similarity_data$site2_log_hpd),
    #site2_nat_hab_sw = scale_object(1, non_forest_similarity_data$site2_nat_hab_sw),
    #site2_log_T30 = scale_object(0, non_forest_similarity_data$site2_log_T30),
    site2_log_roads = scale_object(0, non_forest_similarity_data$site2_log_roads),
    control_roads = scale_object(0, non_forest_similarity_data$control_roads),
    log_environmental_distance = scale_object(log(0 + 1), non_forest_similarity_data$log_environmental_distance),
    log_geographic_distance = scale_object(log(0 + 1), non_forest_similarity_data$log_geographic_distance),
    log_hpd_diff = scale_object(0, non_forest_similarity_data$log_hpd_diff),
    #log_T30_diff = scale_object(0, non_forest_similarity_data$log_T30_diff),
    nat_hab_diff = scale_object(0, non_forest_similarity_data$nat_hab_diff),
    log_roads_diff = scale_object(0, non_forest_similarity_data$log_roads_diff)
  )

non_forest_similarity_baseline_rast <-
  inv_logit(
    terra::predict(
      non_forest_similarity_prediction_rasters,
      non_forest_similarity_model,
      re.form = NA,
      const = non_forest_constants
    ),
    a = 0.0001
  )


print(glue::glue("similarity non_forest: baseline projection done"))

non_forest_similarity_relative_raster <-
  non_forest_similarity_complete_rast / non_forest_similarity_baseline_rast


non_forest_similarity_relative_raster <-
  terra::clamp(
    non_forest_similarity_relative_raster,
    lower = 0,
    upper = 1,
    values = TRUE
  )


non_forest_similarity_relative_raster <- non_forest_similarity_relative_raster * non_forest_biome


save_location <- paste(outdir,"/",resolution,"FII_non_forest_similarity",year,".tif",sep = "")


non_forest_similarity_relative_raster <- terra::rast(save_location)

writeRaster(
  non_forest_similarity_relative_raster,
  filename = save_location,
  overwrite = TRUE,
  gdal = c("COMPRESS=NONE", "TFW=YES"),
  datatype = 'FLT8S'
)



similarity_relative_raster <- sum(forest_similarity_relative_raster, non_forest_similarity_relative_raster, na.rm = TRUE)



save_location <- paste(outdir,"/",resolution,"FII_similarity",year,".tif",sep = "")


writeRaster(
  similarity_relative_raster,
  filename = save_location,
  overwrite = TRUE,
  gdal = c("COMPRESS=NONE", "TFW=YES"),
  datatype = 'FLT8S'
)


print(glue::glue("similiarity FII raster saved at {save_location}"))


print(glue::glue("Deriving FII indicator as the product of alpha and similarity"))

FII_raster <- FII_alpha_diversity * similarity_relative_raster

print(glue::glue("FII derived."))


save_location <- paste(outdir,"/",resolution,"FUNCTIONAL_INTACTNESS_INDEX",year,".tif",sep = "")

writeRaster(
  FII_raster,
  filename = save_location,
  overwrite = TRUE,
  gdal = c("COMPRESS=NONE", "TFW=YES"),
  datatype = 'FLT8S'
)


print(glue::glue("FII {year} raster saved at {save_location}"))

closeAllConnections()