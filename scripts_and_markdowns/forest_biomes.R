rm(list = ls())

require(tidyverse)
require(sf)
require(sp)
require(terra)
require(raster)


World_biomes <- sf::read_sf("../../Datasets/WWF_Terrestrial_Biomes/official/wwf_terr_ecos.shp")
World_biomes$BIOME <- factor(World_biomes$BIOME)


world_raster <- raster::raster("data/projection_rasters/2020-rasters-for-projections/1km/1kmLU-c3ann-2020.tif")


world_raster <- raster::reclassify(world_raster, c(-Inf,Inf,0))


###############################################
#################################################
#################################################

levels(World_biomes$BIOME) <- c("Tropical & Subtropical Moist Broadleaf Forests","Tropical & Subtropical Dry Broadleaf Forests",
                                "Tropical & Subtropical Coniferous Forests", "Temperate Broadleaf & Mixed Forests","Temperate Conifer Forests",
                                "Boreal Forests/Taiga","Tropical & Subtropical Grasslands, Savannas & Shrublands","Temperate Grasslands, Savannas & Shrublands",
                                "Flooded Grasslands & Savannas","Montane Grasslands & Shrublands","Tundra","Mediterranean Forests, Woodlands & Scrub",
                                "Deserts & Xeric Shrublands","Mangroves","Lakes","Rock & Ice")


colours <- c("darkolivegreen4","darkseagreen","lightgreen","darkolivegreen1","turquoise","darkslategray4","darkseagreen2","khaki1","powderblue",
             "cadetblue1","darkslategray3","salmon","lightyellow1","tomato1","steelblue4","white")

wm<-map_data("world")



forest_biomes <- levels(World_biomes$BIOME)[c(1:6,12,14)]

data <- World_biomes %>% dplyr::filter(BIOME %in% forest_biomes)

polygon <- st_as_sf(st_transform(st_combine(data), crs = crs(world_raster)))


forest_biomes <- terra::rast(fasterize::fasterize(sf = polygon, raster = world_raster, fun = "count",background = NA))



non_forest_biomes <- levels(World_biomes$BIOME)[c(7:11,13,15:16)]

non_forest_data <- World_biomes %>% dplyr::filter(BIOME %in% non_forest_biomes)

polygon <- st_as_sf(st_transform(st_combine(non_forest_data), crs = crs(world_raster)))


non_forest_biomes <- terra::rast(fasterize::fasterize(sf = polygon, raster = world_raster, fun = "count",background = NA))



forest_pri <- terra::rast("data/projection_rasters/2000-rasters-for-projections/unformatted_rasters/LU-primf-2000.tif")
forest_sec <- terra::rast("data/projection_rasters/2000-rasters-for-projections/unformatted_rasters/LU-secdf-2000.tif")

forest <- sum(forest_pri, forest_sec, na.rm = TRUE)

forest <- forest > 0.5

forest_biomes <- sum(forest_biomes, forest, na.rm = TRUE)

forest_biomes <- forest_biomes > 0


non_forest_overlap <- sum(non_forest_biomes, forest, na.rm = TRUE)

terra::values(non_forest_biomes)[which(terra::values(non_forest_overlap) == 2)] <- NA



#################################
#################################

non_forest_pri <-  terra::rast("data/projection_rasters/2000-rasters-for-projections/unformatted_rasters/LU-primn-2000.tif")
non_forest_sec <- terra::rast("data/projection_rasters/2000-rasters-for-projections/unformatted_rasters/LU-secdn-2000.tif")

non_forest <- sum(non_forest_pri, non_forest_sec, na.rm = TRUE)

non_forest <- non_forest > 0.5

non_forest_biomes <- sum(non_forest_biomes, non_forest, na.rm = TRUE)


non_forest_biomes <- non_forest_biomes > 0

plot(non_forest_biomes)


forest_overlap <- sum(forest_biomes, non_forest, na.rm = TRUE)

terra::values(forest_biomes)[which(terra::values(forest_overlap) == 2)] <- NA


plot(sum(forest_biomes, non_forest_biomes, na.rm = TRUE))




raster::writeRaster(
  forest_biomes,
  filename = "data/projection_rasters/2020-rasters-for-projections/unformatted_rasters/forest_biomes.tif",
  overwrite = TRUE,
  gdal = c("COMPRESS=NONE", "TFW=YES"),
  datatype = 'FLT8S'
)

raster::writeRaster(
  non_forest_biomes,
  filename = "data/projection_rasters/2020-rasters-for-projections/unformatted_rasters/non_forest_biomes.tif",
  overwrite = TRUE,
  gdal = c("COMPRESS=NONE", "TFW=YES"),
  datatype = 'FLT8S'
)
