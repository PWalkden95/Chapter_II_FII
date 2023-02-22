#### SCRIPT 

## post-hoc cleaning and map generation of the global prrojections of FII

## DEVLEOPMENT: Patrick Alexander Walkden


rm(list = ls())

require(tidyverse)
require(sf)
require(terra)


# -------------
##Load in projection maps for the years projected

FII_2000 <- terra::rast("outputs/10kmFUNCTIONAL_INTACTNESS_INDEX2000.tif")

FII_2020 <- terra::rast("outputs/10kmFUNCTIONAL_INTACTNESS_INDEX2020.tif")


#### make cells that only have a value in one of the maps NA so that conparisons are equal 

values(FII_2020) <- ifelse(!(!is.na(values(FII_2000)) & !is.na(values(FII_2020))), NA, values(FII_2020))
values(FII_2000) <- ifelse(!(!is.na(values(FII_2000)) & !is.na(values(FII_2020))), NA, values(FII_2000))

### we want to mask desserts as it is uncertain what is really going on there, additionally 
### tundra and rock and ice biomes

biome_list <- readRDS("data/biome_list.rds")

for(i in c(16,15,13)){
  
  FII_2020 <- terra::mask(FII_2020, mask = st_as_sf(biome_list[[i]]), inverse = TRUE)
  FII_2000 <- terra::mask(FII_2000, mask = st_as_sf(biome_list[[i]]), inverse = TRUE)
}


### let's see what the mean values are 

mean(terra::values(FII_2000), na.rm = TRUE)
mean(terra::values(FII_2020), na.rm = TRUE)


# --------------- 
## mapping of the global projections



## plotting function

raster_plot <- function(raster, map = FALSE){
  
  mask_frame <- as.data.frame(raster, xy = TRUE) %>% drop_na()
  
  ### colours 
  
  my_colour <-
    colorRampPalette(c("red","orange", "cadetblue3" ,"midnightblue"))
  
  colours <- my_colour(500)
  
  my_colours_2 <- colorRampPalette(c("#4A2545","darkred" ,"red"))
  
  
  colours <- c(my_colours_2(500), colours)
  
  
  
  
  # generate and plot map
  
  raster_plot <- ggplot() + coord_fixed() 
  
  if(map){
    
    wm <-
      map_data("world") %>% filter(region != "Antartica") %>% fortify()
    
    raster_plot <- raster_plot +
      geom_map(
        data = wm,
        map = wm,
        aes(group = group, map_id = region),
        fill = "gray30"
      ) +
      scale_x_continuous(limits = c(-180, 180), breaks = seq(-180, 180, 30)) +
      scale_y_continuous(limits = c(-60, 90), breaks = seq(-60, 90, 30)) 
  }
  
  
  raster_plot <- raster_plot +
    geom_tile( data = mask_frame, aes( x = x, y = y, fill = lyr1)) +
    scale_fill_gradientn(breaks = c(0,0.2,0.4,0.6,0.8,1), limits = c(0,1),
                         labels = c(0,0.2,0.4,0.6,0.8,1), colours = colours) +
    theme(
      axis.line = element_line(colour = "black", linetype = "solid"),
      axis.title.y = element_blank(),
      #axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      # axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      panel.background = element_rect(fill = 'white', color = 'white'),
      panel.grid.major = element_line(color = 'white'),
      panel.grid.minor = element_line(color = 'white')
    )
  
  
  
  return(raster_plot)
}


FII_2000_plot <- raster_plot(FII_2000)
FII_2020_plot <- raster_plot(FII_2020)




ggsave(filename = "FII_2020_plot.png",FII_2020_plot,device = "png", height = 10, width = 40,dpi=300)
ggsave(filename = "FII_2000_plot.png",FII_2000_plot,device = "png", height = 10, width = 40,dpi=300)



# ------------

# ############### Cut out country shapes and extract mean FII
# 

countries <- readOGR("../../Datasets/Country_shapefiles/all_countries.shp")

country_data <- countries@data

country_polygons <- sp::SpatialPolygons(countries@polygons, proj4string=CRS(as.character(crs(FII_2020))))



country_mean_FII_values <- c()


for(i in 1:nrow(country_data)){
  
  
  
  vector <- terra::vect(country_polygons[as.numeric(i)])
  
  if(ext(vector)[3] < ext(FII_2020)[3] | is.na(country_data[i,"iso_short"])){
    next()
  }
  
  print(country_data[i, "iso_short"])
  
  mask_2020 <- terra::crop(FII_2020,vector) %>% terra::mask(vector)
  mask_2000 <- terra::crop(FII_2000,vector) %>% terra::mask(vector)
  
  
  mean_values <-
    data.frame(country = country_data[i, "iso_short"],
               mean_FII_2020 = mean(terra::values(mask_2020), na.rm = TRUE),
               mean_FII_2000 = mean(terra::values(mask_2000), na.rm = TRUE))
  
  country_mean_FII_values <- rbind(country_mean_FII_values,mean_values)
  
}


write_rds(file = "outputs/mean_country_FII.rds", x = country_mean_FII_values)
