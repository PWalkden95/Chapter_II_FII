

rm(list = ls())


require(terra)
require(rgdal)
require(magrittr)
require(tidyverse)
require(raster)
require(sf)



FII_2000 <- terra::rast("outputs/10kmFUNCTIONAL_INTACTNESS_INDEX2000.tif")



FII_2020 <- terra::rast("outputs/10kmFUNCTIONAL_INTACTNESS_INDEX2020.tif")


biome_list <- readRDS("data/biome_list.rds")

values(FII_2020) <- ifelse(!(!is.na(values(FII_2000)) & !is.na(values(FII_2020))), NA, values(FII_2020))
values(FII_2000) <- ifelse(!(!is.na(values(FII_2000)) & !is.na(values(FII_2020))), NA, values(FII_2000))

for(i in c(16,15,13)){
  
  FII_2020 <- terra::mask(FII_2020, mask = st_as_sf(biome_list[[i]]), inverse = TRUE)
  FII_2000 <- terra::mask(FII_2000, mask = st_as_sf(biome_list[[i]]), inverse = TRUE)
}


mean(terra::values(FII_2000), na.rm = TRUE)
mean(terra::values(FII_2020), na.rm = TRUE)




difference <- FII_2020 - FII_2000


hist(terra::values(difference))


plot(difference > 0)
plot(difference < 0)






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
    geom_tile( data = mask_frame, aes( x = x, y = y, fill = layer)) +
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


FII_2000_plot <- raster_plot(FII_2000,map = TRUE)
FII_2020_plot <- raster_plot(FII_2020, map = TRUE)




ggsave(filename = "FII_2020_plot.png",FII_2020_plot,device = "png", height = 10, width = 40,dpi=300)
ggsave(filename = "FII_2000_plot.png",FII_2000_plot,device = "png", height = 10, width = 40,dpi=300)



# ############### Cut out country shapes and extract mean FII
# 

countries <- st_read("../../Datasets/Country_shapefiles/all_countries.shp")






country_median_FII_values <- c()
country_full_FII_values <- c()

for(i in 1:nrow(countries)){
  
  
  
  vector <- terra::vect(countries$geometry[i])
  
  if(ext(vector)[3] < ext(FII_2020)[3] | is.na(countries$iso_short[i])){
    next()
  }
  
  
  
  mask_2020 <- terra::crop(FII_2020,vector) %>% terra::mask(vector)
  mask_2000 <- terra::crop(FII_2000,vector) %>% terra::mask(vector)
  
  
  
  
  sf_use_s2(TRUE)
  if (!st_is_valid(countries$geometry[i])) {
    sf_use_s2(FALSE)
  }
  
  
  if(
    (sum(terra::values(mask_2020) > 0, na.rm = TRUE)/
     
     (as.numeric(st_area(countries$geometry[i]))/(10000*10000))) < 0.3){
    next()
  }
  
  
  full_values <- data.frame(country = countries$iso_short[i],
                            values_2000 = as.data.frame(mask_2000)[,1],
                            values_2020 = as.data.frame(mask_2020)[,1],
                            country_area = as.numeric(st_area(countries$geometry[i])))
  
  
  
  median_values <-
    data.frame(country = countries$iso_short[i],
               country_area = as.numeric(st_area(countries$geometry[i])),
               median_FII_2020 = median(terra::values(mask_2020), na.rm = TRUE),
               median_FII_2000 = median(terra::values(mask_2000), na.rm = TRUE))
  
  country_median_FII_values <- rbind(country_median_FII_values,median_values)
  country_full_FII_values <- rbind(country_full_FII_values, full_values)
  
  
  
}


write_rds(file = "outputs/10kmmean_country_FII.rds", x = country_median_FII_values)
write_rds(file = "outputs/10kmfull_country_FII.rds", x = country_full_FII_values)
