

rm(list = ls())


require(terra)
require(magrittr)
require(tidyverse)
require(sf)
require(rnaturalearth)
require(rnaturalearthdata)



find_position <- function(x,y,difference = FALSE){
  
  value <- which(x > y)
  
  if(is_empty(value)){
    value <- 1
  } else {
    value <- value[length(value)]
  }
  
  return(value)   
}


newcrs <-
  "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

FII_2000 <-
  terra::rast("outputs/FII_rasters/10kmFUNCTIONAL_INTACTNESS_INDEX2000.tif")

oldcrs <- crs(FII_2000)

FII_2000 <- terra::project(x = FII_2000, y = newcrs)


FII_2020 <-
  terra::rast("outputs/FII_rasters/10kmFUNCTIONAL_INTACTNESS_INDEX2020.tif")

FII_2020 <- terra::project(x = FII_2020, newcrs)


biome_list <- readRDS("data/biome_list.rds")


values(FII_2020) <-
  ifelse(!(!is.na(values(FII_2000)) &
             !is.na(values(FII_2020))), NA, values(FII_2020))
values(FII_2000) <-
  ifelse(!(!is.na(values(FII_2000)) &
             !is.na(values(FII_2020))), NA, values(FII_2000))



for (i in c(16, 15, 13)) {
  
  
  
  biome_shape <- terra::vect(biome_list[[i]])
  crs(biome_shape) <- oldcrs
  biome_shape <- terra::project(biome_shape, newcrs)
  
  
  
  FII_2020 <-
    terra::mask(FII_2020, mask = biome_shape, inverse = TRUE)
  FII_2000 <-
    terra::mask(FII_2000, mask = biome_shape, inverse = TRUE)
}


median(terra::values(FII_2000), na.rm = TRUE)
median(terra::values(FII_2020), na.rm = TRUE)


#### legend colours going to view it 

FII_2000_values <- as.data.frame(FII_2000, xy = TRUE) %>% tidyr::drop_na()


FII_quants <- c()

for(i in seq(0,1,length.out = 1000)){
  FII_quants <- c(FII_quants, quantile(FII_2000_values$lyr1, i))
}



FII_colours <- colorRampPalette(c("darkred","red", "orange","cadetblue","midnightblue"))


FII_col <- data.frame(values = FII_quants, colours = FII_colours(length(FII_quants)))

write_rds(file = "outputs/FII_legend_colours.rds", FII_col)

######## differenc eplot and getting the legend colours

difference <- (FII_2020 / FII_2000 * 100) - 100

difference_df <- as.data.frame(difference, xy = TRUE) %>% tidyr::drop_na()

high_values <- difference_df$lyr1[difference_df$lyr1>0]  

high_quants <- c()

for(i in seq(0.001,0.999,length.out = 500)){

high_quants <- c(high_quants,quantile(high_values,i))

}

low_values <- difference_df$lyr1[difference_df$lyr1<0]  

low_quants <- c()

for(i in seq(0.001,0.999,length.out = 500)){
  
  low_quants <- c(low_quants,quantile(low_values,i))
  
}


  

low_col <-
  colorRampPalette(c("orange4" , "orange"))

low_colours <- data.frame(values = low_quants, colours = low_col(length(low_quants)))  


high_col <-
  colorRampPalette(c("springgreen", "darkgreen"))

high_colours <- data.frame(values = high_quants, colours = high_col(length(low_quants)))  

zero_colour <- data.frame(values = 0, colours = "#00FF7F")



all_col <- rbind(low_colours,zero_colour,high_colours[-length(high_colours),])


write_rds(file = "outputs/difference_legend_colours.rds", all_col)



world <- ne_countries(scale = "medium", returnclass = "sf")

world_poly <- st_combine(world$geometry) %>% st_transform(newcrs)



raster_plot <-
  function(raster,
           map = FALSE,
           polygon = NULL,
           difference = FALSE) {

    
    mask_frame <- as.data.frame(raster, xy = TRUE) %>% tidyr::drop_na()
    colnames(mask_frame)[3] <- "layer"
    
    
    
    if(difference){
    
mask_frame$position <- apply(mask_frame, MARGIN = 1,FUN = function(x) find_position(x = x[3], y = all_col$values))
mask_frame$position <- factor(mask_frame$position)
      
      
    } else {
    ### colours
    
      mask_frame$position <- apply(mask_frame, MARGIN = 1,FUN = function(x) find_position(x = x[3], y = FII_col$values))
      mask_frame$position <- factor(mask_frame$position)
      
    
    }
    
    
    # generate and plot map
    
    raster_plot <- ggplot() + coord_fixed()
    
    if (map) {
      
      world_polygon <-
        st_as_sf(world_poly) %>% st_transform(crs = newcrs) %>% fortify()
      

      
      raster_plot <- raster_plot +
        geom_sf(data = world_polygon,
                fill = "grey40",
                linewidth = NA) 
      
   
      
      #      scale_x_continuous(limits = c(-180, 180), breaks = seq(-180, 180, 30)) +
      #     scale_y_continuous(limits = c(-60, 90), breaks = seq(-60, 90, 30))
    }
    
    
    
    
    if(difference){
      
      
      raster_plot <-
        raster_plot + geom_tile(data = mask_frame,
                                aes(x = x, y = y, fill = position),
                                show.legend = FALSE)  +
        scale_fill_manual(name = "position", values = all_col$colours) +xlim(min(mask_frame$x, na.rm = TRUE) - 50000,
                                                                              max(mask_frame$x, na.rm = TRUE) + 50000) +
        ylim(min(mask_frame$y, na.rm = TRUE) - 50000,
             max(mask_frame$y, na.rm = TRUE) + 50000)
      
    } else {
     raster_plot <- raster_plot + geom_tile(data = mask_frame,
                               aes(x = x, y = y, fill = position),
                               show.legend = FALSE)  +
       scale_fill_manual(name = "position", values = FII_col$colours)
    }
    

    
    if (!is.null(polygon)) {
      for (i in 1:length(polygon)) {
        poly <- polygon[[i]]
        raster_plot <- raster_plot +
          geom_sf(
            data = poly,
            fill = NA,
            linewidth = 1.5,
            colour = "black"
          )
      }
      
    }
    
    
   raster_plot <- raster_plot +
      theme(
        axis.line = element_line(colour = "black", linetype = "solid"),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.background = element_rect(fill = 'white', color = 'white'),
        panel.grid.major = element_line(color = 'white'),
        panel.grid.minor = element_line(color = 'white')
      )
    
    return(raster_plot)
  }



FII_2000_plot <- raster_plot(FII_2000, map = TRUE)
FII_2020_plot <- raster_plot(FII_2020, map = TRUE)




ggsave(
  filename = "outputs/FII_maps_and_graphs/10kmFII_2020_plot.png",
  FII_2020_plot,
  device = "png",
  height = 10,
  width = 40,
  dpi = 300
)
ggsave(
  filename = "outputs/FII_maps_and_graphs/10kmFII_2000_plot.png",
  FII_2000_plot,
  device = "png",
  height = 10,
  width = 40,
  dpi = 300
)

############################
## forest vs non-forest plots
###########################

## for these plot we want to highlight the regions that we are going to draw out and highlight how they have
## chnage over time -- for the forest biomes I am going to highlight then amazon and island of borneo

ecoregions <- st_read("data/Ecoregions2017/Ecoregions2017.shp")

bioregions_list <- readRDS("outputs/bioregions_list.rds")

borneo_ecoregions <-
  bioregions_list$`Borneo Tropical Forests & Sundaland Heath Forests`

sf_use_s2(FALSE)

borneo_bioregion <-
  ecoregions %>% dplyr::filter(ECO_NAME %in% borneo_ecoregions) %>%
  st_union() %>%
  st_buffer(0.00001) %>%
  st_transform(crs = newcrs)


## Now for the amazon


amazon_ecoregions <-
  unlist(bioregions_list[c(
    "Northern Amazonian Forests",
    "Western Amazonian Forests & Plains",
    "Southern Amazonian Forests",
    "Amazon River Estuary",
    "Central Amazonian Forests",
    "Venezuelan Coast",
    "Llanos & Dry Forests",
    "Guianan Forests & Savanna"
  )])


sf_use_s2(FALSE)


amazon_bioregion <-
  ecoregions %>% dplyr::filter(ECO_NAME %in% amazon_ecoregions) %>%
  st_union() %>% 
  st_buffer(0.000001) %>%
  st_transform(crs = newcrs)



forest <-
  terra::rast(
    "data/projection_rasters/2000-rasters-for-projections/10km/10kmforest_biomes.tif"
  ) %>%
  terra::project(newcrs)


non_forest <-
  terra::rast(
    "data/projection_rasters/2000-rasters-for-projections/10km/10kmnon_forest_biomes.tif"
  ) %>%
  terra::project(newcrs)




forest_FII_2000 <- FII_2000 * forest
values(forest_FII_2000) <-
  ifelse(values(forest_FII_2000) == 0, NA, values(forest_FII_2000))
median(values(forest_FII_2000), na.rm = TRUE)



forest_FII_2000_plot <-
  raster_plot(forest_FII_2000, map = TRUE, polygon = list(borneo_bioregion,amazon_bioregion))

ggsave(
  filename = "outputs/FII_maps_and_graphs/10kmforest_FII_2000_plot.png",
  forest_FII_2000_plot,
  device = "png",
  height = 10,
  width = 40,
  dpi = 300
)




forest_FII_2020 <- FII_2020 * forest
values(forest_FII_2020) <-
  ifelse(values(forest_FII_2020) == 0, NA, values(forest_FII_2020))
median(values(forest_FII_2020), na.rm = TRUE)



forest_FII_2020_plot <- raster_plot(forest_FII_2020, map = TRUE, polygon = list(borneo_bioregion,amazon_bioregion))

ggsave(
  filename = "outputs/FII_maps_and_graphs/10kmforest_FII_2020_plot.png",
  forest_FII_2020_plot,
  device = "png",
  height = 10,
  width = 40,
  dpi = 300
)

##### Non- forest bioregions that I am going to high are the chaco, cerrado and pantanal in south america
##### and the greater african subequitorial savannas and mixed woodlands 

south_america_ecoregions <- unlist(bioregions_list[c("Rio de la Plata Grasslands", "Cerrado Savannas","Chaco Grasslands","Pantanal Flooded Grasslands & Dry Forests","Patagonia Steppe & Low Mountains")])

south_america_bioregion <- ecoregions %>% 
  dplyr::filter(ECO_NAME %in% south_america_ecoregions) %>%
  st_union() %>% st_buffer(0.0001) %>% 
  st_transform(crs = newcrs)



southern_africa_ecoregions <-
  as.character(c(unlist(bioregions_list[c(
    "Greater African Subequatorial Savannas & Mixed Woodlands",
    "Southeast African Subtropical Grasslands"
  )]),"Kalahari Acacia woodlands"))



southern_africa_bioregion <- ecoregions %>% 
  dplyr::filter(ECO_NAME %in% southern_africa_ecoregions) %>%
  st_union() %>% st_buffer(0.0001) %>%
  st_transform(newcrs) 




non_forest_FII_2000 <- FII_2000 * non_forest
values(non_forest_FII_2000) <-
  ifelse(values(non_forest_FII_2000) == 0,
         NA,
         values(non_forest_FII_2000))
median(values(non_forest_FII_2000), na.rm = TRUE)

non_forest_FII_2000_plot <-
  raster_plot(non_forest_FII_2000, map = TRUE,polygon = list(south_america_bioregion,southern_africa_bioregion))

ggsave(
  filename = "outputs/FII_maps_and_graphs/10kmnon_forest_FII_2000_plot.png",
  non_forest_FII_2000_plot,
  device = "png",
  height = 10,
  width = 40,
  dpi = 300
)



non_forest_FII_2020 <- FII_2020 * non_forest
values(non_forest_FII_2020) <-
  ifelse(values(non_forest_FII_2020) == 0,
         NA,
         values(non_forest_FII_2020))
median(values(non_forest_FII_2020), na.rm = TRUE)

non_forest_FII_2020_plot <-
  raster_plot(non_forest_FII_2020, map = TRUE, polygon = list(south_america_bioregion,southern_africa_bioregion))

ggsave(
  filename = "outputs/FII_maps_and_graphs/10kmnon_forest_FII_2020_plot.png",
  non_forest_FII_2020_plot,
  device = "png",
  height = 10,
  width = 40,
  dpi = 300
)



# ############### Cut out country shapes and extract mean FII
#

countries <-
  st_read("../../Datasets/Country_shapefiles/all_countries.shp")



country_median_FII_values <- c()
country_full_FII_values <- c()

for (i in 1:nrow(countries)) {
  vector <- terra::vect(countries$geometry[i])
  vector <- terra::project(x = vector, newcrs)
  
  if (ext(vector)[3] < ext(FII_2020)[3] |
      is.na(countries$iso_short[i])) {
    next()
  }
  
  
  
  mask_2020 <- terra::crop(FII_2020, vector) %>% terra::mask(vector)
  mask_2000 <- terra::crop(FII_2000, vector) %>% terra::mask(vector)
  
  
  
  
  sf_use_s2(TRUE)
  if (!st_is_valid(countries$geometry[i])) {
    sf_use_s2(FALSE)
  }
  
  
  if ((sum(terra::values(mask_2020) > 0, na.rm = TRUE) /
       
       (as.numeric(st_area(
         countries$geometry[i]
       )) / (10000 * 10000))) < 0.3) {
    next()
  }
  
  
  full_values <- data.frame(
    country = countries$iso_short[i],
    values_2000 = as.data.frame(mask_2000)[, 1],
    values_2020 = as.data.frame(mask_2020)[, 1],
    country_area = as.numeric(st_area(countries$geometry[i]))
  )
  
  
  
  median_values <-
    data.frame(
      country = countries$iso_short[i],
      country_area = as.numeric(st_area(countries$geometry[i])),
      median_FII_2020 = median(terra::values(mask_2020), na.rm = TRUE),
      median_FII_2000 = median(terra::values(mask_2000), na.rm = TRUE)
    )
  
  country_median_FII_values <-
    rbind(country_median_FII_values, median_values)
  country_full_FII_values <-
    rbind(country_full_FII_values, full_values)
  
  
  
}


write_rds(file = "outputs/10kmmean_country_FII.rds", x = country_median_FII_values)
write_rds(file = "outputs/10kmfull_country_FII.rds", x = country_full_FII_values)


####################
## BIOREGIONS #####
###################


## function

difference_function <- function(bioregion, forest = TRUE){
  
  bioregion <- terra::vect(bioregion)
  
  extent <- terra::ext(bioregion)[1:4] + c(-50000,50000,-50000,50000)
  
  
  if(forest){
  
    bioregion_2000 <-
      terra::mask(forest_FII_2000, mask = bioregion, inverse = FALSE) %>%
      terra::crop(extent) 
    
    
    bioregion_2020 <-
      terra::mask(forest_FII_2020, mask = bioregion, inverse = FALSE) %>%
      terra::crop(extent) 

  
  } else {
    
    bioregion_2000 <-
      terra::mask(non_forest_FII_2000, mask = bioregion, inverse = FALSE) %>%
      terra::crop(extent) 
    
    
    bioregion_2020 <-
      terra::mask(non_forest_FII_2020, mask = bioregion, inverse = FALSE) %>%
      terra::crop(extent) 
    
  }
  
  
  difference <- (bioregion_2020 / bioregion_2000 * 100) - 100

  difference_rasters <- c(bioregion_2000,bioregion_2020,difference)
  
  return(difference_rasters)
  
}


###### Amazon

amazon_difference <- difference_function(amazon_bioregion, forest = TRUE)

median(terra::values(amazon_difference[[1]], na.rm = TRUE))
median(terra::values(amazon_difference[[2]], na.rm = TRUE))


amazon_difference_plot <- raster_plot(raster = amazon_difference[[3]],map = TRUE,difference = TRUE)


ggsave(
  filename = "outputs/FII_maps_and_graphs/10kmamazon_FII_plot.png",
  amazon_difference_plot,
  device = "png",
  height = 25,
  width = 20,
  dpi = 300
)


#### borneo 

borneo_difference <- difference_function(borneo_bioregion, forest = TRUE)

median(terra::values(borneo_difference[[1]], na.rm = TRUE))
median(terra::values(borneo_difference[[2]], na.rm = TRUE))

borneo_difference_plot <- raster_plot(raster = borneo_difference, map = TRUE, difference = TRUE) 

ggsave(
  filename = "outputs/FII_maps_and_graphs/10kmborneo_FII_plot.png",
  borneo_difference_plot,
  device = "png",
  height = 25,
  width = 20,
  dpi = 300
)


#########################
### Non-forest bioregions
#########################

### south ameirca

south_america_difference <- difference_function(south_america_bioregion, forest = FALSE)

median(terra::values(south_america_difference[[1]], na.rm = TRUE))
median(terra::values(south_america_difference[[2]], na.rm = TRUE))

south_america_plot <- raster_plot(raster = south_america_difference, map = TRUE, difference = TRUE)


ggsave(
  filename = "outputs/FII_maps_and_graphs/10kmsouth_america_FII_plot.png",
  south_america_plot,
  device = "png",
  height = 30,
  width = 20,
  dpi = 300
)

### southern africa


southern_africa_difference <- difference_function(southern_africa_bioregion, forest = FALSE)

median(terra::values(southern_africa_difference[[1]], na.rm = TRUE))
median(terra::values(southern_africa_difference[[2]], na.rm = TRUE))

southern_africa_difference_plot <- raster_plot(southern_africa_difference, map = TRUE, difference = TRUE)


ggsave(
  filename = "outputs/FII_maps_and_graphs/10kmsouthern_africa_FII_plot.png",
  southern_africa_difference_plot,
  device = "png",
  height = 30,
  width = 20,
  dpi = 300
)
