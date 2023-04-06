## SCRIPT

## Development; Patrick Alexander Walkden

## Quick script getting the PREDICTS site coordinates used in the nanlysis and plotting a map for use in a figure

rm(list = ls())

require(tidyverse)
require(sf)
require(stars)
require(terra)



PREDICTS_alpha <- readRDS("outputs/alpha_diversity_dataframe.rds") 


#### Visulise the sites geographically

wm <-
  map_data("world") %>% filter(region != "Antartica") %>% fortify()

## site coords

site_points <- PREDICTS_alpha %>% distinct(SSBS, Longitude, Latitude)

# generate and plot map

site_plot <- ggplot() + coord_fixed() +
  geom_map(
    data = wm,
    map = wm,
    aes(group = group, map_id = region),
    fill = "darkgrey"
  ) +
  geom_point(
    data = fortify(site_points),
    aes(Longitude, Latitude),
    colour = "black",
    size = 5,alpha = 0.2
  ) +
  scale_x_continuous(limits = c(-180, 180), breaks = seq(-180, 180, 30)) +
  scale_y_continuous(limits = c(-90, 90), breaks = seq(-90, 90, 30)) +
  theme_classic()

plot(site_plot)



###### forest maps 

forest <- terra::rast("data/projection_rasters/2000-rasters-for-projections/10km/10kmforest_biomes.tif") 

non_forest <- terra::rast("data/projection_rasters/2000-rasters-for-projections/10km/10kmnon_forest_biomes.tif") 


plot(non_forest, col = c("darkgreen","chocolate"))

