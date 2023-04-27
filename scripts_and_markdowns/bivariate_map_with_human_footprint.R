####SCRIPT

##Development: Patrick Alexander Walkden

##Description: script to somewhat validate FII by correlating global projections of FII with the 
## the human footpint index 

rm(list = ls())


require(terra)
require(tidyverse)
require(bivariatemaps)
require(biscale)
require(sf)
require(rnaturalearth)
require(rnaturalearthdata)
require(future.apply)
require(doParallel)


find_position <- function(x,y){
  
  value <- which(x > y)
  
  if(is_empty(value)){
    value <- 1
  } else {
    value <- value[length(value)]
  }
  
  return(value)   
}


inv_logit <- function(f, a) {
  a <- (1 - 2 * a)
  (a * (1 + exp(f)) + (exp(f) - 1)) / (2 * a * (1 + exp(f)))
}




newcrs <-
  "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

FII_2020 <- terra::rast("outputs/FII_rasters/10kmFUNCTIONAL_INTACTNESS_INDEX2000.tif") %>%
  terra::project(newcrs)




human_footprint <-terra::rast("data/Human_footprint/wildareas-v3-2009-human-footprint.tif") %>% 
  terra::project(newcrs) %>%
   terra::resample(FII_2020)



terra::values(human_footprint) <- ifelse(!is.nan(terra::values(FII_2020)),
                                                 terra::values(human_footprint),
                                                 NA)

biome_list <- readRDS("data/biome_list.rds")

for (i in c(16, 15, 13)) {
  
  
  
  biome_shape <- terra::vect(biome_list[[i]])
  biome_shape <- terra::project(biome_shape, newcrs)
  
  
  
  FII_2020 <-
    terra::mask(FII_2020, mask = biome_shape, inverse = TRUE)
  human_footprint <-
    terra::mask(human_footprint, mask = biome_shape, inverse = TRUE)
}



hfdf <- terra::as.data.frame(human_footprint, xy = TRUE) %>% drop_na()
fii_df <- terra::as.data.frame(FII_2020, xy = TRUE) %>% drop_na() %>% dplyr::left_join(hfdf)


colnames(fii_df)[3:4] <- c("FII","human_footprint")



hist(log(fii_df$human_footprint))
hist(car::logit(fii_df$FII, adjust = 0.01))


fii_df$log_human <- log(fii_df$human_footprint + 1)
fii_df$logit_fii <- car::logit(fii_df$FII, adjust = 0.01)


model <- lm(logit_fii ~ log_human + 0,  offset = rep(4.59512, nrow(fii_df)), data = fii_df)

mod_sum <- summary(model)



fii_df$predictions <-  predict(model, new_data = fii_df)





fii_df$residuals <- abs(fii_df$predictions - fii_df$logit_fii)




resid_se <- mod_sum$sigma

correlation_breaks <- seq(0,resid_se, length.out = 200)

disagreement_breaks <- seq(resid_se, max(abs(fii_df$residuals)), length.out = 200)


correlation_colours <- colorRampPalette(c("red3" ,"firebrick1" ,"indianred","#d27979"))

cor_cols <- data.frame(values = correlation_breaks, colours = correlation_colours(200))


disagreement_colours <- colorRampPalette(c("#a3b5c7","skyblue","deepskyblue", "dodgerblue"))

dis_cols <- data.frame(values = disagreement_breaks, colours = disagreement_colours(200))


full_cols <- rbind(cor_cols,dis_cols)


registerDoParallel(cores = detectCores() - 1)



position <- foreach(i = 1:nrow(fii_df),.combine = "c", .packages = c("tidyverse")) %dopar% {
  
  
  number <- find_position(x = fii_df[i,8], y = full_cols$values)
  
  return(number)
}




closeAllConnections()

fii_df$position <- position
fii_df$position <- factor(fii_df$position)


world <- ne_countries(scale = "medium", returnclass = "sf")

world_poly <- st_combine(world$geometry) %>% st_transform(newcrs)

world_polygon <-
  st_as_sf(world_poly) %>% fortify()


map_colurs <- full_cols[as.numeric(levels(fii_df$position)),]



map <-
  ggplot() + coord_fixed() +
  geom_sf(data = world_polygon,
          fill = "grey40",
          linewidth = NA) +
  geom_tile(data = fii_df,
                          aes(x = x, y = y, fill = position),
                          show.legend = FALSE)  +
  scale_fill_manual(name = "position", values = map_colurs$colours) +
  theme(
    axis.line = element_line(colour = "black", linetype = "solid", linewidth = 1),
    axis.title.y = element_blank(),
    #axis.ticks.y = element_blank(),
    #axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    #axis.ticks.x = element_blank(),
    #axis.text.x = element_blank(),
    panel.background = element_rect(fill = 'white', color = 'white'),
    panel.grid.major = element_line(color = 'white'),
    panel.grid.minor = element_line(color = 'white')
  )


plot(map)


ggsave(
  filename = "outputs/FII_maps_and_graphs/FII_2020_human_footprint_agreement_map.png",
  map,
  device = "png",
  height = 7,
  width = 15,
  dpi = 2000
)







tile_frame <- data.frame(x = rep(seq(min(fii_df$human_footprint),max(fii_df$human_footprint), length.out = 100),100),
                         y = rep(seq(min(fii_df$FII),max(fii_df$FII), length.out = 100),each = 100))

tile_frame$log_human <- log(tile_frame$x + 1)


plan(multicore(workers = detectCores() - 1))

tile_frame$prediction <- future_apply(tile_frame,MARGIN = 1, FUN = function(x) predict(model, newdata = data.frame(log_human = x[3]))[1])
tile_frame$residuals <- abs(tile_frame$prediction - car::logit(tile_frame$y, adjust = 0.01))

tile_frame$position <- apply(tile_frame, MARGIN = 1, FUN = function(x) find_position(x = x[5], y = full_cols$values))
tile_frame$position <- factor(tile_frame$position)



legend <-
  ggplot() +  geom_tile(data = tile_frame,
                                       aes(x = x, y = y, fill = position),
                                       show.legend = FALSE)  +
  scale_fill_manual(name = "position", values = full_cols$colours) +
  theme(
    axis.line = element_line(colour = "black", linetype = "solid", linewidth = 1),
    axis.title.y = element_blank(),
    #axis.ticks.y = element_blank(),
    #axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    #axis.ticks.x = element_blank(),
    #axis.text.x = element_blank(),
    panel.background = element_rect(fill = 'white', color = 'white'),
    panel.grid.major = element_line(color = 'white'),
    panel.grid.minor = element_line(color = 'white')
  )


plot(legend)


ggsave(
  filename = "outputs/FII_maps_and_graphs/FII_2020_human_footprint_agreement_legend.png",
  legend,
  device = "png",
  height = 3,
  width = 3,
  dpi = 2000
)




  









##############
## Ecoregions
##############


ecoregions <- st_read("data/Ecoregions2017/Ecoregions2017.shp")

extract_FII_values <- function(id,vector){  
  
  if (ext(vector)[3] < ext(FII_2020)[3]) {
    return(NA)
  }
  
  
  
  human_mask <- terra::crop(human_footprint, vector) %>% terra::mask(vector)

  
  
  
  # sf_use_s2(TRUE)
  # if (!st_is_valid(vector)) {
  #   sf_use_s2(FALSE)
  # }
  
  
  
  if (terra::expanse(human_mask, unit = "km")/ (terra::expanse(vector, unit = "km")) < 0.3) {
    return(NA)
  }
  
  
  median_values <- data.frame(
    ID = id,
    values_human = median(terra::values(human_mask), na.rm = TRUE),
    extract_area = terra::expanse(human_mask, unit = "km")
  )
  
  
  
  
  
  function_list <- median_values
  
  
  return(function_list)
  
}

ecoregion_median_human <- c()


sf_use_s2(FALSE)


for(i in 1:nrow(ecoregions)){
  
  id <- ecoregions$ECO_NAME[i]
  
  vector <- ecoregions %>% dplyr::filter(ECO_NAME == id) %>%
    st_transform(newcrs) %>% terra::vect()
  
  
  human_vals <- extract_FII_values(id = id, vector = vector)
  
  if(!is.list(human_vals)){
    next()
  }
  
  ecoregion_median_human <- rbind(ecoregion_median_human, human_vals)
  
  
}


FII_medians <- readRDS("outputs/10kmmedian_ecoregion_FII.rds")


ecoregion_median_human <- ecoregion_median_human %>% dplyr::filter(values_human < 45.5)

FII_medians <- FII_medians %>% dplyr::filter(ID %in% ecoregion_median_human$ID) %>% 
  dplyr::left_join(ecoregions[,c("ECO_NAME","BIOME_NAME")], by = c("ID" = "ECO_NAME"))

FII_medians <- FII_medians[,c(1:5)]

FII_medians$human_footprint <- ecoregion_median_human$values_human


FII_medians$log_human <- log(FII_medians$human_footprint + 1)
FII_medians$logit_fii <- car::logit(FII_medians$median_FII_2020, a = 0.01)
FII_medians$prediction <- apply(FII_medians, MARGIN = 1, FUN = function(x) predict(model, newdata = data.frame(log_human = as.numeric(x[7])))[1])
FII_medians$residuals <- abs(FII_medians$prediction - car::logit(FII_medians$median_FII_2020, adjust = 0.01))

FII_medians$position <- apply(FII_medians, MARGIN = 1, FUN = function(x) find_position(x = x[10], y = full_cols$values))
FII_medians$position <- factor(FII_medians$position)



model_lines <- data.frame(log_human = seq(0,5, length.out = 1000))
model_lines$estimate <- apply(model_lines,MARGIN = 1 ,FUN = function(x) predict(model,data.frame(log_human = x[1]))[1])
model_lines$human_footprint <- exp(model_lines$log_human) - 1
model_lines$fii <- inv_logit(model_lines$estimate, a = 0.01)
model_lines$upper <- inv_logit(model_lines$estimate + resid_se, a = 0.01)
model_lines$lower <- inv_logit(model_lines$estimate - resid_se, a = 0.01)




human_fii_colours <- full_cols[as.numeric(levels(FII_medians$position)),]


human_FII_plot <- ggplot() +
  geom_vline(xintercept = 0, linewidth = 1) + 
  geom_hline(yintercept = 0, linewidth = 1) + 
  geom_point(data = FII_medians, aes(x = human_footprint, y = median_FII_2020, fill = position),shape = 21,size = 4, show.legend = FALSE) +
  scale_fill_manual(values = human_fii_colours$colours) + 
   geom_line(data = model_lines, aes( x = human_footprint, y = fii), linetype = "dashed", linewidth = 2, colour = "blue") + 
   geom_line(data = model_lines, aes( x = human_footprint, y = upper), linetype = "dotted", linewidth = 1, colour = "red") + 
   geom_line(data = model_lines, aes( x = human_footprint, y = lower), linetype = "dotted", linewidth = 1, colour = "red") + 
  scale_x_continuous(breaks =  c(0,10,20,30,40),limits = c(0,42)) + 
  theme(
    axis.line = element_line(colour = "black", linetype = "solid", linewidth = 1),
    axis.title.y = element_blank(),
    #axis.ticks.y = element_blank(),
    #axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    #axis.ticks.x = element_blank(),
    #axis.text.x = element_blank(),
    panel.background = element_rect(fill = 'white', color = 'white'),
    panel.grid.major = element_line(color = 'white'),
    panel.grid.minor = element_line(color = 'white')
  )

plot(human_FII_plot)


ggsave(
  filename = "outputs/FII_maps_and_graphs/FII_2020_human_footprint_agreement_plot.png",
  human_FII_plot,
  device = "png",
  height = 7,
  width = 7,
  dpi = 2000
)




