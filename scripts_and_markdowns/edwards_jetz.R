rm(list = ls())

require(tidyverse)

edwards <- read.csv("data/edwards_predicts.csv")[,-c(1:2)] %>% dplyr::rename(Birdlife_Name = Birdlife_Nominate)
edwards$Predominant_habitat[which(edwards$Predominant_habitat == "Primary Forest")] <- "Primary forest"
edwards$Sample_end_latest <- as.Date(chartr("NA", "30", edwards$Sample_end_latest))
edwards$Sample_start_earliest <- as.Date(chartr("NA", "01", edwards$Sample_start_earliest))
edwards$SSS <- as.character(edwards$SSS)
edwards$SSB <- as.character(edwards$SSB)
edwards$SSB <- "line/belt transects"
edwards$Diversity_metric_type <- "Abundance"




crosswalk <- read.csv("../../Datasets/GBD/BL_Jetz crosswalk v3.csv")


Birdlife_name <- unique(edwards$Birdlife_Name)


bird_cross <- crosswalk %>% dplyr::filter(BirdLife.name %in% Birdlife_name)


straight <- bird_cross %>% dplyr::filter(Match.type == "1BL to 1Jetz")

lump <- bird_cross %>% dplyr::filter(Match.type == "Many BL to 1Jetz")

easy <- rbind(straight,lump)


split <- bird_cross %>% dplyr::filter(Match.type == "1BL to many Jetz")



split_match <- data.frame(BirdLife.name = c("Ceyx erithaca","Chalcites minutillus",	
                                            "Cyanoderma rufifrons","Arachnothera affinis","Kittacincla malabarica"),
                          Jetz.name = c("Ceyx rufidorsa","Chrysococcyx minutillus","Stachyris rufifrons",
                                        "Arachnothera affinis","Copsychus malabaricus"),
                          Match.type = "easy")



easy <- rbind(easy,split_match) %>% dplyr::select(-Match.type) %>% dplyr::rename(Jetz_Name = Jetz.name,
                                                                                 Birdlife_Name = BirdLife.name)



edwards <- edwards %>% dplyr::left_join(easy)

edwards$Jetz_Name[which(edwards$Birdlife_Name == "Psittacula longicauda")] <- "Psittacula longicauda"


write_rds(file = "data/edwards.rds", x = edwards)


wm <-
  map_data("world") %>% filter(region != "Antartica") %>% fortify()

## site coords

site_points <- edwards %>% distinct(SSBS, Longitude, Latitude)

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
    colour = "blue",
    size = 1
  ) +
  scale_x_continuous(limits = c(-180, 180), breaks = seq(-180, 180, 30)) +
  scale_y_continuous(limits = c(-90, 90), breaks = seq(-90, 90, 30)) +
  theme_classic()

plot(site_plot)



unique(PRED_Assem$SS)
## Save


