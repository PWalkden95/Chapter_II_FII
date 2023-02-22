### SCRIPT Edwards ... RAS Jetz Crosswalk

rm(list = ls())


RAS <- readRDS("data/RAS.rds")[,-1]
RAS <- RAS %>%
  dplyr::select(-c("unique", "Corrected"))
RAS$Sample_end_latest <- as.Date(RAS$Sample_end_latest)
RAS$Sample_start_earliest <- as.Date(RAS$Sample_start_earliest)
RAS$SSS <- as.character(RAS$SSS)
RAS$SSB <- as.character(RAS$SSB)
RAS$Diversity_metric_type <- "Abundance"
RAS$Sampling_method <- "line/belt transects"
RAS$Birdlife_Name <- RAS$BirdLife_Nominate
RAS$Study_name <- "RAS_bird_data"
RAS$Sampling_target <- "Entire community"
RAS$Habitat_as_described <- "Ras_habitat"
RAS <- RAS %>% dplyr::select(-c(BirdLife_Nominate))
RAS <- RAS %>% dplyr::left_join(data.frame(SSBS = unique(RAS$SSBS),
                                           Site_number = 1:length(unique(RAS$SSBS)),
                                           Site_name = unique(RAS$SSBS)))
RAS$Block <- RAS$SSB


crosswalk <- read.csv("../../Datasets/GBD/BL_Jetz crosswalk v3.csv")


Birdlife_name <- unique(RAS$Birdlife_Name)


bird_cross <- crosswalk %>% dplyr::filter(BirdLife.name %in% Birdlife_name)


straight <- bird_cross %>% dplyr::filter(Match.type == "1BL to 1Jetz")

lump <- bird_cross %>% dplyr::filter(Match.type == "Many BL to 1Jetz")

easy <- rbind(straight,lump)




split_match <- data.frame(BirdLife.name = c("Butorides striata","Celeus undatus","Chaetura chapmani",
                                            "Geothlypis aequinoctialis","Hylexetastes perrotii",
                                            "Myrmotherula axillaris","Polioptila guianensis","Trogon violaceus",
                                            "Turdus albicollis"),
                          Jetz.name = c("Butorides striata","Celeus undatus","Chaetura chapmani",
                                        "Geothlypis aequinoctialis","Hylexetastes uniformis",
                                        "Myrmotherula axillaris","Polioptila guianensis","Trogon violaceus",
                                        "Turdus albicollis"),
                          Match.type = "easy")



easy <- rbind(easy,split_match) %>% dplyr::select(-Match.type) %>% dplyr::rename(Jetz_Name = Jetz.name,
                                                                                 Birdlife_Name = BirdLife.name)



RAS <- RAS %>% dplyr::left_join(easy)

RAS$Jetz_Name[which(RAS$Birdlife_Name == "Hylocharis cyanus")] <- "Hylocharis cyanus"


write_rds(file = "data/RAS.rds", x = RAS)


wm <-
  map_data("world") %>% filter(region != "Antartica") %>% fortify()

## site coords

site_points <- RAS %>% distinct(SSBS, Longitude, Latitude)

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


