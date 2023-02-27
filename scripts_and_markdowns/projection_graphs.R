##### SCRIPT

## Mean country FII and creating stacked bar chart


### DEVELOPMENT: Patrick Alexander Walkden



rm(list = ls())

require(tidyverse)
require(ggridges)

#------------ 

## load in mean_country FII values



my_colour <-
  colorRampPalette(c("red","orange", "cadetblue3" ,"midnightblue"))

colours <- my_colour(500)

my_colours_2 <- colorRampPalette(c("#4A2545","darkred" ,"red"))


colours <- c(my_colours_2(500), colours)



mean_country_FII <- readRDS("outputs/10kmmean_country_FII.rds") %>% dplyr::filter(!is.nan(median_FII_2020)) 



mean_country_FII <- mean_country_FII[order(mean_country_FII$country_area,decreasing = TRUE)[1:100],] %>%
  dplyr::arrange(median_FII_2020)

mean_country_FII$country <- factor(mean_country_FII$country, levels = mean_country_FII$country)




stacked_bar <- ggplot(data = mean_country_FII, aes( x = median_FII_2020, y = country, fill = median_FII_2020)) +
  geom_bar(stat = "identity") +
  geom_vline(xintercept = 0.6819612) +
  scale_fill_gradientn(breaks = c(0,0.2,0.4,0.6,0.8,1), limits = c(0,1),
                       labels = c(0,0.2,0.4,0.6,0.8,1), colours = colours) +
  theme_classic()

plot(stacked_bar)




ggsave(filename = "country_stacked.png",stacked_bar,device = "png", height = 40, width = 20,units = "cm",dpi=300)






# -----------------


change_colour_low <- colorRampPalette(c("tomato","red","red4"))
change_colour_high <- colorRampPalette(c("skyblue1","blue","midnightblue"))


mean_country_FII$FII_change <-  mean_country_FII$median_FII_2020 - mean_country_FII$median_FII_2000

mean_country_FII <- mean_country_FII %>% dplyr::arrange(FII_change)

mean_country_FII$country <- factor(mean_country_FII$country, levels = mean_country_FII$country)


round_values <- seq(0.001,max(max(mean_country_FII$FII_change),abs(min(mean_country_FII$FII_change))) + 0.001,0.001)

col_low <- change_colour_low(length(round_values))
col_high <- change_colour_high(length(round_values))

col_frame <- data.frame(round_change = as.character(unique(c(0,-round_values,round_values))),
                        colour = c("grey45",col_low,col_high))


mean_country_FII$round_change <- as.character(round(mean_country_FII$FII_change,digits = 3))

mean_country_FII <- mean_country_FII %>% dplyr::left_join(col_frame)


change_bar <- ggplot(data = mean_country_FII, aes( x = FII_change, y = country))+
  geom_bar(stat = "identity", show.legend = FALSE, aes(fill = country)) +
  scale_fill_manual(values = mean_country_FII$colour) +
  theme_classic()

plot(change_bar)


ggsave(filename = "country_FII_change.png",change_bar,device = "png", height = 40, width = 20,units = "cm",dpi=300)


# ------------
## Full change in value plots 








full_country_FII <-
  readRDS("outputs/10kmfull_country_FII.rds") %>% 
  dplyr::filter(country %in% levels(mean_country_FII$country)) 





full_country_FII$FII_change <- full_country_FII$values_2020 - full_country_FII$values_2000

medians <- full_country_FII %>% dplyr::group_by(country) %>% dplyr::summarise(median = median(FII_change))




full_country_FII$country <- factor(full_country_FII$country,levels = as.character(medians$country[order(medians$median)]))


full_change_plot <- ggplot(data = full_country_FII, aes(x = FII_change, y = country, fill = country)) +
  geom_vline(xintercept = 0) +
  geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
  #geom_violin(show.legend = FALSE) +
  scale_fill_manual(values = mean_country_FII$colour) + 
  theme_classic()


plot(full_change_plot)

ggsave(filename = "full_country_FII_change.png",full_change_plot,device = "png", height = 40, width = 30,units = "cm",dpi=300)



