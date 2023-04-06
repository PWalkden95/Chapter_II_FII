##### SCRIPT

## Mean country FII and creating stacked bar chart


### DEVELOPMENT: Patrick Alexander Walkden



rm(list = ls())

require(tidyverse)
require(ggridges)

#------------


find_position <- function(x, y) {
  value <- which(x > y)
  
  if (is_empty(value)) {
    value <- 1
  } else {
    value <- value[length(value)]
  }
  
  return(value)
}


## load in mean_country FII values


FII_colours <- readRDS("outputs/FII_legend_colours.rds")






mean_country_FII <- readRDS("outputs/10kmmean_country_FII.rds")

#%>% dplyr::filter(!is.nan(median_FII_2020))



mean_country_FII <-
  mean_country_FII[order(mean_country_FII$country_area, decreasing = TRUE)[1:100], ] %>%
  dplyr::arrange(median_FII_2020)

mean_country_FII$country <-
  factor(mean_country_FII$country, levels = mean_country_FII$country)

mean_country_FII$position <-
  factor(apply(
    mean_country_FII,
    MARGIN = 1,
    FUN = function(x)
      find_position(x = x[3], y = FII_colours$values)
  ))

stack_colours <-
  FII_colours[as.numeric(unique(as.character(mean_country_FII$position))), ]

stacked_bar <-
  ggplot(data = mean_country_FII, aes(x = median_FII_2020, y = country, fill = position)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_vline(xintercept = 0.6819612) +
  scale_fill_manual(name = "position", values = stack_colours$colours) +
  theme_classic()

plot(stacked_bar)




ggsave(
  filename = "outputs/FII_maps_and_graphs/10kmcountry_stacked.png",
  stacked_bar,
  device = "png",
  height = 40,
  width = 30,
  units = "cm",
  dpi = 600
)





## Full change in value plots going to represent change in FII as a percentage




full_country_FII <-
  readRDS("outputs/10kmfull_country_FII.rds") %>%
  dplyr::filter(country %in% levels(mean_country_FII$country))


percent_change_colours <-
  readRDS("outputs/difference_legend_colours.rds")



full_country_FII$FII_change <-
  ((full_country_FII$values_2020 / full_country_FII$values_2000) * 100) - 100
full_country_FII$FII_change <-
  ifelse(is.nan(full_country_FII$FII_change),
         0 ,
         full_country_FII$FII_change)

medians <-
  full_country_FII %>% dplyr::group_by(country) %>% dplyr::summarise(median = median(FII_change))


full_country_FII$country <-
  factor(full_country_FII$country, levels = levels(mean_country_FII$country))


full_FII_median <- full_country_FII %>% 
  dplyr::group_by(country) %>% 
  dplyr::summarise(median = median(FII_change))

full_FII_median$position <- apply(full_FII_median,MARGIN = 1,FUN = function(x) find_position(as.numeric(x[2]), y = percent_change_colours$values))

full_colours <- percent_change_colours[full_FII_median$position,]



full_change_plot <-
  ggplot(data = full_country_FII, aes(x = FII_change, y = country, fill = country)) +
  geom_vline(xintercept = 0) +
  geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
  #geom_violin(show.legend = FALSE) +
  scale_fill_manual(values = full_colours$colours) +
  xlim(-52,76) +
  theme_classic()


plot(full_change_plot)

ggsave(
  filename = "outputs/FII_maps_and_graphs/10kmfull_country_FII_change.png",
  full_change_plot,
  device = "png",
  height = 40,
  width = 30,
  units = "cm",
  dpi = 600
)
