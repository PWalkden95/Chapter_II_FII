##### SCRIPT

## Mean country FII and creating stacked bar chart


### DEVELOPMENT: Patrick Alexander Walkden



rm(list = ls())

require(tidyverse)
require(ggridges)
require(magrittr)

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

percent_change_colours <-
  readRDS("outputs/difference_legend_colours.rds")



median_FII_plot <- function(median_values,colours, year) {


median_FII <-
  median_values[order(median_values$extract_area, decreasing = TRUE)[1:100], ] %>% set_rownames(1:100)


year_col <- which(grepl(colnames(median_values), pattern = as.character(year)))


median_FII <- median_FII[order(median_FII[,year_col]),] %>% drop_na()

  

median_FII$ID <-
  factor(median_FII$ID, levels = median_FII$ID)

median_FII$position <-
  factor(apply(
    median_FII,
    MARGIN = 1,
    FUN = function(x)
      find_position(x = x[year_col], y = FII_colours$values)
  ))

stack_colours <-
  FII_colours[as.numeric(unique(as.character(median_FII$position))), ]





stacked_bar <-
  ggplot(data = median_FII, aes(x = as.numeric(median_FII[,year_col]), y = ID, fill = position)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_vline(xintercept = 0.652) +
  scale_fill_manual(values = stack_colours$colours) +
  xlim(0,1) +
  theme_classic()



return(stacked_bar)


}




## Full change in value plots going to represent change in FII as a percentage


FII_change_plot <- function(full_values, colours,median_year){


year_col <- grep(colnames(full_values), pattern = as.character(median_year), value = TRUE)



  
median <- full_values %>% dplyr::group_by(ID) %>% dplyr::reframe(extract_area = extract_area, median = median(!!as.name(year_col))) %>%
  dplyr::distinct() %>% dplyr::arrange(desc(extract_area)) %>% dplyr::slice(1:100) %>% dplyr::arrange(median)


  

full_values$FII_change <-
  full_values$values_2020 - full_values$values_2000




full_values <- full_values %>% dplyr::filter(ID %in% median$ID) %>%
  dplyr::mutate(ID = factor(ID, levels = median$ID))


median_change <- full_values %>% dplyr::group_by(ID) %>% dplyr::summarise(median = median(FII_change))


median_change$position <- apply(median_change,MARGIN = 1,FUN = function(x) find_position(as.numeric(x[2]), y = colours$values))

full_colours <- percent_change_colours[median_change$position,]



full_change_plot <-
  ggplot(data = full_values, aes(x = FII_change, y = ID, fill = ID)) +
  geom_vline(xintercept = 0) +
  geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
  #geom_violin(show.legend = FALSE) +
  scale_fill_manual(values = full_colours$colours) +
  xlim(-0.5,0.5) +
  theme_classic()


return(full_change_plot)

}


#################
### Country plots
#################


median_country_FII <- readRDS("outputs/10kmmedian_country_FII.rds")


country_med_plot <- median_FII_plot(median_values = median_country_FII,colours = FII_colours,year = 2020)


ggsave(
  filename = "outputs/FII_maps_and_graphs/10kmcountry_stacked.png",
  country_med_plot,
  device = "png",
  height = 40,
  width = 30,
  units = "cm",
  dpi = 600
)


full_country_FII <- readRDS("outputs/10kmfull_country_FII.rds")


country_change_plot <- FII_change_plot(full_values = full_country_FII,colours = percent_change_colours,median_year = 2020)



ggsave(
  filename = "outputs/FII_maps_and_graphs/10kmfull_country_FII_change.png",
  country_change_plot,
  device = "png",
  height = 40,
  width = 30,
  units = "cm",
  dpi = 600
)


########################
######### bioregion plot
########################


median_bioregion_FII <- readRDS("outputs/10kmmedian_bioregion_FII.rds")


bioregion_med_plot <- median_FII_plot(median_values = median_bioregion_FII,colours = FII_colours,year = 2020)



ggsave(
  filename = "outputs/FII_maps_and_graphs/10kmbioregion_stacked.png",
  bioregion_med_plot,
  device = "png",
  height = 40,
  width = 30,
  units = "cm",
  dpi = 600
)


full_bioregion_FII <- readRDS("outputs/10kmfull_bioregion_FII.rds")


bioregion_change_plot <- FII_change_plot(full_values = full_bioregion_FII,colours = percent_change_colours,median_year = 2020)



ggsave(
  filename = "outputs/FII_maps_and_graphs/10kmfull_bioregion_FII_change.png",
  bioregion_change_plot,
  device = "png",
  height = 40,
  width = 30,
  units = "cm",
  dpi = 600
)



########################
######### ecoregion plot
########################


median_ecoregion_FII <- readRDS("outputs/10kmmedian_ecoregion_FII.rds")


ecoregion_med_plot <- median_FII_plot(median_values = median_ecoregion_FII,colours = FII_colours,year = 2020)


ggsave(
  filename = "outputs/FII_maps_and_graphs/10kmecoregion_stacked.png",
  ecoregion_med_plot,
  device = "png",
  height = 40,
  width = 30,
  units = "cm",
  dpi = 600
)


full_ecoregion_FII <- readRDS("outputs/10kmfull_ecoregion_FII.rds")


ecoregion_change_plot <- FII_change_plot(full_values = full_ecoregion_FII,colours = percent_change_colours,median_year = 2020)



ggsave(
  filename = "outputs/FII_maps_and_graphs/10kmfull_ecoregion_FII_change.png",
  ecoregion_change_plot,
  device = "png",
  height = 40,
  width = 30,
  units = "cm",
  dpi = 600
)


########################
######### subregion plot
########################


median_subregion_FII <- readRDS("outputs/10kmmedian_subregion_FII.rds")


subregion_med_plot <- median_FII_plot(median_values = median_subregion_FII,colours = FII_colours,year = 2020)


ggsave(
  filename = "outputs/FII_maps_and_graphs/10kmsubregion_stacked.png",
  subregion_med_plot,
  device = "png",
  height = 40,
  width = 30,
  units = "cm",
  dpi = 600
)


full_subregion_FII <- readRDS("outputs/10kmfull_subregion_FII.rds")


subregion_change_plot <- FII_change_plot(full_values = full_subregion_FII,colours = percent_change_colours,median_year = 2020)



ggsave(
  filename = "outputs/FII_maps_and_graphs/10kmfull_subregion_FII_change.png",
  subregion_change_plot,
  device = "png",
  height = 40,
  width = 30,
  units = "cm",
  dpi = 600
)





countries <- levels(mean_country_FII$ID)

ID_change_significance <- c()
median_FII <- c()

for(country in countries){
  country_data <- full_country_FII %>% dplyr::filter(ID == country)

  med_FII <- data.frame(ID = country, median = median(country_data$FII_change))
  
  median_FII <- rbind(median_FII,med_FII)
  
  wilcox_test <- wilcox.test(country_data$FII_change)
  
  
  
  significance <- wilcox_test$p.value

  
  data <- data.frame(ID = country, sig = significance)
  
  ID_change_significance <- rbind(ID_change_significance,data)
  
      }


median(full_country_FII$FII_change)
wilcox.test(full_country_FII$FII_change)


median(median_FII$median)
wilcox.test(median_FII$median)

#####################
## bioregion plots ##
#####################



mean_country_FII <- readRDS("outputs/10kmmean_bioregion_FII.rds")

#%>% dplyr::filter(!is.nan(median_FII_2020))



mean_country_FII <-
  mean_country_FII[order(mean_country_FII$extract_area, decreasing = TRUE)[1:100], ] %>%
  dplyr::arrange(median_FII_2020)

mean_country_FII$ID <-
  factor(mean_country_FII$ID, levels = mean_country_FII$ID)

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
  ggplot(data = mean_country_FII, aes(x = median_FII_2020, y = ID, fill = position)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_vline(xintercept = 0.6341) +
  scale_fill_manual(name = "position", values = stack_colours$colours) +
  theme_classic()

plot(stacked_bar)




ggsave(
  filename = "outputs/FII_maps_and_graphs/10kmbioregion_stacked.png",
  stacked_bar,
  device = "png",
  height = 40,
  width = 30,
  units = "cm",
  dpi = 600
)





## Full change in value plots going to represent change in FII as a percentage




full_country_FII <-
  readRDS("outputs/10kmfull_bioregion_FII.rds") %>%
  dplyr::filter(ID %in% levels(mean_country_FII$ID))



full_country_FII$FII_change <-
  ((full_country_FII$values_2020 / full_country_FII$values_2000) * 100) - 100
full_country_FII$FII_change <-
  ifelse(is.nan(full_country_FII$FII_change),
         0 ,
         full_country_FII$FII_change)

medians <-
  full_country_FII %>% dplyr::group_by(ID) %>% dplyr::summarise(median = median(FII_change))


full_country_FII$ID <-
  factor(full_country_FII$ID, levels = levels(mean_country_FII$ID))


full_FII_median <- full_country_FII %>% 
  dplyr::group_by(ID) %>% 
  dplyr::summarise(median = median(FII_change))

full_FII_median$position <- apply(full_FII_median,MARGIN = 1,FUN = function(x) find_position(as.numeric(x[2]), y = percent_change_colours$values))

full_colours <- percent_change_colours[full_FII_median$position,]



full_change_plot <-
  ggplot(data = full_country_FII, aes(x = FII_change, y = ID, fill = ID)) +
  geom_vline(xintercept = 0) +
  geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
  #geom_violin(show.legend = FALSE) +
  scale_fill_manual(values = full_colours$colours) +
  xlim(-52,76) +
  theme_classic()


plot(full_change_plot)

ggsave(
  filename = "outputs/FII_maps_and_graphs/10kmfull_bioregion_FII_change.png",
  full_change_plot,
  device = "png",
  height = 40,
  width = 30,
  units = "cm",
  dpi = 600
)



#####################
## subregion plots ##
#####################



mean_country_FII <- readRDS("outputs/10kmmean_subregion_FII.rds")

#%>% dplyr::filter(!is.nan(median_FII_2020))



mean_country_FII <-
  mean_country_FII[order(mean_country_FII$extract_area, decreasing = TRUE)[1:100], ] %>%
  dplyr::arrange(median_FII_2020) %>% drop_na()

mean_country_FII$ID <-
  factor(mean_country_FII$ID, levels = mean_country_FII$ID)

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
  ggplot(data = mean_country_FII, aes(x = median_FII_2020, y = ID, fill = position)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_vline(xintercept = 0.6341) +
  scale_fill_manual(name = "position", values = stack_colours$colours) +
  theme_classic()

plot(stacked_bar)




ggsave(
  filename = "outputs/FII_maps_and_graphs/10kmsubregion_stacked.png",
  stacked_bar,
  device = "png",
  height = 40,
  width = 30,
  units = "cm",
  dpi = 600
)





## Full change in value plots going to represent change in FII as a percentage




full_country_FII <-
  readRDS("outputs/10kmfull_subregion_FII.rds") %>%
  dplyr::filter(ID %in% levels(mean_country_FII$ID))



full_country_FII$FII_change <-
  ((full_country_FII$values_2020 / full_country_FII$values_2000) * 100) - 100
full_country_FII$FII_change <-
  ifelse(is.nan(full_country_FII$FII_change),
         0 ,
         full_country_FII$FII_change)

medians <-
  full_country_FII %>% dplyr::group_by(ID) %>% dplyr::summarise(median = median(FII_change))


full_country_FII$ID <-
  factor(full_country_FII$ID, levels = levels(mean_country_FII$ID))


full_FII_median <- full_country_FII %>% 
  dplyr::group_by(ID) %>% 
  dplyr::summarise(median = median(FII_change))

full_FII_median$position <- apply(full_FII_median,MARGIN = 1,FUN = function(x) find_position(as.numeric(x[2]), y = percent_change_colours$values))

full_colours <- percent_change_colours[full_FII_median$position,]



full_change_plot <-
  ggplot(data = full_country_FII, aes(x = FII_change, y = ID, fill = ID)) +
  geom_vline(xintercept = 0) +
  geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
  #geom_violin(show.legend = FALSE) +
  scale_fill_manual(values = full_colours$colours) +
  xlim(-52,76) +
  theme_classic()


plot(full_change_plot)

ggsave(
  filename = "outputs/FII_maps_and_graphs/10kmfull_subregion_FII_change.png",
  full_change_plot,
  device = "png",
  height = 40,
  width = 30,
  units = "cm",
  dpi = 600
)


subreg <- unique(mean_country_FII$ID)

ID_change_significance <- c()
median_FII <- c()

for(country in subreg){
  country_data <- full_country_FII %>% dplyr::filter(ID == country)
  
  med_FII <- data.frame(ID = country, median = median(country_data$FII_change))
  
  median_FII <- rbind(median_FII,med_FII)
  
  wilcox_test <- wilcox.test(country_data$FII_change)
  
  
  
  significance <- wilcox_test$p.value
  
  
  data <- data.frame(ID = country, sig = significance)
  
  ID_change_significance <- rbind(ID_change_significance,data)
  
}


median(full_country_FII$FII_change)
wilcox.test(full_country_FII$FII_change)


median(median_FII$median)
wilcox.test(median_FII$median)
