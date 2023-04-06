### SCRIPT: Cretaing the legends for the FII and differences maps

## Development: Patrick Alexander Walkden

## Description: need to make some custom legends to account for the colours being dependent on the quantile rather 
## than a linear progression of chnage in colour

rm(list = ls())

require(tidyverse)

find_position <- function(x,y){
  
  value <- which(x > y)
  
  if(is_empty(value)){
    value <- 1
  } else {
    value <- value[length(value)]
  }
  
  return(value)   
}


FII_colours <- readRDS("outputs/FII_legend_colours.rds")

FII_legend <- data.frame(x = rep(seq(0,1,length.out = 10000),each = 100), y = rep(seq(0,0.2,length.out = 100),10000))

FII_legend$position <- apply(FII_legend, MARGIN = 1, FUN = function(x) find_position(x = x[1], y = FII_colours$values))
FII_legend$position <- factor(FII_legend$position)




FII_legend_plot <- ggplot() + coord_fixed() +
  geom_tile(data = FII_legend, aes(x = x, y = y, fill = position), show.legend = FALSE) +
  scale_fill_manual(name = "position", values = FII_colours$colours)+
  scale_x_continuous(breaks = c(0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1)) + 
  theme(
    axis.line = element_line(colour = "black", linetype = "solid"),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_line(linewidth = 5),
    axis.text.x = element_blank(),
    panel.background = element_rect(fill = 'white', color = 'white'),
    panel.grid.major = element_line(color = 'white'),
    panel.grid.minor = element_line(color = 'white')
  )


ggsave(
  filename = "outputs/FII_maps_and_graphs/FII_legend.png",
  FII_legend_plot,
  device = "png",
  height = 10,
  width = 30,
  dpi = 300, limitsize = FALSE
)


#### FII difference legend 

difference_colours <- readRDS("outputs/difference_legend_colours.rds")


difference_legend <-
  data.frame(x = rep(seq(min(difference_colours$values, na.rm = TRUE), max(difference_colours$values), length.out = 20000), each = 100),
             y = rep(seq(0, 5, length.out = 100), 20000))


difference_legend$position <- apply(difference_legend, MARGIN = 1, FUN = function(x) find_position(x = x[1], y = difference_colours$values))
difference_legend$position <- factor(difference_legend$position)

range = abs(min(difference_colours$values, na.rm = TRUE))

range*(2/3)


difference_legend_plot <- ggplot() + coord_fixed() +
  geom_tile(data = difference_legend, aes(x = x, y = y, fill = position), show.legend = FALSE) +
  scale_fill_manual(name = "position", values = difference_colours$colours) +
  scale_x_continuous(breaks = c(-51.75,-34.5,-17.25,25.27,50.55,75.82)) + 
  theme(
    axis.line = element_line(colour = "black", linetype = "solid"),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    #axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    panel.background = element_rect(fill = 'white', color = 'white'),
    panel.grid.major = element_line(color = 'white'),
    panel.grid.minor = element_line(color = 'white')
  )

ggsave(
  filename = "outputs/FII_maps_and_graphs/difference_legend.png",
  difference_legend_plot,
  device = "png",
  height = 10,
  width = 30,
  dpi = 600, limitsize = FALSE
)
