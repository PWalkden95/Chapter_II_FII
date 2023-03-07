#### This script is going to be for modelling the alpha diversity as a function of land use change and
#### other pressures associated with the land, including human population density, road denisty, proportion
#### of natural habitat and time since land scape was converted to human use.

rm(list = ls())

require(tidyverse)
require(car)
require(lme4)
require(lmerTest)
require(ggResidpanel)


alpha_data <- readRDS("outputs/alpha_diversity_dataframe.rds") %>% dplyr::filter(SS != "GN1_2010__Hvenegaard 2")

table(alpha_data$LUI)

alpha_data <- alpha_data %>% dplyr::mutate(LUI = ifelse(grepl(LUI, pattern = "Urban"), "Urban", LUI),
                                            LUI = ifelse(grepl(LUI, pattern = "Cropland_Intense use")|
                                                           grepl(LUI, pattern = "Cropland_Light use"), "Cropland_High use", LUI),
                                           # LUI = ifelse(grepl(LUI, pattern = "Pasture_Intense use")|
                                           #                grepl(LUI, pattern = "Pasture_Light use"), "Pasture_High use", LUI)
                                           ) %>%
  dplyr::filter(!grepl(LUI , pattern = "Cannot decide"))



alpha_data$LUI <- factor(alpha_data$LUI)

#### put predominant habitat as a factor

table(alpha_data$LUI)



land_use_levels <- c("Primary forest_Minimal use","Primary forest_Light use","Primary forest_Intense use",
                     "Primary non-forest_Minimal use","Primary non-forest_Light use", "Primary non-forest_Intense use",
                     "Secondary vegetation_Minimal use","Secondary vegetation_Light use","Secondary vegetation_Intense use",
                     "Plantation forest_Minimal use","Plantation forest_Light use","Plantation forest_Intense use",
                     "Pasture_Minimal use","Pasture_Light use","Pasture_Intense use",
                     "Cropland_Minimal use", "Cropland_High use",
                     "Urban")


alpha_data$LUI <- factor(alpha_data$LUI, levels = land_use_levels)









### square root transformation looks better and improves ggresidpanel post hoc 



hist(alpha_data$functional_richness)

hist(log(alpha_data$functional_richness))


alpha_data$logRich <- log(alpha_data$functional_richness)


#### do any of the pressure variables need transforming?


boxplot(alpha_data$logRich ~ alpha_data$LUI)

#######




source("https://highstat.com/Books/Book2/HighstatLibV10.R")
corvif(alpha_data[,c( "LUI", "log_hpd_1km", "log_roads","nat_hab_sw" ,"log_T30")])


### now to scale the variables before modelling so that the output is more easilt interpretable



new_scale <- function(x, sds){
  
  mean <- mean(x)
  sd <- sd(x)
  
  
  scale_variable <- (x - mean)/((1/sds)*sd)
  
  
  attr(scale_variable, "scale.scale") <- ((1/sds)*sd)
  attr(scale_variable, "scale.center") <- mean
  
  
  return(scale_variable)
  
}



alpha_data$log_hpd_1km <- new_scale(alpha_data$log_hpd_1km, sds = 2)
alpha_data$log_roads <- new_scale(alpha_data$log_roads, sds = 2)
alpha_data$nat_hab_sw <- new_scale(alpha_data$nat_hab_sw, sds = 2)
alpha_data$log_T30 <- new_scale(alpha_data$log_T30, sds = 2)
alpha_data$control_hpd <- new_scale(alpha_data$control_hpd, sds = 2)
alpha_data$control_roads <- new_scale(alpha_data$control_roads, sds = 2)



###########################################
#### LET'S GET MODELLING ##################
###########################################

## start with the full model with all varaibles included and all interactions 

model_1 <- lmer(logRich ~ LUI  + LUI:log_hpd_1km + LUI:nat_hab_sw + LUI:log_roads + LUI:log_T30 +
                  log_hpd_1km + log_roads + nat_hab_sw + log_T30 + control_hpd + control_roads + 
                  (1|SS) ,data = alpha_data)

summary(model_1)
car::Anova(model_1)




model_2 <- lmer(logRich ~ LUI  + LUI:log_hpd_1km + LUI:nat_hab_sw  + LUI:log_roads +
                  log_hpd_1km + log_roads + nat_hab_sw + log_T30 + control_hpd + control_roads + 
                  (1|SS) ,data = alpha_data)

summary(model_2)
car::Anova(model_2)


model_3 <- lmer(logRich ~ LUI  + LUI:log_hpd_1km + LUI:nat_hab_sw  + LUI:log_roads +
                  log_hpd_1km + log_roads + nat_hab_sw  + control_hpd + control_roads + 
                  (1|SS) ,data = alpha_data)

summary(model_3)
car::Anova(model_3)


model_4 <- lmer(logRich ~ LUI  + LUI:log_hpd_1km + LUI:nat_hab_sw  + 
                  log_hpd_1km + log_roads + nat_hab_sw  + control_hpd + control_roads + 
                  (1|SS) ,data = alpha_data)

summary(model_4)
car::Anova(model_4)


model_5 <- lmer(logRich ~ LUI  + LUI:log_hpd_1km + LUI:nat_hab_sw  + 
                  log_hpd_1km  + nat_hab_sw  + control_hpd + 
                  (1|SS) ,data = alpha_data)

summary(model_5)
car::Anova(model_5)


summary(model_5)

# 
# 
# 
# 




write_rds(file = "outputs/alpha_diversity_rao_model.rds", x = model_5)
write_rds(file = "outputs/alpha_modelling_dataframe.rds", x = alpha_data)


#### chekcing model 


ggResidpanel::resid_panel(model_3)





#######################
### QUICK PREDICTION






inv_logit <- function(f, a){
  a <- (1-2*a)
  (a*(1+exp(f))+(exp(f)-1))/(2*a*(1+exp(f)))
}


unscale <- function(values, column){
  
  return((values * attr(column,"scale.scale")) + attr(column,"scale.center"))
  
}



scale_column <- function(values,column){
  
  return(values - attr(column,"scale.center"))/attr(column,"scale.scale") 
  
}




data <-
  data.frame(
    LUI = land_use_levels,
    log_roads = scale_column(values = 0,alpha_data$log_roads),
    log_hpd_1km = scale_column(0, alpha_data$log_hpd_1km),
    log_T30 = scale_column(0, alpha_data$log_T30),
    nat_hab_sw = scale_column(1, alpha_data$nat_hab_sw),
    control_hpd = scale_column(0, alpha_data$control_hpd),
    control_roads = scale_column(0, alpha_data$control_roads))





pred_fun <- function(x) {
  as.numeric(predict(x, newdata = data, re.form = NA))
}








data$estimate <- NA
data$upper <- NA
data$lower <- NA

booted_mod <-
  bootMer(
    model_3,
    FUN = function(x)
      pred_fun(x),
    nsim =  100,seed = 1234
  )



data$estimate <- booted_mod$t0
bootstraps <- booted_mod$t      



ninety_five_boot <- c()

for (col in 1:ncol(bootstraps)) {
  trim_col <- bootstraps[, col]
  
  trim_col <-
    trim_col[trim_col > as.numeric(quantile(bootstraps[, col], 0)) &
               trim_col < as.numeric(quantile(bootstraps[, col], 1))]
  
  
  ninety_five_boot <- cbind(ninety_five_boot, trim_col)
  
}


ninety_five_boot <- ninety_five_boot/median(ninety_five_boot[,1])

ninety_five_boot <- data.frame(ninety_five_boot)


colnames(ninety_five_boot) <- land_use_levels


boot_plot <-
  ninety_five_boot %>% pivot_longer(cols = colnames(ninety_five_boot), names_to = "land_use")


boot_plot$land_use <- factor(boot_plot$land_use, levels = land_use_levels)   


boot_plot <- boot_plot %>% dplyr::filter(land_use != "Urban_Minimal use")

realm_size_plot <- ggplot(data = boot_plot, aes(x = land_use, y = value, group = land_use)) +
  geom_violin(aes(colour = land_use, fill = land_use), alpha = 0.5, show.legend = FALSE)+
  geom_boxplot(width = 0.3, aes( fill = land_use), show.legend = FALSE, outlier.shape = NA) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  #scale_colour_manual(values = land_use_colours[land_uses, "colours"]) +
  #scale_fill_manual(values = land_use_colours[land_uses, "colours"]) +
  #scale_y_continuous(limits = c(0.2, 2.2),
  #                  breaks = c(0.5,1,1.5,2)) +
  theme(panel.background = element_rect(fill = 'white', color = 'white'),
        panel.grid.major = element_line(color = 'white'),
        panel.grid.minor = element_line(color = 'white'))
# axis.title.y = element_blank(),
#axis.text.y = element_blank(),
#axis.ticks.y = element_blank(),
#axis.title.x = element_blank(),
#axis.line = element_line(colour = "black", linetype = "solid"),
#axis.ticks.x = element_blank(),
#axis.text.x = element_blank())


plot(realm_size_plot)  

#############################################################################
#############################################################################


land_use_colours <-
  data.frame(
    land_use = land_use_levels,
    colours = c(
      "green4",
              "chartreuse4",
              "olivedrab2",
              "springgreen2",
              "#EBF787",
              "#E3D438",
              "#718879"
    )
  )
rownames(land_use_colours) <- land_use_levels




estimates <- summary(model_4)[["coefficients"]]

Predominant_habitat <- as.data.frame(estimates[c(1:7),])
Predominant_habitat$land_use <- factor(land_use_levels, levels = land_use_levels)


Predominant_habitat_model_data <-
  Predominant_habitat %>% dplyr::mutate(relative_estimate = 0 + Estimate,
                                        standard_error = `Std. Error`)

Predominant_habitat_model_data$relative_estimate[1] <- 0


effect_size_model <-
  ggplot(
    data = Predominant_habitat_model_data,
    aes(x = land_use, y = relative_estimate, group = land_use)
  ) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_errorbar(
    aes(
      ymax  = relative_estimate + standard_error ,
      ymin = relative_estimate - standard_error,
      colour = land_use
    ),
    width = 0.1,
    size = 0.5,
    show.legend = FALSE
  ) +
  geom_point(size = 2,
             aes(fill = land_use),
             show.legend = FALSE, position = position_dodge(0), shape = 21) +
  scale_fill_manual(values = land_use_colours$colours) +
  scale_colour_manual(values = land_use_colours$colours) +
  theme(
    panel.background = element_rect(fill = 'white', color = 'white'),
    panel.grid.major = element_line(color = 'white'),
    panel.grid.minor = element_line(color = "white"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.line = element_line(colour = "black", linetype = "solid"),
    #axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    #  axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  ) + 
  scale_x_discrete(expand = c(0,0.5,3,0))


plot(effect_size_model)


ggsave("figures/alpha_diversity_coefficient_plot.png", plot = effect_size_model, device = "png", width = 200, units = "mm", height = 100, dpi = 300)




###### other fixed effects plots 




fixed_effects <- as.data.frame(estimates[c(8:10),])
fixed_effects <- fixed_effects %>% dplyr::mutate(fixed_effects = factor(rownames(fixed_effects)), 
                                                 land_use = "Primary minimal",
                                                 standard_error = `Std. Error`
)


interactions <- unlist(str_split(rownames(estimates)[12:29], pattern = ":"))

land_use_interaction <-
  gsub(interactions[seq(1, length(interactions) - 1, by = 2)], pattern = "Predominant_habitat", replacement = "")

interaction_effect <- interactions[seq(2, length(interactions), by = 2)]


interaction_effects <- as.data.frame(estimates[12:29,]) %>% dplyr::mutate(fixed_effects = interaction_effect,
                                                                          land_use = land_use_interaction,
                                                                          standard_error = `Std. Error`)


fixed_effects <- rbind(fixed_effects,interaction_effects)

fixed_effects$land_use <- factor(fixed_effects$land_use, levels = land_use_levels[seq(length(land_use_levels),1)])





fixed_effect_plot <-
  ggplot(
    data = fixed_effects,
    aes(y = fixed_effects, x = Estimate, group = land_use)
  ) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_errorbar(
    aes(
      xmax  = Estimate + standard_error ,
      xmin = Estimate - standard_error
    ),
    width = 0.1,
    size = 0.5,
    position = position_dodge(0.5),
    show.legend = FALSE
  ) +
  geom_point(size = 2,
             aes(fill = fixed_effects),
             show.legend = FALSE, position = position_dodge(0.5), shape = 21) +
  # scale_fill_manual(values = land_use_colours$colours) +
  #scale_colour_manual(values = land_use_colours$colours) +
  theme(
    panel.background = element_rect(fill = 'white', color = 'white'),
    panel.grid.major = element_line(color = 'white'),
    panel.grid.minor = element_line(color = "white"),
    axis.title.y = element_blank(),
    #axis.text.y = element_blank(),
    axis.line = element_line(colour = "black", linetype = "solid"),
    #axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    #axis.text.x = element_blank()
  ) 

plot(fixed_effect_plot)



ggsave("figures/alpha_diversity_fixed_plot.png", plot = fixed_effect_plot, device = "png", width = 150, units = "mm", height = 200, dpi = 300)
