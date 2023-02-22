##### now we are going to model the beta-diversity sites as a function of the land_use_comparison between
#### bird communities in primary minimal sites and those in other land_uses

rm(list = ls())

require(tidyverse)
require(lme4)
require(lmerTest)
require(ggResidpanel)
require(car)
require(doParallel)
require(parallel)




beta_data <- readRDS("outputs/beta_diversity_dataframe.rds") %>% dplyr::filter(SS != "GN1_2010__Hvenegaard 2")


table(beta_data$land_use_combination)


primary_forest_data <-
  beta_data %>% dplyr::filter(grepl(land_use_combination, pattern = "Primary forest_Minimal use -"))  %>%
  dplyr::mutate(land_use_combination = ifelse(grepl(land_use_combination, pattern = "Primary forest_Minimal use - Secondary vegetation_Intense use")|
                                                grepl(land_use_combination, pattern = "Primary forest_Minimal use - Secondary vegetation_Light use"),
                                              "Primary forest_Minimal use - Secondary vegetation_High use", paste(land_use_combination)),
                land_use_combination = ifelse(grepl(land_use_combination, pattern = "Primary forest_Minimal use - Urban"),
                                              "Primary forest_Minimal use - Urban", paste(land_use_combination)),
                 land_use_combination = ifelse(grepl(land_use_combination, pattern = "Primary forest_Minimal use - Pasture_Intense use")|
                                                 grepl(land_use_combination, pattern = "Primary forest_Minimal use - Pasture_Light use"),
                                               "Primary forest_Minimal use - Pasture_High use", paste(land_use_combination)),
                land_use_combination = ifelse(grepl(land_use_combination, pattern = "Primary forest_Minimal use - Cropland_Intense use")|
                                                grepl(land_use_combination, pattern = "Primary forest_Minimal use - Cropland_Light use"),
                                              "Primary forest_Minimal use - Cropland_High use", paste(land_use_combination))
                
  ) %>% dplyr::filter(!grepl(x = land_use_combination, pattern = "Cannot decide"))






table(primary_forest_data$land_use_combination)



land_use_combination_levels <- c("Primary forest_Minimal use - Primary forest_Minimal use",
                                 "Primary forest_Minimal use - Primary forest_Light use",
                                 "Primary forest_Minimal use - Primary forest_Intense use",
                                 "Primary forest_Minimal use - Primary non-forest_Minimal use",
                                 "Primary forest_Minimal use - Primary non-forest_Light use",
                                 "Primary forest_Minimal use - Primary non-forest_Intense use",
                                 "Primary forest_Minimal use - Secondary vegetation_Minimal use",
                                 "Primary forest_Minimal use - Secondary vegetation_High use",
                                 "Primary forest_Minimal use - Plantation forest_Minimal use",
                                 "Primary forest_Minimal use - Plantation forest_Light use",
                                 "Primary forest_Minimal use - Plantation forest_Intense use",
                                 "Primary forest_Minimal use - Pasture_Minimal use",
                                 "Primary forest_Minimal use - Pasture_High use",
                                 "Primary forest_Minimal use - Cropland_Minimal use",
                                 "Primary forest_Minimal use - Cropland_High use",
                                 "Primary forest_Minimal use - Urban")


primary_forest_data$land_use_combination <-
  factor(primary_forest_data$land_use_combination,
         levels = land_use_combination_levels)



### so other pressure data that is going to be relevant to include in the models that will effect the
## similarity between sites is the absolute pressure at the second site and alos the difference between the
## pressures. So this will include human population density and also denisty of roads, difference in since
## 30 % of the land was converted to human use and the amount of natural habitat in the landscape

hist(primary_forest_data$dissimilarity)

hist(car::logit(
  (1 - primary_forest_data$dissimilarity) * primary_forest_data$beta_shared,
  adjust = 0.0001,
  percents = FALSE
)) ## thats it but also we want to make it similarity as opposed to
## dissimilarity


primary_forest_data <-
  primary_forest_data %>% dplyr::mutate(
    similarity = car::logit((1 - primary_forest_data$dissimilarity) * primary_forest_data$beta_shared,
                            adjust = 0.0001,
                            percents = FALSE
    )
  )

### do any of teh other varibles that are going to be modelled need transforming that haven't already been
### transformed

## 1) environmental distance

hist(primary_forest_data$environmental_distance)

hist(log(primary_forest_data$environmental_distance))


### looks great

primary_forest_data$log_environmental_distance <-
  log(primary_forest_data$environmental_distance + 1)
primary_forest_data$log_environmental_distance <-
  ifelse(is.nan(primary_forest_data$environmental_distance),
         0,
         primary_forest_data$environmental_distance)

### 2) geographic distance

hist(primary_forest_data$geographic_distance)


hist(log(primary_forest_data$geographic_distance))

hist(sqrt(primary_forest_data$geographic_distance))


### log looks good

primary_forest_data$log_geographic_distance <-
  log(primary_forest_data$geographic_distance + 1)







source("https://highstat.com/Books/Book2/HighstatLibV10.R")
corvif(primary_forest_data[,c("land_use_combination",
                        "realm",
                        "site2_log_hpd",
                        "site2_log_T30",
                        "site2_nat_hab_sw",
                        "site2_log_roads",
                        "log_hpd_diff",
                        "log_T30_diff",
                        "nat_hab_diff",
                        "log_roads_diff",
                        "log_environmental_distance",
                        "log_geographic_distance")])




#### great the is no colinearity issues that are obvious here.


new_scale <- function(x, sds) {
  mean <- mean(x, na.rm = TRUE)
  sd <- sd(x)
  
  
  scale_variable <- (x - mean) / ((1 / sds) * sd)
  
  
  attr(scale_variable, "scale.scale") <- ((1 / sds) * sd)
  attr(scale_variable, "scale.center") <- mean
  
  
  return(scale_variable)
  
}



primary_forest_data$site2_log_hpd <- new_scale(primary_forest_data$site2_log_hpd, 2)
primary_forest_data$site2_log_T30 <- new_scale(primary_forest_data$site2_log_T30, 2)
primary_forest_data$site2_nat_hab_sw <-
  new_scale(primary_forest_data$site2_nat_hab_sw, 2)
primary_forest_data$site2_log_roads <- new_scale(primary_forest_data$site2_log_roads, 2)
primary_forest_data$log_hpd_diff <- new_scale(primary_forest_data$log_hpd_diff, 2)
primary_forest_data$log_T30_diff <- new_scale(primary_forest_data$log_T30_diff, 2)
primary_forest_data$nat_hab_diff <- new_scale(primary_forest_data$nat_hab_diff, 2)
primary_forest_data$log_roads_diff <- new_scale(primary_forest_data$log_roads_diff, 2)
primary_forest_data$log_environmental_distance <-
  new_scale(primary_forest_data$log_environmental_distance, 2)
primary_forest_data$log_geographic_distance <-
  new_scale(primary_forest_data$log_geographic_distance, 2)
primary_forest_data$control_hpd <- new_scale(primary_forest_data$control_hpd, 2)
primary_forest_data$control_roads <- new_scale(primary_forest_data$control_roads, 2)



### because the same site gets compared multiple times standard methods of model simplification are unsuitable
### this means that we need to permute the data 1000 times -- I need to read up on this I remeber understanding it before


if (!any(grepl(list.files("outputs"), pattern = "non_forest_permuted_data.rds"))) {
  Permuted_data <- rep(list(NA), 1000)
  
  set.seed(12345)
  
  for (i in 1:1000) {
    sample_data <- c()
    
    for (study in unique(primary_forest_data$SS)) {
      data <- primary_forest_data %>% filter(SS == study)
      
      data$similarity <- data[sample(NROW(data)), "similarity"]
      
      sample_data <- rbind(sample_data, data)
      
    }
    
    Permuted_data[[i]] <- sample_data
    
  }
  write_rds("outputs/beta_modelling_forest_permuted_data.rds", x = Permuted_data)
}

Permuted_data <- readRDS("outputs/beta_modelling_forest_permuted_data.rds")

source("functions/Permuted_model_simplification.R")


#### let's start with the maximal model


model_1 <- lmer(
  similarity ~ land_use_combination +
    land_use_combination:site2_log_hpd + land_use_combination:site2_log_T30 + land_use_combination:site2_nat_hab_sw +
    land_use_combination:site2_log_roads + land_use_combination:log_hpd_diff + land_use_combination:log_T30_diff +
    land_use_combination:nat_hab_diff + land_use_combination:log_roads_diff +
    site2_log_hpd + site2_log_T30 +
    site2_nat_hab_sw +
    site2_log_roads +
    log_hpd_diff +
    log_T30_diff +
    nat_hab_diff +
    log_roads_diff +
    log_environmental_distance + log_geographic_distance +
    control_roads + control_hpd +
    (1 | SS),
  data = primary_forest_data
)


summary(model_1)
car::Anova(model_1)

BIC(model_1)







model_2 <- update(model_1,  ~ . -land_use_combination:site2_log_T30)
model_3 <- update(model_2,  ~ . -land_use_combination:site2_log_hpd)


model_3 <- lmer(
  similarity ~ land_use_combination + land_use_combination:site2_nat_hab_sw +
    land_use_combination:site2_log_roads + land_use_combination:log_hpd_diff + land_use_combination:log_T30_diff +
    land_use_combination:nat_hab_diff + land_use_combination:log_roads_diff +
    site2_log_hpd + site2_log_T30 +
    site2_nat_hab_sw +
    site2_log_roads +
    log_hpd_diff +
    log_T30_diff +
    nat_hab_diff +
    log_roads_diff +
    log_environmental_distance + log_geographic_distance +
    control_roads + control_hpd +
    (1 | SS),
  data = primary_forest_data
)

BIC(model_1)

### tetsing whether a random slope is required 


## site2_nat_hab_sw

model_3a <- lmer(
  similarity ~ land_use_combination + land_use_combination:site2_nat_hab_sw + land_use_combination:site2_log_T30 +
    land_use_combination:site2_log_roads + land_use_combination:log_hpd_diff + land_use_combination:log_T30_diff +
    land_use_combination:nat_hab_diff + land_use_combination:log_roads_diff + land_use_combination:site2_log_hpd +
    site2_log_hpd + site2_log_T30 +
    site2_nat_hab_sw +
    site2_log_roads +
    log_hpd_diff +
    log_T30_diff +
    nat_hab_diff +
    log_roads_diff +
    log_environmental_distance + log_geographic_distance +
    control_roads + control_hpd +
    (site2_nat_hab_sw| SS),
  data = primary_forest_data
)

BIC(model_3a) #### 25936.87 very much lower okay 


## site2_log_roads

model_3b <- lmer(
  similarity ~ land_use_combination + land_use_combination:site2_nat_hab_sw + land_use_combination:site2_log_T30 +
    land_use_combination:site2_log_roads + land_use_combination:log_hpd_diff + land_use_combination:log_T30_diff +
    land_use_combination:nat_hab_diff + land_use_combination:log_roads_diff + land_use_combination:site2_log_hpd +
    site2_log_hpd + site2_log_T30 +
    site2_nat_hab_sw +
    site2_log_roads +
    log_hpd_diff +
    log_T30_diff +
    nat_hab_diff +
    log_roads_diff +
    log_environmental_distance + log_geographic_distance +
    control_roads + control_hpd +
    (site2_log_roads| SS),
  data = primary_forest_data
)

BIC(model_3b) #### 25915.91  very much lower okay 

## site2_log_hpd

model_3c <- lmer(
  similarity ~ land_use_combination + land_use_combination:site2_nat_hab_sw + land_use_combination:site2_log_T30 +
    land_use_combination:site2_log_roads + land_use_combination:log_hpd_diff + land_use_combination:log_T30_diff +
    land_use_combination:nat_hab_diff + land_use_combination:log_roads_diff + land_use_combination:site2_log_hpd +
    site2_log_hpd + site2_log_T30 +
    site2_nat_hab_sw +
    site2_log_roads +
    log_hpd_diff +
    log_T30_diff +
    nat_hab_diff +
    log_roads_diff +
    log_environmental_distance + log_geographic_distance +
    control_roads + control_hpd +
    (site2_log_hpd| SS),
  data = primary_forest_data
)

BIC(model_3c) #### 25961.52 not as low as site2_log_roads  


## site2_log_T30

model_3d <- lmer(
  similarity ~ land_use_combination + land_use_combination:site2_nat_hab_sw + land_use_combination:site2_log_T30 +
    land_use_combination:site2_log_roads + land_use_combination:log_hpd_diff + land_use_combination:log_T30_diff +
    land_use_combination:nat_hab_diff + land_use_combination:log_roads_diff + land_use_combination:site2_log_hpd +
    site2_log_hpd + site2_log_T30 +
    site2_nat_hab_sw +
    site2_log_roads +
    log_hpd_diff +
    log_T30_diff +
    nat_hab_diff +
    log_roads_diff +
    log_environmental_distance + log_geographic_distance +
    control_roads + control_hpd +
    (site2_log_T30| SS),
  data = primary_forest_data
)

BIC(model_3d) #### singular fit -- pass


## nat_hab_diff

model_3e <- lmer(
  similarity ~ land_use_combination + land_use_combination:site2_nat_hab_sw + land_use_combination:site2_log_T30 +
    land_use_combination:site2_log_roads + land_use_combination:log_hpd_diff + land_use_combination:log_T30_diff +
    land_use_combination:nat_hab_diff + land_use_combination:log_roads_diff + land_use_combination:site2_log_hpd +
    site2_log_hpd + site2_log_T30 +
    site2_nat_hab_sw +
    site2_log_roads +
    log_hpd_diff +
    log_T30_diff +
    nat_hab_diff +
    log_roads_diff +
    log_environmental_distance + log_geographic_distance +
    control_roads + control_hpd +
    (nat_hab_diff| SS),
  data = primary_forest_data
)

BIC(model_3e) #### not as low as site2_log_roads

## log_roads_diff

model_3f <- lmer(
  similarity ~ land_use_combination + land_use_combination:site2_nat_hab_sw + land_use_combination:site2_log_T30 +
    land_use_combination:site2_log_roads + land_use_combination:log_hpd_diff + land_use_combination:log_T30_diff +
    land_use_combination:nat_hab_diff + land_use_combination:log_roads_diff + land_use_combination:site2_log_hpd +
    site2_log_hpd + site2_log_T30 +
    site2_nat_hab_sw +
    site2_log_roads +
    log_hpd_diff +
    log_T30_diff +
    nat_hab_diff +
    log_roads_diff +
    log_environmental_distance + log_geographic_distance +
    control_roads + control_hpd +
    (log_roads_diff| SS),
  data = primary_forest_data
)

BIC(model_3f) #### increased BIC

## log_hpd_diff

model_3g <- lmer(
  similarity ~ land_use_combination + land_use_combination:site2_nat_hab_sw + land_use_combination:site2_log_T30 +
    land_use_combination:site2_log_roads + land_use_combination:log_hpd_diff + land_use_combination:log_T30_diff +
    land_use_combination:nat_hab_diff + land_use_combination:log_roads_diff + land_use_combination:site2_log_hpd +
    site2_log_hpd + site2_log_T30 +
    site2_nat_hab_sw +
    site2_log_roads +
    log_hpd_diff +
    log_T30_diff +
    nat_hab_diff +
    log_roads_diff +
    log_environmental_distance + log_geographic_distance +
    control_roads + control_hpd +
    (log_hpd_diff| SS),
  data = primary_forest_data
)

BIC(model_3g) #### not as low as log roads

## log_T30_diff

model_3h <- lmer(
  similarity ~ land_use_combination + land_use_combination:site2_nat_hab_sw + land_use_combination:site2_log_T30 +
    land_use_combination:site2_log_roads + land_use_combination:log_hpd_diff + land_use_combination:log_T30_diff +
    land_use_combination:nat_hab_diff + land_use_combination:log_roads_diff + land_use_combination:site2_log_hpd +
    site2_log_hpd + site2_log_T30 +
    site2_nat_hab_sw +
    site2_log_roads +
    log_hpd_diff +
    log_T30_diff +
    nat_hab_diff +
    log_roads_diff +
    log_environmental_distance + log_geographic_distance +
    control_roads + control_hpd +
    (log_T30_diff| SS),
  data = primary_forest_data
)

BIC(model_3h) #### fit is singular -- pass


## so continue with model 3b -- random slope of site2_log_roads

summary(model_3b)
car::Anova(model_3b)


write_rds(file = "outputs/functional_similarity_forest_model.rds", x = model_3a)
write_rds(file = "outputs/functional_similarity_forest_dataframe.rds", x  = primary_forest_data)



tested_variables <- simplification_test_variables(model_1)


registerDoParallel(cores = length(tested_variables))


model_simplification <-
  foreach(
    variable = tested_variables,
    .combine = "rbind",
    .packages = c("tidyverse", "car", "lme4")
  ) %dopar% {
    mod_sim <-
      Permuted_model_simplification(data = Permuted_data,
                                    model1 = model_1,
                                    remove = variable)
    
    return(mod_sim)
  }

registerDoSEQ()
closeAllConnections()


write_rds(file = "outputs/model_permutation_forest_one.rds", x =  model_simplification)
# #
model_simplification <- readRDS("outputs/model_permutation_forest_one.rds")



# model_4 <- update(model_3, ~ . - land_use_combination:site2_nat_hab_sw)
# 
# 
# summary(model_4)
# 
# 
# 
# ######
# 
# 
# 
# 
# inv_logit <- function(f, a) {
#   a <- (1 - 2 * a)
#   (a * (1 + exp(f)) + (exp(f) - 1)) / (2 * a * (1 + exp(f)))
# }
# 
# 
# scale_column <- function(values, column) {
#   return(values - attr(column, "scale.center")) / attr(column, "scale.scale")
#   
# }
# 
# 
# 
# data <- data.frame(land_use_combination = levels(primary_forest_data$land_use_combination),
#                    site2_log_hpd = scale_column(0,primary_forest_data$site2_log_hpd),
#                    site2_nat_hab_sw = inv_logit(scale_column(1,primary_forest_data$site2_nat_hab_sw), a = 0.001),
#                    site2_log_roads = scale_column(0,primary_forest_data$site2_log_roads),
#                    site2_log_T30 = scale_column(0,primary_forest_data$site2_log_T30),
#                    log_hpd_diff  = scale_column(0,primary_forest_data$log_hpd_diff),
#                    log_T30_diff = scale_column(0,primary_forest_data$log_T30_diff),
#                    nat_hab_diff = scale_column(0,primary_forest_data$nat_hab_diff),
#                    log_roads_diff = scale_column(0,primary_forest_data$log_roads_diff),
#                    log_environmental_distance = scale_column(log(0 + 1),primary_forest_data$log_environmental_distance) ,
#                    log_geographic_distance = scale_column(log(0 + 1),primary_forest_data$log_geographic_distance),
#                    control_roads = scale_column(0,primary_forest_data$control_roads),
#                    control_hpd = scale_column(0,primary_forest_data$control_hpd))
# data <-
#   data.frame(
#     land_use_combination = levels(primary_forest_data$land_use_combination),
#     site2_log_hpd = 0,
#     site2_nat_hab_sw = 0,
#     site2_log_roads = 0,
#     site2_log_T30 = 0 ,
#     log_hpd_diff  = 0,
#     log_T30_diff = 0,
#     nat_hab_diff = 0,
#     log_roads_diff = 0,
#     log_environmental_distance = 0 ,
#     log_geographic_distance = 0,
#     control_roads = 0,
#     control_hpd = 0
#   )
# # 
# # 
# # 
# # # land_use_colours <-
# # #   data.frame(
# # #     land_use_combination = land_use_combination_levels,
# # #     colours = c(
# # #       "green4",
# # #       "chartreuse4",
# # #       "olivedrab2",
# # #       "springgreen2",
# # #       "#EBF787",
# # #       "#E3D438",
# # #       "#718879"
# # #     )
# # #   )
# # # rownames(land_use_colours) <- land_use_combination_levels
# # 
# # 
# # 
# 
# pred_fun <- function(x) {
#   as.numeric(predict(x, newdata = data, re.form = NA))
# }
# 
# 
# 
# 
# 
# 
# 
# 
# data$estimate <- NA
# data$upper <- NA
# data$lower <- NA
# 
# 
# booted_mod <-
#   bootMer(
#     model_6,
#     FUN = function(x)
#       pred_fun(x),
#     nsim =  100,
#     seed = 1234
#   )
# 
# 
# 
# data$estimate <- inv_logit(booted_mod$t0, a = 0.001)
# bootstraps <- inv_logit(booted_mod$t, a = 0.001)
# 
# 
# 
# ninety_five_boot <- c()
# 
# for (col in 1:ncol(bootstraps)) {
#   trim_col <- bootstraps[, col]
#   
#   trim_col <-
#     trim_col[trim_col > as.numeric(quantile(bootstraps[, col], 0)) &
#                trim_col < as.numeric(quantile(bootstraps[, col], 1))]
#   
#   
#   ninety_five_boot <- cbind(ninety_five_boot, trim_col)
#   
# }
# 
# 
# ninety_five_boot <- ninety_five_boot / median(ninety_five_boot[, 1])
# 
# ninety_five_boot <- data.frame(ninety_five_boot)
# 
# 
# colnames(ninety_five_boot) <-
#   land_use_combination_levels
# 
# 
# boot_plot <-
#   ninety_five_boot %>% pivot_longer(cols = colnames(ninety_five_boot), names_to = "land_use_combination")
# 
# 
# boot_plot$land_use_combination <-
#   factor(boot_plot$land_use_combination, levels = land_use_combination_levels)
# 
# 
# realm_size_plot <-
#   ggplot(data = boot_plot,
#          aes(x = land_use_combination, y = value, group = land_use_combination)) +
#   geom_violin(
#     aes(colour = land_use_combination, fill = land_use_combination),
#     alpha = 0.5,
#     show.legend = FALSE
#   ) +
#   geom_boxplot(width = 0.1,
#                aes(fill = land_use_combination),
#                show.legend = FALSE) +
#   geom_hline(yintercept = 1, linetype = "dashed") +
#   #scale_colour_manual(values = land_use_colours$colours) +
#   #scale_fill_manual(values = land_use_colours$colours) +
#   #scale_y_continuous(limits = c(0.2, 2.2),
#   #                  breaks = c(0.5,1,1.5,2)) +
#   theme(
#     panel.background = element_rect(fill = 'white', color = 'white'),
#     panel.grid.major = element_line(color = 'white'),
#     panel.grid.minor = element_line(color = 'white')
#   )
# # axis.title.y = element_blank(),
# #axis.text.y = element_blank(),
# #axis.ticks.y = element_blank(),
# #axis.title.x = element_blank(),
# #axis.line = element_line(colour = "black", linetype = "solid"),
# #axis.ticks.x = element_blank(),
# #axis.text.x = element_blank())
# 
# 
# plot(realm_size_plot)
# 
# 
# 
# 
# 
# 
# 
# boxplot(primary_forest_data$similarity ~ primary_forest_data$land_use_combination)
# 

# 
# 
# 
# 
# # ## drop land_use_combination:site2_nat_hab_sw
# 
# model_4 <- update(model_3, ~ . - land_use_combination:site2_log_T30)
# 
# summary(model_4)
# 
# 
# 
# 
# 
# 
# ### drop land_use_combination:log_T30_diff
# 
# model_5 <- update(model_4, ~ . - land_use_combination:log_T30_diff)
# summary(model_5)
# car::Anova(model_5)
# 
# 
# 
# 
# 
# ### drop land_use_combination:site2_nat_hab_sw
# 
# model_6 <- update(model_5, ~ . - land_use_combination:site2_nat_hab_sw)
# summary(model_6)
# car::Anova(model_6)
# 
# 
# ggResidpanel::resid_panel(model_6)
# 
# 
# 
# 
