### now that we have the alpha diveristy and beta diversity measures we need to gather some more varaibles that we can
### model. some pressures that are going to likely impact a commuities alpha and beta functional diversity include:
## Human population density, density of roads, percentage of the landscape that is natural, and the time since
## 30 % of the landscape was converted to human use.

rm(list = ls())

require(tidyverse)
require(geosphere)
require(future)
require(future.apply)

drop_species <- readRDS("data/assembly_drop_spp.rds")





PREDICTS <-
  readRDS("data/tom_refined_data.rds")  %>%
  dplyr::filter(Predominant_habitat != "Cannot decide") %>%
  dplyr::mutate(Predominant_habitat = ifelse(
    grepl(
      Predominant_habitat,
      pattern = "secondary",
      ignore.case = TRUE
    ),
    "Secondary vegetation",
    paste(Predominant_habitat)
  )) %>% dplyr::filter(!(Birdlife_Name %in% drop_species)) %>% dplyr::mutate(LUI = paste(Predominant_habitat, Use_intensity, sep = "_"))







# Derived traits from the two-step PCA analysis


trait_list <- readRDS("outputs/traits_without_foraging_list_tom.rds")



all_sites <-
  PREDICTS %>% dplyr::distinct(SSBS) %>% pull() %>% as.character()


pressure_data <- readRDS("data/AllSites_pressures.rds")



PREDICTS_coords <-
  PREDICTS %>% dplyr::distinct(
    SS,
    SSBS,
    Longitude,
    Latitude,
    LUI,
    Use_intensity,
    Sample_start_earliest
  ) %>% dplyr::distinct(SSBS, .keep_all = TRUE)

pressure_coords <-
  pressure_data %>% dplyr::distinct(SSBS, Longitude, Latitude)



years_to_extract <- unique(substr(PREDICTS_coords$Sample_start_earliest,1,4))


PREDICTS_coords$year <- substr(PREDICTS_coords$Sample_start_earliest,1,4)


### so we want some data that Andu has extracted for the PREDICTS sites so we can get

# Human population density - log_hpd_1km
# road density 10 km - road_density and log roads
# proportion of natural habitat nat_hab2 / nat_hab_sw
# time since 30 % of the land was converted to human use - T30 / log_T30

## so lets grab those columns






#### need to get the pressure ratsers for the years of 2011, 2009, 2008, and 2010 I can get these from the museum shortly
### will just have to use what I have for now though

hpd_2000 <- terra::rast("data/projection_rasters/log_hpd/log_hpd_1km_2000.tif")
hpd_2001 <- terra::rast("data/projection_rasters/log_hpd/log_hpd_1km_2001.tif")
hpd_2002 <- terra::rast("data/projection_rasters/log_hpd/log_hpd_1km_2002.tif")
hpd_2003 <- terra::rast("data/projection_rasters/log_hpd/log_hpd_1km_2003.tif")
hpd_2004 <- terra::rast("data/projection_rasters/log_hpd/log_hpd_1km_2004.tif")
hpd_2005 <- terra::rast("data/projection_rasters/log_hpd/log_hpd_1km_2005.tif")
hpd_2006 <- terra::rast("data/projection_rasters/log_hpd/log_hpd_1km_2006.tif")
hpd_2007 <- terra::rast("data/projection_rasters/log_hpd/log_hpd_1km_2007.tif")
hpd_2008 <- terra::rast("data/projection_rasters/log_hpd/log_hpd_1km_2008.tif")
hpd_2009 <- terra::rast("data/projection_rasters/log_hpd/log_hpd_1km_2009.tif")
hpd_2010 <- terra::rast("data/projection_rasters/log_hpd/log_hpd_1km_2010.tif")
hpd_2011 <- terra::rast("data/projection_rasters/log_hpd/log_hpd_1km_2011.tif") 
hpd_2012 <- terra::rast("data/projection_rasters/log_hpd/log_hpd_1km_2012.tif")
hpd_2013 <- terra::rast("data/projection_rasters/log_hpd/log_hpd_1km_2013.tif")
hpd_2014 <- terra::rast("data/projection_rasters/log_hpd/log_hpd_1km_2014.tif")
hpd_2015 <- terra::rast("data/projection_rasters/log_hpd/log_hpd_1km_2015.tif")
hpd_2016 <- terra::rast("data/projection_rasters/log_hpd/log_hpd_1km_2016.tif")
hpd_2017 <- terra::rast("data/projection_rasters/log_hpd/log_hpd_1km_2017.tif")
hpd_2018 <- terra::rast("data/projection_rasters/log_hpd/log_hpd_1km_2018.tif")
hpd_2019 <- terra::rast("data/projection_rasters/log_hpd/log_hpd_1km_2019.tif")
hpd_2020 <- terra::rast("data/projection_rasters/log_hpd/log_hpd_1km_2020.tif")


## can work it out based on the year 

T30_1993 <- terra::rast("data/projection_rasters/year_30/log_years_since_30_1993.tif")
T30_1994 <- terra::rast("data/projection_rasters/year_30/log_years_since_30_1994.tif")
T30_1999 <- terra::rast("data/projection_rasters/year_30/log_years_since_30_1999.tif")
T30_2000 <- terra::rast("data/projection_rasters/year_30/log_years_since_30_2000.tif")
T30_2001 <- terra::rast("data/projection_rasters/year_30/log_years_since_30_2001.tif")
T30_2002 <- terra::rast("data/projection_rasters/year_30/log_years_since_30_2002.tif")
T30_2003 <- terra::rast("data/projection_rasters/year_30/log_years_since_30_2003.tif")
T30_2004 <- terra::rast("data/projection_rasters/year_30/log_years_since_30_2004.tif")
T30_2005 <- terra::rast("data/projection_rasters/year_30/log_years_since_30_2005.tif")
T30_2006 <- terra::rast("data/projection_rasters/year_30/log_years_since_30_2006.tif")
T30_2007 <- terra::rast("data/projection_rasters/year_30/log_years_since_30_2007.tif")
T30_2008 <- terra::rast("data/projection_rasters/year_30/log_years_since_30_2008.tif")
T30_2009 <- terra::rast("data/projection_rasters/year_30/log_years_since_30_2009.tif")
T30_2010 <- terra::rast("data/projection_rasters/year_30/log_years_since_30_2010.tif")
T30_2011 <- terra::rast("data/projection_rasters/year_30/log_years_since_30_2011.tif")
T30_2012 <- terra::rast("data/projection_rasters/year_30/log_years_since_30_2012.tif")
T30_2013 <- terra::rast("data/projection_rasters/year_30/log_years_since_30_2013.tif")
T30_2014 <- terra::rast("data/projection_rasters/year_30/log_years_since_30_2014.tif")
T30_2015 <- terra::rast("data/projection_rasters/year_30/log_years_since_30_2015.tif")
T30_2016 <- terra::rast("data/projection_rasters/year_30/log_years_since_30_2016.tif")
T30_2017 <- terra::rast("data/projection_rasters/year_30/log_years_since_30_2017.tif")
T30_2018 <- terra::rast("data/projection_rasters/year_30/log_years_since_30_2018.tif")
T30_2019 <- terra::rast("data/projection_rasters/year_30/log_years_since_30_2019.tif")
T30_2020 <- terra::rast("data/projection_rasters/year_30/log_years_since_30_2020.tif")

## roads is a fixed layer so has no temporal element 

roads_ras <- terra::rast("data/projection_rasters/2000-rasters-for-projections/unformatted_rasters/log_road_density.tif")


### nat hab sw

nat_hab_2000 <- terra::rast("data/projection_rasters/nathabsw/NatHabSlidingWindow-2000.tif") 
nat_hab_2001 <- terra::rast("data/projection_rasters/nathabsw/NatHabSlidingWindow-2001.tif")
nat_hab_2002 <- terra::rast("data/projection_rasters/nathabsw/NatHabSlidingWindow-2002.tif")
nat_hab_2003 <- terra::rast("data/projection_rasters/nathabsw/NatHabSlidingWindow-2003.tif")
nat_hab_2004 <- terra::rast("data/projection_rasters/nathabsw/NatHabSlidingWindow-2004.tif")
nat_hab_2005 <- terra::rast("data/projection_rasters/nathabsw/NatHabSlidingWindow-2005.tif")
nat_hab_2006 <- terra::rast("data/projection_rasters/nathabsw/NatHabSlidingWindow-2006.tif")
nat_hab_2007 <- terra::rast("data/projection_rasters/nathabsw/NatHabSlidingWindow-2007.tif")
nat_hab_2008 <- terra::rast("data/projection_rasters/nathabsw/NatHabSlidingWindow-2008.tif")
nat_hab_2009 <- terra::rast("data/projection_rasters/nathabsw/NatHabSlidingWindow-2009.tif")
nat_hab_2010 <- terra::rast("data/projection_rasters/nathabsw/NatHabSlidingWindow-2010.tif")
nat_hab_2011 <- terra::rast("data/projection_rasters/nathabsw/NatHabSlidingWindow-2011.tif")
nat_hab_2012 <- terra::rast("data/projection_rasters/nathabsw/NatHabSlidingWindow-2012.tif")
nat_hab_2013 <- terra::rast("data/projection_rasters/nathabsw/NatHabSlidingWindow-2013.tif")
nat_hab_2014 <- terra::rast("data/projection_rasters/nathabsw/NatHabSlidingWindow-2014.tif")
nat_hab_2015 <- terra::rast("data/projection_rasters/nathabsw/NatHabSlidingWindow-2015.tif")
nat_hab_2016 <- terra::rast("data/projection_rasters/nathabsw/NatHabSlidingWindow-2016.tif")
nat_hab_2017 <- terra::rast("data/projection_rasters/nathabsw/NatHabSlidingWindow-2017.tif")
nat_hab_2018 <- terra::rast("data/projection_rasters/nathabsw/NatHabSlidingWindow-2018.tif")
nat_hab_2019 <- terra::rast("data/projection_rasters/nathabsw/NatHabSlidingWindow-2019.tif")
nat_hab_2020 <- terra::rast("data/projection_rasters/nathabsw/NatHabSlidingWindow-2020.tif")


pressure_rasters <- list("1993" = list(hpd =hpd_2000, T30 = T30_1993,roads = roads_ras, nat_hab = nat_hab_2000),
                         "1994" = list(hpd =hpd_2000, T30 = T30_1994,roads = roads_ras, nat_hab = nat_hab_2000),
                         "1999" = list(hpd =hpd_2000, T30 = T30_1999,roads = roads_ras, nat_hab = nat_hab_2000),
                         "2000" = list(hpd =hpd_2000, T30 = T30_2000,roads = roads_ras, nat_hab = nat_hab_2000),
                         "2001" = list(hpd =hpd_2001, T30 = T30_2001,roads = roads_ras, nat_hab = nat_hab_2001),
                         "2002" = list(hpd =hpd_2002, T30 = T30_2002,roads = roads_ras, nat_hab = nat_hab_2002),
                         "2003" = list(hpd =hpd_2003, T30 = T30_2003,roads = roads_ras, nat_hab = nat_hab_2003),
                         "2004" = list(hpd =hpd_2004, T30 = T30_2004,roads = roads_ras, nat_hab = nat_hab_2004),
                         "2005" = list(hpd =hpd_2005, T30 = T30_2005,roads = roads_ras, nat_hab = nat_hab_2005),
                         "2006" = list(hpd =hpd_2006, T30 = T30_2006,roads = roads_ras, nat_hab = nat_hab_2006),
                         "2007" = list(hpd =hpd_2007, T30 = T30_2007,roads = roads_ras, nat_hab = nat_hab_2007),
                         "2008" = list(hpd =hpd_2008, T30 = T30_2008,roads = roads_ras, nat_hab = nat_hab_2008),
                         "2009" = list(hpd =hpd_2009, T30 = T30_2009,roads = roads_ras, nat_hab = nat_hab_2009),
                         "2010" = list(hpd =hpd_2010, T30 = T30_2010,roads = roads_ras, nat_hab = nat_hab_2010),
                         "2011" = list(hpd =hpd_2011, T30 = T30_2011,roads = roads_ras, nat_hab = nat_hab_2011),
                         "2012" = list(hpd =hpd_2012, T30 = T30_2012,roads = roads_ras, nat_hab = nat_hab_2012),
                         "2013" = list(hpd =hpd_2013, T30 = T30_2013,roads = roads_ras, nat_hab = nat_hab_2013),
                         "2014" = list(hpd =hpd_2014, T30 = T30_2014,roads = roads_ras, nat_hab = nat_hab_2014),
                         "2015" = list(hpd =hpd_2015, T30 = T30_2015,roads = roads_ras, nat_hab = nat_hab_2015),
                         "2016" = list(hpd =hpd_2016, T30 = T30_2016,roads = roads_ras, nat_hab = nat_hab_2016),
                         "2017" = list(hpd =hpd_2017, T30 = T30_2017,roads = roads_ras, nat_hab = nat_hab_2017),
                         "2018" = list(hpd =hpd_2018, T30 = T30_2018,roads = roads_ras, nat_hab = nat_hab_2018),
                         "2019" = list(hpd =hpd_2019, T30 = T30_2019,roads = roads_ras, nat_hab = nat_hab_2019),
                         "2020" = list(hpd =hpd_2020, T30 = T30_2020,roads = roads_ras, nat_hab = nat_hab_2020))


pressure_data <- c()

for(yr in years_to_extract){
  
  yr_data <- PREDICTS_coords %>% dplyr::filter(year == yr)
  
  hpd <- terra::extract(pressure_rasters[[yr]][["hpd"]], y = yr_data[,c("Longitude","Latitude")]) %>% 
    dplyr::select(2)
  
  T30 <- terra::extract(pressure_rasters[[yr]][["T30"]], y = yr_data[,c("Longitude","Latitude")]) %>% 
    dplyr::mutate(log_T30 = ifelse(is.na(log_T30), 0,log_T30)) %>% dplyr::select(2)
  
  
  roads <- terra::extract(pressure_rasters[[yr]][["roads"]], y = yr_data[,c("Longitude","Latitude")]) 
  
  roads$grip4_tp1_dens_m_km2 <- ifelse(is.na(roads$grip4_tp1_dens_m_km2),0, roads$grip4_tp1_dens_m_km2)
  roads <- roads %>% dplyr::select(2)
  
  nat_hab <- terra::extract(pressure_rasters[[yr]][["nat_hab"]], y = yr_data[,c("Longitude","Latitude")]) %>%
    dplyr::select(2)
  
  extract_data <- data.frame(SSBS = yr_data$SSBS, log_hpd_1km = hpd[,1],T30 = exp(T30[,1]) - 1, log_T30 = T30[,1],
                             nat_hab_sw = nat_hab[,1], road_density = roads[,1], log_roads = log(roads[,1] + 1))
  
  pressure_data <- rbind(pressure_data, extract_data)
  
}


PREDICTS_sites <- PREDICTS_coords %>%
  dplyr::left_join(pressure_data[, c(
    "SSBS",
    "log_hpd_1km",
    "T30",
    "log_T30",
    "nat_hab_sw",
    "road_density",
    "log_roads"
  )])


##### next up is to load in the alpha diversity hypervolumes and calculate functional richness -- although
#### this may not be the best way as it doesn't account for differences in relative abundance - possibly it
### would be good to also calculate a measure of rao's q using the hypervolumes too.

alpha_hypervolumes <-
  readRDS("outputs/alpha_diversity_site_tpds_tom.rds")

PREDICTS_sites <-
  PREDICTS_sites %>% dplyr::filter(SSBS %in% names(alpha_hypervolumes))


get_richness <- function(site) {
  data <- alpha_hypervolumes[[site]]
  cell_volume <- data$data$cell_volume
  
  FRich <- sum(data$TPDc$RelativeAbundance > 0) * cell_volume
  
  return(FRich)
  
}


PREDICTS_sites$functional_richness <-
  apply(
    PREDICTS_sites,
    MARGIN = 1,
    FUN = function(x)
      get_richness(x[2])
  )

##### actually let's ignore rao for now that will have to be done on the HPC... and calculated with sites
#### within the same study

get_site_probabilities <- function(sites) {
  evaluation_grid <-
    alpha_hypervolumes[[sites[1]]]$data$evaluation_grid
  
  
  for (site in sites) {
    evaluation_grid[, site] <-
      alpha_hypervolumes[[site]]$TPDc$RelativeAbundance
    
  }
  
  return(evaluation_grid)
}



all_sites <- names(alpha_hypervolumes)

## or calcualte RaosQ within study - easier computationally as you dont have to generate the full distance
## matrix --



get_raos <- function(study) {
  
  study_sites <-
    PREDICTS_sites %>% dplyr::filter(SS == study) %>% dplyr::distinct(SSBS) %>% pull() %>%
    as.character()
  
  
  site_probabilities <- get_site_probabilities(study_sites)
  
  
  probabilities <-
    as.matrix(site_probabilities[, c(4:ncol(site_probabilities))])
  
  colnames(probabilities) <- study_sites
  
  evaluation_grid <-
    site_probabilities[rowSums(probabilities) > 0, c(1:3)]
  
  probabilities <- t(probabilities[rowSums(probabilities) > 0,])
  rownames(probabilities) <- study_sites
  
  
  gow_dis <- sqrt(as.matrix(FD::gowdis(evaluation_grid)))
  
  distance_times_p1 <- probabilities %*% gow_dis
  
  d_times_p1_times_p2 <-
    rowSums(sweep(probabilities, 1, distance_times_p1, "*", check.margin = FALSE))
  
  # relative_raos_q <-
  #   d_times_p1_times_p2 / sum(d_times_p1_times_p2)
  # 
  
  raos_q <-
    data.frame(SSBS = rownames(probabilities), raosQ = d_times_p1_times_p2)
  
  return(raos_q)
  
}


studies <- unique(PREDICTS_sites$SS)

study_raos <- c()
for (study in studies) {
  study_raos <- rbind(study_raos, get_raos(study))
}

### attach the raos q and calculate the control_hpd and density of roads

PREDICTS_sites <- PREDICTS_sites %>%
  dplyr::left_join(study_raos) %>%
  dplyr::group_by(SS) %>% dplyr::mutate(control_hpd = mean(log_hpd_1km),
                                        control_roads = mean(log_roads),
                                        relative_richness = functional_richness/sum(functional_richness)) %>%
  dplyr::ungroup()


PREDICTS_sites <- PREDICTS_sites %>% dplyr::mutate(LUI = ifelse((grepl(SS,pattern = "GN1_2010__Hvenegaard 1")|grepl(SS,pattern = "GN1_2010__Hvenegaard 2"))&LUI == "Cropland_Cannot decide",
                                                                                 "Cropland_Light use", paste(LUI)))

write_rds(file = "outputs/alpha_diversity_dataframe_tom.rds", x = PREDICTS_sites)



######################################
#### beta diveristy pressures ########
######################################


beta_data <- readRDS("outputs/beta_diversity_dataframe_tom.rds")

beta_data <- beta_data[,c(1:13)]



all(unique(beta_data$site1) %in% PREDICTS_sites$SSBS)

all(unique(beta_data$site2) %in% PREDICTS_sites$SSBS)


beta_data_pressures <-
  beta_data %>% dplyr::left_join(PREDICTS_sites[, c(
    "SSBS",
    "log_hpd_1km",
    "log_T30",
    "nat_hab_sw",
    "log_roads",
    "control_hpd",
    "control_roads"
  )], by = c("site2" = "SSBS")) %>%
  rename(
    site2_log_hpd = log_hpd_1km,
    site2_log_T30 = log_T30,
    site2_nat_hab_sw = nat_hab_sw,
    site2_log_roads = log_roads
  ) %>%
  dplyr::left_join(PREDICTS_sites[, c("SSBS", "log_hpd_1km", "log_T30", "nat_hab_sw", "log_roads")], by = c("site1" = "SSBS")) %>%
  dplyr::mutate(
    log_hpd_diff = site2_log_hpd - log_hpd_1km,
    log_T30_diff = site2_log_T30 - log_T30,
    nat_hab_diff = site2_nat_hab_sw - nat_hab_sw,
    log_roads_diff = site2_log_roads - log_roads
  )

beta_data_pressures <- beta_data_pressures %>% dplyr::mutate(land_use_combination = ifelse((grepl(SS,pattern = "GN1_2010__Hvenegaard 1")|grepl(SS,pattern = "GN1_2010__Hvenegaard 2"))&land_use_combination == "Primary forest_Minimal use - Cropland_Cannot decide",
                                                    "Primary forest_Minimal use - Cropland_Light use", paste(land_use_combination)))

beta_data_pressures <- beta_data_pressures %>% dplyr::mutate(land_use_combination = ifelse((grepl(SS,pattern = "GN1_2010__Hvenegaard 1")|grepl(SS,pattern = "GN1_2010__Hvenegaard 2"))&(land_use_combination == "Primary non-forest_Minimal use - Cropland_Cannot decide"|
                                                                                                                                                                                          land_use_combination == "Primary non-forest_Light use - Cropland_Cannot decide"),
                                                                                           "Primary non-forest_Low use - Cropland_Light use", paste(land_use_combination)))

write_rds(file = "outputs/beta_diversity_dataframe_tom.rds", x = beta_data_pressures)

