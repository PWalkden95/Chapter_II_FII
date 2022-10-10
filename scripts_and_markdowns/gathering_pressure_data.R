### now that we have the alpha diveristy and beta diversity measures we need to gather some more varaibles that we can 
### model. some pressures that are going to likely impact a commuities alpha and beta functional diversity include:
## Human population density, density of roads, percentage of the landscape that is natural, and the time since 
## 30 % of the landscape was converted to human use.

rm(list = ls())

require(tidyverse)
require(geosphere)



drop_species <- readRDS("data/assembly_drop_spp.rds")


PREDICTS <-
  readRDS("data/refined_predicts.rds") %>%
  dplyr::filter(!(Predominant_habitat %in% c("Primary non-forest", "Cannot decide"))) %>%
  dplyr::mutate(
    Predominant_habitat = ifelse(
      grepl(
        Predominant_habitat,
        pattern = "secondary",
        ignore.case = TRUE
      ),
      "Secondary vegetation",
      paste(Predominant_habitat)
    ),
    Predominant_habitat = ifelse(
      grepl(
        Predominant_habitat,
        pattern = "primary",
        ignore.case = TRUE
      ),
      "Primary vegetation",
      paste(Predominant_habitat)
    ),
    Predominant_habitat = ifelse(
      grepl(
        Predominant_habitat,
        pattern = "primary",
        ignore.case = TRUE
      ) &
        Use_intensity == "Minimal use",
      "Primary minimal",
      paste(Predominant_habitat)
    )
  ) %>% dplyr::filter(!(Birdlife_Name %in% drop_species))

# Derived traits from the two-step PCA analysis


trait_list <- readRDS("outputs/traits_without_foraging_list.rds")



all_sites <- PREDICTS %>% dplyr::distinct(SSBS) %>% pull() %>% as.character()


pressure_data <- readRDS("data/AllSites_pressures.rds")


PREDICTS_coords <- PREDICTS %>% dplyr::distinct(SS,SSBS,Longitude,Latitude,Predominant_habitat,Use_intensity,Realm, UN_subregion)
pressure_coords <- pressure_data %>% dplyr::distinct(SSBS,Longitude,Latitude)



closest_site <- function(lat,long){
  
  coords <- as.matrix(data.frame(Longitude = long, Latitude = lat))
  
  close_site <- which.min(distHaversine(coords,pressure_coords[,c(1:2)]))

  return(as.character(pressure_data[close_site,"SSBS"]))
  }


PREDICTS_coords$closest_site <- apply(PREDICTS_coords, MARGIN = 1, FUN = function(x) closest_site(lat = as.numeric(x[4]), long = as.numeric(x[3])))


str(pressure_data)

### so we want some data that Andu has extracted for the PREDICTS sites so we can get

# Human population density - log_hpd_1km
# road density 10 km - road_density and log roads
# proportion of natural habitat nat_hab2 / nat_hab_sw
# time since 30 % of the land was converted to human use - T30 / log_T30

## so lets grab those columns 


PREDICTS_sites <- PREDICTS_coords %>% 
  dplyr::left_join(by = c("closest_site" = "SSBS"), 
                   pressure_data[,c("SSBS","log_hpd_1km","T30","log_T30","nat_hab2","nat_hab_sw","road_density","log_roads")]) %>%
  dplyr::select(-closest_site)


##### next up is to load in the alpha diversity hypervolumes and calculate functional richness -- although 
#### this may not be the best way as it doesn't account for differences in relative abundance - possibly it
### would be good to also calculate a measure of rao's q using the hypervolumes too.

alpha_hypervolumes <- readRDS("outputs/alpha_diversity_site_tpds.rds")

PREDICTS_sites <- PREDICTS_sites %>% dplyr::filter(SSBS %in% names(alpha_hypervolumes))


get_richness <- function(site){
  
  data <- alpha_hypervolumes[[site]]
  cell_volume <- data$data$cell_volume

  FRich <- sum(data$TPDc$RelativeAbundance > 0) * cell_volume

  return(FRich)
      
}


PREDICTS_sites$functional_richness <-  apply(PREDICTS_sites, MARGIN = 1, FUN = function(x) get_richness(x[8]))

##### actually let's ignore rao for now that will have to be done on the HPC... and calculated with sites
#### within the same study

get_site_probabilities <- function(sites){
  
  evaluation_grid <- alpha_hypervolumes[[sites[1]]]$data$evaluation_grid
  
  
  for(site in sites){
    evaluation_grid[,site] <- alpha_hypervolumes[[site]]$TPDc$RelativeAbundance
    
  }
  
  return(evaluation_grid)
}




get_raos <- function(study){
  
  study_sites <- PREDICTS_sites %>% dplyr::filter(SS == study) %>% dplyr::distinct(SSBS) %>% pull() %>% as.character()
  
  site_probabilities <- get_site_probabilities(study_sites)
  
  
  probabilities <- site_probabilities[,c(4:ncol(site_probabilities))]
  
  
  evaluation_grid <- site_probabilities[rowSums(probabilities) > 0,c(1:3)]
  
  probabilities <- t(probabilities[rowSums(probabilities) > 0, ] )
  
  distance_matrix <- as.matrix(dist(evaluation_grid,method = "euclidean"))
  
  distance_times_p1 <- probabilities %*% distance_matrix
  d_times_p1_times_p2 <-rowSums(sweep(probabilities,1,distance_times_p1,"*",check.margin = FALSE))
  
  standardised_raos_q <- d_times_p1_times_p2/max(d_times_p1_times_p2)
  
  
  raos_q <- data.frame(SSBS = rownames(probabilities), raosQ = standardised_raos_q)
 
  return(raos_q)
   
}

study_raos <- c()
for(study in unique(PREDICTS_sites$SS)){

study_raos <- rbind(study_raos,get_raos(study))
}


PREDICTS_sites <- PREDICTS_sites %>% dplyr::left_join(study_raos)


write_rds(file = "outputs/alpha_diversity_dataframe.rds", x = PREDICTS_sites)
