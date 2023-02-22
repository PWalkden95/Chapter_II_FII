#### Computing the alpha diversity hypervolumes -- what should the trait ranges be? Since we are doing a mixed effect 
#### model then I think that trait ranges should encompass all species observed within a study so that the precision
#### of the functional richness calculation is going to be consistent throughout the study.

rm(list = ls())

require(tidyverse)
require(TPD)
require(doParallel)
require(gtools)
require(future)
require(future.apply)
require(magrittr)


source("functions/TPD_computation_functions.R")

## data required initially~ 

# PREDICTS data for the community composition


drop_species <- readRDS("data/assembly_drop_spp.rds")



PREDICTS <-
  readRDS("data/refined_predicts.rds")  %>%

  dplyr::filter(Predominant_habitat != "Cannot decide"
                & Use_intensity != "Cannot decide") %>%
  dplyr::mutate(Predominant_habitat = ifelse(
    grepl(
      Predominant_habitat,
      pattern = "secondary",
      ignore.case = TRUE
    ),
    "Secondary vegetation",
    paste(Predominant_habitat)
  )) %>% dplyr::filter(!(Birdlife_Name %in% drop_species)) %>% dplyr::mutate(LUI = paste(Predominant_habitat, Use_intensity, sep = "_"))

#%>% dplyr::mutate(LUI = paste(Predominant_habitat, Use_intensity, sep = "_"))



# Derived traits from the two-step PCA analysis


trait_list <- readRDS("outputs/traits_without_foraging_list.rds")



## PREDICTS also needs some extra formatting before computation of the TPDs

## filter out any species that need to be dropped 

TPD_data <- data.frame(PREDICTS) %>%
  
  ## group by site and birdlife name a occasionally the sme species pops up in the same site twice.
  
  dplyr::group_by(SSBS,Birdlife_Name) %>% dplyr::mutate(SpeciesSiteAbundance = sum(Effort_Corrected_Measurement), n_spp = n()) %>%
  
  ## filter that that species isn't duplicated and then ungroup 
  
  filter(!duplicated(n_spp)| n_spp == 1) %>% ungroup() %>%
  
  #### group by site and calculate metrics of how many species are in each site and the total site abundance
  
  group_by(SSBS) %>% dplyr::mutate(Site_spp = n_distinct(Birdlife_Name),TotalSiteAbundance = sum(SpeciesSiteAbundance)) %>%
  
  ungroup() %>% filter(Site_spp > 1) %>%
  
  #### calculate Relative abundance by dividing speices site abundance by total site abundance
  
  dplyr::mutate(RelativeAbundance = SpeciesSiteAbundance/TotalSiteAbundance) %>%
  
  ## droplevels
  droplevels() %>%
  
  #as data frame
  data.frame()


######

########################################################################################################
########################################################################################################
######## SPECIES TPD FUNCTION -- This function calculates and combines TPDs for all species given to it.

## I've added a couple of different methods so selecting bandwidths - whether every species is going to take the plug in estimator (bandwidth) or are going to use information from the specimens to  determine a bandwidth or use kde (sds)

studies <- unique(PREDICTS$SS)

TPD_list <- list()



registerDoParallel(cores = 32)


alpha_TPD_list <- foreach(study = studies,
                            .combine = "c",
                            .inorder = FALSE,
                            .packages = c("tidyverse","TPD")) %dopar%{

  
  species <- TPD_data %>% dplyr::filter(SS == study) %>% dplyr::distinct(Birdlife_Name) %>% pull()

  trait_ranges <- get_species_trait_ranges(species = species, traits = trait_list, range = 0.025)
  
  sites <- TPD_data %>% dplyr::filter(SS == study) %>% dplyr::distinct(SSBS) %>% pull() %>% as.character()
  
  study_site_list <- list()
  
  for(site in sites){
    
    site_species <- TPD_data %>% dplyr::filter(SSBS == site) %>% dplyr::distinct(Birdlife_Name) %>% pull()
  
    
  species_tpds <-   species_TPD(species = site_species,trait_ranges = trait_ranges, traits = trait_list)
    
      
  comm <- TPD_data %>% dplyr::filter(SSBS == site) %>% dplyr::select(Birdlife_Name, RelativeAbundance)
  rownames(comm) <- comm$Birdlife_Name
  comm <- comm %>% dplyr::select(-Birdlife_Name)
  
  community_trait_density <- TPD::TPDc(TPDs =  species_tpds,sampUnit = t(comm))
  
  
  community_trait_density$TPDc <- community_trait_density$TPDc$TPDc
  community_trait_density$data <- community_trait_density$data[c("cell_volume", "evaluation_grid")]
  
  study_site_list[[site]] <- community_trait_density
  
  }
  
  return(study_site_list)
  
  }


registerDoSEQ()
closeAllConnections()


write_rds("outputs/alpha_diversity_site_tpds.rds", x = alpha_TPD_list)

###############################
###############################

### This is onw done for alpha diversity but we also want to get beta-diversity or similarity measures from teh 
### comparison between pairs of sites within studies.and some other useful data such latitude and longitude
### don't know what else could be goood

### First thing then within each study we want to see the comparisons between each site


site_comparisons <- c()

for (study in studies) {
  for_min_study_sites <-
    TPD_data %>% dplyr::filter(SS == study, LUI == "Primary forest_Minimal use") %>% dplyr::distinct(SSBS) %>% pull() %>% as.character()
  
  for_non_min_study_sites <-
    TPD_data %>% dplyr::filter(
      SS == study,
      LUI %in% c(
        "Primary non-forest_Minimal use",
        "Primary non-forest_Light use"
      )
    ) %>% dplyr::distinct(SSBS) %>% pull() %>% as.character()
  
  all_study_sites <-
    TPD_data %>% dplyr::filter(SS == study) %>% dplyr::distinct(SSBS) %>% pull() %>% as.character()
  
  if (is_empty(for_min_study_sites) &
      is_empty(for_non_min_study_sites)) {
    next()
  }
  
  
  comparisons <- c()
  if (!is_empty(for_min_study_sites)) {
    sites_together <- c(for_min_study_sites, all_study_sites)
    
    for_min_comparisons <-
      t(apply(
        gtools::combinations(n = length(sites_together), r = 2),
        MARGIN = 1,
        FUN = function(x)
          matrix(c(sites_together[x[1]], sites_together[x[2]]))
      )) %>%
      data.frame() %>% set_colnames(c("site1", "site2")) %>% dplyr::filter(site1 != site2) %>% data.frame() %>% dplyr::filter(site1 %in% for_min_study_sites)
    
    
    comparisons <- rbind(comparisons, for_min_comparisons)
  }
  
  if (!is_empty(for_non_min_study_sites)) {
    sites_together <- c(for_non_min_study_sites, all_study_sites)
    
    for_non_min_comparisons <-
      t(apply(
        gtools::combinations(n = length(sites_together), r = 2),
        MARGIN = 1,
        FUN = function(x)
          matrix(c(sites_together[x[1]], sites_together[x[2]]))
      )) %>%
      data.frame() %>% set_colnames(c("site1", "site2")) %>% dplyr::filter(site1 != site2) %>% data.frame() %>% dplyr::filter(site1 %in% for_non_min_study_sites)
    
    
    comparisons <- rbind(comparisons, for_non_min_comparisons)
  }
  
  
  
  
  
  comparisons$land_use_combination <-
    apply(
      comparisons,
      MARGIN = 1 ,
      FUN =  function(x)
        paste(unique(TPD_data$LUI[TPD_data$SSBS == x[1]]), "-", unique(TPD_data$LUI[TPD_data$SSBS == x[2]]))
    )
  comparisons$site1Latitude <-
    apply(
      comparisons,
      MARGIN = 1,
      FUN = function(x)
        mean(unique(TPD_data$Latitude[TPD_data$SSBS == x[1]]))
    )
  comparisons$site1Longitude <-
    apply(
      comparisons,
      MARGIN = 1,
      FUN = function(x)
        mean(unique(TPD_data$Longitude[TPD_data$SSBS == x[1]]))
    )
  comparisons$site2Latitude <-
    apply(
      comparisons,
      MARGIN = 1,
      FUN = function(x)
        mean(unique(TPD_data$Latitude[TPD_data$SSBS == x[2]]))
    )
  comparisons$site2Longitude <-
    apply(
      comparisons,
      MARGIN = 1,
      FUN = function(x)
        mean(unique(TPD_data$Longitude[TPD_data$SSBS == x[2]]))
    )
  
  comparisons$SS <- study
  
  site_comparisons <- rbind(site_comparisons, comparisons)
  
}


site_comparisons <- cbind(site_comparisons,dissimilarity)

table(site_comparisons$land_use_combination)





plan(multicore(workers = 32))




site_comparisons[, c("dissimilarity", "beta_shared", "beta_non_shared")] <-
  t(future.apply::future_apply(
    site_comparisons,
    MARGIN = 1,
    FUN = function(x)
      unlist(
        get_beta_diversity(
          site_one = x[1],
          site_two = x[2],
          study = x[8],
          predicts = TPD_data,
          traits = trait_list
        )
      )
  ))


closeAllConnections()

write_rds(file = "outputs/beta_diversity_dataframe.rds", x = site_comparisons)


site_comparisons <- readRDS("outputs/beta_diversity_dataframe_tom.rds")

site_comparisons <- site_comparisons[,-c(12:25)]


reorder_combinations <- function(string){
  
  order <- c("Primary minimal",
             "Primary vegetation",
             "Secondary vegetation",
             "Plantation forest",
             "Pasture",
             "Cropland",
             "Urban"
  )
  
  
split_string <-   unlist(str_split(string, pattern = " - "))

ordered_LU  <- which(order %in% split_string) 

return(paste(order[ordered_LU[which.min(ordered_LU)]], order[ordered_LU[which.max(ordered_LU)]], sep = "-"))



}


site_comparisons$land_use_combination <- apply(site_comparisons, MARGIN = 1, FUN = function(x) reorder_combinations(x[3]))

site_comparisions <- site_comparisons[grepl(site_comparisons$land_use_combination, pattern = "Primary minimal"),]



write_rds(file = "outputs/beta_diversity_dataframe.rds", x = site_comparisons)

 ##### now I need to calculate the environmental and geographic distance between sites 


##### Geographic distance 

require(geosphere)



geographic_distance <- function(site1lat,site1long,site2lat,site2long){
  
 dist <-  distHaversine(c(site1long,site1lat),c(site2long,site2lat))

 return(dist)
   
}





for(i in 1:nrow(site_comparisons)){

 site_comparisons$site1Latitude[i] <-  as.numeric(mean(unlist(site_comparisons[i,4])))
 site_comparisons$site1Longitude[i] <-  as.numeric(mean(unlist(site_comparisons[i,5])))
 site_comparisons$site2Latitude[i] <-  as.numeric(mean(unlist(site_comparisons[i,6])))
 site_comparisons$site2Longitude[i] <-  as.numeric(mean(unlist(site_comparisons[i,7])))
    

 
}


site_comparisons$site1Latitude <- as.numeric(site_comparisons$site1Latitude)
site_comparisons$site2Latitude <- as.numeric(site_comparisons$site2Latitude)
site_comparisons$site1Longitude <- as.numeric(site_comparisons$site1Longitude)
site_comparisons$site2Longitude <- as.numeric(site_comparisons$site2Longitude)

plan(multicore(workers = parallel::detectCores() - 1))


site_comparisons$geographic_distance <-
  future_apply(
    site_comparisons,
    MARGIN = 1,
    FUN = function(x)
      geographic_distance(site1lat = as.numeric(x[4]),
                          site1long =  as.numeric(x[5]),
                          site2lat = as.numeric(x[6]),
                          site2long = as.numeric(x[7]))
      )
  


closeAllConnections()

#### environmental distance -- using what bioclimatic variables??

# 5 - Max temperature of the warmest month 

# 6 - Min temperature of the coldest month

# 13 - Precipitation of the wettest month

# 14 - precipitation of the driest month

# Elevation - self explanatory 

require(terra)


bio_5 <- terra::rast("../../Datasets/Environmental_Variables/wc2.1_30s_bio_5.tif")

bio_6 <- terra::rast("../../Datasets/Environmental_Variables/wc2.1_30s_bio_6.tif")

bio_13 <- terra::rast("../../Datasets/Environmental_Variables/wc2.1_30s_bio_13.tif")

bio_14 <- terra::rast("../../Datasets/Environmental_Variables/wc2.1_30s_bio_14.tif")

bio_elevation <- terra::rast("../../Datasets/Environmental_Variables/wc2.1_30s_elev.tif")

site1_coords <- as.matrix(site_comparisons[,c("site1Longitude","site1Latitude")], ncol = 2)
site2_coords <- as.matrix(site_comparisons[,c("site2Longitude","site2Latitude")], ncol = 2)

site1_environmental_variables <- data.frame(bv5 = terra::extract(bio_5,site1_coords),
                                            bv6 = terra::extract(bio_6,site1_coords),
                                            bv13 = terra::extract(bio_13,site1_coords),
                                            bv14 = terra::extract(bio_14,site1_coords),
                                            ele = terra::extract(bio_elevation,site1_coords)
                                            )


site2_environmental_variables <- data.frame(bv5 = terra::extract(bio_5,site2_coords),
                                            bv6 = terra::extract(bio_6,site2_coords),
                                            bv13 = terra::extract(bio_13,site2_coords),
                                            bv14 = terra::extract(bio_14,site2_coords),
                                            ele = terra::extract(bio_elevation,site2_coords)
)

site_comparisons$environmental_distance <-  gower::gower_dist(site1_environmental_variables,site2_environmental_variables)


site_comparisons$environmental_distance <- ifelse(is.nan(site_comparisons$environmental_distance), 0, site_comparisons$environmental_distance)

write_rds(file = "outputs/beta_diversity_dataframe_tom.rds", x = site_comparisons)

