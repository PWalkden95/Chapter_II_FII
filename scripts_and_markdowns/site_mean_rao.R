### SCRIPT

### A different way of calculating functional alpha diversity as opposed to geting functional richness from
### hypervolumes i'll calculate rao's Q using traits and a distance matrix 


rm(list = ls())


require(tidyverse)
require(magrittr)
require(SYNCSA)


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





Rao_data <- data.frame(PREDICTS) %>% filter(!(Birdlife_Name %in% drop_species)) %>% 
  
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



# Derived traits from the two-step PCA analysis


trait_list <- readRDS("outputs/mean_traits.rds")
rownames(trait_list) <- trait_list[,1]
trait_list <- trait_list[,-1]


comm_matrix <-
  matrix(rep(0, nrow(trait_list) * length(
    unique(Rao_data$SSBS)
  )),ncol = nrow(trait_list), dimnames = list(unique(Rao_data$SSBS), rownames(trait_list)))


for(site in unique(Rao_data$SSBS)){
  
  data <- Rao_data %>% dplyr::filter(SSBS == site)
  
  if(any(is.na(data$RelativeAbundance))){
    print(site)
    stop()
  }
  
  comm_matrix[site,data$Birdlife_Name] <- data$RelativeAbundance

  }




gow_dis <- sqrt(as.matrix(FD::gowdis(trait_list)))

distance_times_p1 <- comm_matrix %*% gow_dis

d_times_p1_times_p2 <-
  rowSums(sweep(comm_matrix, 1, distance_times_p1, "*", check.margin = FALSE))



raoQ <- data.frame(SSBS = unique(Rao_data$SSBS), rao = d_times_p1_times_p2)


write_rds("outputs/site_mean_rao.rds", x =raoQ)




