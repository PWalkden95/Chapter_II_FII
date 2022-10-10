#### This is a script that will contain functions that are relevant to computing TPDs and alter the trait ranges,
#### test whether some species were able to be computed, and others 


trait_range_calculation <- function(range = 0.15, traits){
  
  n_range_2 <- dist(c(min(traits[,2]),max(traits[,2])))[1]
  n_range_3 <- dist(c(min(traits[,3]),max(traits[,3])))[1]
  n_range_4 <- dist(c(min(traits[,4]),max(traits[,4])))[1]
  
  trait_ranges <- list(c(min(traits[,2]) -(range * n_range_2),max(traits[,2]) + (range * n_range_2)),
                       c(min(traits[,3]) -(range * n_range_3),max(traits[,3]) + (range * n_range_3)),
                       c(min(traits[,4]) -(range * n_range_4),max(traits[,4]) + (range * n_range_4)))
  return(trait_ranges)
}


###################### Function to find out which species a TPD couldn't be calculated for with the range and division combination given

TPD_check <- function(TPD){
  
  Trait_density <- TPD
  
  fucked_species <- c()
  for(i in 1:length(Trait_density$TPDs)){
    if(any(is.nan(Trait_density$TPDs[[i]]))){
      fucked_species <- c(fucked_species,names(Trait_density$TPDs)[[i]]) 
    }
  }
  return(fucked_species)
}


##################################
##################################

### need study species trait ranges 


get_species_trait_ranges <- function(species, traits, range = 0.15){
  
  full_species <- unique(traits$complete$Birdlife_Name)
  partial_species <- unique(traits$partial$Birdlife_Name)
  single_species <- unique(traits$single$Birdlife_Name)
  
  species_traits <- c()
  
  for(sp in species){
    
    if(sp %in% full_species){
      species_traits <- rbind(species_traits,data.frame(Birdlife_Name = sp, traits$complete[traits$complete$Birdlife_Name == sp,c(2:4)]))
    }
    
    if(sp %in% partial_species){
      species_traits <- rbind(species_traits,data.frame(Birdlife_Name = sp, foraging = traits$partial[traits$partial$Birdlife_Name == sp,2],
                                                        locomotory = traits$partial[traits$partial$Birdlife_Name == sp,3],
                                                        body_size = traits$partial[traits$partial$Birdlife_Name == sp,4]))
    }
    
    if(sp %in% single_species){
      species_traits <- rbind(species_traits,data.frame(Birdlife_Name = sp, foraging = traits$single[traits$single$Birdlife_Name == sp,2],
                                                        locomotory = traits$single[traits$single$Birdlife_Name == sp,3],
                                                        body_size = traits$single[traits$single$Birdlife_Name == sp,4]))
    }
    
  }
  
  study_species_trait_ranges <- trait_range_calculation(range = range, traits = species_traits)
  
  
  return(study_species_trait_ranges)
  
}


##################################
#################################  Species TPD function

species_TPD <- function(species, trait_ranges, traits){
  
  ## if sds and therefore using species information
  
  
  
  
  mtpd <- FALSE
  
  ### are there any species that require mean tpd calculation
  if(any(species %in% c(traits$partial_traits$Birdlife_Name,traits$single_traits$Birdlife_Name))){
    
    # YES
    mtpd <- TRUE
    
    ### what species are they
    Mean_sp <- species[species %in% c(traits$partial_traits$Birdlife_Name,traits$single_traits$Birdlife_Name)]
    
    
    ## combine all partial traits into the same df representing means and bandwidths to use filter for the species in the community and convert to df
    
    mean_TPD_dat <- rbind(traits$partial_traits,traits$single_traits) %>% dplyr::filter(Birdlife_Name %in% Mean_sp) %>% data.frame()
    
    
    ### then calculate mean TPDs
    
    mean_TPD <- TPDsMean(species = mean_TPD_dat[,1], means = mean_TPD_dat[,c(2:4)], sds = mean_TPD_dat[,c(5:7)], trait_ranges = trait_ranges)
    
    check <- TPD_check(mean_TPD)
    
    
    if(!is_null(check)){
      
      print("print CHECK ISSUE -- MEAN")
      
      stop()
    }
    
  }
  
  ## for those species in the community that have sufficient traits extract their traits
  
  comm_traits <- traits[["complete_traits"]] %>% dplyr::filter(Birdlife_Name %in% species)
  
  ## claculate TPD
  
  trait_density <- TPDs(species = comm_traits[,1], traits = comm_traits[,c(2:4)], trait_ranges = trait_ranges)
  
  
  check <- TPD_check(trait_density)
  
  if(!is_null(check)){
    print("print CHECK ISSUE -- COMPLETE")
  }
  
  ## if there were TPDs calculated using the mean and sd join up the data so that both are represented in the same list
  
  if(mtpd){
    trait_density$data$species <- c(trait_density$data$species,mean_TPD$data$species)
    trait_density$TPDs <- c(trait_density$TPDs,mean_TPD$TPDs)
    trait_density$data$traits <- rbind(trait_density$data$traits, mean_TPD$data$means)
  }
  
  
  
  
  
  
  return(trait_density)
}

##########################################
########################################

### getting the beta-diversity sites community data



get_beta_diversity <- function(site_one,site_two,study, predicts, traits){
  
  data <- predicts %>% dplyr::filter(SS == study) %>% dplyr::select(Birdlife_Name,RelativeAbundance,SSBS)
  
  study_species <- unique(data$Birdlife_Name)
  
  beta_trait_ranges <- get_species_trait_ranges(species = study_species,traits = traits,range = 0.05)
  
  
  comm_data <- data.frame(Birdlife_Name = study_species) %>% 
    dplyr::left_join(data[data$SSBS == site_one, c("Birdlife_Name","RelativeAbundance")]) %>% 
    dplyr::rename(site_one = RelativeAbundance) %>%
    dplyr::left_join(data[data$SSBS == site_two, c("Birdlife_Name","RelativeAbundance")]) %>%
    dplyr::rename(site_two = RelativeAbundance)
  
  comm_data$site_one <- ifelse(is.na(comm_data$site_one), 0, comm_data$site_one)
  comm_data$site_two <- ifelse(is.na(comm_data$site_two), 0, comm_data$site_two)
    
  rownames(comm_data) <- comm_data$Birdlife_Name
  comm_data <- comm_data %>% dplyr::select(-Birdlife_Name)

  
  beta_species <- data %>% dplyr::filter(SSBS %in% c(site_one,site_two)) %>% dplyr::distinct(Birdlife_Name) %>% pull()
  
  beta_species_tpd <- species_TPD(species = beta_species,trait_ranges = beta_trait_ranges, traits = traits)
  
  
  community_tpds <- TPDc(beta_species_tpd, t(comm_data[beta_species,]))
  
  overlap <- TPD::dissim(community_tpds)
  
  beta_frame <- data.frame(dissimilarity = overlap$communities$dissimilarity[2],
                           beta_shared = overlap$communities$P_shared[2],
                           beta_not_shared = overlap$communities$P_non_shared[2])
  
  
  return(beta_frame)
  
  }


