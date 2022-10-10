### This follows on from the computing a gawdis distace matrix that incorporates the three meaning axes of variation
### derived from the two step PCA - however the calculation at the specimen level is unbelieveably computationally
### expensive so takes three days in the HPC (if it even does finish).

### So that may be pu on the back burner for now and it it some through those traits axes can be substitued in...
### perphaps some sort of sensitivity analysis. 

## Now let's get on with generating those hypervolumes tha are going to be used to derive the alpha and beta-diversity
## measures. 

rm(list = ls())


require(tidyverse)
require(magrittr)
require(TPD)


## to do this we are going to need the full traits of all the observed species, not including the null
## community species too. Then we are also going to be the foraging scores for the 


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



## Load in the raw specimen data

AVONET_full <- read.csv("../../Datasets/GBD/GBD_BiometricsRaw_combined_11_Aug_2021_MASTER.csv")


### load in two-step PCA function and perform on raw data

source("functions/two_step_pca.R")


PREDICTS_speices <- PREDICTS %>% dplyr::distinct(Birdlife_Name) %>% pull()

foraging_traits = c("Beak.Length_Culmen", "Beak.Length_Nares" , "Beak.Width" ,"Beak.Depth")
locomotory_traits = c("Tarsus.Length", "Wing.Length","Secondary1","Tail.Length" )

species_specimens <- AVONET_full %>% dplyr::filter(Species1_BirdLife %in% PREDICTS_speices)

species_specimens <- species_specimens[,c("Species1_BirdLife",foraging_traits,locomotory_traits)]


## perform the two-step pca - some specimens will be dropped if after some imputation there are still incomplete cases


two_step_dataframe <- two_step_PCA(dataframe = species_specimens, means = FALSE,
                                   foraging_traits = foraging_traits,
                                   locomotory_traits = locomotory_traits)

## great so thats the three "meaning axes" calculated for each specimen, the nets step concerns how this specimen
## data is used to compute trait probability density functions (TPDs). 

## If there are four or more specimens then species TPDs can be calculated with a kernel desnity functions - standard
## for TPD.

## fewer than four specimens then density functions are calculated as multivariate normal distributions

## greater than a single specimen the standard deviation can be derived from specimen values, single specimens,
## sd is calculated through a plug-in bandwidth estimator from the ks package. So let's get on with it.

n_specimens <- two_step_dataframe %>% dplyr::group_by(Birdlife_Name) %>% dplyr::mutate(n_specimen = n())


full_specimens <- n_specimens %>% dplyr::filter(n_specimen >= 4) 

## so we have 3067 species that have sufficient specimens to create full TPDs

full_species <- full_specimens %>% dplyr::distinct(Birdlife_Name) %>% pull()


### how many with fewer that 4 but greater than one specimen

fewer_specimens <- n_specimens %>% dplyr::filter(n_specimen < 4 & n_specimen > 1)


### 29 species with fewer

fewer_species <- fewer_specimens %>% dplyr::distinct(Birdlife_Name) %>% pull()


### now only those with a single specimen

single_specimen <- n_specimens %>% dplyr::filter(n_specimen == 1)

## nine species with a single specimen

single_species <- single_specimen %>% dplyr::distinct(Birdlife_Name) %>% pull()

## Okay great however, you may notice that this does not add up to the number of species within PREDICTS sp
## but pigot et al 2020 had some imputed trait data that had substituted morphologically similar species and these
## have only the mean trait values

impute_mean_traits <- readRDS("../PREDICTS_Taxonomy/PREDICTS_imputed_BL_traits.rds")


no_specimen_species <- PREDICTS_speices[!(PREDICTS_speices %in% c(full_species,fewer_species,single_species))]


no_specimen_traits <- impute_mean_traits[impute_mean_traits$uniqueIDS %in% no_specimen_species,]


### okay will need to add this into the species specimen data frame and perform the two step again.

colnames(no_specimen_traits)[1] <- "Species1_BirdLife"
no_specimen_traits <- no_specimen_traits[,colnames(species_specimens)]


### if there are multiple specimens for some species but they all are missing values for a single trait we can
### possibly impute some more values for the multiple specimens


species_specimens <- species_specimens %>% dplyr::filter(!(Species1_BirdLife %in% no_specimen_species)) %>%
  rbind(no_specimen_traits)




two_step_dataframe <- two_step_PCA(species_specimens,foraging_traits = foraging_traits,locomotory_traits = locomotory_traits,
                                    means = FALSE)


### are all species now considered?

all(PREDICTS_speices %in% two_step_dataframe$Birdlife_Name)

## great, so those mean species and specimens can be added onto the single species list

single_species <- c(single_species,no_specimen_species)


##### so we now know where all the species lie. let's see how they form TPDs

source("functions/TPD_computation_functions.R")

trait_ranges <- trait_range_calculation(traits = full_specimens, range = 0.025)


full_tpds <-
  TPDs(
    species = full_specimens$Birdlife_Name,
    traits = full_specimens[, c(2:4)],
    trait_ranges = trait_ranges, alpha = 0.99
  )


check_species <- TPD_check(full_tpds)



### only two species where a TPD couldn't be calculated these are added onto those species that calculate 
### mean TPD using specimens

fewer_species <- c(fewer_species,check_species)
fewer_specimens <- rbind(fewer_specimens,full_specimens[full_specimens$Birdlife_Name %in% check_species,])


full_specimens <- full_specimens %>% dplyr::filter(!(Birdlife_Name %in% check_species))


## remove the tpd because it large and memory consuming

rm(full_tpds)



### okay now let's check the tpd TPD calculations

fewer_specimens <- fewer_specimens %>% dplyr::group_by(Birdlife_Name) %>% dplyr::summarise(meanfor = mean(foraging),
                                                                                           meanloco = mean(locomotory),
                                                                                           meanbody = mean(body_size),
                                                                                           sdfor = sd(foraging),
                                                                                           sdloco = sd(locomotory),
                                                                                           sdbody = sd(body_size))


fewer_tpds <- TPDsMean(species = fewer_specimens$Birdlife_Name, means = fewer_specimens[,c(2:4)],
                        sds = fewer_specimens[,c(5:7)], alpha = 0.99, trait_ranges = trait_ranges)


mean_check_species <- TPD_check(fewer_tpds)


fewer_specimens <- fewer_specimens %>% dplyr::filter(!(Birdlife_Name %in% mean_check_species))

## one mean species doesn't make it and gets passed onto the single specimen list 

single_species <- unique(c(single_species, mean_check_species,no_specimen_species))


## the bandwidth estimator as done by Carmona et al, is done as such


single_bandwidth <- sqrt(diag(Hpi.diag(two_step_dataframe[,c(2:4)])))

single_traits <- two_step_dataframe %>% dplyr::filter(Birdlife_Name %in% single_species) %>% dplyr::group_by(Birdlife_Name) %>%
  dplyr::summarise(meanfor = mean(foraging),
                   meanloco = mean(locomotory),
                   meanbody = mean(body_size)) %>% dplyr::ungroup() %>%
  dplyr::mutate(sdfor = single_bandwidth[1],
                sdloco = single_bandwidth[2],
                sdbody = single_bandwidth[3])



single_tpds <- TPDsMean(species = single_traits$Birdlife_Name, means = single_traits[,c(2:4)], sds = single_traits[,c(5:7)],
                        trait_ranges = trait_ranges, alpha = 0.99)


## great it's got nothing in it!

final_tpd_check <- TPD_check(single_tpds)


## so now lets combine the dataframes together with the relevant trait data etc

FII_trait_list <- list()

FII_trait_list$complete_traits <- data.frame(full_specimens)
FII_trait_list$partial_traits <- data.frame(fewer_specimens)
FII_trait_list$single_traits <- data.frame(single_traits)
FII_trait_list$full_specimen_dataframe <- two_step_dataframe



#### are all species considered here 

full_sp <- unique(FII_trait_list$complete$Birdlife_Name)
partial_sp <- unique(FII_trait_list$partial$Birdlife_Name)
single_sp <- unique(FII_trait_list$single$Birdlife_Name)

all_species <- c(full_sp,partial_sp,single_sp)

## great

all(all_species %in% PREDICTS_speices)


## all species just appear once?

any(full_sp %in% partial_sp)
any(full_sp %in% single_sp)
any(partial_sp %in% single_sp)


### great, now save




write_rds("outputs/traits_without_foraging_list.rds", x = FII_trait_list)
