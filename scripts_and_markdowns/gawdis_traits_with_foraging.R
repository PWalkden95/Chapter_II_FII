### So let's start to go through he functional intcatness index code and restart the process of getting one together
## it's been a little while and a whole chpater has gone in between, which has shifted some of my thinking
## on the index. In particular, how we calculate the alpha diveristy metric that goes into the final measure
## I now do not think that rao's Q would be the best measure and that we could simply get a measure of 
## functional richness from TPDs. This avoid some of the previous pitfalls of fucntional richness, as it 
## doesn't rely on the computation of a convex hull and just sums the area of occupancy of occupied cells, and 
## therefore not sensitive to outliers. It will be sensitive to teh volume of each cell, but there's not much 
## we can do about that and it will certainly be less severe that convex approaches.

## So without further ado, we already have the community and trait data form chapter one in a state that we can quickly
## use. Therefore, first steps probably should be to calculate the TPDs from alpha diversity measures.
## the difference is going to be in the trait ranges used to project functional trait spaces into.

## Actuallyyy, let's have a think out the traits of course for the previous chapter I defined functional 
## trait space by three "meaningful" axes of variation for the foraging, locomotry and body size niches,
## however, we may want to integrate forgaing traits into the measures of fucntional intactness, but how to do
### ..... 

## one way would be to do something like gawdis and integrate the foraging traits that then subsequently
## performing a PcoA and get the first three axes from that?? 

## have morpho axes and one forgaing based on PCAs or morpho and foraging traits.. don't know how feasible that is 

## additionally something else that should probably be noted is that to use the hypervolume technique at the community level
## we are likely going to limited to a max of three dimensions as beyond that there will be few cells occupied 
## to get any meaningful measures from it 

## okay then I'll stop going on and have a look at gawdis


rm(list = ls())




require(gawdis)
require(tidyverse)
require(magrittr)
require(ecodist)



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



AVONET_full <- read.csv("../../Datasets/GBD/GBD_BiometricsRaw_combined_11_Aug_2021_MASTER.csv")

table(PREDICTS$Predominant_habitat)


## Right then these are the communities that we will be using to generate our preliminary FII
## so let's bring out the species present within and the relevant traits

colnames(PREDICTS)

PREDICTS_species <-
  PREDICTS %>% dplyr::select(
    Birdlife_Name,
    Beak.Length_Culmen,
    Beak.Length_Nares,
    Beak.Width,
    Beak.Depth,
    Tarsus.Length,
    Wing.Length,
    Kipps.Distance,
    Secondary1,
    Hand.Wing.Index,
    Tail.Length,
    Invertivore,
    Aquatic.predator,
    Vertivore,
    Scavenger,
    Nectarivore,
    Frugivore,
    Granivore,
    Herbivore_A,
    Herbivore_T
  ) %>% dplyr::distinct()


## okay then the input into gawdis will be the three meaningful axes derive from a two-step PCA and
## the foraging traits as a grouped trait i.e all relate to the same thing (diet)

source("functions/two_step_pca.R")

foraging_traits = c("Beak.Length_Culmen", "Beak.Length_Nares" , "Beak.Width" ,"Beak.Depth")
locomotory_traits = c("Tarsus.Length", "Wing.Length","Secondary1","Tail.Length" )

species_specimens <- AVONET_full %>% dplyr::filter(Species1_BirdLife %in% PREDICTS_species$Birdlife_Name)

species_specimens <- species_specimens[,c("Species1_BirdLife",foraging_traits,locomotory_traits)]



two_step_dataframe <- two_step_PCA(dataframe = species_specimens, means = FALSE,
                                   foraging_traits = foraging_traits,
                                   locomotory_traits = locomotory_traits)


#### now that we have the three axes of meaningful variation for birds we will need to add the columns
## related to the dietary proportions

dietary_niches <- c("Invertivore","Granivore","Frugivore","Aquatic.predator","Vertivore","Scavenger",
                    "Herbivore_T", "Herbivore_A")



gawdis_dataframe <- two_step_dataframe %>% dplyr::left_join(PREDICTS_species[,c("Birdlife_Name",dietary_niches)]) %>%
  dplyr::select(-Birdlife_Name)

### gawdis to create a distance matrix that accounts for the differential contributions of traits
## to the overall distance matrix

source("functions/new_gawdis.R")

gaw_distance_matrix <-
  new_gawdis(
    x = gawdis_dataframe[1:10000,],
    groups = c( 1, 2, 3, rep(4, 8)),
    fuzzy = c(4),
    w.type = "analytic"
  )

coordinate_analysis <- ecodist::pco(x = gaw_distance_matrix)


write_rds(file = "outputs/specimen_trait_pcoa_including_foraging.rds", x = coordinate_analysis)

