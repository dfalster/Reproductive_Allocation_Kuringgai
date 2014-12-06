# Wraper for the calculations of investments in accessory tissues
source('analysis/IAT_Species.R')
IAT<-function(species)
{
  species <- read.csv("data/species_names.csv", stringsAsFactors=FALSE)
  for(sp in species$Abbreviation)
    IAT_Species(sp)
}