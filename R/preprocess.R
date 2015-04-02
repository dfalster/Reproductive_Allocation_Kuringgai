filterBySpecies <- function(thisSpecies, data){
  filter(data, species == thisSpecies)
}

filterForAllocation <- function(data, IndividualsList){
 # TODO: change column name in reproduction ro be consistent: individual -> tagID
 keep <- IndividualsList$individual[IndividualsList$use_for_allocation_calculations]
 data %>% filter(individual %in% keep)
 }

preprocessHarvest <- function(HarvestData_raw, IndividualsList) {

	keep <- filter(IndividualsList, use_for_allometric_equations)$individual
  filter(HarvestData_raw, individual %in% keep)
}
