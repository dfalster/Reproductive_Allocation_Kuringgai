AccessoryCosts <- function(species, IndividualsList, InvestmentCategories, species_INV) {
  # Read and restrict the data to the subset of interest.

  AgeData <- IndividualsList %>%
    filter(use_for_allocation_calculations & alive) %>%
    select(species,individual, age)

  InvDist <- lapply(unique(AgeData$individual), InvestmentInAccessoryTissues,
                species=species, InvestmentCategories=InvestmentCategories[, c("flower_part", species)],
                species_INV= species_INV) %>% rbind_all

  # TODO: This is temp fix to remove some NAs in prepollen_success_inv. 
  # Figure out why these are needed
  InvDist$prepollen_success_inv[which(is.na(InvDist$prepollen_success_inv))] <- 0

# TODO: Lizzy - is this next line needed? causes errors
#  InvDist$Total= (InvDist$prepollen_aborted_inv + InvDist$prepollen_success_inv + InvDist$postpollen_aborted_inv + InvDist$PD + InvDist$Prop)

  accessory <- merge(AgeData, InvDist, by="individual")

  #calculating percentages of various accessory tissues
  accessory <- accessory %>%
    mutate(accessory_inv = total_repro_inv - propagule_inv,
        prepollen_all_inv = prepollen_aborted_inv + prepollen_success_inv,
        prop_prepollen_aborted = prepollen_aborted_inv / total_repro_inv,
        prop_prepollen_success = prepollen_success_inv / total_repro_inv,
        prop_postpollen_aborted = postpollen_aborted_inv / total_repro_inv,
        prop_packaging_dispersal = packaging_dispersal_inv / total_repro_inv,
        prop_propagule = propagule_inv / total_repro_inv,
        prop_accessory = 1 - prop_propagule,
        prop_prepollen_all = prop_prepollen_aborted + prop_prepollen_success
        )

  for(v in c("prop_prepollen_aborted", "prop_prepollen_success", "prop_postpollen_aborted",
        "prop_packaging_dispersal", "prop_propagule", "prop_accessory")) {
      accessory[[v]][which(is.nan(accessory[[v]]))] <- NA
  }
  accessory
}


InvestmentInAccessoryTissues <- function(individual, species, InvestmentCategories, species_INV) {
  # Read tree data

  # Transform counts to weights and adjust for multiplicity TreeListOrig=WeightCalculationsForTree(Tree) TreeListAdj=AdjustForMultiplicity(TreeList=TreeListOrig)
  # TreeFrame=TreeListToTotalDataFrame(TreeListAdj)

  # Read in the information about lost elements
  SpeciesLoss <- species_INV$FD
  Lost <- SpeciesLoss[SpeciesLoss$individual == individual, ]

  # Find the list of investment categories partition for the species
  IC <- InvestmentCategories[, c("flower_part", species)]
  IC <- IC[!is.na(IC[, 2]), ]

  ############### * Prepolination_abort *
  PreP_A_parts <- IC[IC[, 2] == "prepollination_abort", 1]
  # check if any of the parts can be found in the data for this individual
  PreP_A_parts <- PreP_A_parts[PreP_A_parts %in% Lost$part]
  prepollen_aborted_inv <- 0
  if (length(PreP_A_parts) > 0) {
    prepollen_aborted_inv <- sum(as.numeric(Lost[(Lost$part %in% PreP_A_parts), ]$weight))
  }
  ############### * Prepolination_success *
  PreP_S_parts <- IC[IC[, 2] == "prepollination_success", 1]
  # check if any of the parts can be found in the data for this individual
  PreP_S_parts <- PreP_S_parts[PreP_S_parts %in% Lost$part]
  prepollen_success_inv <- 0
  if (length(PreP_A_parts) > 0) {
    prepollen_success_inv <- sum(as.numeric(Lost[(Lost$part %in% PreP_S_parts), ]$weight))
  }
  ############### * Postpollination_abort *
  PostP_A_parts <- IC[IC[, 2] == "postpollination_abort", 1]
  # check if any of the parts can be found in the data for this individual
  PostP_A_parts <- PostP_A_parts[PostP_A_parts %in% Lost$part]
  postpollen_aborted_inv <- 0
  if (length(PostP_A_parts) > 0) {
    postpollen_aborted_inv <- sum(as.numeric(Lost[(Lost$part %in% PostP_A_parts), ]$weight))
  }

  ############### * Packaging Dispersal *
  PD_parts <- IC[IC[, 2] == "packaging_dispersal", 1]
  # check if any of the parts can be found in the data for this individual
  PD_parts <- PD_parts[PD_parts %in% Lost$part]
  packaging_dispersal_inv <- 0
  if (length(PD_parts) > 0) {
    packaging_dispersal_inv <- sum(as.numeric(Lost[(Lost$part %in% PD_parts), ]$weight))
  }
  ############### * Propagule *
  PROP_parts <- IC[IC[, 2] == "propagule", 1]
  # check if any of the parts can be found in the data for this individual
  PROP_parts <- PROP_parts[PROP_parts %in% Lost$part]
  propagule_inv <- 0
  if (length(PROP_parts) > 0) {
    propagule_inv <- sum(as.numeric(Lost[(Lost$part %in% PROP_parts), ]$weight))
  }
  total_repro_inv <- prepollen_aborted_inv + prepollen_success_inv + postpollen_aborted_inv + packaging_dispersal_inv + propagule_inv
  data.frame(individual = individual, 
        prepollen_aborted_inv = prepollen_aborted_inv, 
        prepollen_success_inv = prepollen_success_inv, 
        postpollen_aborted_inv = postpollen_aborted_inv, 
        packaging_dispersal_inv = packaging_dispersal_inv,
        propagule_inv = propagule_inv, 
        total_repro_inv = total_repro_inv, stringsAsFactors=FALSE)

}

TreeListToTotalDataFrame <- function(TreeList) {
  individual <- TreeList[[1]]
  TreeTotalDataFrame <- data.frame(individual = c(), census = c(), part = c(), count = c(), weight = c())
  for (i in 2:(length(TreeList))) {
    CensusTotal <- TreeList[[i]]$total
    n.types <- length(CensusTotal)
    if (n.types > 0) {
      for (j in seq_len(n.types)) {
        census <- i - 1
        type <- CensusTotal[[j]]$type
        count <- CensusTotal[[j]]$count
        weight <- sum(CensusTotal[[j]]$weight)
        TreeTotalDataFrame <- rbind(TreeTotalDataFrame, data.frame(individual = NA, census = census, part = type, count = count, weight = weight))
      }
    }
  }
  TreeTotalDataFrame$individual <- individual
  TreeTotalDataFrame
}
