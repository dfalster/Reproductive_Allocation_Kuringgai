IAT_Species <- function(species, Reproduction, HarvestData, InvestmentCategories, species_INV) {
  # Read and restrict the data to the subset of interest.

  # TODO :get ages from individual list, not from harvest data
  AgeData <- unique(
    filter(HarvestData, segment == 1, plant_status == "alive", individual %in% Reproduction$individual) %.%
    select(age, individual)
      )

  InvDist <- data.frame(individual = c(), PrePol_A = c(), PrePol_S = c(), PostPol_A = c(), PD = c(), Prop = c(), Total = c(), age = c())

  for (individual in unique(Reproduction$individual)) {
    # print(individual)
    Ind <- InvestmentInAccessoryTissues(individual, species, InvestmentCategories[, c("flower_part", species)], species_INV)
    if (length(AgeData[AgeData[, 2] == individual, 1]) == 1) {
      Ind <- cbind(Ind, age = AgeData[AgeData[, 2] == individual, 1])
      InvDist <- rbind(InvDist, Ind)
    }
  }
  InvDist
}


InvestmentInAccessoryTissues <- function(individual, species, InvestmentCategories, species_INV) {
  # Read tree data

  # Transform counts to weights and adjust for multiplicity TreeListOrig=WeightCalculationsForTree(Tree) TreeListAdj=AdjustForMultiplicity(TreeList=TreeListOrig)
  # TreeFrame=TreeListToTotalDataFrame(TreeListAdj)

  # Read in the information about lost elements
  SpeciesLoss <- species_INV$FD
  Lost <- SpeciesLoss[SpeciesLoss$Individual == individual, ]

  # Find the list of investment categories partition for the species
  IC <- InvestmentCategories[, c("flower_part", species)]
  IC <- IC[!is.na(IC[, 2]), ]

  ############### * Prepolination_abort *
  PreP_A_parts <- IC[IC[, 2] == "prepollination_abort", 1]
  # check if any of the parts can be found in the data for this individual
  PreP_A_parts <- PreP_A_parts[PreP_A_parts %in% Lost$what]
  PrepolinationAbortWeight <- 0
  if (length(PreP_A_parts) > 0) {
    PrepolinationAbortWeight <- sum(as.numeric(Lost[(Lost$what %in% PreP_A_parts), ]$weight))
  }
  ############### * Prepolination_success *
  PreP_S_parts <- IC[IC[, 2] == "prepollination_success", 1]
  # check if any of the parts can be found in the data for this individual
  PreP_S_parts <- PreP_S_parts[PreP_S_parts %in% Lost$what]
  PrepolinationSuccessWeight <- 0
  if (length(PreP_A_parts) > 0) {
    PrepolinationSuccessWeight <- sum(as.numeric(Lost[(Lost$what %in% PreP_S_parts), ]$weight))
  }
  ############### * Postpollination_abort *
  PostP_A_parts <- IC[IC[, 2] == "postpollination_abort", 1]
  # check if any of the parts can be found in the data for this individual
  PostP_A_parts <- PostP_A_parts[PostP_A_parts %in% Lost$what]
  PostpollinationAbortWeight <- 0
  if (length(PostP_A_parts) > 0) {
    PostpollinationAbortWeight <- sum(as.numeric(Lost[(Lost$what %in% PostP_A_parts), ]$weight))
  }

  ############### * Packaging Dispersal *
  PD_parts <- IC[IC[, 2] == "packaging_dispersal", 1]
  # check if any of the parts can be found in the data for this individual
  PD_parts <- PD_parts[PD_parts %in% Lost$what]
  PackaginDispersalWeight <- 0
  if (length(PD_parts) > 0) {
    PackaginDispersalWeight <- sum(as.numeric(Lost[(Lost$what %in% PD_parts), ]$weight))
  }
  ############### * Propagule *
  PROP_parts <- IC[IC[, 2] == "propagule", 1]
  # check if any of the parts can be found in the data for this individual
  PROP_parts <- PROP_parts[PROP_parts %in% Lost$what]
  PropaguleWeight <- 0
  if (length(PROP_parts) > 0) {
    PropaguleWeight <- sum(as.numeric(Lost[(Lost$what %in% PROP_parts), ]$weight))
  }
  TotWeight <- PrepolinationAbortWeight + PrepolinationSuccessWeight + PostpollinationAbortWeight + PackaginDispersalWeight + PropaguleWeight
  data.frame(individual = individual, PrePol_A = PrepolinationAbortWeight, PrePol_S = PrepolinationSuccessWeight, PostPol_A = PostpollinationAbortWeight, PD = PackaginDispersalWeight,
    Prop = PropaguleWeight, Total = TotWeight)
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
