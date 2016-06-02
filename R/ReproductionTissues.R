ReproductiveCosts <- function(species, IndividualsList, InvestmentCategories, species_Investment) {

  # Read and restrict the data to the subset of interest
  FD <- species_Investment$FD
  InvCat <- InvestmentCategories[[species]]

  AgeData <- IndividualsList %>%
    filter(use_for_allocation_calculations & alive) %>%
    select(species,individual, age)

  # For each individual, sum investment by categories
  # Each category includes a list of parts for that species
  # Individual lists are drawn from AgeData so that even individuals with no FD data are included and return 0
  # This function returns a dataframe with multiple columns
  f1 <- function(data) {
    # subset investment data by individual
    df <- FD[FD$individual==data$individual,]
    sapply(c("prepollen_aborted_inv", "prepollen_success_inv", "postpollen_aborted_inv",
    "packaging_dispersal_inv", "propagule_inv"), function(x) sum(df$weight[df$part %in% InvCat[[x]]]))
  }

  Costs <- ddply(AgeData, "individual",  f1)

  # Similar to f1, but returns a single column for seedcosts. The total weight is standardised against the counts of
  # a parts specified by the variable "seed_costs_scale"
  # Takes as argument the individual
  f2 <- function(i) {
    # subset investment data by individual
    df <- FD[FD$individual== i,]
    # return 0 if there are 0 seeds or mature fruits produced
    if(sum(df$count[df$part %in% c("seed", "fruit_mature")], na.rm=TRUE) ==0) {
      return(0)
    }
    weights <- df$weight[match(InvCat[["seed_costs"]], df$part)]
    counts <- df$count[match(InvCat[["seed_costs_scale"]], df$part)]
    sum(weights / counts,  na.rm = TRUE)
  }
  
  Costs$seed_costs = sapply(Costs$individual, f2)

  # Identical format to f2, but returns a single column for prepollencosts. The total weight is standardised against the counts of
  # a parts specified by the variable "prepollen_costs_scale"
  # Takes as argument the individual
  f3 <- function(i) {
    # subset investment data by individual
    df <- FD[FD$individual== i,]
    # return 0 if there are 0 flowers produced
    if(sum(df$count[df$part %in% c("flower_petals","flower_petals_small")], na.rm=TRUE) ==0) {
      return(0)
    }
    weights <- df$weight[match(InvCat[["prepollen_costs"]], df$part)]
    counts <- df$count[match(InvCat[["prepollen_costs_scale"]], df$part)]
    sum(weights / counts,  na.rm = TRUE)
  }
  
  Costs$prepollen_partial_costs = sapply(Costs$individual, f3)
  
  f4 <- function(i) {
    # subset investment data by individual
    df <- FD[FD$individual== i,]
    # return 0 if there are 0 seeds produced
    if(sum(df$count[df$part %in% c("seed", "fruit_mature")], na.rm=TRUE) ==0) {
      return(0)
    }
    weights <- df$weight[match(InvCat[["dispersal_costs"]], df$part)]
    counts <- df$count[match(InvCat[["dispersal_costs_scale"]], df$part)]
    sum(weights / counts,  na.rm = TRUE)
  }
  
  Costs$pack_disp_gross_costs = sapply(Costs$individual, f4)
  
  f5 <- function(i) {
    # subset investment data by individual
    df <- FD[FD$individual== i,]
    # return 0 if there are 0 flowers produced
    if(sum(df$count[df$part %in% c("flower_petals","fruit_mature","seed")], na.rm=TRUE) ==0) {
      return(0)
    }
    weights <- 1*df$weight[match(InvCat[["dispersal_before_pollen_costs"]], df$part)]
    counts <- df$count[match(InvCat[["dispersal_before_pollen_costs_scale"]], df$part)]
    if(length(counts >0))
      adjust <- InvCat[["dispersal_before_pollen_costs_adjust"]]
    else 
      adjust <- numeric(0)
    if(length(adjust)!= length(counts))  {
      stop(paste("problem with length of dispersal_before_pollen_costs_adjust for ", df$species[1]))
    }
    sum(adjust*weights / counts,  na.rm = TRUE)
  }
  
  Costs$pack_disp_early_costs = sapply(Costs$individual, f5)

  f6 <- function(i) {
    # subset investment data by individual
    df <- FD[FD$individual== i,]
    # return 0 if there are 0 flowers produced
    if(sum(df$count[df$part %in% c("flower_petals","fruit_mature","seed")], na.rm=TRUE) ==0) {
      return(0)
    }
    weights <- df$weight[match(InvCat[["seed_before_pollen_costs"]], df$part)]
    counts <- df$count[match(InvCat[["seed_before_pollen_costs_scale"]], df$part)]
    sum(weights / counts,  na.rm = TRUE)
  }  
  
  Costs$seed_early_costs = sapply(Costs$individual, f6)
  
  ret <- merge(AgeData, Costs, by="individual", all=TRUE) %>%
    mutate(
        repro_inv = prepollen_aborted_inv + prepollen_success_inv + postpollen_aborted_inv + packaging_dispersal_inv + propagule_inv,
        accessory_inv = repro_inv - propagule_inv,
        prepollen_all_inv = prepollen_aborted_inv + prepollen_success_inv,
        prop_prepollen_aborted = divide_zero(prepollen_aborted_inv, repro_inv),
        prop_prepollen_success = divide_zero(prepollen_success_inv, repro_inv),
        prop_postpollen_aborted = divide_zero(postpollen_aborted_inv, repro_inv),
        prop_packaging_dispersal = divide_zero(packaging_dispersal_inv, repro_inv),
        prop_propagule = divide_zero(propagule_inv, repro_inv),
        prop_accessory = 1 - prop_propagule,
        prop_prepollen_all = prop_prepollen_aborted + prop_prepollen_success
        )
}

#problems

#1
#for some species, notably EPMI, a listed part sometimes exists and sometimes doesn't in the reproduction spreadsheet, 
#because some individuals skipped that stage
#if the part doesn't exist, it also won't exist in FD or even INV (I think) - then the average part weight for the species should 
#be added in later
