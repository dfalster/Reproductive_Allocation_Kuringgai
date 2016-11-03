ReproductiveCounts <- function(species, IndividualsList, InvestmentCategories, species_Investment) {

  FD <- species_Investment$FD
  

  # Counts of parts
  categories_counts <- InvestmentCategories[[species]][["categories_counts"]]

  accessory_count <- FD %>%
      group_by(individual, part) %>%
      summarise_each(funs(sum), count)

  counts <- ddply(accessory_count, "individual", function(x) {
      sapply(names(categories_counts), function(n)
         sum(filter(x, part %in% categories_counts[[n]])$count))})

  counts <- mutate(counts,
    postpollen_count = postpollen_aborted_count + seed_count,
    prepollen_count = prepollen_count_aborted_preflowering + prepollen_count_reach_flowering,
    ovule_count = postpollen_count + prepollen_count,
    aborted_ovule_count = ovule_count - seed_count,
    seedset = divide_zero(seed_count, ovule_count),
    choosiness = divide_zero(ovule_count, seed_count),
    zygote_set = divide_zero(seed_count,postpollen_count),
    pollen_set = divide_zero(postpollen_count,prepollen_count_reach_flowering)
  )
  
  parts_weights <- list(
    seedpod_weight = "seed_pod",
    fruit_weight   = "fruit_mature")
  
  weights <- ddply(accessory_count, "individual", function(x) {
    sapply(names(parts_weights), function(n)
      sum(filter(x, part %in% parts_weights[[n]])$weight))})
  
  merge(counts, weights, by="individual", all=TRUE)
}


ReproductiveCosts <- function(species, IndividualsList, InvestmentCategories, species_Investment) {
  
  FD <- species_Investment$FD
  
  # Read and restrict the data to the subset of interest
  InvCat <- InvestmentCategories[[species]][["categories_costs"]]
  
  FD <- FD %>%
    group_by(individual,part) %>%
    summarise_each(funs(sum), count,weight)
  
  FD$unit_weight <- FD$weight/FD$count
  
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
    sapply(c("prepollen_inv_aborted_preflowering", "prepollen_inv_reach_flowering", "postpollen_aborted_inv",
             "packaging_dispersal_inv", "propagule_inv"), function(x) sum(df$weight[df$part %in% InvCat[[x]]]))
  }
  
  Costs <- ddply(AgeData, "individual",  f1)
  
  #for f7,f8 currently just matching to generic part. Want to add if,else to say, "First look at FD dataframe to see if there is a parts match - if not, 
  #use the generic species value from PartsSummary. This is necessary, because for some parts there may no longer be any of a "part" in FD
  
  f7 <- function(i) {
    df <- FD[FD$individual== i,]
    ps <- PartsSummary[PartsSummary$species == species,]
    unit_weight <- df$unit_weight[match(InvCat[["prepollen_inv_from_dispersal_unit_weight"]], df$part)]
    unit_weight[is.na(unit_weight)] <- 0
    counts <- df$count[match(InvCat[["prepollen_inv_from_dispersal_count"]], df$part)]
    counts[is.na(counts)] <- 0
    scale <- unlist(InvCat[["prepollen_inv_from_dispersal_prop_to_use"]])
    sum(scale*counts*unit_weight, na.rm = TRUE)
  }

  Costs$prepollen_inv_from_dispersal = sapply(Costs$individual, f7)

  f8 <- function(i) {
    df <- FD[FD$individual== i,]
    unit_weight <- df$unit_weight[match(InvCat[["prepollen_inv_from_postpollen_aborted_unit_weight"]], df$part)]
    unit_weight[is.na(unit_weight)] <- 0
    counts <- df$count[match(InvCat[["prepollen_inv_from_postpollen_aborted_unit_weight"]], df$part)]
    counts[is.na(counts)] <- 0
    FD_count <- df$count[match(InvCat[["prepollen_inv_from_postpollen_aborted_count"]], df$part)]
    FD_count[is.na(FD_count)] <- 0
    scale <- unlist(InvCat[["prepollen_inv_from_postpollen_aborted_prop_to_use"]])
    sum(counts*FD_count*unit_weight*scale, na.rm = TRUE)
  }
  
  Costs$prepollen_inv_from_postpollen_aborted = sapply(Costs$individual, f8)
  
  f9 <- function(i) {
    df <- FD[FD$individual== i,]
    unit_weight <- df$unit_weight[match(InvCat[["prepollen_inv_from propagule_inv"]], df$part)]
    unit_weight[is.na(unit_weight)] <- 0
    counts <- df$count[match(InvCat[["seed_count2"]], df$part)]
    counts[is.na(counts)] <- 0
    sum(counts*unit_weight, na.rm = TRUE)
  }
  
  Costs$prepollen_inv_from_propagule = sapply(Costs$individual, f9)

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
    sum(weights/counts, na.rm = TRUE)
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
    sum(weights/counts, na.rm = TRUE)
  }
  
  Costs$prepollen_partial_costs = sapply(Costs$individual, f3)

  f4 <- function(i) {
    # subset investment data by individual
    df <- FD[FD$individual== i,]
    # return 0 if there are 0 seeds produced
    if(sum(df$count[df$part %in% c("seed", "fruit_mature")], na.rm=TRUE) ==0) {
      return(0)
    }
    weights <- df$weight[match(InvCat[["pack_disp_net_costs"]], df$part)]
    counts <- df$count[match(InvCat[["pack_disp_net_costs_count"]], df$part)]
    if(length(counts >0)){
      adjust <- unlist(InvCat[["pack_disp_net_costs_prop_to_use"]])
    }
    else 
      adjust <- numeric(0)
    sum(adjust*weights/counts,  na.rm = TRUE)
  }
  
  Costs$pack_disp_net_costs = sapply(Costs$individual, f4)
  
  f5 <- function(i) {
    # subset investment data by individual
    df <- FD[FD$individual== i,]
    # return 0 if there are 0 flowers produced
    if(sum(df$count[df$part %in% c("flower_petals","fruit_mature","seed")], na.rm=TRUE) ==0) {
      return(0)
    }
    weights <- 1*df$weight[match(InvCat[["dispersal_before_pollen_costs"]], df$part)]
    
    counts <- df$count[match(InvCat[["dispersal_before_pollen_costs_scale"]], df$part)]
    if(length(counts >0)){
      adjust <- unlist(InvCat[["dispersal_before_pollen_costs_adjust"]])
    }
    else 
      adjust <- numeric(0)
    if(length(adjust)!= length(weights))  {
      stop(paste("problem with length of dispersal_before_pollen_costs_adjust for ", df$species[1]))
    }
    sum(adjust*weights/counts,  na.rm = TRUE)
  }
  
  Costs$pack_disp_early_costs = sapply(Costs$individual, f5)
  
  f6 <- function(i) {
    # subset investment data by individual
    df <- FD[FD$individual== i,]
    # return 0 if there are 0 flowers produced
    if(sum(df$count[df$part %in% c("fruit_mature","seed")], na.rm=TRUE) ==0) {
      return(0)
    }
    unit_weight <- df$unit_weight[match(InvCat[["seed_before_pollen_costs"]], df$part)]
    sum(unit_weight, na.rm = TRUE)
  }  
  
  Costs$seed_early_costs = sapply(Costs$individual, f6)
  
  ret <- merge(AgeData, Costs, by="individual", all=TRUE) %>%
    mutate(
        repro_inv = prepollen_inv_aborted_preflowering + prepollen_inv_reach_flowering + postpollen_aborted_inv + packaging_dispersal_inv + propagule_inv,
        accessory_inv = repro_inv - propagule_inv,
        prepollen_all_inv = prepollen_inv_aborted_preflowering + prepollen_inv_reach_flowering,
        prop_propagule = divide_zero(propagule_inv, repro_inv),
        prop_accessory = 1 - prop_propagule,
        prop_prepollen_all = divide_zero(prepollen_all_inv, repro_inv)
        )
}

#problems

#1
#for some species, notably EPMI, a listed part sometimes exists and sometimes doesn't in the reproduction spreadsheet, 
#because some individuals skipped that stage
#if the part doesn't exist, it also won't exist in FD or even INV (I think) - then the average part weight for the species should 
#be added in later