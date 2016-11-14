ReproductiveCosts <- function(species, IndividualsList, InvestmentCategories, species_Investment) {
  FD <- species_Investment$FD
  # Read and restrict the data to the subset of interest
  InvCat <- InvestmentCategories[[species]]

  FD <- FD %>%
    group_by(individual,part) %>%
    summarise_each(funs(sum), count,weight)
  
  FD$unit_weight <- FD$weight/FD$count
  
  AgeData <- IndividualsList %>%
    filter(use_for_allocation_calculations=="TRUE" & alive=="TRUE"&site!="baby") %>%
    select(species,individual, age)
  
  # For each individual, sum investment by categories
  # Each category includes a list of parts for that species
  # Individual lists are drawn from AgeData so that even individuals with no FD data are included and return 0
  # This function returns a dataframe with multiple columns
  
  f1 <- function(data) {
    # subset investment data by individual
    df <- FD[FD$individual==data$individual,]
    sapply(c("prepollen_parts_aborted_preflowering", "postpollen_parts_aborted",
             "propagule_parts","prepollen_parts_all","postpollen_parts_all"), 
           function(x) sum(df$weight[df$part %in% InvCat[[x]]]))
  }
  
  Costs <- ddply(AgeData, "individual",  f1)
  
  names(Costs) <- c("individual","prepollen_inv_aborted_preflowering", "postpollen_aborted_inv",
                    "propagule_inv","prepollen2_all_inv","postpollen2_all_inv")
  
  # Counts of parts
  f_count <- function(data) {
    # subset investment data by individual
    df <- FD[FD$individual==data$individual,]
    sapply(c("count_prepollen_reach_flowering", "count_prepollen_aborted_preflowering", "count_postpollen_aborted","propagule_parts",
             "cone_count", "inflorescence_count"), function(x) sum(df$count[df$part %in% InvCat[[x]]]))
  }
  
  Counts <- ddply(AgeData, "individual",  f_count)
  names(Counts) <- c("individual","prepollen_count_reach_flowering", "prepollen_count_aborted_preflowering", "postpollen_aborted_count","seed_count",
                     "cone_count", "inflorescence_count")
  
  Counts <- mutate(Counts,
                   postpollen_count = postpollen_aborted_count + seed_count,
                   stop_at_flower_count = prepollen_count_reach_flowering - postpollen_count,
                   prepollen_count = prepollen_count_aborted_preflowering + stop_at_flower_count,
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
  
  weights <- ddply(FD, "individual", function(x) {
    sapply(names(parts_weights), function(n)
      sum(filter(x, part %in% parts_weights[[n]])$unit_weight))})
  #need to make sure this is just taking value once-LIZZY
  
  Counts <- merge(Counts, weights, by="individual", all=TRUE)
  
  Costs <- merge(Costs, Counts, by="individual", all=TRUE)
  
  #for f7,f8 currently just matching to generic part. Want to add if,else to say, "First look at FD dataframe to see if there is a parts match - if not, 
  #use the generic species value from PartsSummary. This is necessary, because for some parts there may no longer be any of a "part" in FD
  
  #Calculate investment in propagule structures that should be re-allocated to pre-pollination investment pools
  f8 <- function(i) {
    df <- FD[FD$individual== i,]
    weight <- df$unit_weight[match(InvCat[["prepollen_parts_continue_developing_into_propagule"]], df$part)]
    sum(weight, na.rm = TRUE)
  }
  
  Costs$prepollen_costs_from_seed_tissues = sapply(Costs$individual, f8)
  
  
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
    scale <- unlist(InvCat[["seed_costs_prop_to_use"]])
    sum(scale*weights/counts, na.rm = TRUE)
  }
  
  Costs$seed_costs = sapply(Costs$individual, f2)
  
  # Identical format to f2, but returns a single column for prepollencosts. The total weight is standardised against the counts of
  # a parts specified by the variable "prepollen_costs_count"
  # Takes as argument the individual
  f3 <- function(i) {
    # subset investment data by individual
    df <- FD[FD$individual== i,]
    # return 0 if there are 0 flowers produced
    if(sum(df$count[df$part %in% c("flower_petals","flower_petals_small")], na.rm=TRUE) ==0) {
      return(0)
    }
    weights <- df$weight[match(InvCat[["prepollen_FD_success_parts"]], df$part)]
    divisor <- df$count[match(InvCat[["prepollen_FD_success_parts_divisor"]], df$part)]
    sum(weights/divisor, na.rm = TRUE)
  }
  
  Costs$prepollen_partial_costs = sapply(Costs$individual, f3)
  
  
  #Calculate investment in packaging and dispersal structures that should be re-allocated to pre-pollination investment pools
  f7 <- function(i) {
    df <- FD[FD$individual== i,]
    # return 0 if there are 0 flowers produced
    if(sum(df$count[df$part %in% c("flower_petals","fruit_mature","seed")], na.rm=TRUE) ==0) {
      return(0)
    }
    weight <- df$weight[match(InvCat[["prepollen_parts_continue_developing_into_pack_disp"]], df$part)]
    if(length(weight > 0)){
      scale <- unlist(InvCat[["prepollen_parts_continue_developing_into_pack_disp_prepollen_prop_to_use"]])
    }
    else 
      adjust <- numeric(0)
    (sum(scale*weight, na.rm = TRUE))
  }
  
  Costs$prepollen_inv_from_pack_disp_tissues = sapply(Costs$individual, f7)
  
  f4 <- function(i) {
    # subset investment data by individual
    df <- FD[FD$individual== i,]
    # return 0 if there are 0 seeds produced
    if(sum(df$count[df$part %in% c("seed", "fruit_mature")], na.rm=TRUE) ==0) {
      return(0)
    }
    weights <- df$weight[match(InvCat[["packaging_dispersal_parts"]], df$part)]
    weights_divisor <- df$count[match(InvCat[["packaging_dispersal_parts_divisor"]], df$part)]
    divisor <- df$unit_weight[match(InvCat[["packaging_dispersal_parts"]], df$part)]
    numerator <- df$unit_weight[match(InvCat[["packaging_dispersal_parts_numerator"]], df$part)]
    prop_to_use <- unlist(InvCat[["packaging_dispersal_parts_numerator2"]])
    if(species=="BAER"|species=="PEPU") {
      sum((1-prop_to_use)*(weights/weights_divisor),  na.rm = TRUE)
    } else 
      sum((numerator/divisor)*(weights/weights_divisor),  na.rm = TRUE)
  }
  
  Costs$pack_disp_net_costs = sapply(Costs$individual, f4)
  
  
  f14 <- function(i) {
    # subset investment data by individual
    df <- FD[FD$individual== i,]
    # return 0 if there are 0 seeds produced
    if(sum(df$count[df$part %in% c("seed", "fruit_mature")], na.rm=TRUE) ==0) {
      return(0)
    }
    weights <- df$weight[match(InvCat[["packaging_dispersal_parts"]], df$part)]
    divisor <- df$unit_weight[match(InvCat[["packaging_dispersal_parts"]], df$part)]
    numerator <- df$unit_weight[match(InvCat[["packaging_dispersal_parts_numerator"]], df$part)]
    prop_to_use <- unlist(InvCat[["packaging_dispersal_parts_numerator2"]])
    if(species=="BAER"|species=="PEPU") {
      sum((1-prop_to_use)*(weights),  na.rm = TRUE)
    } else 
      sum((numerator/divisor)*(weights),  na.rm = TRUE)
  }
  
  Costs$pack_disp_inv = sapply(Costs$individual, f14)
  
  f10 <- function(i) {
    # subset investment data by individual
    df <- FD[FD$individual== i,]
    weights <- df$unit_weight[match(InvCat[["propagule_parts"]], df$part)]
    sum(weights,  na.rm = TRUE)
  }
  
  Costs$propagule_costs = sapply(Costs$individual, f10)
  
  
  for(v in c("prepollen_count_reach_flowering","prepollen_count_aborted_preflowering",
             "postpollen_aborted_count","seed_count","cone_count","inflorescence_count","postpollen_count",
             "stop_at_flower_count","prepollen_count","ovule_count","aborted_ovule_count","seedset","choosiness",
             "zygote_set","pollen_set","seedpod_weight","fruit_weight","prepollen_costs_from_pack_disp_tissues",
             "prepollen_costs_from_seed_tissues","seed_costs","prepollen_partial_costs","pack_disp_net_costs","propagule_costs")) {
    i <- is.na(Costs[[v]])
    Costs[[v]][i] <- 0
    i <- is.infinite(Costs[[v]])
    Costs[[v]][i] <- 0
  }  
  
  ret <- merge(AgeData, Costs, by="individual", all=TRUE) %>%
    mutate(
      prepollen_costs_from_pack_disp_tissues = divide_zero(prepollen_inv_from_pack_disp_tissues, prepollen_count_reach_flowering),
      prepollen_inv_from_aborted_dispersal = prepollen_costs_from_pack_disp_tissues*postpollen_aborted_count,
      prepollen_success_inv = (seed_count*(prepollen_partial_costs + prepollen_costs_from_pack_disp_tissues + prepollen_costs_from_seed_tissues)),
      prepollen_aborted_inv = ((stop_at_flower_count+postpollen_aborted_count)*(prepollen_partial_costs + prepollen_costs_from_pack_disp_tissues + prepollen_costs_from_seed_tissues)) + prepollen_inv_aborted_preflowering,
      prepollen_all_inv = prepollen_success_inv + prepollen_aborted_inv,
      pack_disp_success_inv = (pack_disp_net_costs)*(seed_count),
      postpollen_aborted_inv = postpollen_main_aborted_inv + packaging_dispersal_inv - pack_disp_success_inv - ((postpollen_aborted_count)*(prepollen_partial_costs + prepollen_costs_from_pack_disp_tissues + prepollen_costs_from_seed_tissues)),
      propagule_inv = (seed_count)*(propagule_costs-prepollen_costs_from_seed_tissues),
      repro_inv = prepollen_all_inv + postpollen_aborted_inv + pack_disp_success_inv + propagule_inv,
      required_inv = pack_disp_success_inv + propagule_inv + prepollen_success_inv,
      accessory_inv = repro_inv - propagule_inv,
      prop_propagule = divide_zero(propagule_inv, repro_inv),
      prop_accessory = 1 - prop_propagule,
      prop_prepollen_all = divide_zero(prepollen_all_inv, repro_inv),
      seed_costs3 = required_inv/seed_count,
      postpollen_aborted_inv2 = postpollen_all_inv - (postpollen_aborted_count*prepollen_costs_from_pack_disp_tissues) - (postpollen_aborted_count*prepollen_costs_from_seed_tissues) - pack_disp_success_inv - propagule_inv,
      prepollen_aborted_inv2 = prepollen_all_inv - prepollen_success_inv
    )
}


#problems

#1
#for some species, notably EPMI, a listed part sometimes exists and sometimes doesn't in the reproduction spreadsheet, 
#because some individuals skipped that stage
#if the part doesn't exist, it also won't exist in FD or even INV (I think) - then the average part weight for the species should 
#be added in later