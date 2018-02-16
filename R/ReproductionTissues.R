# Defines functions that take the investment data and breaks it down into
# specific reproductive cost categories based on the data in `accessoryParts.yml`


ReproductiveCosts <- function(species, IndividualsList, InvestmentCategories, species_Investment,
  species_PartsSummary) {

  # Read and restrict the data to the subset of interest
  InvCat <- InvestmentCategories[[species]]

  FD <- species_Investment$FD %>%
        group_by(individual, part) %>%
        summarise_at(vars(count, weight), sum) %>%
        mutate(unit_weight = divide_zero(weight, count))

  # add in embryo_endo_size from seed size spreadsheet
  AgeData <- IndividualsList %>%
        filter(use_for_allocation_calculations == "TRUE" & alive == "TRUE" & site != "baby") %>%
        select(species, individual, age)

  # For each individual, sum investment by categories Each category includes a
  # list of parts for that species Individual lists are drawn from AgeData so
  # that even individuals with no FD data are included and return 0 This function
  # returns a dataframe with multiple columns

  # Counts of parts
  f_count <- function(data) {
    # subset investment data by individual
    df <- FD[FD$individual == data$individual, ]
    sapply(c("count_prepollen_reach_flowering", "count_prepollen_aborted_preflowering",
      "count_postpollen_aborted", "propagule_parts", "cone_count", "inflorescence_count"),
      function(x) sum(df$count[df$part %in% InvCat[[x]]]))
  }

  Counts <- ddply(AgeData, "individual", f_count)
  names(Counts) <- c("individual", "prepollen_count_reach_flowering", "prepollen_count_aborted_preflowering",
    "postpollen_aborted_count", "seed_count", "cone_count", "inflorescence_count")

  Counts <- mutate(Counts,
        postpollen_count = postpollen_aborted_count + seed_count,
        stop_at_flower_count = prepollen_count_reach_flowering - postpollen_count,
        prepollen_count = prepollen_count_aborted_preflowering + stop_at_flower_count,
        ovule_count = postpollen_count + prepollen_count,
        aborted_ovule_count = ovule_count - seed_count,
        seedset = divide_zero(seed_count, ovule_count),
        choosiness = divide_zero(ovule_count, seed_count),
        #choosiness2 = divide_zero(prepollen_count_reach_flowering, seed_count),
        zygote_set = divide_zero(seed_count, postpollen_count),
        pollen_set = divide_zero(postpollen_count, prepollen_count_reach_flowering))

  f1 <- function(data) {
    # subset investment data by individual
    df <- FD[FD$individual == data$individual, ]
    sapply(c("prepollen_parts_aborted_preflowering", "postpollen_parts_aborted",
      "prepollen_parts_all", "postpollen_parts_all"), function(x) sum(df$weight[df$part %in%
      InvCat[[x]]]))
  }

  Costs <- ddply(AgeData, "individual", f1)

  names(Costs) <- c("individual", "prepollen_inv_aborted_preflowering", "postpollen_aborted_inv",
    "prepollen_temp_inv", "postpollen_temp_inv")
  Costs <- merge(Costs, Counts, by = "individual", all = TRUE)

  # for f7,f8 currently just matching to generic part. Want to add if,else to
  # say, 'First look at FD dataframe to see if there is a parts match - if not,
  # use the generic species value from PartsSummary. This is necessary, because
  # for some parts there may no longer be any of a 'part' in FD

  # The total weight is standardised against the counts of a parts specified by
  # the variable 'prepollen_costs_count' Takes as argument the individual
  f2 <- function(i) {
    # subset investment data by individual
    df <- FD[FD$individual == i, ]
    # return 0 if there are 0 seeds produced
    if (sum(df$count[df$part %in% c("seed", "fruit_mature")], na.rm = TRUE) ==
      0) {
      return(0)
    }
    weights <- df$weight[match(InvCat[["prepollen_FD_success_parts"]], df$part)]
    denominator <- df$count[match(InvCat[["prepollen_FD_success_parts_denominator"]],
      df$part)]
    sum(weights/denominator, na.rm = TRUE)
  }

  Costs$prepollen_partial_costs <- sapply(Costs$individual, f2)

  # Calculate investment in packaging and dispersal structures that should be
  # re-allocated to pre-pollination investment pools For BAER and PEPU there are
  # individuals that have no green cones left, so you can't use a pre-pollen part
  # as the weight basis.  Therefore determined a fixed percentage of brown cone
  # weight to attribute to pre-pollen construction costs in combination with the
  # weight of any cones that do not develop post-pollination

  f3 <- function(i) {
    df <- FD[FD$individual == i, ]
    if (sum(df$count[df$part %in% c("seed", "fruit_mature")], na.rm = TRUE) ==
      0) {
      return(0)
    }
    if (species == "BAER" | species == "PEPU") {
      weights <- df$weight[match(InvCat[["prepollen_parts_continue_developing_into_pack_disp"]],
        df$part)]
      prop_to_use <- unlist(InvCat[["prepollen_parts_continue_developing_into_pack_disp_prepollen_prop_to_use"]])
      sum(prop_to_use * weights, na.rm = TRUE)
    } else {
      unit_weights <- df$unit_weight[match(InvCat[["prepollen_parts_continue_developing_into_pack_disp"]],
        df$part)]
      sum(unit_weights, na.rm = TRUE)
    }
  }

  Costs$prepollen_inv_from_pack_disp_tissues <- sapply(Costs$individual, f3)

  # For some individuals of COER, GRSP, HEPU no longer an FD parts of relevant
  # part - all continued to post-pollination part types For these individuals use
  # the species mean value for the pre-pollination part weight For HEPU need to
  # divide by 4, since part weight divided among 4 ovules and PartsSummary has
  # not yet been adjusted by multiplier table

  for (i in 1:length(Costs$individual)) {
    if ((Costs$prepollen_inv_from_pack_disp_tissues[i] == 0) & (Costs$seed_count[i] >
      0) & (species %in% c("COER", "GRSP", "HEPU"))) {
      unit_weights <- species_PartsSummary$weight[match(InvCat[["prepollen_parts_continue_developing_into_pack_disp"]],
        species_PartsSummary$part)]
      Costs$prepollen_inv_from_pack_disp_tissues[i] <- sum(unit_weights,
        na.rm = TRUE)
      if (species == "HEPU") {
        Costs$prepollen_inv_from_pack_disp_tissues[i] <- Costs$prepollen_inv_from_pack_disp_tissues[i]/4
      }
    }
  }


  # For species where pack&disp parts in question are on a flower-level, multiply
  # by flower count for total INVESTMENT numbers
  if (species %in% c("BOLE", "EPMI", "HEPU", "LEES")) {
    Costs$prepollen_inv_from_pack_disp_tissues <- Costs$prepollen_inv_from_pack_disp_tissues *
      Costs$prepollen_count_reach_flowering
  }

  # For species where pack&disp parts in question are on a inflorscence-level,
  # multiply by inflorscence count for total INVESTMENT numbers
  if (species %in% c("COER", "GRSP")) {
    Costs$prepollen_inv_from_pack_disp_tissues <- Costs$prepollen_inv_from_pack_disp_tissues *
      Costs$inflorescence_count
  }

  # Take total investment - calculated many different ways - and divide by number
  # of flowers that reach pollination
  Costs$prepollen_costs_from_pack_disp_tissues <- divide_zero(Costs$prepollen_inv_from_pack_disp_tissues,
    Costs$prepollen_count_reach_flowering)


  f4 <- function(i) {
    # subset investment data by individual
    df <- FD[FD$individual == i, ]
    # return 0 if there are 0 seeds produced
    if (sum(df$count[df$part %in% c("seed", "fruit_mature")], na.rm = TRUE) ==
      0) {
      return(0)
    }
    weights <- df$weight[match(InvCat[["packaging_dispersal_parts"]], df$part)]
    weights_denominator <- df$count[match(InvCat[["packaging_dispersal_parts_denominator"]],
      df$part)]
    if (species %in% c("BAER", "PEPU")) {
      prop_to_use <- unlist(InvCat[["packaging_dispersal_parts_prop_to_use"]])
      sum((prop_to_use) * (weights/weights_denominator), na.rm = TRUE)
    } else {
      denominator <- df$unit_weight[match(InvCat[["packaging_dispersal_parts"]],
        df$part)]
      numerator <- df$unit_weight[match(InvCat[["packaging_dispersal_parts_numerator"]],
        df$part)]
      sum((numerator/denominator) * (weights/weights_denominator), na.rm = TRUE)
    }

  }

  Costs$packaging_dispersal_costs_temp <- sapply(Costs$individual, f4)


  # Calculate investment in propagule structures that should be re-allocated to
  # pre-pollination investment pools
  f5 <- function(i) {
    df <- FD[FD$individual == i, ]
    if (sum(df$count[df$part %in% c("seed", "fruit_mature")], na.rm = TRUE) ==
      0) {
      return(0)
    }
    weights <- df$unit_weight[match(InvCat[["prepollen_parts_continue_developing_into_propagule"]],
      df$part)]
    sum(weights, na.rm = TRUE)
  }

  Costs$prepollen_costs_from_seed_tissues <- sapply(Costs$individual, f5)


  f6 <- function(i) {
    df <- FD[FD$individual == i, ]
    if (sum(df$count[df$part %in% c("seed", "fruit_mature")], na.rm = TRUE) ==
      0) {
      return(0)
    }
    weights <- df$unit_weight[match(InvCat[["propagule_parts"]], df$part)]
    sum(weights, na.rm = TRUE)
  }

  Costs$propagule_costs_temp <- sapply(Costs$individual, f6)


  f7 <- function(i) {
    df <- FD[FD$individual == i, ]
    if (sum(df$count[df$part %in% c("seed", "fruit_mature")], na.rm = TRUE) ==
      0) {
      return(0)
    }
    weights <- df$unit_weight[match(InvCat[["fruit_parts"]], df$part)]
    sum(weights, na.rm = TRUE)
  }

  Costs$fruit_costs <- sapply(Costs$individual, f7)


  # Where variables in this function return NA, they should be zero, b because it
  # represents cases was no investment in that thing
  for (v in names(Costs)[sapply(Costs, is.numeric)]) {
    i <- is.na(Costs[[v]])
    Costs[[v]][i] <- 0
  }

  merge(AgeData, Costs, by="individual", all=TRUE) %>%
    mutate(
      prepollen_all_inv = prepollen_temp_inv + (postpollen_count*(prepollen_costs_from_pack_disp_tissues + prepollen_costs_from_seed_tissues)),
      postpollen_all_inv = postpollen_temp_inv - (postpollen_count*(prepollen_costs_from_pack_disp_tissues + prepollen_costs_from_seed_tissues)),
      prepollen_inv_from_aborted_dispersal = prepollen_costs_from_pack_disp_tissues*postpollen_aborted_count,
      pollen_attract_costs = prepollen_partial_costs + prepollen_costs_from_pack_disp_tissues + prepollen_costs_from_seed_tissues,
      prepollen_success_inv = seed_count*pollen_attract_costs,
      prepollen_discarded_inv = prepollen_all_inv - prepollen_success_inv,
      packaging_dispersal_costs = packaging_dispersal_costs_temp - prepollen_costs_from_pack_disp_tissues,
      propagule_costs = propagule_costs_temp - prepollen_costs_from_seed_tissues,
      pack_disp_success_inv = seed_count*packaging_dispersal_costs,
      propagule_inv = seed_count*propagule_costs,
      success_inv = pack_disp_success_inv + propagule_inv + prepollen_success_inv,
      postpollen_aborted_inv = postpollen_all_inv - propagule_inv - pack_disp_success_inv,
      fruit_inv = fruit_costs*seed_count,
      repro_inv = prepollen_all_inv + postpollen_all_inv,
      discarded_inv = repro_inv - success_inv,
      discarded_costs = divide_zero(discarded_inv,seed_count),
      success_costs = divide_zero(success_inv,seed_count)
      )

}
