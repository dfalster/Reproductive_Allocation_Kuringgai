ReproductiveCosts <- function(species, IndividualsList, InvestmentCategories, species_Investment) {

  groups <- c("prepollen_aborted_inv", "prepollen_success_inv", "postpollen_aborted_inv",
    "packaging_dispersal_inv", "propagule_inv")

  # Read and restrict the data to the subset of interest
  AgeData <- IndividualsList %>%
    filter(use_for_allocation_calculations & alive) %>%
    select(species,individual, age)
  FD <- species_Investment$FD
  InvCat <- InvestmentCategories[[species]]

  # For each individual, sum investment by categories
  # Each category includes a list of parts for that species
  # Individual lists are drawn from AgeData so that even individuals with no FD data are included and return 0
  InvDist <- ddply(AgeData, "individual",   function(z) {
    # subset investment data by individual
    df <- FD[FD$individual==z$individual,]
    sapply(groups, function(x) sum(df$weight[df$part %in% InvCat[[x]]]))
  })

  ret <- merge(AgeData, InvDist, by="individual", all=TRUE) %>%
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

 ret
}

