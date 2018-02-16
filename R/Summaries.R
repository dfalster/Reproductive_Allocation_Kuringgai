filterBySpecies <- function(thisSpecies, data) {
  filter(data, species == thisSpecies)
}

filterForAllocation <- function(data, IndividualsList) {
  # TODO: change column name in reproduction ro be consistent: individual ->
  # tagID
  keep <- IndividualsList$individual[IndividualsList$use_for_allocation_calculations]
  data %>% filter(individual %in% keep)
}


# Get average LMA and leaf size for each species
process_LMA <- function(LMA_raw) {
  LMA <- filter(LMA_raw, species != "" & species != " ") %>% select(species,
    age, LMA, branch_age, leaf_number, leaf_area)

  LMA$leaf_size <- LMA$leaf_area/LMA$leaf_number

  LMA %>% group_by(species, age) %>% summarise_at(vars(LMA, leaf_size), mean)
}

# Get average leaves per length for each species
process_leaves_per_length <- function(leavesPerLength_raw) {
  filter(leavesPerLength_raw, use == "yes") %>%
  mutate(count_per_length = leaf_count/length_mm) %>%
  group_by(species) %>%
  summarise(count_per_length = mean(count_per_length, na.rm = TRUE))
}


# Leaf loss calculations based on surveys of number of leaves on branch.  Loss
# is calculated as # leaves lost during interval / # at start
# Two types of counts are implemented for each branch and added together:
# 1. Direct counts -- these use variables: shoot_leaf_count_start_count, lvs_end_count,
#     shoot_leaf_count_new_count, shoot_leaf_count_new_and_shed_count
# 2. Counts  based on number of leaves per unit stem length and estimated from stem length
# -- these use variables shoot_length_start, growth_shoot_length, shoot_leaf_count_start_length lvs_end_length
# shoot_leaf_count_growth_shoot_length
process_leaf_loss <- function(data_raw, leavesPerLength) {

  data <- filter(data_raw, dont_use != "dead" & dont_use != "dont_use") %>% select(-age_exact,
    -replicate, -site, -segment, -notes, -dont_use, -shoot_diameter_start,
    -shoot_diameter_end, -mm_lvs_spec, -count_lvs_spec)

  # For individuals with 'length' measures, translate into count via spp average
  data$count_per_length <- leavesPerLength$count_per_length[match(data$species,
    leavesPerLength$species)]

  # set these to zero so that sums below
  for (v in c("shoot_leaf_count_start_length", "shoot_leaf_count_start_count",
    "lvs_end_length", "lvs_end_count", "shoot_leaf_count_growth_shoot_length",
    "shoot_leaf_count_new_count", "shoot_leaf_count_new_and_shed_count")) {
    data[[v]][is.na(data[[v]])] <- 0
  }

  # calculating leaf loss
  mutate(data,
         shoot_leaf_count_start = shoot_leaf_count_start_count +
          (shoot_leaf_count_start_length *  count_per_length),
    lvs_end = lvs_end_count + (lvs_end_length * count_per_length),
    leaf_loss_count = shoot_leaf_count_start - lvs_end,
    prop_leaf_loss = leaf_loss_count/shoot_leaf_count_start,
    shoot_leaf_count_new = shoot_leaf_count_new_count +
          (shoot_leaf_count_growth_shoot_length *  count_per_length),
    shoot_leaf_count = lvs_end + shoot_leaf_count_new) %>%
  select(species, age, individual, shoot_length_start, growth_shoot_length,
         shoot_leaf_count_new_and_shed_count, shoot_leaf_count_start, prop_leaf_loss,
         shoot_leaf_count_new, shoot_leaf_count,lvs_end,lvs_end_count,lvs_end_length,count_per_length)
}

# Calculate average wood density by species
process_wood_density <- function(wood_density_spp) {
  wood <- filter(wood_density_spp, use == "use") %>% select(species, density) %>%
    group_by(species) %>% summarise_at(vars(density), mean)
  names(wood) <- c("species", "wood_density")
  wood
}


combine_by_individual <- function(IndividualsList, Growth_all, ReproductiveCosts_all,
                                  LMA, leafLoss, wood_density_spp, seedsize) {
  
  # adding investment and accessory costs data to leafLoss dataframe to create a
  # dataframe with all individual level data
  SummaryInd <- merge(select(IndividualsList, species, age, individual, mature),
                      select(Growth_all, -species, -age), by = "individual", all = FALSE)
  SummaryInd <- merge(SummaryInd, select(leafLoss, -species, -age), by = "individual",
                      all.x = TRUE)
  SummaryInd <- merge(SummaryInd, select(ReproductiveCosts_all, -species, -age),
                      by = "individual", all.x = TRUE)
  SummaryInd <- merge(SummaryInd, LMA, by = c("species", "age"), all.x = TRUE)
  SummaryInd <- merge(SummaryInd, wood_density_spp, by = c("species"), all.x = TRUE)
  SummaryInd <- merge(SummaryInd, select(seedsize, -propagule_weight, -pod_weight),
                      by = c("species"), all.x = TRUE)
  
  # Remove seedlings collected solely for allometric equations
  SummaryInd <- filter(SummaryInd, age > 1)
  
  SummaryInd <- SummaryInd %>%
    mutate(growth_inv = growth_stem + growth_leaf,
           total_inv = repro_inv + growth_inv,
           leaf_repro_inv = repro_inv + growth_leaf,
           total_weight_0 = total_weight - growth_inv,
           stem_weight_0 = stem_weight - growth_stem,
           leaf_weight_0 = leaf_weight - growth_leaf,
           height_0 = height - growth_height,
           diameter_0 = diameter - growth_stem_diameter,
           RA = divide_zero(repro_inv,  total_inv),
           leaf_area = leaf_weight/(1000 * LMA),
           growth_leaf_area = growth_leaf/(1000 * LMA),
           leaf_area_0 = leaf_area - growth_leaf_area,
           leaf_shed = leaf_weight_0 * prop_leaf_loss,
           leaf_inv_gross = leaf_shed +  growth_leaf,
           growth_leaf_neg = 0,
           growth_leaf_pos = 0
           )
  
  for (i in seq_along(SummaryInd$individual)) {
    if (SummaryInd$growth_leaf[i] < 0) {
      SummaryInd$growth_leaf_neg[i] <- SummaryInd$growth_leaf[i]
    } else {
      SummaryInd$growth_leaf_pos[i] <- SummaryInd$growth_leaf[i]
    }
  }
  
  SummaryInd <- SummaryInd %>% mutate(
    reproducing = RA>0,
    seed_size = propagule_costs,
    embryo_endo_costs = seed_size*prop_endo,
    seed_coat_costs = propagule_costs - embryo_endo_costs,
    embryo_endo_inv = embryo_endo_costs*seed_count,
    packaging_dispersal_costs = packaging_dispersal_costs + seed_coat_costs, #add in non-embryo-endo seed weight; don't need to subtract prepollen_costs_from_pack_disp_tissues because already removed
    pack_disp_success_inv = pack_disp_success_inv + (seed_coat_costs*seed_count),
    repro_costs = divide_zero(repro_inv,seed_count),
    accessory_costs = repro_costs - embryo_endo_costs,
    accessory_inv = repro_inv - embryo_endo_inv,
    accessory_costs_using_seedweight = repro_costs - propagule_costs,
    provisioning_costs = packaging_dispersal_costs + embryo_endo_costs,
    flower_inv = pollen_attract_costs*ovule_count, #a proxy measure showing good way to estimate total repro inv; doesn't have exact meaning since "repro all count" includes aborted buds
    prepollen_all_costs = divide_zero(prepollen_all_inv,seed_count),
    prepollen_discarded_costs = divide_zero(prepollen_discarded_inv,seed_count), #discarded is parts that don't progress to seeds
    postpollen_aborted_costs = divide_zero(postpollen_aborted_inv,seed_count), #all parts that aren't pack & dispersal for successful seeds
    postpollen_all_costs = divide_zero(postpollen_all_inv,seed_count),
    prop_success = divide_zero(success_inv, repro_inv),
    prop_discarded_vs_all_repro = 1 - prop_success,
    prop_pollen_attract_vs_all_repro = divide_zero(pollen_attract_costs,repro_costs),
    prop_prepollen_discarded_vs_all_repro = divide_zero(prepollen_discarded_costs,repro_costs),
    prop_prepollen_all_vs_all_repro = divide_zero(prepollen_all_inv, repro_inv),
    prop_postpollen_all_vs_all_repro = divide_zero(postpollen_all_inv, repro_inv),
    prop_pack_disp_vs_all_repro = divide_zero(packaging_dispersal_costs,repro_costs),
    prop_postpollen_discarded_vs_all_repro = divide_zero(postpollen_aborted_costs,repro_costs),
    prop_embryo_endo_vs_all_repro = divide_zero(embryo_endo_costs,repro_costs),
    prop_propagule_vs_all_repro = divide_zero(propagule_inv,repro_inv),
    prop_accessory_vs_embryo_endo = 1 - prop_embryo_endo_vs_all_repro,
    prop_accessory_vs_propagule = 1 - prop_propagule_vs_all_repro,
    prop_pollen_attract_vs_success = divide_zero(pollen_attract_costs,success_costs),
    prop_provisioning_vs_success = 1 - prop_pollen_attract_vs_success,
    prop_embryo_endo_vs_success = divide_zero(embryo_endo_costs,success_costs),
    prop_pack_disp_vs_success = divide_zero(packaging_dispersal_costs,success_costs),
    prop_postpollen_success = divide_zero(provisioning_costs,postpollen_all_costs),
    prop_postpollen_discarded = 1 - prop_postpollen_success,
    prop_prepollen_success = divide_zero(pollen_attract_costs,prepollen_all_costs),
    prop_prepollen_discarded = 1 - prop_prepollen_success,
    scaled_seed_count = divide_zero(seed_count,leaf_area_0),
    scaled_ovule_count = divide_zero(ovule_count,leaf_area_0),
    scaled_reach_flowering_count = divide_zero(prepollen_count_reach_flowering,leaf_area_0),
    scaled_repro_inv = divide_zero(repro_inv,leaf_area_0),
    scaled_pollen_attract_costs = divide_zero(pollen_attract_costs,leaf_area_0),
    scaled_provisioning_costs = divide_zero(provisioning_costs,leaf_area_0)
  )
  
  
  
  # Where these ratios are infinite we are setting them to zero. It represents
  # cases where no successful seeds were produced
  for (v in c("prop_prepollen_discarded_vs_all_repro", "prop_postpollen_discarded_vs_all_repro")) {
    i <- is.na(SummaryInd[[v]])
    SummaryInd[[v]][i] <- 0
  }
  
  SummaryInd <- SummaryInd %>%
    mutate(
      leaf_replacement = leaf_shed + growth_leaf_neg,
      surplus_inv = repro_inv + growth_leaf_pos,
      all_leaf_inv = leaf_replacement + growth_leaf_pos,
      all_leaf_and_repro_inv = repro_inv + leaf_replacement + growth_leaf_pos,
      RA_max_1 = divide_zero(repro_inv, surplus_inv),
      gross_inv = repro_inv + growth_leaf_pos + leaf_replacement + growth_stem,
      prop_repro = divide_zero(repro_inv, gross_inv),
      prop_leaf_expand = divide_zero(surplus_inv, gross_inv),
      prop_leaf_replacement = divide_zero(all_leaf_and_repro_inv, gross_inv),
      prop_surplus = divide_zero(surplus_inv, all_leaf_and_repro_inv),
      repro_prop_all_leaf = divide_zero(repro_inv,all_leaf_and_repro_inv), 
      repro_and_leaf_growth_prop_surplus = divide_zero((growth_leaf_pos+repro_inv),all_leaf_and_repro_inv), 
      seed_prop_veg = divide_zero(embryo_endo_inv, (growth_leaf_pos + leaf_replacement + growth_stem + repro_inv)),
      seed_prop_surplus = divide_zero(embryo_endo_inv,(growth_leaf_pos + repro_inv)),
      seed_prop_all_acc = divide_zero(embryo_endo_inv,(growth_leaf_pos + leaf_replacement + growth_stem + repro_inv)),
      seed_prop_all = divide_zero(embryo_endo_inv,(growth_leaf_pos + leaf_replacement + growth_stem + embryo_endo_inv)),
      growth_leaf_prop_no_accessory = divide_zero((embryo_endo_inv+growth_leaf_pos),(growth_leaf_pos + leaf_replacement + growth_stem + embryo_endo_inv)),
      replace_leaf_prop_no_accessory = divide_zero((embryo_endo_inv+growth_leaf_pos+leaf_replacement),(growth_leaf_pos + leaf_replacement + growth_stem + embryo_endo_inv)),
    )
  
  # Where these ratios are infinite we are setting them to zero. It represents
  # cases where no successful seeds were produced
  for (v in c("choosiness", "discarded_costs", "repro_costs",
              "accessory_costs", "accessory_costs_using_seedweight", "prepollen_all_costs",
              "prepollen_discarded_costs", "postpollen_aborted_costs", "postpollen_all_costs",
              "prop_pollen_attract_vs_success", "prop_provisioning_vs_success", "prop_pack_disp_vs_success")) {
    i <- is.infinite(SummaryInd[[v]])
    SummaryInd[[v]][i] <- 0
  }
  
  for (v in c("seed_prop_surplus","seed_prop_all","seed_prop_veg","seed_prop_all_acc")) {
    i <- is.na(SummaryInd[[v]])
    SummaryInd[[v]][i] <- 0
  }
  
  for (v in c("prop_leaf_loss")) {
    i <- is.infinite(SummaryInd[[v]])
    SummaryInd[[v]][i] <- 0
  }
  
  for (v in c("prop_leaf_loss")) {
    i <- (SummaryInd[[v]] < 0)
    SummaryInd[[v]][i] <- 0
  }
  
  SummaryInd
  
}

# re-orders columns in data to match order of values in 'variable' column of
# variable_list
sort_by_variable <- function(data, variable_list) {
  
  vars <- variable_list$variable[variable_list$variable %in% names(data)]
  vars <- base::union(vars, names(data)) 
  data[, vars] %>%  dplyr::arrange(species, individual, age, start_end)
}

get_species_values <- function(SummaryInd, groups) {
  
  # function to apply
  fs <- c("max", "min", "mean", "sd", "length")
  
  # note use of `group_by_` below - allows for passing in of character vectors
  dots <- lapply(groups, as.symbol)
  
  # summarizing data by species, age
  out <- list()
  out[[1]] <- lapply(fs, function(f) {
    SummaryInd %>%
      group_by_(.dots = dots) %>%
      summarise_at(vars(height, growth_inv,
                     total_weight, total_weight_0, total_inv, RA,
                     growth_leaf_pos, leaf_weight, stem_weight, growth_stem_diameter,
                     growth_leaf, leaf_weight_0, stem_weight_0,
                     repro_inv, growth_stem, diameter, diameter_0, LMA, wood_density, leaf_area,
                     leaf_area_0, growth_leaf_neg,
                     RA_max_1, lifespan, maturity, height_0), f)
  })
  names(out[[1]]) <- fs
  
  #remove individuals with no data for leaf lifespan
  out[[2]] <- lapply(fs, function(f) {
    SummaryInd %>% filter(!(individual %in% c("COER_806","EPMI_907","GRBU_408","GRBU_906","HATE_105","HATE_003","LEES_354",
                                              "LEES_352","LEES_355","LEES_353","LEES_351","PELA_161","PELA_162",
                                              "PUTU_108"))) %>% group_by_(.dots = dots) %>% 
      summarise_at(vars(prop_leaf_loss,leaf_shed,leaf_replacement,
                     repro_prop_all_leaf, repro_and_leaf_growth_prop_surplus,prop_leaf_expand, prop_leaf_replacement,
                     gross_inv,prop_repro,prop_surplus, all_leaf_inv,all_leaf_and_repro_inv,surplus_inv,seed_prop_all_acc,
                     growth_leaf_prop_no_accessory,replace_leaf_prop_no_accessory,seed_prop_veg,seed_prop_all,seed_prop_surplus,
                     prop_leaf_loss), f)
  })
  names(out[[2]]) <- fs
  
  out[[3]] <- lapply(fs, function(f) {
    SummaryInd %>% filter(repro_inv > 0) %>% group_by_(.dots = dots) %>% summarise_at(vars(ovule_count, prepollen_count_reach_flowering), f)
  })
  names(out[[3]]) <- fs
  
  out[[4]] <- lapply(fs, function(f) {
    SummaryInd %>%
      filter(seed_count > 0) %>%
      filter(repro_inv > 0) %>%
      group_by_(.dots = dots) %>%
      summarise_at(vars(seed_size, seedset, seed_count, packaging_dispersal_costs,
                     accessory_costs_using_seedweight, scaled_provisioning_costs, success_inv,
                     success_costs, scaled_seed_count, prop_provisioning_vs_success,
                     fruit_costs, prop_discarded_vs_all_repro, zygote_set,
                     postpollen_aborted_inv, propagule_inv, embryo_endo_inv, discarded_costs,
                     scaled_reach_flowering_count, accessory_inv, prop_pack_disp_vs_success,
                     prepollen_discarded_costs, prepollen_all_costs, accessory_costs,
                     prop_embryo_endo_vs_success, postpollen_aborted_costs,
                     propagule_costs, repro_costs, prop_postpollen_success, discarded_inv,
                     prop_propagule_vs_all_repro, prop_postpollen_all_vs_all_repro,
                     scaled_ovule_count, prop_pollen_attract_vs_success,
                     pollen_attract_costs, prepollen_costs_from_pack_disp_tissues, prepollen_costs_from_seed_tissues,
                     prepollen_inv_aborted_preflowering, prepollen_all_inv, prop_prepollen_success,
                     prop_prepollen_discarded_vs_all_repro, prop_postpollen_discarded_vs_all_repro,
                     prop_pollen_attract_vs_all_repro, prop_pack_disp_vs_all_repro,
                     prop_embryo_endo_vs_all_repro, provisioning_costs, embryo_endo_costs,
                     prop_prepollen_all_vs_all_repro, prop_accessory_vs_embryo_endo,
                     prop_accessory_vs_propagule, prop_success, scaled_repro_inv, 
                     postpollen_all_costs, prop_prepollen_discarded, prop_postpollen_discarded,
                     choosiness, prepollen_success_inv, scaled_pollen_attract_costs), f)
  })
  names(out[[4]]) <- fs
  
  ret <- list()
  for (f in fs) {
    ret[[f]] <- out[[1]][[f]]
    for (i in 2:4) ret[[f]] <- merge(ret[[f]], out[[i]][[f]], by = groups,
                                     all = TRUE)
    
    # reorder to be same as input
    order <- names(SummaryInd)[names(SummaryInd) %in% names(ret[[f]])]
    ret[[f]] <- ret[[f]][, order]
  }
  ret[["se"]] <- ret[["sd"]]
  ii <- sapply(ret[["se"]], is.numeric) & names(ret[["se"]]) != "age"
  ret[["se"]][, ii] <- ret[["se"]][, ii]/sqrt(ret[["length"]][, ii])
  ret
}

filterToMature <- function(data) {
  filter(data, mature == TRUE)
}

scale_individual_variable <- function(SummaryInd, SummarySpp) {
  
  get_species_value <- function(f, v) {
    i <- match(SummaryInd[["species"]], SummarySpp[[f]][["species"]])
    SummarySpp[[f]][[v]][i]
  }
  
  mutate(SummaryInd,
         maxH = get_species_value("max", "height"),
         prop_maxH = height_0/maxH,
         prop_max_weight = total_weight_0/get_species_value("max", "total_weight"),
         prop_max_repro = repro_inv/get_species_value("max", "repro_inv"),
         scaled_growth_stem_diameter = growth_stem_diameter/get_species_value("mean", "growth_stem_diameter"),
         growth_leaf_min = get_species_value("min",  "growth_leaf"),
         ratio_leaf_growth = log10(leaf_area) - log10(leaf_area_0)
  )
  
}
