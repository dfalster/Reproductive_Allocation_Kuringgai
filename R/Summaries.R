process_parts_summary <- function(PartsSummary_all) {
  i <- is.na(weight[[v]])
  weight[[v]][i] <- 0
}

                       
process_LMA <- function(LMA_raw) {
  LMA <- filter(LMA_raw, species!="" & species!=" ") %>%
    select(species, age, LMA, branch_age, leaf_number, leaf_area)

  LMA$leaf_size <- LMA$leaf_area/LMA$leaf_number

  LMA %>%  group_by(species, age) %>%
    summarise_each(funs(mean), LMA, leaf_size)
}

process_leaves_per_length <- function(leavesPerLength_raw) {
    # get average leaves per length for spp
  filter(leavesPerLength_raw, use=="yes") %>%
    mutate(count_per_length = leaf_count/length_mm) %>%
    group_by(species) %>%
    summarise(count_per_length = mean(count_per_length, na.rm=TRUE))
}

process_leaf_lifespan <- function(leafLifespan_raw, leavesPerLength) {


  leafLifespan <- filter(leafLifespan_raw, dont_use!="dead" & dont_use!="dont_use") %>%
    select(species, age, individual, shoot_diameter_start, shoot_diameter_end, shoot_length_start, growth_shoot_length, shoot_leaf_count_start_length, shoot_leaf_count_start_count, lvs_end_length, lvs_end_count, shoot_leaf_count_growth_shoot_length, shoot_leaf_count_new_count, shoot_leaf_count_new_and_shed_count, mm_lvs_spec, count_lvs_spec)

  # Calculating Leaf Lifespan

  # where counts did not previously exists, set as 0; CHECK WITH DANIEL THAT "V" IS A SET "NAME" IN THESE FUNCTIONS
  for(v in c("shoot_leaf_count_start_count","lvs_end_count","shoot_leaf_count_new_count","shoot_leaf_count_new_and_shed_count")) {
    i <- is.na(leafLifespan[[v]])
    leafLifespan[[v]][i] <- 0
  }

  # For individuals with "length" measures, translate into count
  # get spp average for all individuals
  leafLifespan$count_per_length <- leavesPerLength$count_per_length[match(leafLifespan$species, leavesPerLength$species)]

  # if possible use data for that individual, if it exists; this code substitutes in species count per length #s for individuals where this data is available

  # TODO Lizzy:  variable mm_lvs does not exist
  #  i <- !is.na(leafLifespan$mm_lvs)
  #  leafLifespan$count_per_length[i] <- (leafLifespan$count_lvs_spec/leafLifespan$mm_lvs)[i]

  #get total leaf counts
  for(v in c("shoot_leaf_count_start_length","shoot_leaf_count_start_count","lvs_end_length","lvs_end_count","shoot_leaf_count_growth_shoot_length","shoot_leaf_count_new_count","shoot_leaf_count_new_and_shed_count")) {
    i <- is.na(leafLifespan[[v]])
    leafLifespan[[v]][i] <- 0
   }

  #calculating leaf lifespan
  leafLifespan <- mutate(leafLifespan,
    shoot_leaf_count_start = shoot_leaf_count_start_count + (shoot_leaf_count_start_length * count_per_length),
    lvs_end = lvs_end_count + (lvs_end_length * count_per_length),
    shoot_leaf_count_new = shoot_leaf_count_new_count + (shoot_leaf_count_growth_shoot_length * count_per_length),
    LL_bd = (shoot_leaf_count_start)/(((shoot_leaf_count_start)-(lvs_end)) + (shoot_leaf_count_new)/2),
    LL_death = (shoot_leaf_count_start)/((shoot_leaf_count_start)-(lvs_end)),
    LL_birth = (shoot_leaf_count_start)/(shoot_leaf_count_new),
    growth_shoot_diameter = (shoot_diameter_end - shoot_diameter_start),
    growth_shoot_area = (3.14*((shoot_diameter_end/2)^2)) - (3.14*((shoot_diameter_start/2)^2)),
    shoot_leaf_count = lvs_end + shoot_leaf_count_new
    )

  #TODO Lizzy: why do we need this next line?
  i <- which(is.infinite(leafLifespan$LL_death))
  for(v in c("LL_death", "LL_bd", "LL_birth")) {
    leafLifespan[[v]][i] <- NA
  }
  select(leafLifespan, -shoot_leaf_count_start_length,
    -shoot_leaf_count_start_count, -lvs_end_length, -lvs_end_count,
    -shoot_leaf_count_growth_shoot_length, -shoot_leaf_count_new_count,
    -mm_lvs_spec, -count_lvs_spec, -count_per_length, -lvs_end)
}

process_wood_density <- function(wood_density_spp) {
  # wood density summary by species
  wood <- filter(wood_density_spp, use=="use") %>%
    select(species, density) %>%
    group_by(species) %>%
    summarise_each(funs(mean), density)
  names(wood)<-c("species","wood_density")
  wood
}

combine_by_individual <- function(IndividualsList, Growth_all, Accessory_counts_all, ReproductiveCosts_all, LMA, leafLifespan, wood_density_spp, seedsize) {

  #adding investment and accessory costs data to leafLifespan dataframe to create a dataframe with all individual level data
  SummaryInd <- merge(Growth_all, select(leafLifespan, -species, -age), by="individual", all=TRUE)
  SummaryInd <- merge(SummaryInd, select(ReproductiveCosts_all, -species, -age), by="individual", all=TRUE)
  SummaryInd <- merge(SummaryInd, select(IndividualsList, individual, mature), by="individual", all=TRUE)
  SummaryInd <- merge(SummaryInd, Accessory_counts_all, by="individual", all=TRUE)
  SummaryInd <- merge(SummaryInd, LMA, by=c("species","age"), all=TRUE)
  SummaryInd <- merge(SummaryInd, wood_density_spp, by=c("species"), all=TRUE)
  SummaryInd <- merge(SummaryInd, seedsize, by=c("species"), all=TRUE)

  #TODO: Why do we need these, where are NAs coming from?
  SummaryInd <- filter(SummaryInd, !is.na(species) & !is.na(individual))

  for(v in c("seed_count","seedset","propagule_inv" , "seedpod_weight" , "fruit_weight","accessory_inv")) {
    i <- is.na(SummaryInd[[v]])
    SummaryInd[[v]][i] <- 0
    }
  
  #DANIEL TODO: For the various "per_seed" measures and other accessory costs, need to include only individuals that have non-zero seed count, otherwise combining lots of zeros with real numbers and mean is "meaningless"
  SummaryInd <- SummaryInd %>% mutate(
    total_inv = repro_inv + growth_inv,
    total_weight_0 = total_weight - growth_inv,
    stem_weight_0 = stem_weight - growth_stem,
    leaf_weight_0 = leaf_weight - growth_leaf,
    height_0 = height - growth_height,
    diameter_0 = diameter - growth_stem_diameter,
    growth_height_log = log10(height)-log10(height_0),
    RA = repro_inv/total_inv,
    RGR = log(total_weight)-log(total_weight - growth_inv),
    leaf_area = leaf_weight / (1000*LMA),
    growth_leaf_area = growth_leaf / (1000*LMA),
    leaf_area_0 = leaf_area - growth_leaf_area,
    growth_leaf_area_log = log10(leaf_area) - log10(leaf_area_0),
    shoot_leaf_area = shoot_leaf_count*leaf_size,
    shoot_growth_leaf_area = shoot_leaf_count_new*leaf_size,
    shoot_leaf_area_0 = shoot_leaf_area - shoot_growth_leaf_area,
    reproducing = RA>0,
    prop_allocation = propagule_inv/(growth_inv + repro_inv),
    RA_leaf_area = repro_inv / (repro_inv + growth_leaf),
    leaf_area_midyear = (leaf_area_0 + leaf_area)/2,
    count_aborted = repro_all_count - seed_count,
    prepollen_costs = prepollen_partial_costs + pack_disp_early_costs,
    pack_disp_costs = pack_disp_gross_costs - pack_disp_early_costs - seed_early_costs,  #subtract early
    prepollen_all_inv = prepollen_aborted_inv + prepollen_success_inv, #prepollen_success_inv is also flowers, so much bigger than investment in flowers that become seeds
    prepollen_failure_inv = prepollen_all_inv - (seed_count*prepollen_costs), #failure is all parts that don't progress to seeds; aborted is parts that don't reach flowering
    postpollen_all_inv = postpollen_aborted_inv + packaging_dispersal_inv,
    postpollen_aborted_inv = postpollen_all_inv - (seed_count*pack_disp_costs), #so energy of empty seedpods becomes part of aborted
    repro_inv_per_seed = repro_inv / seed_count,
    accessory_per_seed = accessory_inv/seed_count,
    propagule_per_seed = propagule_inv/seed_count,
    prepollen_all_per_seed = prepollen_all_inv/seed_count,
    prepollen_aborted_per_seed = prepollen_aborted_inv/seed_count, #aborted is parts that don't reach open flower stage
    prepollen_failure_per_seed = prepollen_failure_inv/seed_count, #failure is parts that don't progress to seeds
    prepollen_success_per_seed = prepollen_success_inv/seed_count, #success is parts that DO reach open flower stage
    postpollen_aborted_per_seed = postpollen_aborted_inv/seed_count, #all parts that aren't pack & dispersal for successful seeds
    packaging_dispersal_per_seed = packaging_dispersal_inv/seed_count,
    flower_inv = prepollen_costs*repro_all_count, #a proxy measure showing good way to estimate total repro inv; doesn't have exact meaning since "repro all count" includes aborted buds 
    fruit_inv = propagule_inv + (seed_count*seedpod_weight),
    success_inv = seed_costs*seed_count,
    failure_inv = repro_inv - success_inv,
    failure_to_ovule_ratio = 1 - seedset,
    failure_per_seed = prepollen_failure_per_seed + postpollen_aborted_per_seed,
    prop_success = success_inv/ repro_inv,
    prop_failure = failure_inv/ repro_inv,
    prop_propagule = propagule_inv/repro_inv,
    prop_prepollen_success = prepollen_success_inv/repro_inv,
    prop_prepollen_all = prepollen_all_inv/repro_inv,
    prop_accessory = 1 - prop_propagule,
    prop_propagule_nonzero = prop_propagule, #for taking species means on non-zero numbers only
    prop_accessory_nonzero = prop_accessory, #for taking species means on non-zero numbers only
    prop_prepollen_costs = prepollen_costs/seed_costs,
    prop_postpollencosts = 1 - prop_prepollen_costs,
    prop_seed_costs = seed_size/seed_costs,
    prop_dispersalcosts = pack_disp_costs / seed_costs,
    scaled_failure_count = count_aborted / total_weight,
    scaled_seed_count = seed_count / total_weight,
    scaled_bud_count = repro_all_count / total_weight,
    scaled_repro_inv = repro_inv/ total_weight,
    repro_inv_mature = repro_inv,
    repro_all_count_mature = repro_all_count,
    leaf_area_0_mature = leaf_area_0,
    provisioning = ((seed_size - embryo_endo_size)*seed_count) + packaging_dispersal_inv,
    provisioning_per_seed = pack_disp_costs - embryo_endo_size,
    postpollen_all_per_seed = postpollen_aborted_per_seed + seed_size + packaging_dispersal_per_seed
    )

  #if seedset is low, prepollen costs increase, because the cost of producing pollen across the whole plant is higher per seed matured  
  #if there are very few fruits aborted post-pollination the cost of aborted post-pollination tissues is very low
  
   
  
  for(v in c("seed_costs","prepollen_costs","count_aborted","flower_inv","fruit_inv","prepollen_all_inv","prepollen_success_inv",
              "prepollen_failure_inv","repro_inv_per_seed","accessory_per_seed","propagule_per_seed","prepollen_all_per_seed",
              "prepollen_aborted_per_seed","prepollen_failure_per_seed", "pack_disp_costs",
              "prepollen_success_per_seed","postpollen_aborted_per_seed",
              "packaging_dispersal_per_seed","success_inv","failure_inv","failure_to_ovule_ratio","prop_success","prop_failure",
              "prop_propagule","prop_postpollen_aborted","prop_prepollen_success","prop_prepollen_aborted","prop_prepollen_all",
              "prop_accessory","prop_propagule_nonzero","prop_accessory_nonzero","prop_prepollen_costs","prop_seed_costs",
              "prop_dispersalcosts","scaled_failure_count","scaled_seed_count","scaled_bud_count","embryo_endo_size",
             "scaled_repro_inv","prop_postpollencosts","failure_per_seed","leaf_area_0","leaf_area_0_mature","total_weight_0","postpollen_all_per_seed")) {
    i <- is.na(SummaryInd[[v]])
    SummaryInd[[v]][i] <- 0
    i <- is.infinite(SummaryInd[[v]])
    SummaryInd[[v]][i] <- 0
  }
  
  
    years_reproducing <- function(RA, age) {
      ret <- age-min(age[RA>0])
      ret[ret<0] <-0
#      if(any(is.na(ret))) browser()
      ret
    }
  
  SummaryInd <-  SummaryInd %>% 
    group_by(species) %>%
    mutate(years_repro = years_reproducing(RA, age)) %>%
    ungroup()
  

  SummaryInd

}

# re-orders columns in data to match order of values in "variable" column of 
# variable_list
sort_by_variable <- function(data, variable_list){

  vars <- variable_list$variable[variable_list$variable %in% names(data)]
  vars <- base::union(vars, names(data))
  data[,vars]
}

get_species_values <- function(SummaryInd, groups) {

  # function to apply
  fs <- c("max", "min","mean", "sd", "length")

  # note use of `group_by_` below - allows for passing in of
  # character vectors
  dots <-  lapply(groups, as.symbol)

  # summarizing data by species, age
  out <- list()
  out[[1]] <-lapply(fs, function(f) {
    SummaryInd %>%
    group_by_(.dots=dots) %>%
    summarise_each(f, seed_size,height, growth_inv, total_weight, total_weight_0,total_inv, RA, RA_leaf_area, diameter, stem_area,
      leaf_weight, stem_weight, growth_stem_diameter, growth_stem_area, growth_leaf,
      growth_stem, diameter, diameter_0,LMA, wood_density,leaf_area,leaf_area_0,leaf_area_midyear,
      repro_all_count,repro_inv)
  })
  names(out[[1]]) <- fs

  out[[2]] <-lapply(fs, function(f) {
    SummaryInd %>%
    filter(LL_bd > 0 & LL_bd < 6 & LL_birth < 8 & LL_death < 8) %>%
    group_by_(.dots=dots) %>%
    summarise_each(f, LL_bd, LL_death, LL_birth, growth_shoot_length,
      shoot_leaf_count, growth_shoot_diameter, growth_shoot_area, RGR, shoot_diameter_end)
  })
  names(out[[2]]) <- fs

  out[[3]] <-lapply(fs, function(f) {
    SummaryInd %>%
    filter(repro_inv != 0) %>%
    group_by_(.dots=dots) %>%
    summarise_each(f, flower_count)
  })
  names(out[[3]]) <- fs

  out[[4]] <-lapply(fs, function(f) {
    SummaryInd %>%
    filter(repro_inv > 0) %>%
    group_by_(.dots=dots) %>%
    summarise_each(f, fruit_weight,repro_inv_mature,repro_all_count_mature,leaf_area_0_mature)
  })
  names(out[[4]]) <- fs

  out[[5]] <-lapply(fs, function(f) {
    SummaryInd %>%
    filter(seed_count > 0) %>%
    filter(repro_inv >0 ) %>%
    group_by_(.dots=dots) %>%
    summarise_each(f, propagule_per_seed, packaging_dispersal_per_seed,seedset,seed_count, pack_disp_costs,
      prop_propagule_nonzero,prop_accessory_nonzero,success_inv,seed_costs,scaled_seed_count,prop_postpollencosts,
      postpollen_aborted_inv, packaging_dispersal_inv, propagule_inv, prop_postpollen_aborted, prop_propagule,
      prop_dispersalcosts,prop_seed_costs, prepollen_failure_per_seed,prepollen_all_per_seed,accessory_per_seed,
      prepollen_aborted_per_seed, prepollen_success_per_seed, postpollen_aborted_per_seed,repro_inv_per_seed,failure_inv,
      scaled_failure_count,scaled_bud_count,prop_prepollen_costs,prepollen_costs,embryo_endo_size,
      accessory_inv, prepollen_aborted_inv, prepollen_success_inv,prepollen_all_inv,prop_prepollen_aborted, prop_prepollen_success,
      prop_prepollen_all, prop_accessory,prop_failure,prop_success,scaled_repro_inv,failure_to_ovule_ratio,failure_per_seed,postpollen_all_per_seed)
  })
  names(out[[5]]) <- fs

 ret <- list()
  for(f in fs){
    ret[[f]] <- out[[1]][[f]]
    for(i in 2:5)
      ret[[f]] <- merge(ret[[f]], out[[i]][[f]], by=groups, all=TRUE)

    # reorder to be same as input
    order <- names(SummaryInd)[names(SummaryInd) %in% names(ret[[f]])]
    ret[[f]] <- ret[[f]][, order]
  }
  ret[["se"]] <- ret[["sd"]]
  ii <- sapply(ret[["se"]], is.numeric) & names(ret[["se"]]) != "age"
  ret[["se"]][,ii] <- ret[["se"]][,ii]/sqrt(ret[["length"]][,ii])
  ret
}

filterToMature <- function(data) {
  filter(data, mature==TRUE)
}

scale_individual_variable <- function(SummaryInd, SummarySpp) {

    get_species_value <- function(f, v) {
      i <- match(SummaryInd[["species"]], SummarySpp[[f]][["species"]])
      SummarySpp[[f]][[v]][i]
    }

      mutate(SummaryInd,
        maxH =  get_species_value("max", "height"),
        prop_maxH = height_0 / maxH,
        prop_max_weight = total_weight_0 / get_species_value("max", "total_weight"),
        prop_max_repro = repro_inv / get_species_value("max", "repro_inv"),
        scaled_growth_stem_diameter = growth_stem_diameter / get_species_value("mean", "growth_stem_diameter"),
        scaled_growth_shoot_diameter = growth_shoot_diameter / get_species_value("mean", "growth_shoot_diameter"),
        growth_leaf_min = get_species_value("min","growth_leaf"),
        ratio_leaf_growth = log10(leaf_area) - log10(leaf_area_0),
        repro_inv_nonzero = repro_inv + 1
      )

}