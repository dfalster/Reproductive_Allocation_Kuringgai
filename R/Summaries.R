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

combine_by_individual <- function(IndividualsList, Growth_all, ReproductiveCosts_all, LMA, leafLifespan, wood_density_spp, seedsize) {
  
  #adding investment and accessory costs data to leafLifespan dataframe to create a dataframe with all individual level data
  SummaryInd <- merge(Growth_all, select(leafLifespan, -species, -age), by="individual", all=TRUE)
  SummaryInd <- merge(SummaryInd, select(ReproductiveCosts_all, -species, -age), by="individual", all=TRUE)
  SummaryInd <- merge(SummaryInd, select(IndividualsList, individual, mature), by="individual", all=TRUE)
  SummaryInd <- merge(SummaryInd, LMA, by=c("species","age"), all=TRUE)
  SummaryInd <- merge(SummaryInd, wood_density_spp, by=c("species"), all=TRUE)
  SummaryInd <- merge(SummaryInd, seedsize, by=c("species"), all=TRUE)
  
  #TODO: Why do we need these, where are NAs coming from?
  SummaryInd <- filter(SummaryInd, !is.na(species) & !is.na(individual) &age >1)
  
  #DANIEL TODO: For the various "per_seed" measures and other accessory costs, need to include only individuals that have non-zero seed count, otherwise combining lots of zeros with real numbers and mean is "meaningless"
  SummaryInd <- SummaryInd %>% mutate(
    growth_inv = growth_stem + growth_leaf,
    total_inv = repro_inv + growth_inv,
    leaf_repro_inv = repro_inv + growth_leaf,
    total_weight_0 = total_weight - growth_inv,
    stem_weight_0 = stem_weight - growth_stem,
    leaf_weight_0 = leaf_weight - growth_leaf,
    height_0 = height - growth_height,
    diameter_0 = diameter - growth_stem_diameter,
    RA = repro_inv/total_inv,
    RGR = log(total_weight)-log(total_weight - growth_inv),
    leaf_area = leaf_weight / (1000*LMA),
    growth_leaf_area = growth_leaf / (1000*LMA),
    leaf_area_0 = leaf_area - growth_leaf_area,
    growth_leaf_area_log = log10(leaf_area) - log10(leaf_area_0),
    growth_leaf_log = log10(leaf_weight) - log10(leaf_weight_0),
    shoot_leaf_area = shoot_leaf_count*leaf_size,
    shoot_growth_leaf_area = shoot_leaf_count_new*leaf_size,
    shoot_leaf_area_0 = shoot_leaf_area - shoot_growth_leaf_area,
    prop_shoot_growth_leaf_area = shoot_growth_leaf_area / shoot_leaf_area_0,
    leaf_shed = leaf_weight_0 / LL_death,
    leaf_inv_gross = leaf_shed + growth_leaf,
    growth_leaf_neg = growth_leaf,
    RA_leaf_area = repro_inv / (repro_inv + growth_leaf),
    leaf_area_midyear = (leaf_area_0 + leaf_area)/2,
    leaf_area_0_mature = leaf_area_0 #included, because when summarizing by species, this averages leaf area only for individuals that are reproducing
  )
  
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
    flower_inv2 = pollen_attract_costs*prepollen_count_reach_flowering,
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
    scaled_discarded_count = divide_zero(aborted_ovule_count,leaf_area_0),
    scaled_seed_count = divide_zero(seed_count,leaf_area_0),
    scaled_ovule_count = divide_zero(ovule_count,leaf_area_0),
    scaled_reach_flowering_count = divide_zero(prepollen_count_reach_flowering,leaf_area_0),
    scaled_repro_inv = divide_zero(repro_inv,leaf_area_0),
    scaled_pollen_attract_costs = divide_zero(pollen_attract_costs,leaf_area_0),
    scaled_provisioning_costs = divide_zero(provisioning_costs,leaf_area_0),
    discarded_to_ovule_ratio = 1 - seedset
  )
  
  
   for (i in 1:length(SummaryInd$individual)) {
     if (SummaryInd$growth_leaf[i] < 0) {
       SummaryInd$growth_leaf_neg[i] <- SummaryInd$growth_leaf_neg[i]
       SummaryInd$growth_leaf_pos[i] <- 0
     }else{
       SummaryInd$growth_leaf_pos[i] <- SummaryInd$growth_leaf[i]
       SummaryInd$growth_leaf_neg[i] <- 0  
     }
   }
  
   SummaryInd <- SummaryInd %>% mutate(
     leaf_replacement = leaf_shed + growth_leaf_neg,
     surplus_inv = repro_inv + growth_leaf_pos,
     all_leaf_inv = leaf_replacement + growth_leaf_pos,
     all_leaf_and_repro_inv = repro_inv + leaf_replacement + growth_leaf_pos,
     RA_max_1 = repro_inv / surplus_inv,
     gross_inv = repro_inv + growth_leaf_pos + leaf_replacement + growth_stem,
     prop_repro = repro_inv/gross_inv,
     prop_leaf_expand = surplus_inv /gross_inv,
     prop_leaf_replacement = all_leaf_and_repro_inv/gross_inv,
     prop_surplus = surplus_inv / all_leaf_and_repro_inv,
     RA_vs_all_leaf = repro_inv / all_leaf_and_repro_inv,
     RA_seed = embryo_endo_inv / total_inv,
     prop_stem = 1,
     prop_leaf_replacement_vs_all_leaf = leaf_replacement / all_leaf_inv
    )
  
  #if seedset is low, prepollen costs increase, because the cost of producing pollen across the whole plant is higher per seed matured  
  #if there are very few fruits aborted post-pollination the cost of aborted post-pollination tissues is very low
  
  
  for(v in c("growth_inv","total_inv","leaf_repro_inv","total_weight_0","stem_weight_0","leaf_weight_0","height_0","diameter_0","RA","RGR","leaf_area",
             "growth_leaf_area","leaf_area_0","growth_leaf_area_log","growth_leaf_log","shoot_leaf_area","shoot_growth_leaf_area","shoot_leaf_area_0","prop_shoot_growth_leaf_area",
             "leaf_shed","leaf_inv_gross","growth_leaf_neg","growth_leaf_pos","RA_leaf_area","leaf_area_midyear","leaf_area_0_mature","reproducing","seed_coat_costs",
             "pollen_attract_costs","packaging_dispersal_costs","success_costs","embryo_endo_costs","repro_costs","accessory_costs","prop_postpollen_all_vs_all_repro",
             "accessory_costs_using_seedweight","provisioning_costs","prepollen_all_inv","prepollen_success_inv","prepollen_discarded_inv","postpollen_all_inv",
             "postpollen_aborted_inv","flower_inv","fruit_inv","success_inv","embryo_endo_inv","prepollen_all_costs","discarded_inv","choosiness2","zygote_set",
             "prepollen_discarded_costs","postpollen_aborted_costs","postpollen_all_costs","prop_success","prop_propagule_vs_all_repro","scaled_provisioning_costs",
             "prop_prepollen_all_vs_all_repro","prop_accessory_vs_embryo_endo","prop_accessory_vs_propagule","prop_prepollen_discarded_vs_all_repro","prop_postpollen_discarded_vs_all_repro",
             "prop_pollen_attract_vs_success","prop_provisioning_vs_success","prop_embryo_endo_vs_success","prop_discarded_vs_all_repro","accessory_inv",
             "prop_pack_disp_vs_success","prop_pollen_attract_vs_all_repro","prop_pack_disp_vs_all_repro","prop_embryo_endo_vs_all_repro","prop_postpollen_success","discarded_costs",
             "prop_postpollen_discarded","prop_prepollen_success","prop_prepollen_discarded","scaled_discarded_count","scaled_reach_flowering_count",
             "scaled_seed_count","scaled_ovule_count","scaled_repro_inv","discarded_to_ovule_ratio","leaf_replacement","scaled_pollen_attract_costs",
             "RA_max_1","gross_inv","prop_repro","prop_leaf_expand","prop_leaf_replacement","prop_stem","RA_vs_all_leaf",
             "all_leaf_inv","RA_seed","prop_surplus","surplus_inv","all_leaf_and_repro_inv")) {
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
    summarise_each(f, height, growth_inv, total_weight, total_weight_0,total_inv, RA, RA_leaf_area, stem_area,growth_leaf_pos,prop_surplus,
      leaf_weight, stem_weight, growth_stem_diameter, growth_stem_area, growth_leaf, leaf_shed,leaf_weight_0,stem_weight_0,repro_inv,
      growth_stem, diameter, diameter_0,LMA, wood_density,leaf_area,leaf_area_0,leaf_area_midyear,leaf_replacement,growth_leaf_neg,
      RA_max_1,gross_inv,prop_repro,prop_leaf_expand,prop_leaf_replacement,prop_stem,lifespan,maturity,surplus_inv,all_leaf_and_repro_inv,
      RA_vs_all_leaf,height_0,all_leaf_inv)
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
    summarise_each(f, prepollen_count_reach_flowering)
  })
  names(out[[3]]) <- fs

  out[[4]] <-lapply(fs, function(f) {
    SummaryInd %>%
    filter(repro_inv > 0) %>%
    group_by_(.dots=dots) %>%
    summarise_each(f,ovule_count,leaf_area_0_mature)
  })
  names(out[[4]]) <- fs

  out[[5]] <-lapply(fs, function(f) {
    SummaryInd %>%
    filter(seed_count > 0) %>%
    filter(repro_inv >0 ) %>%
    group_by_(.dots=dots) %>%
    summarise_each(f, seed_size, seedset,seed_count, packaging_dispersal_costs,accessory_costs_using_seedweight,scaled_provisioning_costs,
      success_inv,success_costs,scaled_seed_count,prop_provisioning_vs_success,fruit_costs,prop_discarded_vs_all_repro,zygote_set,asymptotic_RA,
      postpollen_aborted_inv, propagule_inv, embryo_endo_inv,discarded_costs,scaled_reach_flowering_count,choosiness2,accessory_inv,
      prop_pack_disp_vs_success, prepollen_discarded_costs,prepollen_all_costs,accessory_costs,prop_embryo_endo_vs_success,RA_seed,
      postpollen_aborted_costs,repro_costs,prop_postpollen_success,discarded_inv,prop_propagule_vs_all_repro,prop_postpollen_all_vs_all_repro,
      scaled_discarded_count,scaled_ovule_count,prop_pollen_attract_vs_success,pollen_attract_costs,prepollen_costs_from_pack_disp_tissues, prepollen_costs_from_seed_tissues,
      prepollen_inv_aborted_preflowering, prepollen_all_inv,prop_prepollen_success,prop_prepollen_discarded_vs_all_repro,prop_postpollen_discarded_vs_all_repro,
      prop_pollen_attract_vs_all_repro, prop_pack_disp_vs_all_repro,prop_embryo_endo_vs_all_repro,provisioning_costs,embryo_endo_costs,
      prop_prepollen_all_vs_all_repro, prop_accessory_vs_embryo_endo,prop_accessory_vs_propagule,prop_success,scaled_repro_inv,discarded_to_ovule_ratio,
      postpollen_all_costs,prop_prepollen_discarded,prop_postpollen_discarded,choosiness,prepollen_success_inv,scaled_pollen_attract_costs)
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
        ratio_leaf_growth = log10(leaf_area) - log10(leaf_area_0)
      )
      
}