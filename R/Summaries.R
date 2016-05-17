
process_seed_costs <- function(seedSize_raw, PartsSummary_all) {

  seedsize <- seedSize_raw %>%
      select(species, seed_size)

  #all of these calculations should really be done on the individual level, since different scaling per individual and different parts weights
  # adding info on all costs to produce 1 seed
  seedcosts <- list()
  seedcosts[["BAER"]] <- list(parts=c("cone_brown","cone_base_brown","flower_petals","seed_pod","seed","flower_style"), scale=c(1/60,1/60,1/2,1/2,1,1/2))
  seedcosts[["BOLE"]] <- list(parts=c("late_flower_petals","seed_pod","seed","late_finished_flower","pedicel"), scale=c(1/4,1,1,1/4))
  seedcosts[["COER"]] <- list(parts=c("inflorescence_stalk_in_fruit","flower_petals", "bract_fruit","fruit_mature"), scale=c(1/6,1,1,1))
  seedcosts[["EPMI"]] <- list(parts=c("flower_petals", "seed_pod","seed"), scale=c(1/15,1/15,1))
  seedcosts[["GRBU"]] <- list(parts=c("inflorescence_stalk","flower_petals","pedicel","seed_pod","seed"), scale=c(1/20,1/2,1/2,1/2,1))
  seedcosts[["GRSP"]] <- list(parts=c("inflorescence_stalk_in_fruit","flower_petals","pedicel","seed_pod","seed"), scale=c(1/20,1/2,1/2,1/2,1))
  seedcosts[["HATE"]] <- list(parts=c("inflorescence_bud_big_bracts","flower_petals","seed_pod","seed"), scale=c(1/2,1/2,1/2,1))
  seedcosts[["HEPU"]] <- list(parts=c("flower_petals","calyx_fruit","fruit_mature"), scale=c(1/4,1/4,1/4))
  seedcosts[["LEES"]] <- list(parts=c("flower_petals","calyx_fruit","fruit_mature"), scale=c(1,1,1))
  seedcosts[["PELA"]] <- list(parts=c("flower_petals","pedicel","seed_pod","seed"), scale=1)
  seedcosts[["PEPU"]] <- list(parts=c("cone_brown","flower_petals","flower_calyx","fruit_mature"), scale=c(1/40,1,1,1))
  seedcosts[["PHPH"]] <- list(parts=c("flower_petals","flower_calyx","bract_flower_or_finished_flower","seed_pod","seed"), scale=c(1/2,1/2,1/2,1/2,1))
  seedcosts[["PILI"]] <- list(parts=c("inflorescence_stalk","bract_flower_or_finished_flower","flower_petals","flower_calyx","seed_pod","seed"), scale=c(1/10,1/10,1/10,1/10,1,1))
  seedcosts[["PUTU"]] <- list(parts=c("flower_petals","flower_calyx","bract_flower_or_finished_flower","seed_pod","seed"), scale=c(1/2,1/2,1/2,1/2,1))

  species <- names(seedcosts)

  costs <- data.frame(species = species,
    seedcosts = sapply(names(seedcosts), function(sp) {
                      x <- filterBySpecies(sp, PartsSummary_all)
                      sum(x$weight[x$part %in% seedcosts[[sp]][["parts"]]]
                          * seedcosts[[sp]][["scale"]])
                    }))
  seedsize <- merge(seedsize, costs, by="species", all.x=TRUE)
  #seedsize$repro_inv_per_seed <- (seedsize$seedcosts - seedsize$seed_size)/(seedsize$seedcosts)
  seedsize
}

process_prepollination_costs <- function(seedsize, PartsSummary_all) {
 

# adding info on all costs to produce flower at time of pollination
  prepollencosts_spp <- list()
  prepollencosts_spp[["BAER"]] <- list(parts=c("cone_green","flower_petals","flower_stigma","flower_style"), scale=c(1/60,1/2,1/2,1/2))
  prepollencosts_spp[["BOLE"]] <- list(parts=c("flower_petals","flower_calyx","pedicel"), scale=c(1/4,1/4,1/4))
  prepollencosts_spp[["COER"]] <- list(parts=c("inflorescence_stalk","flower_petals", "flower_stigma"), scale=c(1/6,1,1))
  prepollencosts_spp[["EPMI"]] <- list(parts=c("flower_petals", "flower_calyx"), scale=c(1/15,1/15))
  prepollencosts_spp[["GRBU"]] <- list(parts=c("inflorescence_stalk","flower_petals","flower_stigma"), scale=c(1/20,1/2,1/2))
  prepollencosts_spp[["GRSP"]] <- list(parts=c("inflorescence_stalk","flower_petals","flower_stigma"), scale=c(1/20,1/2,1/2))
  prepollencosts_spp[["HATE"]] <- list(parts=c("inflorescence_bud_big_bracts","flower_petals","flower_stigma"), scale=c(1/2,1/2,1/2))
  prepollencosts_spp[["HEPU"]] <- list(parts=c("flower_petals","flower_calyx"), scale=c(1/4,1/4))
  prepollencosts_spp[["LEES"]] <- list(parts=c("flower_petals","flower_calyx"), scale=c(1,1))
  prepollencosts_spp[["PELA"]] <- list(parts=c("flower_petals","pedicel","flower_stigma"), scale=c(1,1,1))
  prepollencosts_spp[["PEPU"]] <- list(parts=c("cone_green","flower_petals","flower_calyx","flower_stigma"), scale=c(1/200,1,1,1))
  prepollencosts_spp[["PHPH"]] <- list(parts=c("flower_petals","flower_calyx","bract_flower_or_finished_flower","flower_stigma"), scale=c(1/2,1/2,1/2,1/2))
  prepollencosts_spp[["PILI"]] <- list(parts=c("inflorescence_stalk","bract_flower_or_finished_flower","flower_petals","flower_calyx","flower_stigma"), 
                                       scale=c(1/10,1/10,1,1,1))
  prepollencosts_spp[["PUTU"]] <- list(parts=c("flower_petals","flower_calyx","bract_flower_or_finished_flower","flower_stigma"), scale=c(1/2,1/2,1/2,1/2))
  
  species <- names(prepollencosts_spp)
  costs <- data.frame(species = species,
                      prepollencosts_spp = sapply(names(prepollencosts_spp), function(sp) {
                        x <- filterBySpecies(sp, PartsSummary_all)
                       sum(x$weight[x$part %in% prepollencosts_spp[[sp]][["parts"]]]
                            * prepollencosts_spp[[sp]][["scale"]])
                      }))
  View(PartsSummary_all)
  seedsize <- merge(seedsize, costs, by="species", all.x=TRUE)
  seedsize
}


process_dispersal_costs <- function(seedsize, PartsSummary_all) {
dispersalcosts <- list()
dispersalcosts[["BAER"]] <- list(parts=c("cone_brown","seed_pod"), scale=c(1/60,1/2))
dispersalcosts[["BOLE"]] <- list(parts=c("late_finished_flower","seed_pod","late_flower_petals"), scale=c(1/4,1,1/4))
dispersalcosts[["COER"]] <- list(parts=c("inflorescence_stalk_in_fruit","bract_fruit"), scale=c(1/6,1))
dispersalcosts[["EPMI"]] <- list(parts=c("seed_pod"), scale=c(1/15))
dispersalcosts[["GRBU"]] <- list(parts=c("inflorescence_stalk","pedicel","seed_pod"), scale=c(1/20,1/2,1/2))
dispersalcosts[["GRSP"]] <- list(parts=c("inflorescence_stalk_in_fruit","pedicel","seed_pod"), scale=c(1/20,1/2,1/2))
dispersalcosts[["HATE"]] <- list(parts=c("seed_pod"), scale=c(1/2))
dispersalcosts[["HEPU"]] <- list(parts=c("calyx_fruit"), scale=c(1/4))
dispersalcosts[["LEES"]] <- list(parts=c("calyx_fruit"), scale=c(1))
dispersalcosts[["PELA"]] <- list(parts=c("seed_pod"), scale=c(1))
dispersalcosts[["PEPU"]] <- list(parts=c("cone_brown"), scale=c(1/40))
dispersalcosts[["PHPH"]] <- list(parts=c("seed_pod"), scale=c(1/2))
dispersalcosts[["PILI"]] <- list(parts=c("seed_pod"), scale=c(1))
dispersalcosts[["PUTU"]] <- list(parts=c("seed_pod"), scale=c(1/2))
  species <- names(dispersalcosts)
  costs <- data.frame(species = species,
                      dispersalcosts = sapply(names(dispersalcosts), function(sp) {
                        x <- filterBySpecies(sp, PartsSummary_all)
                        sum(x$weight[x$part %in% dispersalcosts[[sp]][["parts"]]]
                            * dispersalcosts[[sp]][["scale"]])
                      }))

#note, for some species a decimal fraction is given, reflecting that the part turns both into a dispersal component and the actual propagule. 
#I'm not sure how to figure out the correct value to use

dispersal_at_pollination <- list()
dispersal_at_pollination[["BAER"]] <- list(parts=c("cone_green","flower_stigma"), scale=c(1/60,1/2))
dispersal_at_pollination[["BOLE"]] <- list(parts=c("flower_calyx","flower_petals"), scale=c(1/4,1/4))
dispersal_at_pollination[["COER"]] <- list(parts=c("inflorescence_stalk","flower_stigma"), scale=c(1/6,1/2))
dispersal_at_pollination[["EPMI"]] <- list(parts=c("flower_calyx"), scale=c(1/15))
dispersal_at_pollination[["GRBU"]] <- list(parts=c("inflorescence_stalk","flower_stigma"), scale=c(1/20,1/2))
dispersal_at_pollination[["GRSP"]] <- list(parts=c("inflorescence_stalk","flower_stigma"), scale=c(1/20,1/2))
dispersal_at_pollination[["HATE"]] <- list(parts=c("flower_stigma"), scale=c(1/2))
dispersal_at_pollination[["HEPU"]] <- list(parts=c("flower_calyx"), scale=c(1/4))
dispersal_at_pollination[["LEES"]] <- list(parts=c("flower_calyx"), scale=c(1))
dispersal_at_pollination[["PELA"]] <- list(parts=c("flower_stigma"), scale=c(1))
dispersal_at_pollination[["PEPU"]] <- list(parts=c("cone_green","flower_stigma"), scale=c(1/40,1))
dispersal_at_pollination[["PHPH"]] <- list(parts=c("flower_stigma"), scale=c(1/2))
dispersal_at_pollination[["PILI"]] <- list(parts=c("flower_stigma"), scale=c(1))
dispersal_at_pollination[["PUTU"]] <- list(parts=c("flower_stigma"), scale=c(1/2))
  species <- names(dispersal_at_pollination)
  costs2 <- data.frame(species = species,
                     dispersal_at_pollination = sapply(names(dispersal_at_pollination), function(sp) {
                      x <- filterBySpecies(sp, PartsSummary_all)
                      sum(x$weight[x$part %in% dispersal_at_pollination[[sp]][["parts"]]]
                          * dispersal_at_pollination[[sp]][["scale"]])
                    }))

  seedsize <- merge(seedsize, costs, by="species", all.x=TRUE)
  seedsize <- merge(seedsize, costs2, by="species", all.x=TRUE)
  seedsize
}

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

  SummaryInd$repro_inv = SummaryInd$prepollen_aborted_inv + SummaryInd$prepollen_success_inv + SummaryInd$postpollen_aborted_inv + SummaryInd$packaging_dispersal_inv + SummaryInd$propagule_inv
  
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
    prepollencosts = prepollencosts_spp,
    count_aborted = repro_all_count - seed_count,
    flower_inv = prepollencosts*repro_all_count,    
    fruit_inv = propagule_inv + (seed_count*seedpod_weight),
    prepollen_all_inv = prepollen_aborted_inv + prepollen_success_inv,
    prepollen_success_inv = seed_count*prepollencosts,  
    prepollen_failure_inv = prepollen_all_inv - prepollen_success_inv,
    repro_inv_per_seed = repro_inv / seed_count,
    accessory_per_seed = accessory_inv/seed_count,
    propagule_per_seed = propagule_inv/seed_count,
    prepollen_all_per_seed = prepollen_all_inv/seed_count,
    prepollen_aborted_per_seed = prepollen_aborted_inv/seed_count,
    prepollen_failure_per_seed = prepollen_failure_inv/seed_count,
    prepollen_success_per_seed = prepollencosts,
    postpollen_aborted_per_seed = postpollen_aborted_inv/seed_count,
    packaging_dispersal_per_seed = (packaging_dispersal_inv - (seed_count*dispersal_at_pollination))/seed_count,
    seedcosts = seed_size + packaging_dispersal_per_seed + prepollencosts,
    success_inv = seedcosts*seed_count,
    failure_inv = repro_inv - success_inv,
    failure_to_ovule_ratio = 1 - seedset,
    failure_per_seed = prepollen_failure_per_seed + postpollen_aborted_per_seed,
    prop_success = success_inv/ repro_inv,
    prop_failure = failure_inv/ repro_inv,
    prop_propagule = propagule_inv/repro_inv,
    prop_postpollen_aborted = postpollen_aborted_inv/repro_inv,
    prop_prepollen_success = prepollen_success_inv/repro_inv,
    prop_prepollen_aborted = prepollen_aborted_inv/repro_inv,
    prop_prepollen_all = prepollen_all_inv/repro_inv,
    prop_accessory = 1 - prop_propagule,
    prop_propagule_nonzero = prop_propagule,
    prop_accessory_nonzero = prop_accessory,
    prop_prepollencosts = prepollencosts/seedcosts,
    prop_postpollencosts = 1 - prop_prepollencosts,
    prop_seedcosts = seed_size/seedcosts,
    prop_dispersalcosts = 1-prop_prepollencosts-prop_seedcosts,
    prop_dispersalcosts2 = packaging_dispersal_per_seed / seedcosts,
    scaled_failure_count = count_aborted / total_weight,
    scaled_seed_count = seed_count / total_weight,
    scaled_bud_count = repro_all_count / total_weight,
    scaled_repro_inv = repro_inv/ total_weight,
    repro_inv_mature = repro_inv,
    repro_all_count_mature = repro_all_count,
    leaf_area_0_mature = leaf_area_0,
    postpollen_all_per_seed = postpollen_aborted_per_seed + seed_size + packaging_dispersal_per_seed
    )

  #if seedset is low, prepollen costs increase, because the cost of producing pollen across the whole plant is higher per seed matured  
  #if there are very few fruits aborted post-pollination the cost of aborted post-pollination tissues is very low
  
   
  
  for(v in c("seedcosts","prepollencosts","count_aborted","flower_inv","fruit_inv","prepollen_all_inv","prepollen_success_inv",
              "prepollen_failure_inv","repro_inv_per_seed","accessory_per_seed","propagule_per_seed","prepollen_all_per_seed",
              "prepollen_aborted_per_seed","prepollen_failure_per_seed","prepollen_success_per_seed","postpollen_aborted_per_seed",
              "packaging_dispersal_per_seed","success_inv","failure_inv","failure_to_ovule_ratio","prop_success","prop_failure",
              "prop_propagule","prop_postpollen_aborted","prop_prepollen_success","prop_prepollen_aborted","prop_prepollen_all",
              "prop_accessory","prop_propagule_nonzero","prop_accessory_nonzero","prop_prepollencosts","prop_seedcosts",
              "prop_dispersalcosts","prop_dispersalcosts2","scaled_failure_count","scaled_seed_count","scaled_bud_count",
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
    summarise_each(f, propagule_per_seed, packaging_dispersal_per_seed,seedset,seed_count,
      prop_propagule_nonzero,prop_accessory_nonzero,success_inv,seedcosts,scaled_seed_count,prop_postpollencosts,
      postpollen_aborted_inv, packaging_dispersal_inv, propagule_inv, prop_postpollen_aborted, prop_propagule,
      prop_dispersalcosts,prop_seedcosts,
      prepollen_failure_per_seed,prepollen_all_per_seed,accessory_per_seed,
      prepollen_aborted_per_seed, prepollen_success_per_seed, postpollen_aborted_per_seed,repro_inv_per_seed,failure_inv,
      scaled_failure_count,scaled_bud_count,prop_prepollencosts,prepollencosts,
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