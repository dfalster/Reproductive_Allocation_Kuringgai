

make_summary_species <- function() {

	
}

process_seed_costs <- function(seedSize_raw, PartsSummary_all) {

  seedsize <- seedSize_raw %>%
      select(species, seed_size)

  # adding info on all costs to produce 1 seed
  # Todo: Lizzy you need to replace numbers below with actual names, see examples
  seedcosts <- list() 
  seedcosts[["BAER"]] <- list(parts=NULL, scale=1)  #w[2]/10 + w[4]/10 + w[13] + w[12] + w[19] + w[20]
  seedcosts[["BOLE"]] <- list(parts=NULL, scale=1) #w[5] + w[6] + w[11] + w[15] + w[16]
  seedcosts[["COER"]] <- list(parts=c("inflorescence_stalk_in_fruit","flower_petals", "bract_fruit","fruit_mature"), scale=c(1/6,1,1,1))
  seedcosts[["EPMI"]] <- list(parts=c("flower_petals", "fruit_mature"), scale=1)
  seedcosts[["GRBU"]] <- list(parts=NULL, scale=1) #w[2] + w[7] + w[12] + w[14] + w[15]
  seedcosts[["GRSP"]] <- list(parts=NULL, scale=1) #w[4] + w[9] + w[14] + w[16] + w[18]
  seedcosts[["HATE"]] <- list(parts=NULL, scale=1) #w[4] + w[6] + w[13] + w[16]
  seedcosts[["HEPU"]] <- list(parts=NULL, scale=1) #w[3] + w[10] + w[15]
  seedcosts[["LEES"]] <- list(parts=NULL, scale=1) #w[5] + w[10] + w[13]
  seedcosts[["PELA"]] <- list(parts=NULL, scale=1) #w[4] + w[5] + w[11] + w[12]
  seedcosts[["PEPU"]] <- list(parts=NULL, scale=1) #w[4]/30 + w[9] + w[16]
  seedcosts[["PHPH"]] <- list(parts=NULL, scale=1) #w[4] + w[5] + w[6] + w[15] + w[16]
  seedcosts[["PILI"]] <- list(parts=NULL, scale=1) #w[2]/10 + w[3] + w[5] + w[7] + w[11] + w[12]
  seedcosts[["PUTU"]] <- list(parts=NULL, scale=1) # w[2] + w[3] + w[4] + w[10] + w[11]

  species <- names(seedcosts)

  costs <- data.frame(species = species,
    seedcosts = sapply(names(seedcosts), function(sp) {
                      x <- filterBySpecies(sp, PartsSummary_all)
                      sum(x$weight[x$part %in% seedcosts[[sp]][["parts"]]] 
                          * seedcosts[[sp]][["scale"]]) 
                    }))
  seedsize <- merge(seedsize, costs, by="species", all.x=TRUE)
  seedsize$costs_per_seed <- (seedsize$seedcosts - seedsize$seed_size)/(seedsize$seedcosts)
  seedsize
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
    select(species, age, individual, d_start, d_end, length_start, new_length, lvs_start_length, lvs_start_count, lvs_end_length, lvs_end_count, lvs_new_length, lvs_new_count, new_but_shed_count, mm_lvs_spec, count_lvs_spec)

  # Calculating Leaf Lifespan

  # where counts did not previously exists, set as 0; CHECK WITH DANIEL THAT "V" IS A SET "NAME" IN THESE FUNCTIONS
  for(v in c("lvs_start_count","lvs_end_count","lvs_new_count","new_but_shed_count")) {
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
  for(v in c("lvs_start_length","lvs_start_count","lvs_end_length","lvs_end_count","lvs_new_length","lvs_new_count","new_but_shed_count")) {
    i <- is.na(leafLifespan[[v]])
    leafLifespan[[v]][i] <- 0
   }

  #calculating leaf lifespan
  leafLifespan <- mutate(leafLifespan,
    lvs_start = lvs_start_count + (lvs_start_length * count_per_length),
    lvs_end = lvs_end_count + (lvs_end_length * count_per_length),
    lvs_new = lvs_new_count + (lvs_new_length * count_per_length),
    LL_bd = (lvs_start)/(((lvs_start)-(lvs_end)) + (lvs_new)/2),
    LL_death = (lvs_start)/((lvs_start)-(lvs_end)),
    LL_birth = (lvs_start)/(lvs_new),
    growth_shoot_diam = (d_end - d_start),
    growth_shoot_area = (3.14*((d_end/2)^2)) - (3.14*((d_start/2)^2)),
    lvs_end_total = lvs_end + lvs_new
    )

  #TODO Lizzy: why do we need this next line?
  i <- which(is.infinite(leafLifespan$LL_death))
  for(v in c("LL_death", "LL_bd", "LL_birth")) {
    leafLifespan[[v]][i] <- NA
  }
  leafLifespan
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

combine_by_individual <- function(IndividualsList, ReproductionAllocation_all, Accessory_counts_all, AccessoryCosts_all, LMA, leafLifespan, wood_density_spp, seedsize) {

  #adding investment and accessory costs data to leafLifespan dataframe to create a dataframe with all individual level data
  SummaryInd <- merge(ReproductionAllocation_all, select(leafLifespan, -species, -age), by="individual", all=TRUE)
  SummaryInd <- merge(SummaryInd, select(AccessoryCosts_all, -species, -age), by="individual", all=TRUE)
  SummaryInd <- merge(SummaryInd, select(IndividualsList, individual, mature), by="individual", all=TRUE)
  SummaryInd <- merge(SummaryInd, Accessory_counts_all, by="individual", all=TRUE)
  SummaryInd <- merge(SummaryInd, LMA, by=c("species","age"), all=TRUE)  
  SummaryInd <- merge(SummaryInd, wood_density_spp, by=c("species"), all=TRUE)
  SummaryInd <- merge(SummaryInd, seedsize, by=c("species"), all=TRUE)

  #TODO: Why do we need these, where are NAs coming from?
  SummaryInd <- filter(SummaryInd, !is.na(species) & !is.na(individual))
  
  SummaryInd <- SummaryInd %>% mutate(
    RGR = log(total_weight)-log(total_weight - growth_inv),
    leaf_area = leaf_weight / (1000*LMA),
    leaf_area_growth = growth_leaf / (1000*LMA),
    shoot_leaf_area = lvs_end_total*leaf_size,
    shoot_leaf_area_growth = lvs_new*leaf_size,
    reproducing = RA>0,
    prop_allocation = propagule_inv/(growth_inv + repro_inv),
    fruit_weight = propagule_inv + seedpod_weight + fruit_weight,
    accessory_per_seed = accessory_inv/seed_count,
    propagule_per_seed = propagule_inv/seed_count,
    prepollen_all_per_seed = prepollen_all_inv/seed_count,
    prepollen_aborted_per_seed = prepollen_aborted_inv/seed_count,
    prepollen_success_per_seed = prepollen_success_inv/seed_count,
    postpollen_aborted_per_seed = postpollen_aborted_inv/seed_count,
    packaging_dispersal_per_seed = packaging_dispersal_inv/seed_count)

  SummaryInd
}


get_species_values <- function(SummaryInd, groups) {

  # function to apply
  fs <- c("max", "mean", "sd", "length")

  # note use of `group_by_` below - allows for passing in of 
  # character vectors
  dots <-  lapply(groups, as.symbol)

  # summarizing data by species, age
  out <- list()
  out[[1]] <-lapply(fs, function(f) {
    SummaryInd %>%
    group_by_(.dots=dots) %>%
    summarise_each(f, prepollen_aborted_inv, prepollen_success_inv, 
      postpollen_aborted_inv, packaging_dispersal_inv, propagule_inv, prepollen_all_inv,
      height, growth_inv, repro_inv, total_weight, total_inv, RA, diameter, stem_area, 
      leaf_weight, stem_weight, growth_stem_diam, growth_stem_area, growth_leaf, 
      growth_stem, diameter)
  })
  names(out[[1]]) <- fs

  out[[2]] <-lapply(fs, function(f) {
    SummaryInd %>%
    filter(LL_bd > 0 & LL_bd < 6 & LL_birth < 8 & LL_death < 8) %>%
    group_by_(.dots=dots) %>%
    summarise_each(f, LL_bd, LL_death, LL_birth, new_length, lvs_end, 
      lvs_end_total, growth_shoot_diam, growth_shoot_area, RGR, d_end)
  })  
  names(out[[2]]) <- fs
 
  out[[3]] <-lapply(fs, function(f) {
    SummaryInd %>%
    filter(total_repro_inv != 0) %>%
    group_by_(.dots=dots) %>%
    summarise_each(f, prop_prepollen_aborted, prop_prepollen_success, 
      prop_postpollen_aborted, prop_packaging_dispersal, prop_propagule, 
      prop_prepollen_all, prop_accessory)
  })  
  names(out[[3]]) <- fs

  out[[4]] <-lapply(fs, function(f) {
    SummaryInd %>%
    filter(repro_inv > 0) %>%
    group_by_(.dots=dots) %>%
    summarise_each(f, seedset, seed_count, flower_count,fruit_weight,leaf_area)
  })  
  names(out[[4]]) <- fs

  out[[5]] <-lapply(fs, function(f) {
    SummaryInd %>%
    filter(seed_count > 0) %>%
    group_by_(.dots=dots) %>%
    summarise_each(f, accessory_per_seed, propagule_per_seed, prepollen_all_per_seed,
      prepollen_aborted_per_seed, prepollen_success_per_seed, postpollen_aborted_per_seed, 
      packaging_dispersal_per_seed)
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
      prop_maxH = height / maxH,
      prop_max_weight = total_weight / get_species_value("max", "total_weight"),
      prop_max_repro = repro_inv / get_species_value("max", "repro_inv"),
      scaled_growth_stem_diam = growth_stem_diam / get_species_value("mean", "growth_stem_diam"),
      scaled_growth_shoot_diam = growth_shoot_diam / get_species_value("mean", "growth_shoot_diam")
      ) 
}