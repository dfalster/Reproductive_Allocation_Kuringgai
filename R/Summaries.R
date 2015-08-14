

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
  LMA
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

  #TODO Lizzy: why do we need this next line?
  leafLifespan$individual <- as.factor(leafLifespan$individual)
  leafLifespan$species <- as.factor(leafLifespan$species)

  #loading this column as character, so using this for now

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
  leafLifespan <- leafLifespan %>% mutate(
    lvs_start = lvs_start_count + (lvs_start_length * count_per_length),
    lvs_end = lvs_end_count + (lvs_end_length * count_per_length),
    lvs_new = lvs_new_count + (lvs_new_length * count_per_length),
    LL_bd = (lvs_start)/(((lvs_start)-(lvs_end)) + (lvs_new)/2),
    LL_death = (lvs_start)/((lvs_start)-(lvs_end)),
    LL_birth = (lvs_start)/(lvs_new)
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
    summarise_each(funs(mean, se, length), density)
  names(wood)<-c("species","WD_mean","WD_se","WD_length")
  wood
}

summarise_by_individual <- function(IndividualsList, ReproductionAllocation_all, Investment_FD_all, AccessoryCosts_all, LMA, leafLifespan, wood_density_spp, seedsize) {

  investment <- ReproductionAllocation_all
  investment$age <- round(investment$age, digits=1)

  accessory <- AccessoryCosts_all

  #adding investment and accessory costs data to leafLifespan dataframe to create a dataframe with all individual level data
  SummaryInd <- merge(investment, select(leafLifespan, -species, -age), by="individual", all.x=TRUE)
  SummaryInd <- merge(SummaryInd, select(accessory, -species, -age), by="individual", all.x=TRUE)
  SummaryInd <- merge(SummaryInd, select(IndividualsList, individual, mature), by="individual", all.x=TRUE)

  #adding variables to look at change across years, RGR
  SummaryInd <- SummaryInd %>% mutate(
    growth_shoot_diam = (d_end - d_start)/100,
    growth_shoot_area = (3.14*((d_end/200)^2)) - (3.14*((d_start/200)^2)),
    lvs_end_total = lvs_end + lvs_new,
    RGR = log(total_weight)-log(total_weight - GrowthInv),
    RGR_leaf = log(leaf_weight)-log(leaf_weight - growth_leaf),
    RGR_stem = log(stem_weight)-log(stem_weight - growth_stem)
  )

  #summarizing data by species, age

  #leaf lifespan summary by species, age
  LL_summary <- SummaryInd %>%
    filter(LL_bd>0 & LL_bd<6 & LL_birth<8 & LL_death<8) %>%
    group_by(species, age) %>%
    summarise_each(funs(mean, se, length), LL_bd, LL_death, LL_birth, new_length, lvs_end, lvs_end_total, growth_shoot_diam, growth_shoot_area, RGR)

  #LMA summary by species, age
  LMA_summary <- LMA %>%
    group_by(species, age) %>%
    summarise_each(funs(mean, se, length), LMA, leaf_size)

  #accessory tissue summary by species, age
  accessory_summary <- SummaryInd %>%
    group_by(species, age) %>%
    summarise_each(funs(mean, se, length), prepollen_aborted_inv, prepollen_success_inv, postpollen_aborted_inv, packaging_dispersal_inv, propagule_inv, prepollen_all_inv)

  accessory_summary2 <- SummaryInd %>%
    filter(total_repro_inv!=0) %>%
    group_by(species, age) %>%
    summarise_each(funs(mean, se, length), prop_prepollen_aborted, prop_prepollen_success, prop_postpollen_aborted, prop_packaging_dispersal, prop_propagule, prop_prepollen_all, prop_accessory)

  #accessory tissue by species
  accessory_spp <- SummaryInd %>%
    filter(total_repro_inv!=0) %>%
    group_by(species) %>%
    summarise_each(funs(mean, se, length), prop_prepollen_aborted, prop_prepollen_success, prop_postpollen_aborted, prop_packaging_dispersal, prop_propagule, prop_prepollen_all, prop_accessory)

  #investment summary by species, age
  investment_summary <- investment %>%
    group_by(species, age) %>%
    summarise_each(funs(mean, se, length), height, GrowthInv, ReproInv, total_weight, TotalInv, RA, diameter, stem_area, leaf_weight, stem_weight, growth_stem_diam, growth_stem_area, growth_leaf, growth_stem)

  investment_spp_age <- investment_summary %>%
    select(species, age, height_mean, RA_mean)
    
  mature_RA <- SummaryInd %>%
    filter(mature==TRUE) %>%
    group_by(species) %>%
    summarise_each(funs(mean, se, length), RA, ReproInv)
  names(mature_RA) <- c("species","mature_RA_mean" ,"ReproInv_mean","mature_RA_se","ReproInv_se","mature_RA_length","ReproInv_length")

  #maximum RA and height for each species/age combination
  maxRA <- investment %>%
    group_by(species, age) %>%
    summarise_each(funs(max), RA, height)

  #leaf lifespan summary by species
  LL_spp1 <- leafLifespan %>%
    filter(age>2 & LL_death!="NA"&LL_death<10) %>%
    group_by(species) %>%
    summarise_each(funs(mean, se, length), LL_death)
  names(LL_spp1)<-c("species","LL_death_mean","LL_death_se","LL_death_length")

  LL_spp2 <- leafLifespan %>%
    filter(age>2 & LL_birth<10) %>%
    group_by(species) %>%
    summarise_each(funs(mean, se, length), LL_birth, LL_bd)

  #LMA summary by species
  LMA_spp <- LMA %>%
    filter(age>2) %>%
    group_by(species) %>%
    summarise_each(funs(mean, se, length), LMA)
  names(LMA_spp)<-c("species", "LMA_mean","LMA_se","LMA_length")

  #investment summary by species
  investment_spp <- investment %>%
      group_by(species) %>%
    summarise_each(funs(mean, se, length), total_weight, ReproInv, leaf_weight, stem_weight, RA)

  #maximum height for each species
  maxH_spp <- investment %>%
    group_by(species) %>%
    summarise_each(funs(max), height, stem_area, diameter)
  names(maxH_spp)<-c("species", "maxH_spp","max_stem_area_spp","max_stem_diam_spp")

  #maximum height for each species
  max_investment_spp <- investment %>%
    group_by(species) %>%
    summarise_each(funs(max), RA, ReproInv, total_weight, leaf_weight)
  names(max_investment_spp)<-c("species", "max_RA", "max_repro_inv","max_total_weight","max_leaf_weight")

  #mean stem, shoot diam for each species
  mean_value_spp <- SummaryInd %>%
    group_by(species) %>%
    summarise_each(funs(median), stem_area, diameter, d_end)
  names(mean_value_spp)<-c("species", "mean_stem_area_spp","mean_stem_diam_spp","mean_shoot_diam_spp")

  SummaryInd <- merge(SummaryInd, wood_density_spp, by=c("species"), all.x=TRUE)
  SummaryInd <- merge(SummaryInd, LMA_summary, by=c("species","age"), all.x=TRUE)
  SummaryInd <- merge(SummaryInd, maxH_spp, by=c("species"), all.x=TRUE)
  SummaryInd <- merge(SummaryInd, seedsize, by=c("species"), all.x=TRUE)
  SummaryInd <- merge(SummaryInd, max_investment_spp, by=c("species"), all.x=TRUE)
  SummaryInd <- merge(SummaryInd, investment_spp_age, by=c("species","age"), all.x=TRUE)
  SummaryInd <- merge(SummaryInd, mean_value_spp, by=c("species"), all.x=TRUE)

  SummaryInd <- SummaryInd %>% mutate(
    prop_maxH = height / maxH_spp,
    prop_max_weight = total_weight / max_total_weight,
    prop_max_repro = ReproInv / max_repro_inv,
    prop_allocation = propagule_inv/(GrowthInv + ReproInv),
    leaf_area = leaf_weight / (1000*LMA_mean),
    leaf_area_growth = growth_leaf / (1000*LMA_mean),
    shoot_leaf_area = lvs_end_total*leaf_size_mean,
    shoot_leaf_area_growth = lvs_new*leaf_size_mean,
    RA2 = total_repro_inv/TotalInv,
    reproducing = RA>0,
    RA_asin = asin(sqrt(RA)),
    prop_propagule_asin = asin(sqrt(prop_propagule)),
    scaled_growth_stem_diam = growth_stem_diam / mean_stem_diam_spp,
    scaled_growth_shoot_diam = growth_shoot_diam / mean_shoot_diam_spp
    ) 
  accessory_count <- Investment_FD_all %>%
      group_by(species, individual, part) %>%
      summarise_each(funs(sum), count, weight) %>%
      filter(count >0)

  count_flower <- subset(accessory_count, part=="flower_petals"|part=="flower_petals_small", select=c("individual","count"))
  names(count_flower) <- c("individual","flower_count")
  count_flower$individual <- as.factor(count_flower$individual)

  count_bud <- subset(accessory_count, part=="bud_tiny"|part=="bud_small"|part=="bud_mid"|part=="bud_large"|part=="flower_aborted_without_petals")
  count_bud <- count_bud %>%
    group_by(individual) %>%
    summarise_each(funs(sum), count)
  names(count_bud) <- c("individual","bud_count")
  count_bud$individual <- as.factor(count_bud$individual)

  count_seed <- subset(accessory_count, part=="seed"|part=="fruit_mature")
  count_seed <- count_seed %>%
    group_by(individual) %>%
    summarise_each(funs(sum), count)
  names(count_seed) <- c("individual","seed_count")
  count_seed$individual <- as.factor(count_seed$individual)

  count_fruit_aborted <- subset(accessory_count, part=="fruit_just_starting"|part=="fruit_young"|part=="fruit_large_immature_01"|part=="fruit_large_immature_02"|part=="fruit_large_immature_03"|part=="fruit_large_immature_04"|part==" fruit_large_immature_05"|part==" fruit_large_immature_06"|part=="fruit_empty")
  count_fruit_aborted <- count_fruit_aborted %>%
    group_by(individual) %>%
    summarise_each(funs(sum), count)
  names(count_fruit_aborted) <- c("individual","aborted_fruit_count")
  count_fruit_aborted$individual <- as.factor(count_fruit_aborted$individual)

  count_seed_pod <- subset(accessory_count, part=="seed_pod", select=c("individual","weight"))
  names(count_seed_pod) <- c("individual","seedpod_weight")
  count_seed_pod$individual <- as.factor(count_seed_pod$individual)

  count_fruit_mature <- subset(accessory_count, part=="fruit_mature", select=c("individual","weight"))
  names(count_fruit_mature) <- c("individual","fruit_weight")
  count_fruit_mature$individual <- as.factor(count_fruit_mature$individual)


  SummaryInd <- merge(SummaryInd, count_flower, by="individual", all.x=TRUE)
  SummaryInd <- merge(SummaryInd, count_bud, by="individual", all.x=TRUE)
  SummaryInd <- merge(SummaryInd, count_fruit_aborted, by="individual", all.x=TRUE)
  SummaryInd <- merge(SummaryInd, count_seed, by="individual", all.x=TRUE)
  SummaryInd <- merge(SummaryInd, count_seed_pod, by="individual", all.x=TRUE)
  SummaryInd <- merge(SummaryInd, count_fruit_mature, by="individual", all.x=TRUE)

  #TODO Lizzy: why do we need this next line?
  for(v in c("flower_count","bud_count","seed_count","aborted_fruit_count","seedpod_weight","fruit_weight")) {
    i <- is.na(SummaryInd[[v]])
    SummaryInd[[v]][i] <- 0
  }

  SummaryInd <- mutate(SummaryInd,
    seedset = seed_count/(bud_count + flower_count + seed_count + aborted_fruit_count),
    fruit_weight = propagule_inv + seedpod_weight + fruit_weight,
    prepollen_all_count = bud_count + flower_count,
    prop_prepollen_count = (bud_count + flower_count)/(bud_count + flower_count + seed_count + aborted_fruit_count),
    repro_all_count = bud_count + flower_count + seed_count,
    accessory_per_seed = accessory_inv/seed_count,
    propagule_per_seed = propagule_inv/seed_count,
    prepollen_all_per_seed = prepollen_all_inv/seed_count,
    prepollen_aborted_per_seed = prepollen_aborted_inv/seed_count,
    prepollen_success_per_seed = prepollen_success_inv/seed_count,
    postpollen_aborted_per_seed = postpollen_aborted_inv/seed_count,
    packaging_dispersal_per_seed = packaging_dispersal_inv/seed_count)

  #TODO Lizzy: why do we need this next line?
  for(v in c("seedset")) {
    i <- is.na(SummaryInd[[v]])
    SummaryInd[[v]][i] <- 0
  }


  SummaryInd$species <- as.factor(SummaryInd$species)
  SummaryInd
}
