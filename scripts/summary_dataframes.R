# load packages and "open" dataframes to use


  SummaryInd <- Individual_all


  #summarizing data by species, age

  #leaf lifespan summary by species, age
  LL_summary <- SummaryInd %>%
    filter(LL_bd>0 & LL_bd<6 & LL_birth<8 & LL_death<8) %>%
    group_by(species, age) %>%
    summarise_each(funs(mean, se, length), LL_bd, LL_death, LL_birth, new_length, lvs_end, lvs_end_total, growth_shoot_diam, growth_shoot_area, RGR)

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
  investment_summary <- ReproductionAllocation_all %>%
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
  maxRA <- ReproductionAllocation_all %>%
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
  investment_spp <- ReproductionAllocation_all %>%
      group_by(species) %>%
    summarise_each(funs(mean, se, length), total_weight, ReproInv, leaf_weight, stem_weight, RA)

  #maximum height for each species
  maxH_spp <- ReproductionAllocation_all %>%
    group_by(species) %>%
    summarise_each(funs(max), height, stem_area, diameter)
  names(maxH_spp)<-c("species", "maxH_spp","max_stem_area_spp","max_stem_diam_spp")

  #maximum height for each species
  max_investment_spp <- ReproductionAllocation_all %>%
    group_by(species) %>%
    summarise_each(funs(max), RA, ReproInv, total_weight, leaf_weight)
  names(max_investment_spp)<-c("species", "max_RA", "max_repro_inv","max_total_weight","max_leaf_weight")

  #mean stem, shoot diam for each species
  mean_value_spp <- SummaryInd %>%
    group_by(species) %>%
    summarise_each(funs(median), stem_area, diameter, d_end)
  names(mean_value_spp)<-c("species", "mean_stem_area_spp","mean_stem_diam_spp","mean_shoot_diam_spp")

  SummaryInd <- merge(SummaryInd, maxH_spp, by=c("species"), all.x=TRUE)
  SummaryInd <- merge(SummaryInd, max_investment_spp, by=c("species"), all.x=TRUE)
  SummaryInd <- merge(SummaryInd, investment_spp_age, by=c("species","age"), all.x=TRUE)
  SummaryInd <- merge(SummaryInd, mean_value_spp, by=c("species"), all.x=TRUE)

  SummaryInd <- SummaryInd %>% mutate(
    prop_maxH = height / maxH_spp,
    prop_max_weight = total_weight / max_total_weight,
    prop_max_repro = ReproInv / max_repro_inv,
    scaled_growth_stem_diam = growth_stem_diam / mean_stem_diam_spp,
    scaled_growth_shoot_diam = growth_shoot_diam / mean_shoot_diam_spp
    ) 

count_spp <- SummaryInd %>%
  filter(ReproInv>0) %>%
  group_by(species) %>%
  summarise_each(funs(mean, se, length), seedset,seed_count,flower_count,fruit_weight,leaf_area)

per_seed <- SummaryInd %>%
  filter(seed_count>0) %>%
  group_by(species) %>%
  summarise_each(funs(mean, se, length), accessory_per_seed,propagule_per_seed,prepollen_all_per_seed,prepollen_aborted_per_seed, prepollen_success_per_seed, postpollen_aborted_per_seed, packaging_dispersal_per_seed)



#merge various data summaries into SummarySppAge dataframe
SummarySppAge <- merge(LL_summary, LMA, by=c("species","age"))
SummarySppAge <- merge(SummarySppAge, wood_density_spp, by=c("species"))
SummarySppAge <- merge(SummarySppAge, seedsize, by=c("species"))
SummarySppAge <- merge(SummarySppAge, maxH_spp, by=c("species"))
SummarySppAge <- merge(SummarySppAge, maxRA, by=c("species","age"))
SummarySppAge <- merge(SummarySppAge, investment_summary, by=c("species","age"))
SummarySppAge <- merge(SummarySppAge, accessory_summary, by=c("species","age"))
SummarySppAge <- merge(SummarySppAge, accessory_summary2, by=c("species","age"),all=TRUE)
#SummarySppAge$leaf_area <- SummarySppAge$leaf_weight / SummarySppAge$LMA
#SummarySppAge <- merge(SummarySppAge, harvest_summary, by=c("species","age"))

#merge various data summaries into SummarySpp dataframe
SummarySpp <- merge(LL_spp1, LL_spp2, by=c("species"))
SummarySpp <- merge(SummarySpp, LMA_spp, by=c("species"))
SummarySpp <- merge(SummarySpp, wood_density_spp, by=c("species"))
SummarySpp <- merge(SummarySpp, seedsize, by=c("species"))
SummarySpp <- merge(SummarySpp, maxH_spp, by=c("species"))
SummarySpp <- merge(SummarySpp, accessory_spp, by=c("species"))
SummarySpp <- merge(SummarySpp, investment_spp, by=c("species"))
SummarySpp <- merge(SummarySpp, mature_RA, by=c("species"))

SummarySppAge$species <- as.factor(SummarySppAge$species)


SummarySpp <- merge(SummarySpp, count_spp, by="species",all.x=TRUE)
SummarySpp <- merge(SummarySpp, per_seed, by="species",all.x=TRUE)
