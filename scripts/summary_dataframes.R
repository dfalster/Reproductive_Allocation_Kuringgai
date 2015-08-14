# load packages and "open" dataframes to use



individuals <- IndividualsList %>%
  select(individual,species, age, mature)

investment <- ReproductionAllocation_all
investment$age <- round(investment$age, digits=1)

accessory <- AccessoryCosts_all

#adding investment and accessory costs data to leafLifespan dataframe to create a dataframe with all individual level data
SummaryInd <- merge(investment, select(leafLifespan, -species, -age), by="individual",all.x=TRUE)
SummaryInd <- merge(SummaryInd, select(accessory, -species, -age), by="individual",all.x=TRUE)
SummaryInd <- merge(SummaryInd, select(individuals, -species, -age), by="individual",all.x=TRUE)
#SummaryInd <- merge(SummaryInd, select(harvest, -species, -age), by="individual",all.x=TRUE)

#adding variables to look at change across years, RGR
SummaryInd$growth_shoot_diam <- (SummaryInd$d_end - SummaryInd$d_start)/100
SummaryInd$growth_shoot_area <- (3.14*((SummaryInd$d_end/200)^2)) - (3.14*((SummaryInd$d_start/200)^2))
SummaryInd$lvs_end_total <- SummaryInd$lvs_end + SummaryInd$lvs_new
SummaryInd$RGR <- log(SummaryInd$total_weight)-log(SummaryInd$total_weight-SummaryInd$GrowthInv)
SummaryInd$RGR_leaf <- log(SummaryInd$leaf_weight)-log(SummaryInd$leaf_weight-SummaryInd$growth_leaf)
SummaryInd$RGR_stem <- log(SummaryInd$stem_weight)-log(SummaryInd$stem_weight-SummaryInd$growth_stem)
SummaryInd$prop_prepollen_all <- SummaryInd$prop_prepollen_aborted + SummaryInd$prop_prepollen_success


#summarizing data by species, age

#leaf lifespan summary by species, age
LL_summary <- SummaryInd %>%
  filter(LL_bd>0 & LL_bd<6 & LL_birth<8 & LL_death<8) %>%
  group_by(species, age) %>%
  summarise_each(funs(mean, se, length), LL_bd, LL_death,LL_birth,new_length, lvs_end, lvs_end_total, growth_shoot_diam, growth_shoot_area, RGR)

#LMA summary by species, age
LMA_summary <- LMA %>%
  group_by(species, age) %>%
  summarise_each(funs(mean, se, length), LMA, leaf_size)

#accessory tissue summary by species, age
accessory_summary <- SummaryInd %>%
  group_by(species, age) %>%
  summarise_each(funs(mean, se, length),prepollen_aborted_inv,prepollen_success_inv, postpollen_aborted_inv, packaging_dispersal_inv, propagule_inv, prepollen_all_inv)

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
  select(species, age, height_mean,RA_mean)
  
mature_RA <- SummaryInd %>%
  filter(mature==TRUE) %>%
  group_by(species) %>%
  summarise_each(funs(mean, se, length), RA, ReproInv)
names(mature_RA) <- c("species","mature_RA_mean" ,"ReproInv_mean","mature_RA_se","ReproInv_se","mature_RA_length","ReproInv_length")

#harvest summary by species, age
#harvest_summary <- harvest %>%
 # group_by(species, age) %>%
  #summarise_each(funs(mean, se, length), height,total_leaf_weight)
#names(harvest_summary)<-c("species","age","height_mean","height_se","height_length")

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
  summarise_each(funs(mean, se, length), total_weight,ReproInv, leaf_weight, stem_weight, RA)

#maximum height for each species
maxH_spp <- investment %>%
  group_by(species) %>%
  summarise_each(funs(max), height, stem_area, diameter)
names(maxH_spp)<-c("species", "maxH_spp","max_stem_area_spp","max_stem_diam_spp")

#maximum height for each species
max_investment_spp <- investment %>%
  group_by(species) %>%
  summarise_each(funs(max),RA,ReproInv,total_weight,leaf_weight)
names(max_investment_spp)<-c("species", "max_RA", "max_repro_inv","max_total_weight","max_leaf_weight")

SummaryInd <- merge(SummaryInd, wood_density_spp, by=c("species"),all.x=TRUE)
SummaryInd <- merge(SummaryInd,LMA_summary, by=c("species","age"),all.x=TRUE)
SummaryInd <- merge(SummaryInd,maxH_spp, by=c("species"),all.x=TRUE)
SummaryInd <- merge(SummaryInd,seedsize, by=c("species"),all.x=TRUE)
SummaryInd <- merge(SummaryInd,max_investment_spp, by=c("species"),all.x=TRUE)
SummaryInd <- merge(SummaryInd,investment_spp_age, by=c("species","age"),all.x=TRUE)
SummaryInd$prop_maxH <- SummaryInd$height / SummaryInd$maxH_spp
SummaryInd$prop_max_weight <- SummaryInd$total_weight / SummaryInd$max_total_weight
SummaryInd$prop_max_repro <- SummaryInd$ReproInv / SummaryInd$max_repro_inv
SummaryInd$prop_allocation <- SummaryInd$propagule_inv/(SummaryInd$GrowthInv + SummaryInd$ReproInv)
SummaryInd$leaf_area <- SummaryInd$leaf_weight / (1000*SummaryInd$LMA_mean)
SummaryInd$leaf_area_growth <- SummaryInd$growth_leaf / (1000*SummaryInd$LMA_mean)
SummaryInd$shoot_leaf_area <- SummaryInd$lvs_end_total*SummaryInd$leaf_size_mean
SummaryInd$shoot_leaf_area_growth <- SummaryInd$lvs_new*SummaryInd$leaf_size_mean
SummaryInd$RA2 <- SummaryInd$total_repro_inv/SummaryInd$TotalInv
SummaryInd$reproducing <- SummaryInd$RA>0

SummaryInd$RA_asin <- asin(sqrt(SummaryInd$RA))
SummaryInd$prop_propagule_asin <- asin(sqrt(SummaryInd$prop_propagule))

#mean stem, shoot diam for each species
mean_value_spp <- SummaryInd %>%
  group_by(species) %>%
  summarise_each(funs(median), stem_area, diameter, d_end)
names(mean_value_spp)<-c("species", "mean_stem_area_spp","mean_stem_diam_spp","mean_shoot_diam_spp")

SummaryInd <- merge(SummaryInd,mean_value_spp, by=c("species"),all.x=TRUE)
SummaryInd$scaled_growth_stem_diam <- SummaryInd$growth_stem_diam / SummaryInd$mean_stem_diam_spp
SummaryInd$scaled_growth_shoot_diam <- SummaryInd$growth_shoot_diam / SummaryInd$mean_shoot_diam_spp

#merge various data summaries into SummarySppAge dataframe
SummarySppAge <- merge(LL_summary, LMA_summary, by=c("species","age"))
SummarySppAge <- merge(SummarySppAge, wood_density_spp, by=c("species"))
SummarySppAge <- merge(SummarySppAge, seedsize, by=c("species"))
SummarySppAge <- merge(SummarySppAge, maxH_spp, by=c("species"))
SummarySppAge <- merge(SummarySppAge, maxRA, by=c("species","age"))
SummarySppAge <- merge(SummarySppAge, investment_summary, by=c("species","age"))
SummarySppAge <- merge(SummarySppAge, accessory_summary, by=c("species","age"))
SummarySppAge <- merge(SummarySppAge, accessory_summary2, by=c("species","age"),all=TRUE)
SummarySppAge$leaf_area_mean <- SummarySppAge$leaf_weight_mean / SummarySppAge$LMA_mean
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

SummaryInd$species <- as.factor(SummaryInd$species)
SummarySppAge$species <- as.factor(SummarySppAge$species)

accessory_count <- Investment_FD_all %>%
    group_by(species, individual, part) %>%
    summarise_each(funs(sum), count, weight) %>%
    filter(count >0)

count_flower <- subset(accessory_count, part=="flower_petals"|part=="flower_petals_small",select=c("individual","count"))
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

count_seed_pod <- subset(accessory_count, part=="seed_pod",select=c("individual","weight"))
names(count_seed_pod) <- c("individual","seedpod_weight")
count_seed_pod$individual <- as.factor(count_seed_pod$individual)

count_fruit_mature <- subset(accessory_count, part=="fruit_mature",select=c("individual","weight"))
names(count_fruit_mature) <- c("individual","fruit_weight")
count_fruit_mature$individual <- as.factor(count_fruit_mature$individual)


SummaryInd <- merge(SummaryInd, count_flower, by="individual",all.x=TRUE)
SummaryInd <- merge(SummaryInd, count_bud, by="individual",all.x=TRUE)
SummaryInd <- merge(SummaryInd, count_fruit_aborted, by="individual",all.x=TRUE)
SummaryInd <- merge(SummaryInd, count_seed, by="individual",all.x=TRUE)
SummaryInd <- merge(SummaryInd, count_seed_pod, by="individual",all.x=TRUE)
SummaryInd <- merge(SummaryInd, count_fruit_mature, by="individual",all.x=TRUE)

for(v in c("flower_count","bud_count","seed_count","aborted_fruit_count","seedpod_weight","fruit_weight")) {
  i <- is.na(SummaryInd[[v]])
  SummaryInd[[v]][i] <- 0
}

SummaryInd$seedset <- SummaryInd$seed_count/(SummaryInd$bud_count+SummaryInd$flower_count+SummaryInd$seed_count+SummaryInd$aborted_fruit_count)
SummaryInd$fruit_weight <- SummaryInd$propagule_inv + SummaryInd$seedpod_weight + SummaryInd$fruit_weight
SummaryInd$prepollen_all_count <- SummaryInd$bud_count+SummaryInd$flower_count
SummaryInd$prop_prepollen_count <- (SummaryInd$bud_count+SummaryInd$flower_count)/(SummaryInd$bud_count+SummaryInd$flower_count+SummaryInd$seed_count+SummaryInd$aborted_fruit_count)
SummaryInd$repro_all_count <- SummaryInd$bud_count+SummaryInd$flower_count+SummaryInd$seed_count
SummaryInd$accessory_per_seed <- SummaryInd$accessory_inv/SummaryInd$seed_count
SummaryInd$propagule_per_seed <- SummaryInd$propagule_inv/SummaryInd$seed_count
SummaryInd$prepollen_all_per_seed <- SummaryInd$prepollen_all_inv/SummaryInd$seed_count
SummaryInd$prepollen_aborted_per_seed <- SummaryInd$prepollen_aborted_inv/SummaryInd$seed_count
SummaryInd$prepollen_success_per_seed <- SummaryInd$prepollen_success_inv/SummaryInd$seed_count
SummaryInd$postpollen_aborted_per_seed <- SummaryInd$postpollen_aborted_inv/SummaryInd$seed_count
SummaryInd$packaging_dispersal_per_seed <- SummaryInd$packaging_dispersal_inv/SummaryInd$seed_count

for(v in c("seedset")) {
  i <- is.na(SummaryInd[[v]])
  SummaryInd[[v]][i] <- 0
}

count_spp <- SummaryInd %>%
  filter(ReproInv>0) %>%
  group_by(species) %>%
  summarise_each(funs(mean, se, length), seedset,seed_count,flower_count,fruit_weight,leaf_area)

per_seed <- SummaryInd %>%
  filter(seed_count>0) %>%
  group_by(species) %>%
  summarise_each(funs(mean, se, length), accessory_per_seed,propagule_per_seed,prepollen_all_per_seed,prepollen_aborted_per_seed, prepollen_success_per_seed, postpollen_aborted_per_seed, packaging_dispersal_per_seed)

SummarySpp <- merge(SummarySpp, count_spp, by="species",all.x=TRUE)
SummarySpp <- merge(SummarySpp, per_seed, by="species",all.x=TRUE)
