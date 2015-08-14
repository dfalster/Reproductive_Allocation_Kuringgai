# load packages and "open" dataframes to use




count_spp <- SummaryInd %>%
  filter(ReproInv>0) %>%
  group_by(species) %>%
  summarise_each(funs(mean, se, length), seedset,seed_count,flower_count,fruit_weight,leaf_area)

per_seed <- SummaryInd %>%
  filter(seed_count>0) %>%
  group_by(species) %>%
  summarise_each(funs(mean, se, length), accessory_per_seed,propagule_per_seed,prepollen_all_per_seed,prepollen_aborted_per_seed, prepollen_success_per_seed, postpollen_aborted_per_seed, packaging_dispersal_per_seed)



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

SummarySppAge$species <- as.factor(SummarySppAge$species)


SummarySpp <- merge(SummarySpp, count_spp, by="species",all.x=TRUE)
SummarySpp <- merge(SummarySpp, per_seed, by="species",all.x=TRUE)
