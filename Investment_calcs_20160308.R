budweight <- dplyr::filter(PartsSummary,part%in%c("bud_big","inflorescence_bud_big_flowers"))
budweight2 <-dplyr::filter(PartsSummary,part=="bud_mid"&species=="PHPH")
budweight <- dplyr::bind_rows(budweight,budweight2)
budweight$species <- as.character(budweight$species)
budweight <-select(budweight,species,weight)
names(budweight) <-c("species","budweight")

SummaryInd <- dplyr::left_join(SummaryInd,budweight,by="species")
SummaryInd$bud_inv <-SummaryInd$budweight*SummaryInd$repro_all_count
SummaryInd$flower_inv <-SummaryInd$prepollencosts*SummaryInd$repro_all_count
SummaryInd$budprop <- SummaryInd$bud_inv + SummaryInd$propagule_inv




#dispersalcosts_increment = dispersalcosts - dispersal_at_pollination,
#abortedcosts = ((prepollen_failure_inv + postpollen_aborted_inv)/seed_count),
#b_div_p = prepollencosts / seedset,
#c_div_p = ((abortedcosts)*(1-seedset))/seedset,
#dispersalcosts_increment2 = accessory_per_seed - b_div_p - c_div_p,
#total_accessory_Lord = dispersalcosts_increment + b_div_p + c_div_p,