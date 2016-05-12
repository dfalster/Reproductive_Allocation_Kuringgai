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






dispersal_at_pollination[["BAER"]] <- list(parts=c("cone_green","flower_stigma"), scale=c("SummaryInd$repro_all_count/SummaryInd$cone_count",1/2))
dispersal_at_pollination[["BOLE"]] <- list(parts=c("flower_calyx","flower_petals"), scale=c(1/4,1/4))
dispersal_at_pollination[["COER"]] <- list(parts=c("inflorescence_stalk","flower_stigma"), scale=c("SummaryInd$repro_all_count/SummaryInd$inflorescence_count",1/2))
dispersal_at_pollination[["EPMI"]] <- list(parts=c("flower_calyx"), scale=c(1/15))
dispersal_at_pollination[["GRBU"]] <- list(parts=c("inflorescence_stalk","flower_stigma"), scale=c("SummaryInd$repro_all_count/SummaryInd$inflorescence_count",1/2))
dispersal_at_pollination[["GRSP"]] <- list(parts=c("inflorescence_stalk","flower_stigma"), scale=c("SummaryInd$repro_all_count/SummaryInd$inflorescence_count",1/2))
dispersal_at_pollination[["HATE"]] <- list(parts=c("flower_stigma"), scale=c(1/2))
dispersal_at_pollination[["HEPU"]] <- list(parts=c("flower_calyx"), scale=c(1/4))
dispersal_at_pollination[["LEES"]] <- list(parts=c("flower_calyx"), scale=c(1/4))
dispersal_at_pollination[["PELA"]] <- list(parts=c("flower_stigma"), scale=c(1))
dispersal_at_pollination[["PEPU"]] <- list(parts=c("cone_green","flower_stigma"), scale=c("SummaryInd$repro_all_count/SummaryInd$cone_count",1))
dispersal_at_pollination[["PHPH"]] <- list(parts=c("flower_stigma"), scale=c(1/2))
dispersal_at_pollination[["PILI"]] <- list(parts=c("flower_stigma"), scale=c(1))
dispersal_at_pollination[["PUTU"]] <- list(parts=c("flower_stigma"), scale=c(1/2))

#For BAER, PEPU, the weight of cone_green to be used is determined by a match of census date and part name between the file "cones_census_to_use" and a hidden dataframe 
#which I think is called "Investment" (or "Inv" or "Investments");
#these will be matched by individual; for some individuals there will be multiple cones and different censuses to sum together. 
#I suspect it would be easiest to first sum up all the cone weights by individual and then match that output to individuals for the code above

#For all species, the weight of other parts should be the "per item" weight availble in "FD"; this is also output as: InvestmentByPart <- readRDS("export/Investment_FD_all.rds") in the remake file. 
#It is essential the weights are recalculated by individual part, because the number of parts in the "FD"/"InvestmentByPart" file is NOT the total number produced (e.g. because some stigmas will have become fruit)