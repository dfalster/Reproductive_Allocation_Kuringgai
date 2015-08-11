# load packages and "open" dataframes to use
LMA <- filter(LMA_raw, species!="" & species!=" ") %>%
  select(species,age,LMA,branch_age,leaf_number,leaf_area)
LMA$leaf_size <- LMA$leaf_area/LMA$leaf_number

wood <- filter(woodDensity_raw, use=="use") %>%
  select(species,density)

seedsize <- seedSize_raw %>%
  select(species,seed_size)

individuals <- IndividualsList %>%
  select(individual,species, age, mature)

axis_labels <- read_csv("data/axis_labels.csv")

#harvest <- filter(HarvestData,
#                  individuals$use_for_allocation_calculations[match(HarvestData$individual, individuals$individual)] & segment==1 & start_end=="end") %>%
#  select(species,individual,age,leaf_weight,stem_weight, total_weight,height,site)

investment <- ReproductionAllocation_all

names(investment) <-  c("individual","ReproInv","species","site","age","height","diameter","stem_area","leaf_weight","stem_weight","total_weight","GrowthInv","growth_stem","growth_leaf","growth_height","growth_stem_diam","growth_stem_area","TotalInv","RA")

investment$age <- round(investment$age, digits=1)

accessory <- AccessoryCosts_all
names(accessory) <- c("individual","species","age","prepollen_aborted_inv","prepollen_success_inv","postpollen_aborted_inv","packaging_dispersal_inv","propagule_inv","total_repro_inv")
#accessory$age2 <- accessory$age
#accessory$age <- round(accessory$age, digits=1)

#accessory$PerAccessory <- {
#  select(accessory, Total >0,
#  1 - (accessory$propagule_inv/accessory$Total)
#}

#accessory$PerPropagule = 1 - accessory$PerAccessory

leafLifespan <- filter(leafLifespan_raw, dont_use!="dead" & dont_use!="dont_use") %>%
  select(species,age,individual,d_start,d_end,length_start,new_length,lvs_start_length,lvs_start_count,lvs_end_length,lvs_end_count,lvs_new_length,lvs_new_count,new_but_shed_count,mm_lvs_spec,count_lvs_spec)

leafLifespan$individual <- as.factor(leafLifespan$individual)
leafLifespan$species <- as.factor(leafLifespan$species)

#loading this column as character, so using this for now

# Calculating Leaf Lifespan

# where counts did not previously exists, set as 0; CHECK WITH DANIEL THAT "V" IS A SET "NAME" IN THESE FUNCTIONS

leaf_lifespan_temp <- leafLifespan %>%
  select(species,age,individual,d_start,d_end,length_start,new_length,lvs_start_count,lvs_end_count,lvs_new_count,new_but_shed_count)

for(v in c("lvs_start_count","lvs_end_count","lvs_new_count","new_but_shed_count")) {
  i <- is.na(leaf_lifespan_temp[[v]])
  leaf_lifespan_temp[[v]][i] <- 0
}

# For individuals with "length" measures, translate into count

# get average leaves per length for spp
leaves_per_length_spp <-  filter(leavesPerLength_raw,use=="yes") %>%
  mutate(count_per_length = leaf_count/length_mm) %>%
  group_by(species) %>%
  summarise(count_per_length = mean(count_per_length, na.rm=TRUE))

# get spp average for all individuals
leafLifespan$count_per_length <- leaves_per_length_spp$count_per_length[match(leafLifespan$species, leaves_per_length_spp$species)]

# if possible use data for that individual, if it exists; this code substitutes in species count per length #s for individuals where this data is available
i <- !is.na(leafLifespan$mm_lvs)
leafLifespan$count_per_length[i] <- (leafLifespan$count_lvs_spec/leafLifespan$mm_lvs)[i]

#this is the end of the section Daniel helped me rewrite - next section is mine

#get total leaf counts
for(v in c("lvs_start_length","lvs_start_count","lvs_end_length","lvs_end_count","lvs_new_length","lvs_new_count","new_but_shed_count")) {
  i <- is.na(leafLifespan[[v]])
  leafLifespan[[v]][i] <- 0
}
leafLifespan$lvs_start <- leafLifespan$lvs_start_count + (leafLifespan$lvs_start_length * leafLifespan$count_per_length)
leafLifespan$lvs_end <- leafLifespan$lvs_end_count + (leafLifespan$lvs_end_length * leafLifespan$count_per_length)
leafLifespan$lvs_new <- leafLifespan$lvs_new_count + (leafLifespan$lvs_new_length * leafLifespan$count_per_length)

#calculating leaf lifespan
leafLifespan$LL_bd <- (leafLifespan$lvs_start)/(((leafLifespan$lvs_start)-(leafLifespan$lvs_end))+(leafLifespan$lvs_new)/2)
leafLifespan$LL_death <- (leafLifespan$lvs_start)/((leafLifespan$lvs_start)-(leafLifespan$lvs_end))
leafLifespan$LL_birth <- (leafLifespan$lvs_start)/(leafLifespan$lvs_new)

leafLifespan$LL_death[which(is.infinite(leafLifespan$LL_death))] <- NA
leafLifespan$LL_bd[which(is.infinite(leafLifespan$LL_death))] <- NA
leafLifespan$LL_birth[which(is.infinite(leafLifespan$LL_death))] <- NA

#calculating percentages of various accessory tissues

accessory$accessory_inv <- accessory$total_repro_inv - accessory$propagule_inv
accessory$prepollen_all_inv <- accessory$prepollen_aborted_inv + accessory$prepollen_success_inv

accessory$prop_prepollen_aborted <- accessory$prepollen_aborted_inv / accessory$total_repro_inv
accessory$prop_prepollen_success <- accessory$prepollen_success_inv / accessory$total_repro_inv
accessory$prop_postpollen_aborted <- accessory$postpollen_aborted_inv / accessory$total_repro_inv
accessory$prop_packaging_dispersal <- accessory$packaging_dispersal_inv / accessory$total_repro_inv
accessory$prop_propagule <- accessory$propagule_inv / accessory$total_repro_inv
accessory$prop_accessory <- 1- accessory$prop_propagule


accessory$prop_prepollen_aborted[which(is.nan(accessory$prop_prepollen_aborted))] <- NA
accessory$prop_prepollen_success[which(is.nan(accessory$prop_prepollen_success))] <- NA
accessory$prop_postpollen_aborted[which(is.nan(accessory$prop_postpollen_aborted))] <- NA
accessory$prop_packaging_dispersal[which(is.nan(accessory$prop_packaging_dispersal))] <- NA
accessory$prop_propagule[which(is.nan(accessory$prop_propagule))] <- NA
accessory$prop_accessory[which(is.nan(accessory$prop_accessory))] <- NA

#adding investment and accessory costs data to leafLifespan dataframe to create a dataframe with all individual level data
SummaryInd <- merge(investment, select(leafLifespan, -species, -age), by="individual",all.x=TRUE)
SummaryInd <- merge(SummaryInd, select(accessory, -species, -age), by="individual",all.x=TRUE)
SummaryInd <- merge(SummaryInd, select(individuals, -species, -age), by="individual",all.x=TRUE)
#SummaryInd <- merge(SummaryInd, select(harvest, -species, -age), by="individual",all.x=TRUE)

#adding variables to look at change across years, RGR
SummaryInd$growth_shoot_diam <- SummaryInd$d_end - SummaryInd$d_start
SummaryInd$growth_shoot_area <- (3.14*((SummaryInd$d_end/2)^2)) - (3.14*((SummaryInd$d_start/2)^2))
SummaryInd$lvs_end_total <- SummaryInd$lvs_end + SummaryInd$lvs_new
SummaryInd$RGR <- log(SummaryInd$total_weight)-log(SummaryInd$total_weight-SummaryInd$GrowthInv)
SummaryInd$RGR_leaf <- log(SummaryInd$leaf_weight)-log(SummaryInd$leaf_weight-SummaryInd$growth_leaf)
SummaryInd$RGR_stem <- log(SummaryInd$stem_weight)-log(SummaryInd$stem_weight-SummaryInd$growth_stem)
SummaryInd$prop_prepollen_all <- SummaryInd$prop_prepollen_aborted + SummaryInd$prop_prepollen_success

#adding info on all costs to produce 1 seed
BAER_parts <- as.data.frame(BAER_PartsSummary[4])
names(BAER_parts) <- c("species","what","count","weight")
BOLE_parts <- as.data.frame(BOLE_PartsSummary[4])
names(BOLE_parts) <- c("species","what","count","weight")
COER_parts <- as.data.frame(COER_PartsSummary[4])
names(COER_parts) <- c("species","what","count","weight")
EPMI_parts <- as.data.frame(EPMI_PartsSummary[4])
names(EPMI_parts) <- c("species","what","count","weight")
GRBU_parts <- as.data.frame(GRBU_PartsSummary[4])
names(GRBU_parts) <- c("species","what","count","weight")
GRSP_parts <- as.data.frame(GRSP_PartsSummary[4])
names(GRSP_parts) <- c("species","what","count","weight")
HATE_parts <- as.data.frame(HATE_PartsSummary[4])
names(HATE_parts) <- c("species","what","count","weight")
HEPU_parts <- as.data.frame(HEPU_PartsSummary[4])
names(HEPU_parts) <- c("species","what","count","weight")
LEES_parts <- as.data.frame(LEES_PartsSummary[4])
names(LEES_parts) <- c("species","what","count","weight")
PELA_parts <- as.data.frame(PELA_PartsSummary[4])
names(PELA_parts) <- c("species","what","count","weight")
PEPU_parts <- as.data.frame(PEPU_PartsSummary[4])
names(PEPU_parts) <- c("species","what","count","weight")
PHPH_parts <- as.data.frame(PHPH_PartsSummary[4])
names(PHPH_parts) <- c("species","what","count","weight")
PILI_parts <- as.data.frame(PILI_PartsSummary[4])
names(PILI_parts) <- c("species","what","count","weight")
PUTU_parts <- as.data.frame(PUTU_PartsSummary[4])
names(PUTU_parts) <- c("species","what","count","weight")

seedcosts <- as.data.frame(c(1:14))
seedcosts$species <- c("BAER","BOLE","COER","EPMI","GRBU","GRSP","HATE","HEPU","LEES","PELA","PEPU","PHPH","PILI","PUTU")
names(seedcosts) <- c("seedcosts","species")

seedcosts[1,1] <- (BAER_parts[2,4])/10+(BAER_parts[4,4])/10+BAER_parts[13,4]+BAER_parts[12,4]+BAER_parts[19,4]+BAER_parts[20,4]
seedcosts[2,1] <- BOLE_parts[5,4]+BOLE_parts[6,4]+BOLE_parts[11,4]+BOLE_parts[15,4]+BOLE_parts[16,4]
seedcosts[3,1] <- (COER_parts[5,4])/6+COER_parts[8,4]+COER_parts[12,4]+COER_parts[15,4]
seedcosts[4,1] <- EPMI_parts[4,4]+EPMI_parts[9,4]
seedcosts[5,1] <- GRBU_parts[2,4]+GRBU_parts[7,4]+GRBU_parts[12,4]+GRBU_parts[14,4]+GRBU_parts[15,4]
seedcosts[6,1] <- GRSP_parts[4,4]+GRSP_parts[9,4]+GRSP_parts[14,4]+GRSP_parts[16,4]+GRSP_parts[18,4]
seedcosts[7,1] <- HATE_parts[4,4]+HATE_parts[6,4]+HATE_parts[13,4]+HATE_parts[16,4]
seedcosts[8,1] <- HEPU_parts[3,4]+HEPU_parts[10,4]+HEPU_parts[15,4]
seedcosts[9,1] <- LEES_parts[5,4]+LEES_parts[10,4]+LEES_parts[13,4]
seedcosts[10,1] <- PELA_parts[4,4]+PELA_parts[5,4]+PELA_parts[11,4]+PELA_parts[12,4]
seedcosts[11,1] <- (PEPU_parts[4,4])/30+PEPU_parts[9,4]+PEPU_parts[16,4]
seedcosts[12,1] <- PHPH_parts[4,4]+PHPH_parts[5,4]+PHPH_parts[6,4]+PHPH_parts[15,4]+PHPH_parts[16,4]
seedcosts[13,1] <- (PILI_parts[2,4])/10+PILI_parts[3,4]+PILI_parts[5,4]+PILI_parts[7,4]+PILI_parts[11,4]+PILI_parts[12,4]
seedcosts[14,1] <- PUTU_parts[2,4]+PUTU_parts[3,4]+PUTU_parts[4,4]+PUTU_parts[10,4]+PUTU_parts[11,4]

seedsize <- merge(seedsize, seedcosts, by="species",all.x=TRUE)
seedsize$costs_per_seed <- (seedsize$seedcosts-seedsize$seed_size)/(seedsize$seedcosts)

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

#wood density summary by species
wood_summary <- wood %>%
  group_by(species) %>%
  summarise_each(funs(mean, se, length), density)
names(wood_summary)<-c("species","WD_mean","WD_se","WD_length")

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

SummaryInd <- merge(SummaryInd,wood_summary, by=c("species"),all.x=TRUE)
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
SummarySppAge <- merge(SummarySppAge, wood_summary, by=c("species"))
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
SummarySpp <- merge(SummarySpp, wood_summary, by=c("species"))
SummarySpp <- merge(SummarySpp, seedsize, by=c("species"))
SummarySpp <- merge(SummarySpp, maxH_spp, by=c("species"))
SummarySpp <- merge(SummarySpp, accessory_spp, by=c("species"))
SummarySpp <- merge(SummarySpp, investment_spp, by=c("species"))
SummarySpp <- merge(SummarySpp, mature_RA, by=c("species"))

SummaryInd$species <- as.factor(SummaryInd$species)
SummarySppAge$species <- as.factor(SummarySppAge$species)

#getting counts out of Konrad's reproduction data
tmp <- BAER_Investment
FD <- as.data.frame(tmp$FD)
BAER_count <- FD %>%
  group_by(individual, what) %>%
  summarise_each(funs(sum), count, weight)
BAER_count <- subset(BAER_count, count!=0)
BAER_count$species <- "BAER"

tmp <- BOLE_Investment
FD <- as.data.frame(tmp$FD)
BOLE_count <- FD %>%
  group_by(individual, what) %>%
  summarise_each(funs(sum), count, weight)
BOLE_count <- subset(BOLE_count, count!=0)
BOLE_count$species <- "BOLE"

tmp <- COER_Investment
FD <- as.data.frame(tmp$FD)
COER_count <- FD %>%
  group_by(individual, what) %>%
  summarise_each(funs(sum), count, weight)
COER_count <- subset(COER_count, count!=0)
COER_count$species <- "COER"

tmp <- EPMI_Investment
FD <- as.data.frame(tmp$FD)
EPMI_count <- FD %>%
  group_by(individual, what) %>%
  summarise_each(funs(sum), count, weight)
EPMI_count <- subset(EPMI_count, count!=0)
EPMI_count$species <- "EPMI"

tmp <- GRBU_Investment
FD <- as.data.frame(tmp$FD)
GRBU_count <- FD %>%
  group_by(individual, what) %>%
  summarise_each(funs(sum), count, weight)
GRBU_count <- subset(GRBU_count, count!=0)
GRBU_count$species <- "GRBU"

tmp <- GRSP_Investment
FD <- as.data.frame(tmp$FD)
GRSP_count <- FD %>%
  group_by(individual, what) %>%
  summarise_each(funs(sum), count, weight)
GRSP_count <- subset(GRSP_count, count!=0)
GRSP_count$species <- "GRSP"

tmp <- HATE_Investment
FD <- as.data.frame(tmp$FD)
HATE_count <- FD %>%
  group_by(individual, what) %>%
  summarise_each(funs(sum), count, weight)
HATE_count <- subset(HATE_count, count!=0)
HATE_count$species <- "HATE"

tmp <- HEPU_Investment
FD <- as.data.frame(tmp$FD)
HEPU_count <- FD %>%
  group_by(individual, what) %>%
  summarise_each(funs(sum), count, weight)
HEPU_count <- subset(HEPU_count, count!=0)
HEPU_count$species <- "HEPU"

tmp <- LEES_Investment
FD <- as.data.frame(tmp$FD)
LEES_count <- FD %>%
  group_by(individual, what) %>%
  summarise_each(funs(sum), count, weight)
LEES_count <- subset(LEES_count, count!=0)
LEES_count$species <- "LEES"

tmp <- PELA_Investment
FD <- as.data.frame(tmp$FD)
PELA_count <- FD %>%
  group_by(individual, what) %>%
  summarise_each(funs(sum), count, weight)
PELA_count <- subset(PELA_count, count!=0)
PELA_count$species <- "PELA"

tmp <- PEPU_Investment
FD <- as.data.frame(tmp$FD)
PEPU_count <- FD %>%
  group_by(individual, what) %>%
  summarise_each(funs(sum), count, weight)
PEPU_count <- subset(PEPU_count, count!=0)
PEPU_count$species <- "PEPU"

tmp <- PHPH_Investment
FD <- as.data.frame(tmp$FD)
PHPH_count <- FD %>%
  group_by(individual, what) %>%
  summarise_each(funs(sum), count, weight)
PHPH_count <- subset(PHPH_count, count!=0)
PHPH_count$species <- "PHPH"

tmp <- PILI_Investment
FD <- as.data.frame(tmp$FD)
PILI_count <- FD %>%
  group_by(individual, what) %>%
  summarise_each(funs(sum), count, weight)
PILI_count <- subset(PILI_count, count!=0)
PILI_count$species <- "PILI"

tmp <- PUTU_Investment
FD <- as.data.frame(tmp$FD)
PUTU_count <- FD %>%
  group_by(individual, what) %>%
  summarise_each(funs(sum), count, weight)
PUTU_count <- subset(PUTU_count, count!=0)
PUTU_count$species <- "PUTU"

accessory_count <- rbind(BAER_count,BOLE_count,COER_count,EPMI_count,GRBU_count,GRSP_count,HATE_count,HEPU_count,LEES_count,PELA_count,PEPU_count,PHPH_count,PILI_count,PUTU_count)

count_flower <- subset(accessory_count, what=="flower_petals"|what=="flower_petals_small",select=c("individual","count"))
names(count_flower) <- c("individual","flower_count")
count_flower$individual <- as.factor(count_flower$individual)

count_bud <- subset(accessory_count, what=="bud_tiny"|what=="bud_small"|what=="bud_mid"|what=="bud_large"|what=="flower_aborted_without_petals")
count_bud <- count_bud %>%
  group_by(individual) %>%
  summarise_each(funs(sum), count)
names(count_bud) <- c("individual","bud_count")
count_bud$individual <- as.factor(count_bud$individual)

count_seed <- subset(accessory_count, what=="seed"|what=="fruit_mature")
count_seed <- count_seed %>%
  group_by(individual) %>%
  summarise_each(funs(sum), count)
names(count_seed) <- c("individual","seed_count")
count_seed$individual <- as.factor(count_seed$individual)

count_fruit_aborted <- subset(accessory_count, what=="fruit_just_starting"|what=="fruit_young"|what=="fruit_large_immature_01"|what=="fruit_large_immature_02"|what=="fruit_large_immature_03"|what=="fruit_large_immature_04"|what==" fruit_large_immature_05"|what==" fruit_large_immature_06"|what=="fruit_empty")
count_fruit_aborted <- count_fruit_aborted %>%
  group_by(individual) %>%
  summarise_each(funs(sum), count)
names(count_fruit_aborted) <- c("individual","aborted_fruit_count")
count_fruit_aborted$individual <- as.factor(count_fruit_aborted$individual)

count_seed_pod <- subset(accessory_count, what=="seed_pod",select=c("individual","weight"))
names(count_seed_pod) <- c("individual","seedpod_weight")
count_seed_pod$individual <- as.factor(count_seed_pod$individual)

count_fruit_mature <- subset(accessory_count, what=="fruit_mature",select=c("individual","weight"))
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


SummaryIndPCA <- SummaryInd %>%
  select(height,maxH_spp,WD_mean,LMA_mean,seedset,RA,seed_size,growth_leaf,propagule_inv,prepollen_all_inv,RGR,leaf_area, seed_count)
  
  seedsize <- seedSize_raw %>%
  select(species,seed_size)

#plotting and stats
#creating lists of information for plotting symbols, colors, labels
pch.spp <- c(0,1,2,15,16,17,18,0,1,2,15,16,17,18)
col.spp <- c("red", "darkolivegreen4", "blue", "purple", "grey", "light green","gold1","cyan2","black", "chocolate1","light blue","brown","deep pink","orchid1")
col.age <-c("orchid1","gold1","darkolivegreen3","cyan2","dodgerblue","purple")
pch.age <- c(1,1,16,16,16,16)
labels.spp <- c("Banksia ericifolia","Boronia ledifolia","Conospermum ericifolium","Epacris micraphylla", "Grevillea buxifolia","Grevillea speciosa","Hakea teretifolia","Hemigenia purpurea","Leucopogon esquamatus","Persoonia lanceolata", "Petrophile puchella", "Phyllota phyllicoides", "Pimelea linifolia", "Pultanaea tuberculata")
names(labels.spp) <- c("BAER","BOLE","COER","EPMI","GRBU","GRSP","HATE","HEPU","LEES","PELA","PEPU","PHPH","PILI","PUTU")
labels.age <- c("1.4","2.4","5","7","9","32")
names(labels.age) <- c("1.4","2.4","5","7","9","32")
labels.age2 <- c("2.4","5","7","9","32")
col.age2 <-c("gold1","darkolivegreen3","cyan2","dodgerblue","purple")
labels.age3 <- c("5","7","9","32")
col.age3 <-c("darkolivegreen3","cyan2","dodgerblue","purple")
labels.spp.noHATE <- c("Banksia ericifolia","Boronia ledifolia","Conospermum ericifolium","Epacris micraphylla", "Grevillea buxifolia","Grevillea speciosa","Hemigenia purpurea","Leucopogon esquamatus","Persoonia lanceolata", "Petrophile puchella", "Phyllota phyllicoides", "Pimelea linifolia", "Pultanaea tuberculata")
names(labels.spp.noHATE) <- c("BAER","BOLE","COER","EPMI","GRBU","GRSP","HEPU","LEES","PELA","PEPU","PHPH","PILI","PUTU")
col.spp.noHATE <- c("red", "darkolivegreen4", "blue", "purple", "grey", "light green","cyan2","black", "chocolate1","light blue","brown","deep pink","orchid1")
col.LMA <-c("orchid1","gold1","darkolivegreen3","cyan2","dodgerblue","purple","red")
labels.LMA <-c("age 1.4", "age 2.4", "age 5", "age 7", "age 9", "age 32", "HATE")
col.lots <- c("red", "darkolivegreen4", "blue", "purple", "grey", "light green","gold1","cyan2","black", "chocolate1","light blue","brown","deep pink","orchid1",
              "dodgerblue","darkcyan","brown2","bisque4","blue4","cadetblue","darkseagreen","darkslateblue","firebrick4","red", "darkolivegreen4", "blue", "purple", "grey", "light green","gold1","cyan2","black", "chocolate1","light blue","brown","deep pink","orchid1",
              "dodgerblue","darkcyan","brown2","bisque4","blue4","cadetblue","darkseagreen","darkslateblue","firebrick4","red", "darkolivegreen4", "blue", "purple", "grey", "light green","gold1","cyan2","black", "chocolate1","light blue","brown","deep pink","orchid1",
              "dodgerblue","darkcyan","brown2","bisque4","blue4","cadetblue","darkseagreen","darkslateblue","firebrick4")
