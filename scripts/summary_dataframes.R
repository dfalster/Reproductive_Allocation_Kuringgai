
# load packages and "open" dataframes to use
LMA <- filter(LMA_raw, species!="" & species!=" ") %>%
  select(species,age,LMA,branch_age)
wood <- filter(woodDensity_raw, use=="use") %>%
  select(species,density)

seedsize <- seedSize_raw %>%
  select(species,seed_size)

individuals <- IndividualsList

axis_labels <- read_csv("data/axis_labels.csv")

harvest <- filter(HarvestData,
                  individuals$use_for_allocation_calculations[match(HarvestData$individual, individuals$individual)] & segment==1 & start_end=="end") %>%
  select(species,individual,age,height,total_leaf_weight,site)

investment <- ReproductionAllocation_all

investment$age <- round(investment$age, digits=1)

accessory <- AccessoryCosts_all
#accessory$age2 <- accessory$age
#accessory$age <- round(accessory$age, digits=1)

#accessory$PerAccessory <- {
#  select(accessory, Total >0,
#  1 - (accessory$Prop/accessory$Total)
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

accessory$PerPrePol_A <- accessory$PrePol_A / accessory$Total
accessory$PerPrePol_S <- accessory$PrePol_S / accessory$Total
accessory$PerPostPol_A <- accessory$PostPol_A / accessory$Total
accessory$PerPackDisp <- accessory$PD / accessory$Total
accessory$PerPropagule <- accessory$Prop / accessory$Total
accessory$PerAcc <- 1- accessory$PerPropagule


accessory$PerPrePol_A[which(is.nan(accessory$PerPrePol_A))] <- NA
accessory$PerPrePol_S[which(is.nan(accessory$PerPrePol_S))] <- NA
accessory$PerPostPol_A[which(is.nan(accessory$PerPostPol_A))] <- NA
accessory$PerPackDisp[which(is.nan(accessory$PerPackDisp))] <- NA
accessory$PerPropagule[which(is.nan(accessory$PerPropagule))] <- NA
accessory$PerAcc[which(is.nan(accessory$PerAcc))] <- NA



#adding investment and accessory costs data to leafLifespan dataframe to create a dataframe with all individual level data
SummaryInd <- merge(investment, select(leafLifespan, -species, -age), by="individual",all.x=TRUE)
SummaryInd <- merge(SummaryInd, select(accessory, -species, -age), by="individual",all.x=TRUE)
SummaryInd <- merge(SummaryInd, select(harvest, -species, -age), by="individual",all.x=TRUE)

#adding variables to look at change across years, RGR
SummaryInd$change_shoot_diam <- SummaryInd$d_end - SummaryInd$d_start
SummaryInd$change_basal_diam <- SummaryInd$FinalBasalDiam - SummaryInd$StartBasalDiam
SummaryInd$change_shoot_area <- (3.14*((SummaryInd$d_end/2)^2)) - (3.14*((SummaryInd$d_start/2)^2))
SummaryInd$change_basal_area <- (3.14*((SummaryInd$FinalBasalDiam/2)^2)) - (3.14*((SummaryInd$StartBasalDiam/2)^2))
SummaryInd$lvs_end_total <- SummaryInd$lvs_end + SummaryInd$lvs_new
SummaryInd$RGR <- log(SummaryInd$FinalWeight)-log(SummaryInd$FinalWeight-SummaryInd$GrowthInv)
SummaryInd$PrePol_All <- SummaryInd$PrePol_A + SummaryInd$PrePol_S
SummaryInd$PerPrePol_All <- SummaryInd$PerPrePol_A + SummaryInd$PerPrePol_S


#summarizing data by species, age

#leaf lifespan summary by species, age
LL_summary <- SummaryInd %>%
  filter(LL_bd>0 & LL_bd<6 & LL_birth<8 & LL_death<8) %>%
  group_by(species, age) %>%
  summarise_each(funs(mean, se, length), LL_bd, LL_death,LL_birth,new_length, lvs_end, lvs_end_total, change_shoot_diam, change_basal_diam, change_shoot_area, change_basal_area,RGR)

#LMA summary by species, age
LMA_summary <- LMA %>%
  group_by(species, age) %>%
  summarise_each(funs(mean, se, length), LMA)
names(LMA_summary)<-c("species","age","LMA_mean","LMA_se","LMA_length")

#accessory tissue summary by species, age
accessory_summary <- SummaryInd %>%
  group_by(species, age) %>%
  summarise_each(funs(mean, se, length), PrePol_A, PrePol_S, PostPol_A, PD, Prop, PrePol_All)

accessory_summary2 <- SummaryInd %>%
  filter(Total!=0) %>%
  group_by(species, age) %>%
  summarise_each(funs(mean, se, length), PerPrePol_A, PerPrePol_S, PerPostPol_A, PerPackDisp, PerPropagule, PerAcc, PerPrePol_All)

#accessory tissue by species
accessory_spp <- SummaryInd %>%
  filter(Total!=0) %>%
  group_by(species) %>%
  summarise_each(funs(mean, se, length), PerPrePol_A, PerPrePol_S, PerPostPol_A, PerPackDisp, PerPropagule, PerAcc, PerPrePol_All)

#investment summary by species, age
investment_summary <- investment %>%
  group_by(species, age) %>%
  summarise_each(funs(mean, se, length), GrowthInv, ReproInv, FinalWeight, FinalBasalDiamAv, TotalInvestment, RA)

#harvest summary by species, age
harvest_summary <- harvest %>%
  group_by(species, age) %>%
  summarise_each(funs(mean, se, length), height,total_leaf_weight)
#names(harvest_summary)<-c("species","age","height_mean","height_se","height_length")

#maximum RA for each species/age combination
maxRA <- investment %>%
  group_by(species, age) %>%
  summarise_each(funs(max), RA)
names(maxRA)<-c("species","age","maxRA")

#maximum H for each species/age combination
maxH <- harvest %>%
  group_by(species, age) %>%
  summarise_each(funs(max), height)
names(maxH)<-c("species","age","maxH")

#LMA summary by species, age
LMA_summary <- LMA %>%
  group_by(species, age) %>%
  summarise_each(funs(mean, se, length), LMA)
names(LMA_summary)<-c("species","age","LMA_mean","LMA_se","LMA_length")

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
  summarise_each(funs(mean, se, length), FinalWeight,ReproInv)

#maximum height for each species
maxH_spp <- harvest %>%
  group_by(species) %>%
  summarise_each(funs(max), height)
names(maxH_spp)<-c("species", "maxH_spp")

#maximum height for each species
MaxInvestment_spp <- investment %>%
  group_by(species) %>%
  summarise_each(funs(max), RA,ReproInv,FinalWeight)
names(MaxInvestment_spp)<-c("species", "MaxRA_spp", "MaxReproInv_spp","MaxFinalWeight_spp")

SummaryInd <- merge(SummaryInd,wood_summary, by=c("species"),all.x=TRUE)
SummaryInd <- merge(SummaryInd,LMA_summary, by=c("species","age"),all.x=TRUE)
SummaryInd <- merge(SummaryInd,maxH_spp, by=c("species"),all.x=TRUE)
SummaryInd <- merge(SummaryInd,seedsize, by=c("species"),all.x=TRUE)
SummaryInd <- merge(SummaryInd,MaxInvestment_spp, by=c("species"),all.x=TRUE)
SummaryInd$PropH <- SummaryInd$height / SummaryInd$maxH_spp
SummaryInd$PropMaxWeight <- SummaryInd$FinalWeight / SummaryInd$MaxFinalWeight_spp
SummaryInd$PropMaxRepro <- SummaryInd$ReproInv / SummaryInd$MaxReproInv_spp
SummaryInd$PropAllocation <- SummaryInd$Prop/(SummaryInd$GrowthInv + SummaryInd$ReproInv)

SummaryInd$leafArea <- SummaryInd$total_leaf_weight * SummaryInd$LMA_mean

#merge various data summaries into SummarySppAge dataframe
SummarySppAge <- merge(LL_summary, LMA_summary, by=c("species","age"))
SummarySppAge <- merge(SummarySppAge, wood_summary, by=c("species"))
SummarySppAge <- merge(SummarySppAge, seedsize, by=c("species"))
SummarySppAge <- merge(SummarySppAge, maxH_spp, by=c("species"))
SummarySppAge <- merge(SummarySppAge, maxH, by=c("species","age"))
SummarySppAge <- merge(SummarySppAge, investment_summary, by=c("species","age"))
SummarySppAge <- merge(SummarySppAge, accessory_summary, by=c("species","age"))
SummarySppAge <- merge(SummarySppAge, accessory_summary2, by=c("species","age"),all=TRUE)
SummarySppAge <- merge(SummarySppAge, harvest_summary, by=c("species","age"))

#merge various data summaries into SummarySpp dataframe
SummarySpp <- merge(LL_spp1, LL_spp2, by=c("species"))
SummarySpp <- merge(SummarySpp, LMA_spp, by=c("species"))
SummarySpp <- merge(SummarySpp, wood_summary, by=c("species"))
SummarySpp <- merge(SummarySpp, seedsize, by=c("species"))
SummarySpp <- merge(SummarySpp, maxH_spp, by=c("species"))
SummarySpp <- merge(SummarySpp, accessory_spp, by=c("species"))
SummarySpp <- merge(SummarySpp, investment_spp, by=c("species"))

SummaryInd$species <- as.factor(SummaryInd$species)
SummarySppAge$species <- as.factor(SummarySppAge$species)

#getting counts out of Konrad's reproduction data
tmp <- BAER_Investment
FD <- as.data.frame(tmp$FD)
BAER_count <- FD %>%
  group_by(Individual, what) %>%
  summarise_each(funs(sum), count, weight)
BAER_count <- subset(BAER_count, count!=0)
BAER_count$species <- "BAER"

tmp <- BOLE_Investment
FD <- as.data.frame(tmp$FD)
BOLE_count <- FD %>%
  group_by(Individual, what) %>%
  summarise_each(funs(sum), count, weight)
BOLE_count <- subset(BOLE_count, count!=0)
BOLE_count$species <- "BOLE"

tmp <- COER_Investment
FD <- as.data.frame(tmp$FD)
COER_count <- FD %>%
  group_by(Individual, what) %>%
  summarise_each(funs(sum), count, weight)
COER_count <- subset(COER_count, count!=0)
COER_count$species <- "COER"

tmp <- EPMI_Investment
FD <- as.data.frame(tmp$FD)
EPMI_count <- FD %>%
  group_by(Individual, what) %>%
  summarise_each(funs(sum), count, weight)
EPMI_count <- subset(EPMI_count, count!=0)
EPMI_count$species <- "EPMI"

tmp <- GRBU_Investment
FD <- as.data.frame(tmp$FD)
GRBU_count <- FD %>%
  group_by(Individual, what) %>%
  summarise_each(funs(sum), count, weight)
GRBU_count <- subset(GRBU_count, count!=0)
GRBU_count$species <- "GRBU"

tmp <- GRSP_Investment
FD <- as.data.frame(tmp$FD)
GRSP_count <- FD %>%
  group_by(Individual, what) %>%
  summarise_each(funs(sum), count, weight)
GRSP_count <- subset(GRSP_count, count!=0)
GRSP_count$species <- "GRSP"

tmp <- HATE_Investment
FD <- as.data.frame(tmp$FD)
HATE_count <- FD %>%
  group_by(Individual, what) %>%
  summarise_each(funs(sum), count, weight)
HATE_count <- subset(HATE_count, count!=0)
HATE_count$species <- "HATE"

tmp <- HEPU_Investment
FD <- as.data.frame(tmp$FD)
HEPU_count <- FD %>%
  group_by(Individual, what) %>%
  summarise_each(funs(sum), count, weight)
HEPU_count <- subset(HEPU_count, count!=0)
HEPU_count$species <- "HEPU"

tmp <- LEES_Investment
FD <- as.data.frame(tmp$FD)
LEES_count <- FD %>%
  group_by(Individual, what) %>%
  summarise_each(funs(sum), count, weight)
LEES_count <- subset(LEES_count, count!=0)
LEES_count$species <- "LEES"

tmp <- PELA_Investment
FD <- as.data.frame(tmp$FD)
PELA_count <- FD %>%
  group_by(Individual, what) %>%
  summarise_each(funs(sum), count, weight)
PELA_count <- subset(PELA_count, count!=0)
PELA_count$species <- "PELA"

tmp <- PEPU_Investment
FD <- as.data.frame(tmp$FD)
PEPU_count <- FD %>%
  group_by(Individual, what) %>%
  summarise_each(funs(sum), count, weight)
PEPU_count <- subset(PEPU_count, count!=0)
PEPU_count$species <- "PEPU"

tmp <- PHPH_Investment
FD <- as.data.frame(tmp$FD)
PHPH_count <- FD %>%
  group_by(Individual, what) %>%
  summarise_each(funs(sum), count, weight)
PHPH_count <- subset(PHPH_count, count!=0)
PHPH_count$species <- "PHPH"

tmp <- PILI_Investment
FD <- as.data.frame(tmp$FD)
PILI_count <- FD %>%
  group_by(Individual, what) %>%
  summarise_each(funs(sum), count, weight)
PILI_count <- subset(PILI_count, count!=0)
PILI_count$species <- "PILI"

tmp <- PUTU_Investment
FD <- as.data.frame(tmp$FD)
PUTU_count <- FD %>%
  group_by(Individual, what) %>%
  summarise_each(funs(sum), count, weight)
PUTU_count <- subset(PUTU_count, count!=0)
PUTU_count$species <- "PUTU"

accessory_count <- rbind(BAER_count,BOLE_count,COER_count,EPMI_count,GRBU_count,GRSP_count,HATE_count,HEPU_count,LEES_count,PELA_count,PEPU_count,PHPH_count,PILI_count,PUTU_count)

count_flower <- subset(accessory_count, what=="flower_petals",select=c("Individual","count"))
names(count_flower) <- c("individual","flower_count")
count_flower$individual <- as.factor(count_flower$individual)

count_bud <- subset(accessory_count, what=="bud_small"|what=="bud_mid"|what=="bud_large")
count_bud <- count_bud %>%
  group_by(Individual) %>%
  summarise_each(funs(sum), count)
names(count_bud) <- c("individual","bud_count")
count_bud$individual <- as.factor(count_bud$individual)

count_seed <- subset(accessory_count, what=="seed"|what=="fruit_mature")
count_seed <- count_seed %>%
  group_by(Individual) %>%
  summarise_each(funs(sum), count)
names(count_seed) <- c("individual","seed_count")
count_seed$individual <- as.factor(count_seed$individual)

count_fruit_aborted <- subset(accessory_count, what=="fruit_just_starting"|what=="fruit_young"|what=="fruit_large_immature_01"|what=="fruit_large_immature_02"|what=="fruit_large_immature_03"|what=="fruit_large_immature_04"|what==" fruit_large_immature_05"|what==" fruit_large_immature_06"|what=="fruit_empty")
count_fruit_aborted <- count_fruit_aborted %>%
  group_by(Individual) %>%
  summarise_each(funs(sum), count)
names(count_fruit_aborted) <- c("individual","aborted_fruit_count")
count_fruit_aborted$individual <- as.factor(count_fruit_aborted$individual)

count_seed_pod <- subset(accessory_count, what=="seed_pod",select=c("Individual","weight"))
names(count_seed_pod) <- c("individual","seedpod_weight")
count_seed_pod$individual <- as.factor(count_seed_pod$individual)

count_fruit_mature <- subset(accessory_count, what=="fruit_mature",select=c("Individual","weight"))
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
SummaryInd$fruit_weight <- SummaryInd$Prop + SummaryInd$seedpod_weight + SummaryInd$fruit_weight
SummaryInd$PrePolCount <- SummaryInd$bud_count+SummaryInd$flower_count
SummaryInd$PrePolAbort <- (SummaryInd$bud_count+SummaryInd$flower_count)/(SummaryInd$bud_count+SummaryInd$flower_count+SummaryInd$seed_count+SummaryInd$aborted_fruit_count)
SummaryInd$repro_count <- SummaryInd$bud_count+SummaryInd$flower_count+SummaryInd$seed_count

for(v in c("seedset")) {
  i <- is.na(SummaryInd[[v]])
  SummaryInd[[v]][i] <- 0
}

count_spp <- SummaryInd %>%
  filter(ReproInv>0) %>%
  group_by(species) %>%
  summarise_each(funs(mean, se, length), seedset,seed_count,flower_count,fruit_weight)

SummarySpp <- merge(SummarySpp, count_spp, by="species",all.x=TRUE)

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
