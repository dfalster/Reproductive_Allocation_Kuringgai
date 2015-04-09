```{r, echo=FALSE,results='hide', message=FALSE}

# load packages and "open" dataframes to use
library(remake)
create_bindings()
```

```{r, echo=FALSE,results='hide', message=FALSE}
LMA <- filter(LMA_raw, species!="" & species!=" ") %>%
  select(species,age,LMA,branch_age)

wood <- filter(woodDensity_raw, use=="use") %>%
  select(species,density)

seedsize <- seedSize_raw %>%
  select(species,seed_size)

individuals <- IndividualsList


harvest <- filter(HarvestData, 
                  individuals$use_for_allocation_calculations[match(HarvestData$individual, individuals$individual)] & segment==1 & start_end=="end") %>%
  select(species,individual,age,height,total_leaf_weight,site)

investment <- ReproductionAllocation_all

investment$age <- round(investment$age, digits=1)

accessory <- AccessoryCosts_all
accessory$age2 <- accessory$age
accessory$age <- round(accessory$age, digits=1)

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
SummaryInd <- merge(leafLifespan, select(investment, -species, -age), by="individual")
SummaryInd <- merge(SummaryInd, select(accessory, -species, -age, -age2), by="individual")
SummaryInd <- merge(SummaryInd, select(harvest, -species, -age, -age2), by="individual")

#adding variables to look at change across years, RGR
SummaryInd$change_shoot_diam <- SummaryInd$d_end - SummaryInd$d_start
SummaryInd$change_basal_diam <- SummaryInd$FinalBasalDiam - SummaryInd$StartBasalDiam
SummaryInd$change_shoot_area <- (3.14*((SummaryInd$d_end/2)^2)) - (3.14*((SummaryInd$d_start/2)^2))
SummaryInd$change_basal_area <- (3.14*((SummaryInd$FinalBasalDiam/2)^2)) - (3.14*((SummaryInd$StartBasalDiam/2)^2))
SummaryInd$lvs_end_total <- SummaryInd$lvs_end + SummaryInd$lvs_new
SummaryInd$RGR <- log(SummaryInd$FinalWeight)-log(SummaryInd$FinalWeight-SummaryInd$GrowthInv)
```

```{r, echo=FALSE,results='hide', message=FALSE}
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
accessory_summary <- accessory %>%
  group_by(species, age) %>%
  summarise_each(funs(mean, se, length), PrePol_A, PrePol_S, PostPol_A, PD, Prop)

accessory_summary2 <- accessory %>%
  filter(Total!=0) %>%
  group_by(species, age) %>%
  summarise_each(funs(mean, se, length), PerPrePol_A, PerPrePol_S, PerPostPol_A, PerPackDisp, PerPropagule, PerAcc)

#investment summary by species, age
investment_summary <- investment %>%
  group_by(species, age) %>%
  summarise_each(funs(mean, se, length), GrowthInv, FinalWeight, FinalBasalDiamAv, TotalInvestment, RA)

#harvest summary by species, age
harvest_summary <- harvest %>%
  group_by(species, age) %>%
  summarise_each(funs(mean, se, length), height,total_leaf_weight)
#names(harvest_summary)<-c("species","age","height_mean","height_se","height_length")

#RA for each species/age combination
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
LL_spp <- leafLifespan %>%
  filter(age>2) %>%
  group_by(species) %>%
  summarise_each(funs(mean, se, length), LL_bd, LL_death,LL_birth)

#LMA summary by species
LMA_spp <- LMA %>%
  filter(age>2) %>%
  group_by(species) %>%
  summarise_each(funs(mean, se, length), LMA)
names(LMA_spp)<-c("species", "LMA_mean","LMA_se","LMA_length")

#maximum height for each species
maxH_spp <- harvest %>%
  group_by(species) %>%
  summarise_each(funs(max), height)
names(maxH_spp)<-c("species", "maxH_spp")

SummaryInd <- merge(SummaryInd,wood_summary, by=c("species"))
SummaryInd <- merge(SummaryInd,LMA_summary, by=c("species","age"))
SummaryInd <- merge(SummaryInd,maxH_spp, by=c("species"))
SummaryInd <- merge(SummaryInd,seedsize, by=c("species"))

SummaryInd$leafArea <- SummaryInd$total_leaf_weight * SummaryInd$LMA_mean

#merge various data summaries into SummarySppAge dataframe
SummarySppAge <- merge(LL_summary, LMA_summary, by=c("species","age"))
SummarySppAge <- merge(SummarySppAge, wood_summary, by=c("species"))
SummarySppAge <- merge(SummarySppAge, seedsize, by=c("species"))
SummarySppAge <- merge(SummarySppAge, maxH_spp, by=c("species"))
SummarySppAge <- merge(SummarySppAge, maxH, by=c("species","age"))
SummarySppAge <- merge(SummarySppAge, investment_summary, by=c("species","age"))
SummarySppAge <- merge(SummarySppAge, accessory_summary, by=c("species","age"))
SummarySppAge <- merge(SummarySppAge, accessory_summary2, by=c("species","age"))
SummarySppAge <- merge(SummarySppAge, harvest_summary, by=c("species","age"))

#merge various data summaries into SummarySpp dataframe
SummarySpp <- merge(LL_spp, LMA_spp, by=c("species"))
SummarySpp <- merge(SummarySpp, wood_summary, by=c("species"))
SummarySpp <- merge(SummarySpp, seedsize, by=c("species"))
SummarySpp <- merge(SummarySpp, select(maxH, -age), by=c("species"))

#plotting and stats
#creating lists of information for plotting symbols, colors, labels
pch.spp <- c(0,1,2,15,16,17,18,0,1,2,15,16,17,18)
col.spp <- c("red", "dark green", "blue", "purple", "orange", "light green","salmon","turquoise","grey", "black","light blue","brown","yellow","salmon")
col.age <-c("purple","pink","green","orange","blue","dark green")
pch.age <- c(1,1,16,16,16,16)
labels.spp <- c("Banksia ericifolia","Boronia ledifolia","Conospermum ericifolium","Epacris micraphylla", "Grevillea buxifolia","Grevillea speciosa","Hakea teretifolia","Hemigenia purpurea","Leucopogon esquamatus","Persoonia lanceolata", "Petrophile puchella", "Phyllota phyllicoides", "Pimelea linifolia", "Pultanaea tuberculata")
names(labels.spp) <- c("BAER","BOLE","COER","EPMI","GRBU","GRSP","HATE","HEPU","LEES","PELA","PEPU","PHPH","PILI","PUTU")
labels.age <- c("1.4","2.4","5","7","9","32")
names(labels.age) <- c("1.4","2.4","5","7","9","32")
labels.age2 <- c("2.4","5","7","9","32")
col.age2 <-c("pink","green","orange","blue","dark green")
```

#writing csv files for checking errors
write.csv(SummaryInd, file = "SummaryInd.csv")
