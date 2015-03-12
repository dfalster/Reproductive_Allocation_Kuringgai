#to calculate
##seed set - count seeds/count buds



# load packages
library(remake)
library(dplyr)
library(tidyr)


#load files
seedWeights <- read.csv("~/RA schedules - Kuringgai research/learningR/data/seedsize.csv",header=TRUE,as.is=TRUE,stringsAsFactors=FALSE)
Acc_BAER <- read.csv("~/RA schedules - Kuringgai research/learningR/output/BAER_Acc.csv",header=TRUE,as.is=TRUE,stringsAsFactors=FALSE)
Acc_BOLE <- read.csv("~/RA schedules - Kuringgai research/learningR/output/BOLE_Acc.csv",header=TRUE,as.is=TRUE,stringsAsFactors=FALSE)
Acc_COER <- read.csv("~/RA schedules - Kuringgai research/learningR/output/COER_Acc.csv",header=TRUE,as.is=TRUE,stringsAsFactors=FALSE)
Acc_EPMI <- read.csv("~/RA schedules - Kuringgai research/learningR/output/EPMI_Acc.csv",header=TRUE,as.is=TRUE,stringsAsFactors=FALSE)
Acc_GRBU <- read.csv("~/RA schedules - Kuringgai research/learningR/output/GRBU_Acc.csv",header=TRUE,as.is=TRUE,stringsAsFactors=FALSE)
Acc_GRSP <- read.csv("~/RA schedules - Kuringgai research/learningR/output/GRSP_Acc.csv",header=TRUE,as.is=TRUE,stringsAsFactors=FALSE)
Acc_HATE <- read.csv("~/RA schedules - Kuringgai research/learningR/output/HATE_Acc.csv",header=TRUE,as.is=TRUE,stringsAsFactors=FALSE)
Acc_HEPU <- read.csv("~/RA schedules - Kuringgai research/learningR/output/HEPU_Acc.csv",header=TRUE,as.is=TRUE,stringsAsFactors=FALSE)
Acc_LEES <- read.csv("~/RA schedules - Kuringgai research/learningR/output/LEES_Acc.csv",header=TRUE,as.is=TRUE,stringsAsFactors=FALSE)
Acc_PELA <- read.csv("~/RA schedules - Kuringgai research/learningR/output/PELA_Acc.csv",header=TRUE,as.is=TRUE,stringsAsFactors=FALSE)
Acc_PEPU <- read.csv("~/RA schedules - Kuringgai research/learningR/output/PEPU_Acc.csv",header=TRUE,as.is=TRUE,stringsAsFactors=FALSE)
Acc_PHPH <- read.csv("~/RA schedules - Kuringgai research/learningR/output/PHPH_Acc.csv",header=TRUE,as.is=TRUE,stringsAsFactors=FALSE)
Acc_PILI <- read.csv("~/RA schedules - Kuringgai research/learningR/output/PILI_Acc.csv",header=TRUE,as.is=TRUE,stringsAsFactors=FALSE)
Acc_PUTU <- read.csv("~/RA schedules - Kuringgai research/learningR/output/PUTU_Acc.csv",header=TRUE,as.is=TRUE,stringsAsFactors=FALSE)

#define functions
se <- function(x) sd(x)/sqrt(length(x))
con95 <- function(x) (sd(x)/sqrt(length(x)))*1.96

#add species variable
species <-c("BAER")
Acc_BAER <- cbind(Acc_BAER, species)
species <-c("BOLE")
Acc_BOLE <- cbind(Acc_BOLE, species)
species <-c("COER")
Acc_COER <- cbind(Acc_COER, species)
species <-c("EPMI")
Acc_EPMI <- cbind(Acc_EPMI, species)
species <-c("GRBU")
Acc_GRBU <- cbind(Acc_GRBU, species)
species <-c("GRSP")
Acc_GRSP <- cbind(Acc_GRSP, species)
species <-c("HATE")
Acc_HATE <- cbind(Acc_HATE, species)
species <-c("HEPU")
Acc_HEPU <- cbind(Acc_HEPU, species)
species <-c("LEES")
Acc_LEES <- cbind(Acc_LEES, species)
species <-c("PELA")
Acc_PELA <- cbind(Acc_PELA, species)
species <-c("PEPU")
Acc_PEPU <- cbind(Acc_PEPU, species)
species <-c("PHPH")
Acc_PHPH <- cbind(Acc_PHPH, species)
species <-c("PILI")
Acc_PILI <- cbind(Acc_PILI, species)
species <-c("PUTU")
Acc_PUTU <- cbind(Acc_PUTU, species)


#combine files 
accessory <- bind_rows(Acc_BAER, Acc_BOLE, Acc_COER, Acc_EPMI, Acc_GRBU, Acc_GRSP, Acc_HATE, Acc_HEPU, Acc_LEES, Acc_PELA, Acc_PEPU, Acc_PHPH, Acc_PILI, Acc_PUTU)

#zeros for non reproducers
accessory_zero <- subset(accessory, Total == 0)

prepollen_A_per <- c(0)
prepollen_S_per <- c(0)
postpollen_A_per <- c(0)
packaging_per <- c(0)
propagule <- c(0)
accessory_zero <- cbind(accessory_zero, prepollen_A_per, prepollen_S_per, postpollen_A_per, packaging_per, propagule)

#proportions for reproducers
accessory_nonzero <- subset(accessory, Total != 0)

accessory_nonzero$prepollen_A_per <- accessory_nonzero$PrePol_A/accessory_nonzero$Total
accessory_nonzero$prepollen_S_per <- accessory_nonzero$PrePol_S / accessory_nonzero$Total
accessory_nonzero$postpollen_A_per <- accessory_nonzero$PostPol_A / accessory_nonzero$Total
accessory_nonzero$packaging_per <- accessory_nonzero$PD / accessory_nonzero$Total
accessory_nonzero$propagule <- accessory_nonzero$Prop / accessory_nonzero$Total

accessory_per<- bind_rows(accessory_nonzero, accessory_zero)

#summarizing data and removing zeros
accessory_per_nz <- subset(accessory_per, Total >0)
accessory_sum <- accessory_per_nz %>%
  group_by(species, age) %>%
  summarise_each(funs(mean, se, length), prepollen_A_per, prepollen_S_per, postpollen_A_per, packaging_per, propagule)

seedsize <- aggregate(seed_size ~ species, seedWeights, FUN=mean)


propagule_spp <- aggregate(propagule ~ species, data=subset(accessory_per,age>2), FUN=mean)

by_species <- merge(propagule_spp, seedsize, by.x=c("species"), by.y=c("species"))
accessory_per <- merge(accessory_per, seedsize, by.x=c("species"), by.y=c("species"))
accessory_sum <- merge(accessory_sum, seedsize, by.x=c("species"), by.y=c("species"))
by_species <- cbind(by_species,propagule_spp)
by_species$propagule <- (1-by_species$propagule)
names(by_species) <- c("species","accessory","seedsize","species","propagule")

plot(accessory ~ log(seedsize), by_species, cex=1.3, pch=16, col="blue",las=1, xlab="seed size (log mg)", ylab="accessory costs (%)", main="Relationship between seed size and accessory costs")
mod <- lm(accessory ~ log(seedsize), by_species)
summary(mod)
abline (mod)


mod <- lm(log(seedsize) ~ accessory, accessory_per)
summary(mod)
abline (mod)


#creating lists of colors, symbols, etc
pch.spp <- c(0,1,2,15,16,17,18,0,1,2,15,16,17,18)
col.spp <- c("red", "dark green", "blue", "purple", "orange", "light green","salmon","sky blue","grey", "black","light blue","brown","orange2","salmon")

#plot means by age
plot(propagule_mean ~ age,accessory_sum, data=subset(accessory_sum,species=="BAER"), xlab="age", ylab="percent", cex.axis=.8, las=1, main="Banksia ericifolia")
plot(prepollen_A_per_mean ~ age,accessory_sum)

plot(prepollen_A_per_mean ~ age,accessory_sum, xlab="age", ylab="percent", cex.axis=.8, las=1, main="Banksia ericifolia")

plot(prepollen_S_per_mean ~ age,accessory_sum)
plot(prepollen_S_per_mean ~ age,accessory_sum, col=col.spp[species])

mod <- lm(prepollen_S_per ~ age + species + age*species, accessory_per)
summary(mod)

plot(postpollen_A_per_mean ~ age,accessory_sum)
plot(packaging_per_mean ~ age,accessory_sum)




#overlying plots, all data + mean, by species

plot(prepollen_A_per_mean ~ age,accessory_sum, xlab="age", ylab="percent", cex.axis=.8, las=1,type="n", main="Banksia ericifolia")
points(propagule_mean ~ age, col="black", pch=16, cex=2, data=subset(accessory_sum,species=="BAER"))
points(propagule ~ age, col="black", pch=1, cex=.8, data=subset(accessory_per,species=="BAER"))
points(prepollen_A_per_mean ~ age, col="red", pch=16, cex=1.5, data=subset(accessory_sum,species=="BAER"))
points(prepollen_A_per ~ age, col="red", pch=1, cex=.8, data=subset(accessory_per,species=="BAER"))
points(prepollen_S_per_mean ~ age, col="blue", pch=16, cex=1.5, data=subset(accessory_sum,species=="BAER"))
points(prepollen_S_per ~ age, col="blue", pch=1, cex=.8, data=subset(accessory_per,species=="BAER"))
points(postpollen_A_per_mean ~ age, col="light green", pch=16, cex=1.5, data=subset(accessory_sum,species=="BAER"))
points(postpollen_A_per ~ age, col="light green", pch=1, cex=.8, data=subset(accessory_per,species=="BAER"))
points(packaging_per_mean ~ age, col="orange2", pch=16, cex=1.5, data=subset(accessory_sum,species=="BAER"))
points(packaging_per ~ age, col="orange2", pch=1, cex=.8, data=subset(accessory_per,species=="BAER"))


#plot all data by age


plot(propagule ~ age,accessory_per)
mod <- lm(propagule ~ age + species + age*species, accessory_per_nz)
summary(mod)

plot(prepollen_A_per ~ age,accessory_per)
mod <- lm(prepollen_A_per ~ age + species + age*species, accessory_per_nz)
summary(mod)

plot(prepollen_S_per ~ age,accessory_per)
mod <- lm(prepollen_S_per ~ age + species + age*species, accessory_per)
summary(mod)

plot(postpollen_A_per ~ age,accessory_per)
mod <- lm(postpollen_A_per ~ age + species, accessory_per)
summary(mod)

plot(packaging_per ~ age,accessory_per)
mod <- lm(packaging_per ~ age + species + age*species, accessory_per)
summary(mod)

#plot propagule by species
plot(propagule_mean ~ age,accessory_sum)
mod <- lm(propagule ~ age + species + age*species, accessory_per_nz)
summary(mod)

plot(propagule_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="BAER"), xlab="age", ylab="investment in propagules (%)", cex.axis=.8, las=1, main="Banksia ericifolia")
mod <- lm(propagule ~ age, data=subset(accessory_per_nz,species=="BAER"))
summary(mod)
plot(mod)

plot(propagule_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="BOLE"), xlab="age", ylab="investment in propagules (%)", cex.axis=.8, las=1, main="Boronia ledifolia")
mod <- lm(propagule ~ age, data=subset(accessory_per_nz,species=="BOLE"))
summary(mod)

plot(propagule_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="COER"), xlab="age", ylab="investment in propagules (%)", cex.axis=.8, las=1, main="Conospermum ericifolium")
mod <- lm(propagule ~ age, data=subset(accessory_per_nz,species=="COER"))
summary(mod)

plot(propagule_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="EPMI"), xlab="age", ylab="investment in propagules (%)", cex.axis=.8, las=1, main="Epacris microphylla")
mod <- lm(propagule ~ age, data=subset(accessory_per_nz,species=="EPMI"))
summary(mod)

plot(propagule_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="GRBU"), xlab="age", ylab="investment in propagules (%)", cex.axis=.8, las=1, main="Grevillea buxifolia")
mod <- lm(propagule ~ age, data=subset(accessory_per_nz,species=="GRBU"))
summary(mod)

plot(propagule_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="GRSP"), xlab="age", ylab="investment in propagules (%)", cex.axis=.8, las=1, main="Grevillea speciosa")
mod <- lm(propagule ~ age, data=subset(accessory_per_nz,species=="GRSP"))
summary(mod)

plot(propagule_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="HATE"), xlab="age", ylab="investment in propagules (%)", cex.axis=.8, las=1, main="Hakea teretifolia")
mod <- lm(propagule ~ age, data=subset(accessory_per_nz,species=="HATE"))
summary(mod)

plot(propagule_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="HEPU"), xlab="age", ylab="investment in propagules (%)", cex.axis=.8, las=1, main="Hemigenia purpurea")
mod <- lm(propagule ~ age, data=subset(accessory_per_nz,species=="HEPU"))
summary(mod)

plot(propagule_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="LEES"), xlab="age", ylab="investment in propagules (%)", cex.axis=.8, las=1, main="Leucopogon esquamatus")
mod <- lm(propagule ~ age, data=subset(accessory_per_nz,species=="LEES"))
summary(mod)

plot(propagule_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="PELA"), xlab="age", ylab="investment in propagules (%)", cex.axis=.8, las=1, main="Persoonia lanceolata")
mod <- lm(propagule ~ age, data=subset(accessory_per_nz,species=="PELA"))
summary(mod)

plot(propagule_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="PEPU"), xlab="age", ylab="investment in propagules (%)", cex.axis=.8, las=1, main="Petrophile pulchella")
mod <- lm(propagule ~ age, data=subset(accessory_per_nz,species=="PEPU"))
summary(mod)

plot(propagule_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="PHPH"), xlab="age", ylab="investment in propagules (%)", cex.axis=.8, las=1, main="Phyllota phylicoides")
mod <- lm(propagule ~ age, data=subset(accessory_per_nz,species=="PHPH"))
summary(mod)

plot(propagule_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="PILI"), xlab="age", ylab="investment in propagules (%)", cex.axis=.8, las=1, main="Pimelea linifolia")
mod <- lm(propagule ~ age, data=subset(accessory_per_nz,species=="PILI"))
summary(mod)

plot(propagule_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="PUTU"), xlab="age", ylab="investment in propagules (%)", cex.axis=.8, las=1, main="Pultenaea tuberculata")
mod <- lm(propagule ~ age, data=subset(accessory_per_nz,species=="PUTU"))
summary(mod)

##prepollen_abprted

plot(prepollen_A_per_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="BAER"), xlab="age", ylab="investment in prepollen_A_pers (%)", cex.axis=.8, las=1, main="Banksia ericifolia")
mod <- lm(prepollen_A_per ~ age, data=subset(accessory_per_nz,species=="BAER"))
summary(mod)
plot(mod)

plot(prepollen_A_per_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="BOLE"), xlab="age", ylab="investment in prepollen_A_pers (%)", cex.axis=.8, las=1, main="Boronia ledifolia")
mod <- lm(prepollen_A_per ~ age, data=subset(accessory_per_nz,species=="BOLE"))
summary(mod)

plot(prepollen_A_per_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="COER"), xlab="age", ylab="investment in prepollen_A_pers (%)", cex.axis=.8, las=1, main="Conospermum ericifolium")
mod <- lm(prepollen_A_per ~ age, data=subset(accessory_per_nz,species=="COER"))
summary(mod)

plot(prepollen_A_per_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="EPMI"), xlab="age", ylab="investment in prepollen_A_pers (%)", cex.axis=.8, las=1, main="Epacris microphylla")
mod <- lm(prepollen_A_per ~ age, data=subset(accessory_per_nz,species=="EPMI"))
summary(mod)

plot(prepollen_A_per_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="GRBU"), xlab="age", ylab="investment in prepollen_A_pers (%)", cex.axis=.8, las=1, main="Grevillea buxifolia")
mod <- lm(prepollen_A_per ~ age, data=subset(accessory_per_nz,species=="GRBU"))
summary(mod)

plot(prepollen_A_per_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="GRSP"), xlab="age", ylab="investment in prepollen_A_pers (%)", cex.axis=.8, las=1, main="Grevillea speciosa")
mod <- lm(prepollen_A_per ~ age, data=subset(accessory_per_nz,species=="GRSP"))
summary(mod)

plot(prepollen_A_per_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="HATE"), xlab="age", ylab="investment in prepollen_A_pers (%)", cex.axis=.8, las=1, main="Hakea teretifolia")
mod <- lm(prepollen_A_per ~ age, data=subset(accessory_per_nz,species=="HATE"))
summary(mod)

plot(prepollen_A_per_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="HEPU"), xlab="age", ylab="investment in prepollen_A_pers (%)", cex.axis=.8, las=1, main="Hemigenia purpurea")
mod <- lm(prepollen_A_per ~ age, data=subset(accessory_per_nz,species=="HEPU"))
summary(mod)

plot(prepollen_A_per_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="LEES"), xlab="age", ylab="investment in prepollen_A_pers (%)", cex.axis=.8, las=1, main="Leucopogon esquamatus")
mod <- lm(prepollen_A_per ~ age, data=subset(accessory_per_nz,species=="LEES"))
summary(mod)

plot(prepollen_A_per_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="PELA"), xlab="age", ylab="investment in prepollen_A_pers (%)", cex.axis=.8, las=1, main="Persoonia lanceolata")
mod <- lm(prepollen_A_per ~ age, data=subset(accessory_per_nz,species=="PELA"))
summary(mod)

plot(prepollen_A_per_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="PEPU"), xlab="age", ylab="investment in prepollen_A_pers (%)", cex.axis=.8, las=1, main="Petrophile pulchella")
mod <- lm(prepollen_A_per ~ age, data=subset(accessory_per_nz,species=="PEPU"))
summary(mod)

plot(prepollen_A_per_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="PHPH"), xlab="age", ylab="investment in prepollen_A_pers (%)", cex.axis=.8, las=1, main="Phyllota phylicoides")
mod <- lm(prepollen_A_per ~ age, data=subset(accessory_per_nz,species=="PHPH"))
summary(mod)

plot(prepollen_A_per_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="PILI"), xlab="age", ylab="investment in prepollen_A_pers (%)", cex.axis=.8, las=1, main="Pimelea linifolia")
mod <- lm(prepollen_A_per ~ age, data=subset(accessory_per_nz,species=="PILI"))
summary(mod)

plot(prepollen_A_per_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="PUTU"), xlab="age", ylab="investment in prepollen_A_pers (%)", cex.axis=.8, las=1, main="Pultenaea tuberculata")
mod <- lm(prepollen_A_per ~ age, data=subset(accessory_per_nz,species=="PUTU"))
summary(mod)

#post pollination aborted
plot(postpollen_A_per_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="BAER"), xlab="age", ylab="investment in postpollen_A_pers (%)", cex.axis=.8, las=1, main="Banksia ericifolia")
mod <- lm(postpollen_A_per ~ age, data=subset(accessory_per_nz,species=="BAER"))
summary(mod)
plot(mod)

plot(postpollen_A_per_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="BOLE"), xlab="age", ylab="investment in postpollen_A_pers (%)", cex.axis=.8, las=1, main="Boronia ledifolia")
mod <- lm(postpollen_A_per ~ age, data=subset(accessory_per_nz,species=="BOLE"))
summary(mod)

plot(postpollen_A_per_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="COER"), xlab="age", ylab="investment in postpollen_A_pers (%)", cex.axis=.8, las=1, main="Conospermum ericifolium")
mod <- lm(postpollen_A_per ~ age, data=subset(accessory_per_nz,species=="COER"))
summary(mod)

plot(postpollen_A_per_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="EPMI"), xlab="age", ylab="investment in postpollen_A_pers (%)", cex.axis=.8, las=1, main="Epacris microphylla")
mod <- lm(postpollen_A_per ~ age, data=subset(accessory_per_nz,species=="EPMI"))
summary(mod)

plot(postpollen_A_per_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="GRBU"), xlab="age", ylab="investment in postpollen_A_pers (%)", cex.axis=.8, las=1, main="Grevillea buxifolia")
mod <- lm(postpollen_A_per ~ age, data=subset(accessory_per_nz,species=="GRBU"))
summary(mod)

plot(postpollen_A_per_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="GRSP"), xlab="age", ylab="investment in postpollen_A_pers (%)", cex.axis=.8, las=1, main="Grevillea speciosa")
mod <- lm(postpollen_A_per ~ age, data=subset(accessory_per_nz,species=="GRSP"))
summary(mod)

plot(postpollen_A_per_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="HATE"), xlab="age", ylab="investment in postpollen_A_pers (%)", cex.axis=.8, las=1, main="Hakea teretifolia")
mod <- lm(postpollen_A_per ~ age, data=subset(accessory_per_nz,species=="HATE"))
summary(mod)

plot(postpollen_A_per_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="HEPU"), xlab="age", ylab="investment in postpollen_A_pers (%)", cex.axis=.8, las=1, main="Hemigenia purpurea")
mod <- lm(postpollen_A_per ~ age, data=subset(accessory_per_nz,species=="HEPU"))
summary(mod)

plot(postpollen_A_per_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="LEES"), xlab="age", ylab="investment in postpollen_A_pers (%)", cex.axis=.8, las=1, main="Leucopogon esquamatus")
mod <- lm(postpollen_A_per ~ age, data=subset(accessory_per_nz,species=="LEES"))
summary(mod)

plot(postpollen_A_per_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="PELA"), xlab="age", ylab="investment in postpollen_A_pers (%)", cex.axis=.8, las=1, main="Persoonia lanceolata")
mod <- lm(postpollen_A_per ~ age, data=subset(accessory_per_nz,species=="PELA"))
summary(mod)

plot(postpollen_A_per_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="PEPU"), xlab="age", ylab="investment in postpollen_A_pers (%)", cex.axis=.8, las=1, main="Petrophile pulchella")
mod <- lm(postpollen_A_per ~ age, data=subset(accessory_per_nz,species=="PEPU"))
summary(mod)

plot(postpollen_A_per_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="PHPH"), xlab="age", ylab="investment in postpollen_A_pers (%)", cex.axis=.8, las=1, main="Phyllota phylicoides")
mod <- lm(postpollen_A_per ~ age, data=subset(accessory_per_nz,species=="PHPH"))
summary(mod)

plot(postpollen_A_per_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="PILI"), xlab="age", ylab="investment in postpollen_A_pers (%)", cex.axis=.8, las=1, main="Pimelea linifolia")
mod <- lm(postpollen_A_per ~ age, data=subset(accessory_per_nz,species=="PILI"))
summary(mod)

plot(postpollen_A_per_mean ~ as.factor(age), accessory_sum, data=subset(accessory_sum,species=="PUTU"), xlab="age", ylab="investment in postpollen_A_pers (%)", cex.axis=.8, las=1, main="Pultenaea tuberculata")
mod <- lm(postpollen_A_per ~ age, data=subset(accessory_per_nz,species=="PUTU"))
summary(mod)

