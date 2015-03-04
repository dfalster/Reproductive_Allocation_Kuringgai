se <- function(x) sd(x)/sqrt(length(x))
con95 <- function(x) (sd(x)/sqrt(length(x)))*1.96

InvestmentSummary <- read.csv("~/RA schedules - Kuringgai research/learningR/output/InvestmentSummary.csv",header=TRUE,row.names=1,as.is=TRUE,stringsAsFactors=FALSE)
WoodDensity <- read.csv("~/RA schedules - Kuringgai research/learningR/data/woodDensity.csv",header=TRUE,row.names=1,as.is=TRUE,stringsAsFactors=FALSE)
LMA <- read.csv("~/RA schedules - Kuringgai research/learningR/data/LMA_short.csv",header=TRUE,as.is=TRUE,stringsAsFactors=FALSE)
harvest <- read.csv("~/RA schedules - Kuringgai research/learningR/data/harvest.csv",header=TRUE,as.is=TRUE,stringsAsFactors=FALSE)
#flowerParts <- read.csv("~/RA schedules - Kuringgai research/learningR/data/flowerParts.csv",header=TRUE,as.is=TRUE,stringsAsFactors=FALSE)
seedWeights <- read.csv("~/RA schedules - Kuringgai research/learningR/data/seedsize.csv",header=TRUE,as.is=TRUE,stringsAsFactors=FALSE)
leaves_per_length <- read.csv("~/RA schedules - Kuringgai research/learningR/data/leaves_per_length.csv",header=TRUE,as.is=TRUE,stringsAsFactors=FALSE)
leaf_lifespan <- read.csv("~/RA schedules - Kuringgai research/learningR/data/leaderGrowthLL.csv",header=TRUE,as.is=TRUE,stringsAsFactors=FALSE)


###need to subset wood density for column $use equal "use"
WoodDensity <- subset(WoodDensity,use=="use")
leaves_per_length <- subset(leaves_per_length,use=="yes")

##leaf_lifespan$lvs_start_count <- as.numeric(leaf_lifespan$lvs_start_count)
harvest <- subset(harvest,species!="xx")
seeds <- subset(flowerParts,part=="seed")
fruit <- subset(flowerParts,part=="fruit_mature")
seedpod <- subset(flowerParts,part=="seed_pod")

View(InvestmentSummary)
View(WoodDensity)

colnames(InvestmentSummary)
colnames(WoodDensity)
colnames(LMA)




InvestmentSummary$agexspp <- paste(InvestmentSummary$species, InvestmentSummary$age, sep="_")
LMA$agexspp <- paste(LMA$species, LMA$age, sep="_")
  
repro <- aggregate(ReproInv ~ species + age, InvestmentSummary, FUN=mean)
repro_se <- aggregate(ReproInv ~ species + age, InvestmentSummary, FUN=se)

growth <- aggregate(GrowthInv ~ species + age, InvestmentSummary, FUN=mean)
growth_se <- aggregate(GrowthInv ~ species + age, InvestmentSummary, FUN=se)

FinalWeight <- aggregate(FinalWeight ~ species + age, InvestmentSummary, FUN=mean)

RA <- aggregate(RA ~ species + age, InvestmentSummary, FUN=mean)
RA_se <- aggregate(RA ~ species + age, InvestmentSummary, FUN=se)

wood <- aggregate(density ~ species, WoodDensity, FUN=mean)
wood_se <- aggregate(density ~ species, WoodDensity, FUN=se)

LMA <- aggregate(LMA ~ species + age, LMA, FUN=mean)
#LMA_se <- aggregate(LMA ~ species + age, LMA, FUN=se)

maxH  <- aggregate(height ~ species, harvest, FUN=max)
maxH_age <- aggregate(height ~ species + age, harvest, FUN=max)

seedsize <- aggregate(seed_size ~ species, seedWeights, FUN=mean)

#leaf lifespan file cleanup
leaf_lifespan <- subset(leaf_lifespan,dont_use!="dead")
leaf_lifespan <- subset(leaf_lifespan,dont_use!="dont_use")

#specific leaves per length data
#turning NAs into zeros
colnames(leaf_lifespan)
leaf_lifespan$lvs_start_length[is.na(leaf_lifespan$lvs_start_length)] <-0
leaf_lifespan$lvs_start_count[is.na(leaf_lifespan$lvs_start_count)] <-0
leaf_lifespan$lvs_end_length[is.na(leaf_lifespan$lvs_end_length)] <-0
leaf_lifespan$lvs_end_count[is.na(leaf_lifespan$lvs_end_count)] <-0
leaf_lifespan$lvs_new_length[is.na(leaf_lifespan$lvs_new_length)] <-0
leaf_lifespan$lvs_new_count[is.na(leaf_lifespan$lvs_new_count)] <-0
leaf_lifespan$new.shed[is.na(leaf_lifespan$new.shed)] <-0
leaf_lifespan$mm[is.na(leaf_lifespan$mm)] <-0
leaf_lifespan$leaves[is.na(leaf_lifespan$leaves)] <-0

leaf_lifespan_specific <- subset(leaf_lifespan,mm!="0")
leaf_lifespan_specific$spec_leaves_length <- leaf_lifespan_specific$leaves/leaf_lifespan_specific$mm
leaf_lifespan_specific$leaf_start_total <- leaf_lifespan_specific$lvs_start_count + (leaf_lifespan_specific$spec_leaves_length*leaf_lifespan_specific$lvs_start_length)
leaf_lifespan_specific$leaf_end_total <- leaf_lifespan_specific$lvs_end_count + (leaf_lifespan_specific$spec_leaves_length*leaf_lifespan_specific$lvs_end_length)
leaf_lifespan_specific$leaf_new_total <- leaf_lifespan_specific$lvs_new_count + (leaf_lifespan_specific$spec_leaves_length*leaf_lifespan_specific$lvs_new_length)
leaf_lifespan_specific

#generic leaves per length data
leaves_per_length$count_per_length <- leaves_per_length$leaf_count/leaves_per_length$length_mm
per_length_spp <- aggregate(count_per_length ~ species, leaves_per_length, FUN=mean)
per_length_spp


leaf_lifespan_generic <- subset(leaf_lifespan,mm=="0")
leaf_lifespan_generic <- merge(leaf_lifespan_generic, per_length_spp, by.x="species", by.y="species")
leaf_lifespan_generic$leaf_start_total <- leaf_lifespan_generic$lvs_start_count + (leaf_lifespan_generic$count_per_length*leaf_lifespan_generic$lvs_start_length)
leaf_lifespan_generic$leaf_end_total <- leaf_lifespan_generic$lvs_end_count + (leaf_lifespan_generic$count_per_length*leaf_lifespan_generic$lvs_end_length)
leaf_lifespan_generic$leaf_new_total <- leaf_lifespan_generic$lvs_new_count + (leaf_lifespan_generic$count_per_length*leaf_lifespan_generic$lvs_new_length)
leaf_lifespan_generic

#trying without two dataframes
leaves_per_length$count_per_length <- leaves_per_length$leaf_count/leaves_per_length$length_mm
per_length_spp <- aggregate(count_per_length ~ species, leaves_per_length, FUN=mean)
per_length_spp

###HELP HERE
ifelse(leaf_lifespan$mm!="0",
       leaf_lifespan$count_per_length2 <- leaves_per_length$leaf_count/leaves_per_length$mm,
       leaf_lifespan$count_per_length2 <- merge(leaf_lifespan_generic, per_length_spp, by.x="species", by.y="species"))

leaf_lifespan$leaf_start_total <- leaf_lifespan$lvs_start_count + (leaf_lifespan$count_per_length2*leaf_lifespan$lvs_start_length)
leaf_lifespan$leaf_end_total <- leaf_lifespan$lvs_end_count + (leaf_lifespa$count_per_length2*leaf_lifespan$lvs_end_length)
leaf_lifespan$leaf_new_total <- leaf_lifespanlvs_new_count + (leaf_lifespan$count_per_length2*leaf_lifespan$lvs_new_length)




leaf_lifespan_specific <- subset(leaf_lifespan,mm!="0")
leaf_lifespan_specific$spec_leaves_length <- leaf_lifespan_specific$leaves/leaf_lifespan_specific$mm
leaf_lifespan_specific$leaf_start_total <- leaf_lifespan_specific$lvs_start_count + (leaf_lifespan_specific$spec_leaves_length*leaf_lifespan_specific$lvs_start_length)
leaf_lifespan_specific$leaf_end_total <- leaf_lifespan_specific$lvs_end_count + (leaf_lifespan_specific$spec_leaves_length*leaf_lifespan_specific$lvs_end_length)
leaf_lifespan_specific$leaf_new_total <- leaf_lifespan_specific$lvs_new_count + (leaf_lifespan_specific$spec_leaves_length*leaf_lifespan_specific$lvs_new_length)
leaf_lifespan_specific







#ifelse


#seeds$weight_per_seed <- seeds$weight/seeds$count
#seed_weight <- aggregate(weight_per_seed ~ species, seeds, FUN=mean)

#fruit$weight_per_fruit <- fruit$weight/fruit$count
#fruit_weight <- aggregate(weight_per_fruit ~ species, fruit, FUN=mean)

#seedpod$weight_per_pod <- seedpod$weight/seedpod$count
#seedpod_weight <- aggregate(weight_per_pod ~ species, seedpod, FUN=mean)


colnames(InvestmentSummary)
colnames(WoodDensity)
colnames(LMA)

rm(Combined)

Combined <- merge(repro, repro_se, by.x=c("species","age"), by.y=c("species","age"))
Combined <- merge(Combined, growth, by.x=c("species","age"), by.y=c("species","age"))
Combined <- merge(Combined, growth_se, by.x=c("species","age"), by.y=c("species","age"))
Combined <- merge(Combined, FinalWeight, by.x=c("species","age"), by.y=c("species","age"))

Combined <- merge(Combined, RA, by.x=c("species","age"), by.y=c("species","age"))
Combined <- merge(Combined, RA_se, by.x=c("species","age"), by.y=c("species","age"))
Combined <- merge(Combined, wood, by.x=c("species"), by.y=c("species"))
Combined <- merge(Combined, wood_se, by.x=c("species"), by.y=c("species"))
Combined <- merge(Combined, LMA, by.x=c("species","age"), by.y=c("species","age"))
#Combined <- merge(Combined, LMA_se, by.x=c("species","age"), by.y=c("species","age"))
Combined <- merge(Combined, maxH, by.x="species", by.y="species")
Combined <- merge(Combined, maxH_age, by.x=c("species","age"), by.y=c("species","age"))
Combined <- merge(Combined, seedsize, by.x="species", by.y="species")

names(Combined)<-c("Species","age","Repro","Repro_SE","Growth","Growth_SE","FinalWeight", "RA","RA_SE","WoodDensity","WoodDensity_SE","LMA","maxH","maxH_age","seedWeight")
colnames(Combined)

Combined$age2 <- as.factor(Combined$age)
Combined$Repro <- round(Combined$Repro,digits=2)
Combined$Repro_SE <- round(Combined$Repro_SE,digits=2)
Combined$Growth <- round(Combined$Growth,digits=2)
Combined$Growth_SE <- round(Combined$Growth_SE,digits=2)
Combined$RA <- round(Combined$RA,digits=3)
Combined$RA_SE <- round(Combined$RA_SE,digits=3)
Combined$WoodDensity <- round(Combined$WoodDensity,digits=3)
Combined$WoodDensity_SE <- round(Combined$WoodDensity_SE,digits=3)
Combined$LMA <- round(Combined$LMA,digits=3)
Combined$logRepro <- log(Combined$Repro+1)
Combined$logGrowth <- log(Combined$Growth+1)
Combined$logFinalWeight <- log(Combined$FinalWeight)
nonzeroRA <- subset (Combined, RA != 0)
youngplants <- subset (Combined, age < 4)
oldplants <- subset (Combined, age > 4)
#nonzeroRA <- subset (nonzeroRA, age != 1.35)
#nonzeroRA <- subset (nonzeroRA, age != 2.4)
#Combined$LMA_SE <- round(Combined$LMA_SE,digits=3)
Combined

#setting colors, symbols, symbol sizes
pch.age <- c(0,1,2,15,16,17)
col.age <- c("red", "dark green", "blue", "purple", "orange", "light green")
cex.age <- c(.2,.5,.8,1.2,1.5,2)

##RV curve
BAER <- subset (Combined, Species == "BAER")
plot (logRepro ~ logFinalWeight, BAER)

BOLE <- subset (Combined, Species == "BOLE")
plot (logRepro ~ logFinalWeight, BOLE)

COER <- subset (Combined, Species == "COER")
plot (logRepro ~ logFinalWeight, COER)

EPMI <- subset (Combined, Species == "EPMI")
plot (logRepro ~ logFinalWeight, EPMI)

LEES <- subset (Combined, Species == "LEES")
plot (logRepro ~ logFinalWeight, LEES)

PUTU <- subset (Combined, Species == "PUTU")
plot (logRepro ~ logFinalWeight, PUTU)


##LMA vs non-zero RA
plot(LMA ~ RA, nonzeroRA, col=col.age[age2], pch=pch.age[age2], cex=cex.age[age2])
mod2 <- lm(RA ~ LMA * as.factor(age), nonzeroRA)
summary(mod2)
anova(mod2)
abline(mod2)
plot(mod2)

##nothing significant

##LMA vs growth

plot(LMA ~ logGrowth, Combined, col=col.age[age2], pch=pch.age[age2], cex=cex.age[age2])
mod <- lm(logGrowth ~ LMA * age2, Combined)
summary(mod)
anova(mod)
abline(mod)
plot(mod)
##only intercept significant



##Wood Density vs growth

plot(WoodDensity ~ logGrowth, Combined, col=col.age[age2], pch=pch.age[age2], cex=cex.age[age2])
mod <- lm(logGrowth ~ WoodDensity * age2, Combined)
summary(mod)
anova(mod)
abline(mod)
plot(mod)
##older plants relationship different from younger plants in both slope and intercept

plot(WoodDensity ~ logGrowth, youngplants, col=col.age[age2], pch=pch.age[age2], cex=cex.age[age2])
mod <- lm(logGrowth ~ WoodDensity * age2, youngplants)
summary(mod)
anova(mod)
abline(mod)

plot(mod)


plot(WoodDensity ~ logGrowth, oldplants, col=col.age[age2], pch=pch.age[age2], cex=cex.age[age2])
mod <- lm(logGrowth ~ WoodDensity * age2, oldplants)
summary(mod)
anova(mod)
abline(mod)
plot(mod)


plot(LMA ~ logGrowth, youngplants, col=col.age[age2], pch=pch.age[age2], cex=cex.age[age2])
mod <- lm(logGrowth ~ LMA * age2, youngplants)
summary(mod)
anova(mod)
abline(mod)

plot(mod)



plot(LMA ~ logGrowth, oldplants, col=col.age[age2], pch=pch.age[age2], cex=cex.age[age2])
mod <- lm(logGrowth ~ LMA * age2, oldplants)
summary(mod)
anova(mod)
abline(mod)
plot(mod)


#for both LMA and wood density, no effect of age within "old" versus "young" categories, so rerunning models without age term

plot(WoodDensity ~ logGrowth, youngplants, col=col.age[age2], pch=pch.age[age2], cex=cex.age[age2])
mod <- lm(logGrowth ~ WoodDensity, youngplants)
summary(mod)
anova(mod)
abline(mod)
plot(mod)

plot(WoodDensity ~ logGrowth, oldplants, col=col.age[age2], pch=pch.age[age2], cex=cex.age[age2])
mod <- lm(logGrowth ~ WoodDensity, oldplants)
summary(mod)
anova(mod)
abline(mod)
plot(mod)


plot(LMA ~ logGrowth, youngplants, col=col.age[age2], pch=pch.age[age2], cex=cex.age[age2])
mod <- lm(logGrowth ~ LMA, youngplants)
summary(mod)
anova(mod)
abline(mod)

plot(mod)

plot(LMA ~ logGrowth, oldplants, col=col.age[age2], pch=pch.age[age2], cex=cex.age[age2])
mod <- lm(logGrowth ~ LMA, oldplants)
summary(mod)
anova(mod)
abline(mod)
plot(mod)




##LMA vs growth

plot(LMA ~ logGrowth, Combined, col=col.age[age2], pch=pch.age[age2], cex=cex.age[age2])
mod <- lm(logGrowth ~ LMA * age2, Combined)
summary(mod)
anova(mod)
abline(mod)
plot(mod)
##only intercept significant






plot(Combined$RA,Combined$LMA,col="red",xlab="RA",ylab="LMA")
mod <- lm(Combined$LMA~Combined$RA)
abline(mod, col="blue")
summary(mod)

plot(Combined$RA,Combined$LMA,col="red",xlab="RA",ylab="LMA")
mod <- lm(Combined$LMA~Combined$RA)
abline(mod, col="blue")
summary(mod)





plot(LMA ~ RA, Combined, col=col.age[as.factor(age)], pch=pch.age[as.factor(age)], cex=cex.age[as.factor(age)])

mod2 <- lm(RA ~ LMA * as.factor(age), Combined)

summary(mod2)
anova(mod2)

abline(mod)


#using log growth
pch.age <- c(15,16,17,15,16,17)
col.age <- c("red", "dark green", "skyblue3", "purple", "orange2", "light green")
cex.age <- c(.6,.8,1,1.2,1.5,2)


plot(LMA ~ logGrowth, Combined, col=col.age[age2], pch=pch.age[age2], cex=cex.age[as.factor(age)])

mod3 <- lm(logGrowth ~ LMA * age2, Combined)

summary(mod3)
anova(mod3)

pch.age <- c(15,16,17,15,16,17)
col.age <- c("red", "dark green", "skyblue3", "purple", "orange2", "light green")
cex.age <- c(.6,.8,1,1.2,1.5,2)


plot(WoodDensity ~ logGrowth, Combined, col=col.age[age2], pch=pch.age[age2], cex=cex.age[as.factor(age)])

mod3 <- lm(logGrowth ~ WoodDensity * age2, Combined)

summary(mod3)
anova(mod3)


pch.age <- c(2,3,15,16,17,1)
col.age <- c("red", "dark green", "skyblue2", "purple", "salmon2", "light green")
positiveRA <- (Combined$RA!=0)

plot(RA ~ logGrowth, Combined, col=col.age[age2], pch=pch.age[age2], cex=cex.age[as.factor(age)])

mod4 <- lm(logGrowth ~ RA * age2, Combined)

summary(mod4)
anova(mod4)


  
pch.age <- c(2,3,15,16,17,1)
col.age <- c("red", "dark green", "skyblue2", "purple", "salmon2", "light green")
positiveRA <- (Combined$RA!=0)

plot(RA ~ logGrowth, Combined, col=col.age[age2], pch=pch.age[age2], cex=cex.age[as.factor(age)])

mod4 <- lm(logGrowth ~ RA * age2, Combined)

summary(mod4)
anova(mod4)

