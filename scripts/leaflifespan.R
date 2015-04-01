#general results
    #strong correlation between LMA and LL when all species*site combinations included
    #LL variable across species, as expected; same for LMA
    #LL does not change as plants age; LMA does not change as plants age
    #leaf count along the leader decreases in older plants for many species - which is why LL appears to decrease



#next to do
    #check how overall plant leaf numbers changes with age, diameter
    

# load packages
library(remake)
library(dplyr)
library(tidyr)


#load files
leaves_per_length <- read.csv("~/RA schedules - Kuringgai research/Reproductive_Allocation_Kuringgai/data/leaves_per_length.csv",header=TRUE,as.is=TRUE,stringsAsFactors=FALSE)
leaf_lifespan <- read.csv("~/RA schedules - Kuringgai research/Reproductive_Allocation_Kuringgai/data/leaderGrowthLL.csv",header=TRUE,as.is=TRUE,stringsAsFactors=FALSE)
LMA <- read.csv("~/RA schedules - Kuringgai research/Reproductive_Allocation_Kuringgai/data/LMA_short.csv",header=TRUE,as.is=TRUE,stringsAsFactors=FALSE)
InvestmentSummary <- make("ReproductionAllocation_all")


#define functions
se <- function(x) sd(x)/sqrt(length(x))
con95 <- function(x) (sd(x)/sqrt(length(x)))*1.96

#leaf lifespan file subset
leaf_lifespan <- subset(leaf_lifespan,dont_use!="dead")
leaf_lifespan <- subset(leaf_lifespan,dont_use!="dont_use")
leaves_per_length <- subset(leaves_per_length,use=="yes")

#turning NAs into zeros
colnames(leaf_lifespan)
leaf_lifespan$lvs_start_length[is.na(leaf_lifespan$lvs_start_length)] <-0
leaf_lifespan$lvs_start_count[is.na(leaf_lifespan$lvs_start_count)] <-0
leaf_lifespan$lvs_end_length[is.na(leaf_lifespan$lvs_end_length)] <-0
leaf_lifespan$lvs_end_count[is.na(leaf_lifespan$lvs_end_count)] <-0
leaf_lifespan$lvs_new_length[is.na(leaf_lifespan$lvs_new_length)] <-0
leaf_lifespan$lvs_new_count[is.na(leaf_lifespan$lvs_new_count)] <-0
leaf_lifespan$new_but_shed_count[is.na(leaf_lifespan$new_but_shed_count)] <-0
leaf_lifespan$mm_lvs_spec[is.na(leaf_lifespan$mm_lvs_spec)] <-0
leaf_lifespan$count_lvs_spec[is.na(leaf_lifespan$count_lvs_spec)] <-0

#specific leaves per length data
leaf_lifespan_specific <- subset(leaf_lifespan,mm_lvs_spec!="0")
leaf_lifespan_specific$count_per_length <- leaf_lifespan_specific$count_lvs_spec/leaf_lifespan_specific$mm
leaf_lifespan_specific$leaf_start_total <- leaf_lifespan_specific$lvs_start_count + (leaf_lifespan_specific$count_per_length*leaf_lifespan_specific$lvs_start_length)
leaf_lifespan_specific$leaf_end_total <- leaf_lifespan_specific$lvs_end_count + (leaf_lifespan_specific$count_per_length*leaf_lifespan_specific$lvs_end_length)
leaf_lifespan_specific$leaf_new_total <- leaf_lifespan_specific$lvs_new_count + leaf_lifespan_specific$new_but_shed_count + (leaf_lifespan_specific$count_per_length*leaf_lifespan_specific$lvs_new_length)
head(leaf_lifespan_specific)

#generic leaves per length data
leaves_per_length$count_per_length <- leaves_per_length$leaf_count/leaves_per_length$length_mm
per_length_spp <- aggregate(count_per_length ~ species, leaves_per_length, FUN=mean)

leaf_lifespan_generic <- subset(leaf_lifespan,mm_lvs_spec=="0")
leaf_lifespan_generic <- merge(leaf_lifespan_generic, per_length_spp, by.x="species", by.y="species")
leaf_lifespan_generic$leaf_start_total <- leaf_lifespan_generic$lvs_start_count + (leaf_lifespan_generic$count_per_length*leaf_lifespan_generic$lvs_start_length)
leaf_lifespan_generic$leaf_end_total <- leaf_lifespan_generic$lvs_end_count + (leaf_lifespan_generic$count_per_length*leaf_lifespan_generic$lvs_end_length)
leaf_lifespan_generic$leaf_new_total <- leaf_lifespan_generic$lvs_new_count + leaf_lifespan_generic$new_but_shed_count + (leaf_lifespan_generic$count_per_length*leaf_lifespan_generic$lvs_new_length)
leaf_lifespan_generic <- subset(leaf_lifespan_generic,leaf_new_total>"0")
head(leaf_lifespan_generic)

#combine two files into 1
leaf_counts <- bind_rows(leaf_lifespan_specific, leaf_lifespan_generic)
head(leaf_counts)

#calculating leaf lifespan
leaf_counts$LL_bd <- (leaf_counts$leaf_start_total)/(((leaf_counts$leaf_start_total)-(leaf_counts$leaf_end_total))+(leaf_counts$leaf_new_total)/2)
leaf_counts$LL_death <- (leaf_counts$leaf_start_total)/((leaf_counts$leaf_start_total)-(leaf_counts$leaf_end_total))
leaf_counts$LL_birth <- (leaf_counts$leaf_start_total)/(leaf_counts$leaf_new_total)

leaf_counts$LL_death[which(is.infinite(leaf_counts$LL_death))] <- NA
leaf_counts$LL_bd[which(is.infinite(leaf_counts$LL_death))] <- NA
leaf_counts$LL_birth[which(is.infinite(leaf_counts$LL_death))] <- NA

write.csv(leaf_counts, file = "leaf_counts.csv")

#removng crazy numbers
leaf_counts <- subset(leaf_counts,LL_bd>0)
leaf_counts <- subset(leaf_counts,LL_bd<6)
leaf_counts <- subset(leaf_counts,LL_birth<8)
leaf_counts <- subset(leaf_counts,LL_death<8)


#leaf lifespan summary by species, age
LLSummary_av <- leaf_counts %>%
  group_by(species, age) %>%
  summarise_each(funs(mean, se, length), LL_bd, LL_death,LL_birth)

#LMA summary by species, age
LMA_summary <- LMA %>%
  group_by(species, age) %>%
  summarise_each(funs(mean, se, length), LMA)
names(LMA_summary)<-c("species","age","LMA_mean","LMA_se","LMA_length")

leaf_counts_nobabies <- subset(leaf_counts, age>2)
LMA_nobabies <- subset(LMA, age>2)

#leaf lifespan summary by species
LL_spp <- leaf_counts_nobabies %>%
  group_by(species) %>%
  summarise_each(funs(mean, se, length), LL_bd, LL_death,LL_birth)

#LMA summary by species
LMA_spp <- LMA_nobabies %>%
  group_by(species) %>%
  summarise_each(funs(mean, se, length), LMA)
names(LMA_spp)<-c("species", "LMA_mean","LMA_se","LMA_length")

#merge with LL data
LLSummary_av <- merge(LLSummary_av, LMA_summary, by.x=c("species","age"), by.y=c("species","age"))
LL_spp <- merge(LL_spp, LMA_spp, by.x=c("species"), by.y=c("species"))



#plotting and stats
#LL vs LMA
pch.spp <- c(0,1,2,15,16,17,18,0,1,2,15,16,17,18)
col.spp <- c("red", "dark green", "blue", "purple", "orange", "light green","salmon","sky blue","grey", "black","light blue","brown","orange2","salmon")
col.age <- c("red", "dark green", "light green", "light blue", "purple", "orange2")
pch.age <- c(1,1,16,16,16,16)
labels.spp <- c("Banksia","Boronia","Conospermum","Epacris", "Grevillea buxifolia","Grevillea speciosa","Hakea","Hemigenia","Leucopogon","Persoonia", "Petrophile", "Phyllota", "Pimelea", "Pultanaea")


#remove HATE, babies
LL_nobabies <- subset(LLSummary_av, age > 2)
LL_noHate <- subset(LLSummary_av, species != "HATE")
LL_noHatebabies <- subset(LL_nobabies, species != "HATE")



#relationship between LMA, LL by age, species
#just 7 year old plants
plot(LL_birth_mean ~ LMA_mean, data=subset(LL_noHate,age==2.4), xlab="LMA", ylab="leaf lifespan (years)", main="leaf lifespan versus LMA (all species and ages)", las=1)
mod <- lm(LL_birth_mean ~ LMA_mean, data=subset(LL_noHate,age==2.4))
summary(mod)
abline(mod)


#####Use next 6 lines of code for LL-LMA relationship
plot(LL_birth_mean ~ LMA_mean, LLSummary_av, xlab="LMA", ylab="leaf lifespan (years)", main="leaf lifespan versus LMA (all species and ages)", las=1)
mod <- lm(LL_birth_mean ~ LMA_mean, LLSummary_av)
abline(mod, col="red", lty=2)
mod_no_HATE <- lm(LL_birth_mean ~ LMA_mean, LL_noHate)
abline(mod_no_HATE)
summary(mod)

#plot showing LL vs LMA across, splitting points by age
#shows lack of relationship when HATE included - because suh a high LMA - and also that youngest and oldest plants have no pattern, presumably because LL data are inaccurate
LL_noHate[order(age),]
col.LMAage <-c("purple","pink","green","orange","blue","light blue")
col.LMA <-c("black","purple", "pink", "light green", "orange","blue", "light blue","red")
labels.LMA <-c("all except HATE","age 1.4", "age 2.4", "age 5", "age 7", "age 9", "age 32", "including HATE")
plot(LL_birth_mean ~ LMA_mean, LLSummary_av, xlab="LMA", ylab="leaf lifespan (years)", main="leaf lifespan versus LMA (all species and ages)", las=1,pch=17, col="red")
points(LL_birth_mean ~ LMA_mean, LL_noHate, pch=16, cex=1.2, col=col.LMAage[age])
mod <- lm(LL_birth_mean ~ LMA_mean, LLSummary_av)
abline(mod, col="red", lty=2)
mod_no_HATE <- lm(LL_birth_mean ~ LMA_mean, LL_noHate)
abline(mod_no_HATE, lwd=2)
mod_no_HATE5 <- lm(LL_birth_mean ~ LMA_mean, data=subset(LL_noHate,age==5))
abline(mod_no_HATE5, col="light green", lwd=1.5)
mod_no_HATE7 <- lm(LL_birth_mean ~ LMA_mean, data=subset(LL_noHate,age==7))
abline(mod_no_HATE7, col="orange", lwd=1.5)
mod_no_HATE9 <- lm(LL_birth_mean ~ LMA_mean, data=subset(LL_noHate,age==9))
abline(mod_no_HATE9, col="blue", lwd=1.5)
mod_no_HATE2.4 <- lm(LL_birth_mean ~ LMA_mean, data=subset(LL_noHate,age==2.4))
abline(mod_no_HATE2.4, col="pink", lwd=1.5)
mod_no_HATE1 <- lm(LL_birth_mean ~ LMA_mean, data=subset(LL_noHate,age<2.4))
abline(mod_no_HATE1, col="purple", lwd=1.5)
mod_no_HATE32 <- lm(LL_birth_mean ~ LMA_mean, data=subset(LL_noHate,age==32))
abline(mod_no_HATE32, col="light blue", lwd=1.5)

legend("topright",col=col.LMA, labels.LMA, pch=16, cex=1.4)
summary(mod)


LMA$age
LL_noHate$age


plot(LL_birth_mean ~ LMA_mean, LL_noHate, xlab="LMA", ylab="leaf lifespan (years)", main="leaf lifespan versus LMA (without HATE)", las=1)
mod_no_HATE <- lm(LL_birth_mean ~ LMA_mean, LL_noHate)
abline(mod_no_HATE)
####how could I label this line as "regression with Hakea omitted" ; i.e. place words on diagonal above line
summary(mod_no_HATE)



plot(LL_birth_mean ~ LMA_mean, LL_nobabies, xlab="LMA", ylab="leaf lifespan (years)", main="leaf lifespan versus LMA (no baby plants)", las=1)
mod <- lm(LL_birth_mean ~ LMA_mean, LL_nobabies)
abline(mod)
summary(mod)



plot(LL_birth_mean ~ LMA_mean, LL_noHatebabies, xlab="LMA", ylab="leaf lifespan (years)", main="leaf lifespan versus LMA (without HATE)", las=1)
mod <- lm(LL_birth_mean ~ LMA_mean, LL_noHatebabies)
abline(mod)
summary(mod)

#model with species included, since HATE so different from others
plot(LL_birth_mean ~ LMA_mean, LLSummary_av, xlab="LMA", ylab="leaf lifespan (years)", main="leaf lifespan versus LMA (all species and ages)", las=1)
mod <- lm(LL_birth_mean ~ LMA_mean + species, LLSummary_av)
summary(mod)


#LL by age, species, without youngest plants - graphs
par(mar = c(10, 6, 4, 1) + 0.1)

LLplot <- plot(LL_bd ~ as.factor(species), leaf_counts_nobabies, xlab="species", ylab="leaf lifespan", cex.axis=.8, las=1, xaxt="n")
LLplot <- plot(LL_bd ~ as.factor(species), leaf_counts_nobabies, xlab="species", ylab="leaf lifespan", cex.axis=.8, las=1)

text(LLplot, par("usr")[3] - 0.25, srt=45, adj=1, labels=labels.spp, xpd=TRUE)

axis(1,1:14,labels=labels.spp,col.axis="black", cex.axis=.4, tck=0)

#LL by age, species, without youngest plants - model
mod <- lm(LL_bd ~ age +species + age*species, leaf_counts_nobabies)
summary(mod)

#pdf(file="leaf lifespan plots.pdf",width=6,height=4)
plot(LL_birth~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="BAER"), xlab="age", ylab="leaf lifespan (leaf births)", cex.axis=.8, las=1, main="Banksia ericifolia")
plot(LL_birth~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="BOLE"), xlab="age", ylab="leaf lifespan (leaf births)", cex.axis=.8, las=1, main="Boronia ledifolia")
plot(LL_birth~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="COER"), xlab="age", ylab="leaf lifespan (leaf births)", cex.axis=.8, las=1, main="Conospermum ericifolium")
plot(LL_birth~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="EPMI"), xlab="age", ylab="leaf lifespan (leaf births)", cex.axis=.8, las=1, main="Epacris microphylla")
plot(LL_birth~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="GRBU"), xlab="age", ylab="leaf lifespan (leaf births)", cex.axis=.8, las=1, main="Grevillea buxifolia")
plot(LL_birth~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="GRSP"), xlab="age", ylab="leaf lifespan (leaf births)", cex.axis=.8, las=1, main="Grevillea speciosa")
plot(LL_birth~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="HATE"), xlab="age", ylab="leaf lifespan (leaf births)", cex.axis=.8, las=1, main="Hakea teretifolia")
plot(LL_birth~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="HEPU"), xlab="age", ylab="leaf lifespan (leaf births)", cex.axis=.8, las=1, main="Hemigenia purpurea")
plot(LL_birth~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="LEES"), xlab="age", ylab="leaf lifespan (leaf births)", cex.axis=.8, las=1, main="Leucopogon esquamatus")
plot(LL_birth~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="PELA"), xlab="age", ylab="leaf lifespan (leaf births)", cex.axis=.8, las=1, main="Persoonia lanceolata")
plot(LL_birth~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="PEPU"), xlab="age", ylab="leaf lifespan (leaf births)", cex.axis=.8, las=1, main="Petrophile pulchella")
plot(LL_birth~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="PHPH"), xlab="age", ylab="leaf lifespan (leaf births)", cex.axis=.8, las=1, main="Phyllota phylicoides")
plot(LL_birth~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="PILI"), xlab="age", ylab="leaf lifespan (leaf births)", cex.axis=.8, las=1, main="Pimelea linifolia")
plot(LL_birth~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="PUTU"), xlab="age", ylab="leaf lifespan (leaf births)", cex.axis=.8, las=1, main="Pultenaea tuberculata")
#dev.off()
# age not significant, but some significant interactions (COER, HATE, HEPU)
    # many species have sigificantly different LL from one another

#LMA by age, species, without youngest plants
plot(mean ~ as.factor(species), LL_nobabies, xlab="species", ylab="LMA", cex.axis=.8, las=1)
mod <- lm(LMA ~ age +species + age*species, LMA)
summary(mod)

plot(LMA ~ as.factor(age), LMA, data=subset(LMA,species=="BAER"), xlab="age", ylab="LMA", cex.axis=.8, las=1, main="Banksia ericifolia")
plot(LMA ~ as.factor(age), LMA, data=subset(LMA,species=="BOLE"), xlab="age", ylab="LMA", cex.axis=.8, las=1, main="Boronia ledifolia")
plot(LMA ~ as.factor(age), LMA, data=subset(LMA,species=="COER"), xlab="age", ylab="LMA", cex.axis=.8, las=1, main="Conospermum ericifolium")
plot(LMA ~ as.factor(age), LMA, data=subset(LMA,species=="EPMI"), xlab="age", ylab="LMA", cex.axis=.8, las=1, main="Epacris microphylla")
plot(LMA ~ as.factor(age), LMA, data=subset(LMA,species=="GRBU"), xlab="age", ylab="LMA", cex.axis=.8, las=1, main="Grevillea buxifolia")
plot(LMA ~ as.factor(age), LMA, data=subset(LMA,species=="GRSP"), xlab="age", ylab="LMA", cex.axis=.8, las=1, main="Grevillea speciosa")
plot(LMA ~ as.factor(age), LMA, data=subset(LMA,species=="HATE"), xlab="age", ylab="LMA", cex.axis=.8, las=1, main="Hakea teretifolia")
plot(LMA ~ as.factor(age), LMA, data=subset(LMA,species=="HEPU"), xlab="age", ylab="LMA", cex.axis=.8, las=1, main="Hemigenia purpurea")
plot(LMA ~ as.factor(age), LMA, data=subset(LMA,species=="LEES"), xlab="age", ylab="LMA", cex.axis=.8, las=1, main="Leucopogon esquamatus")
plot(LMA ~ as.factor(age), LMA, data=subset(LMA,species=="PELA"), xlab="age", ylab="LMA", cex.axis=.8, las=1, main="Persoonia lanceolata")
plot(LMA ~ as.factor(age), LMA, data=subset(LMA,species=="PEPU"), xlab="age", ylab="LMA", cex.axis=.8, las=1, main="Petrophile pulchella")
plot(LMA ~ as.factor(age), LMA, data=subset(LMA,species=="PHPH"), xlab="age", ylab="LMA", cex.axis=.8, las=1, main="Phyllota phylicoides")
plot(LMA ~ as.factor(age), LMA, data=subset(LMA,species=="PILI"), xlab="age", ylab="LMA", cex.axis=.8, las=1, main="Pimelea linifolia")
plot(LMA ~ as.factor(age), LMA, data=subset(LMA,species=="PUTU"), xlab="age", ylab="LMA", cex.axis=.8, las=1, main="Pultenaea tuberculata")



#models with leaf counts per age
plot(leaf_end_total ~ age, leaf_counts, xlab="age", ylab="leaf counts", cex.axis=.8, las=1)
        #####I don't understand why this plot only works when age is a factor####
plot(leaf_end_total ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="BAER"), xlab="age", ylab="leaf counts", cex.axis=.8, las=1, main="Banksia ericifolia")
plot(leaf_end_total ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="BOLE"), xlab="age", ylab="leaf counts", cex.axis=.8, las=1, main="Boronia ledifolia")
plot(leaf_end_total ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="COER"), xlab="age", ylab="leaf counts", cex.axis=.8, las=1, main="Conospermum ericifolium")
plot(leaf_end_total ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="EPMI"), xlab="age", ylab="leaf counts", cex.axis=.8, las=1, main="Epacris microphylla")
plot(leaf_end_total ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="GRBU"), xlab="age", ylab="leaf counts", cex.axis=.8, las=1, main="Grevillea buxifolia")
plot(leaf_end_total ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="GRSP"), xlab="age", ylab="leaf counts", cex.axis=.8, las=1, main="Grevillea speciosa")
plot(leaf_end_total ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="HATE"), xlab="age", ylab="leaf counts", cex.axis=.8, las=1, main="Hakea teretifolia")
plot(leaf_end_total ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="HEPU"), xlab="age", ylab="leaf counts", cex.axis=.8, las=1, main="Hemigenia purpurea")
plot(leaf_end_total ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="LEES"), xlab="age", ylab="leaf counts", cex.axis=.8, las=1, main="Leucopogon esquamatus")
plot(leaf_end_total ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="PELA"), xlab="age", ylab="leaf counts", cex.axis=.8, las=1, main="Persoonia lanceolata")
plot(leaf_end_total ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="PEPU"), xlab="age", ylab="leaf counts", cex.axis=.8, las=1, main="Petrophile pulchella")
plot(leaf_end_total ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="PHPH"), xlab="age", ylab="leaf counts", cex.axis=.8, las=1, main="Phyllota phylicoides")
plot(leaf_end_total ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="PILI"), xlab="age", ylab="leaf counts", cex.axis=.8, las=1, main="Pimelea linifolia")
plot(leaf_end_total ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="PUTU"), xlab="age", ylab="leaf counts", cex.axis=.8, las=1, main="Pultenaea tuberculata")


mod <- lm(leaf_end_total ~ age +species +age*species, leaf_counts)
summary(mod)


#LMA versus LL
plot(LL_bd_mean ~ LMA_mean, LLSummary_av, xlab="LMA",ylab="leaf lifespans (years)", main="leaf lifespan versus LMA")
legend("bottomright", c("Banksia","Boronia","Conospermum","Epacris", "Grevillea buxifolia","Grevillea speciosa","Hakea","Hemigenia","Leucopogon","Persoonia", "Petrophile", "Phyllota", "Pimelea", "Pultanaea"), col = col.spp, pch = pch.spp, text.col = "black", border = "black", title="species")

    #the above commands work, but if I try to color points by species, it fails
#plot(LL_bd_mean ~ mean , LLSummary_av, col=col.spp[species], pch=pch.spp[species])
  
  #when I try and plot by age, some ages show up and others don't
plot(LL_bd_mean ~ mean, LLSummary_av, col=col.age[age], pch=pch.age[age], xlab="LMA",ylab="leaf lifespans (years)", main="leaf lifespan versus LMA")
legend("bottomright", c("1.35","2.4","5","7","9","32"), col = col.age, pch = pch.age, text.col = "black", border = "black", title="age categories")

    #can't get syntax correct for moving this legend to locations specified by x,y coordinates




mod <- lm(LL_bd_mean ~ mean + age, LLSummary_av)
no_HATE <- lm(LL_bd_mean ~ mean + age, LLSummary_av,data=subset(LLSummary_av, species != "HATE"))
#this doesn't work
abline(mod)
summary(mod)



#playing with other information

plot(LL_bd ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="BAER"))
plot(LL_bd ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="BOLE"))
plot(LL_bd ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="COER"))
plot(LL_bd ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="GRBU"))
plot(LL_bd ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="GRSP"))
plot(LL_bd ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="HATE"))
plot(LL_bd ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="HEPU"))
plot(LL_bd ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="LEES"))
plot(LL_bd ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="PELA"))
plot(LL_bd ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="PEPU"))
plot(LL_bd ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="PHPH"))
plot(LL_bd ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="PILI"))
plot(LL_bd ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="PUTU"))



