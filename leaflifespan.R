#load files
leaves_per_length <- read.csv("~/RA schedules - Kuringgai research/Reproductive_Allocation_Kuringgai/data/leaves_per_length.csv",header=TRUE,as.is=TRUE,stringsAsFactors=FALSE)
leaf_lifespan <- read.csv("~/RA schedules - Kuringgai research/Reproductive_Allocation_Kuringgai/data/leaderGrowthLL.csv",header=TRUE,as.is=TRUE,stringsAsFactors=FALSE)

# load packages
library(remake)
library(dplyr)
library(tidyr)

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
leaf_lifespan_specific$spec_leaves_length <- leaf_lifespan_specific$count_lvs_spec/leaf_lifespan_specific$mm
leaf_lifespan_specific$leaf_start_total <- leaf_lifespan_specific$lvs_start_count + (leaf_lifespan_specific$spec_leaves_length*leaf_lifespan_specific$lvs_start_length)
leaf_lifespan_specific$leaf_end_total <- leaf_lifespan_specific$lvs_end_count + (leaf_lifespan_specific$spec_leaves_length*leaf_lifespan_specific$lvs_end_length)
leaf_lifespan_specific$leaf_new_total <- leaf_lifespan_specific$lvs_new_count + leaf_lifespan_generic$new_but_shed_count + (leaf_lifespan_specific$spec_leaves_length*leaf_lifespan_specific$lvs_new_length)
head(leaf_lifespan_specific)

#generic leaves per length data
leaves_per_length$count_per_length <- leaves_per_length$leaf_count/leaves_per_length$length_mm
per_length_spp <- aggregate(count_per_length ~ species, leaves_per_length, FUN=mean)

leaf_lifespan_generic <- subset(leaf_lifespan,mm_lvs_spec=="0")
leaf_lifespan_generic <- merge(leaf_lifespan_generic, per_length_spp, by.x="species", by.y="species")
leaf_lifespan_generic$leaf_start_total <- leaf_lifespan_generic$lvs_start_count + (leaf_lifespan_generic$count_per_length*leaf_lifespan_generic$lvs_start_length)
leaf_lifespan_generic$leaf_end_total <- leaf_lifespan_generic$lvs_end_count + (leaf_lifespan_generic$count_per_length*leaf_lifespan_generic$lvs_end_length)
leaf_lifespan_generic$leaf_new_total <- leaf_lifespan_generic$lvs_new_count + leaf_lifespan_generic$new_but_shed_count + (leaf_lifespan_generic$count_per_length*leaf_lifespan_generic$lvs_new_length)
leaf_lifespan_generic <- subset(leaf_lifespan,leaf_new_total>"0")
head(leaf_lifespan_generic)

#combine two files into 1
leaf_counts <- bind_rows(leaf_lifespan_specific, leaf_lifespan_generic)
head(leaf_counts)

#calculating LeafLifespan
leaf_counts$leaf_lifespan_db <- (leaf_counts$leaf_start_total)/(((leaf_counts$leaf_start_total)-(leaf_counts$leaf_end_total))+(leaf_counts$leaf_new_total)/2)
leaf_counts$leaf_lifespan_death <- (leaf_counts$leaf_start_total)/((leaf_counts$leaf_start_total)-(leaf_counts$leaf_end_total))
leaf_counts$leaf_lifespan_birth <- (leaf_counts$leaf_start_total)/(leaf_counts$leaf_new_total)

leaf_counts$leaf_lifespan_death[which(is.infinite(leaf_counts$leaf_lifespan_death))] <- NA
leaf_counts$leaf_lifespan_db[which(is.infinite(leaf_counts$leaf_lifespan_death))] <- NA
leaf_counts$leaf_lifespan_birth[which(is.infinite(leaf_counts$leaf_lifespan_death))] <- NA

write.csv(leaf_counts, file = "leaf_counts.csv")

#removng crazy numbers
leaf_counts <- subset(leaf_counts,leaf_lifespan_db>0)
leaf_counts <- subset(leaf_counts,leaf_lifespan_db<6)
leaf_counts <- subset(leaf_counts,leaf_lifespan_birth<8)
leaf_counts <- subset(leaf_counts,leaf_lifespan_death<8)


#leaf lifespan summary by species, age
LeafLLSummary_av <- leaf_counts %>%
  group_by(species, age) %>%
  summarise_each(funs(mean, se, length), leaf_lifespan_db, leaf_lifespan_death,leaf_lifespan_birth)



#load files
LMA <- read.csv("~/RA schedules - Kuringgai research/Reproductive_Allocation_Kuringgai/data/LMA_short.csv",header=TRUE,as.is=TRUE,stringsAsFactors=FALSE)


#summarize
#leaf lifespan summary by species, age
LMA_summary <- LMA %>%
  group_by(species, age) %>%
  summarise_each(funs(mean, se, length), LMA)

#merge with LL data
LeafLLSummary_av <- merge(LeafLLSummary_av, LMA_summary, by.x=c("species","age"), by.y=c("species","age"))




#plotting and stats
#LL vs LMA
pch.spp <- c(0,1,2,15,16,17,18,0,1,2,15,16,17,18)
col.spp <- c("red", "dark green", "blue", "purple", "orange", "light green","salmon","sky blue","grey", "black","light blue","brown","orange2")

plot(leaf_lifespan_db_mean ~ mean, LeafLLSummary_av)
mod <- lm(leaf_lifespan_db_mean ~ mean + age, LeafLLSummary_av)
summary(mod)



#plotting across species
pch.spp <- c(0,1,2,15,16,17,18,0,1,2,15,16,17,18)
col.spp <- c("red", "dark green", "blue", "purple", "orange", "light green","salmon","sky blue","grey", "black","light blue","brown","orange2")

plot(leaf_lifespan_db_mean ~ age, LeafLLSummary_av, col=col.spp[species], pch=pch.spp[species], cex=2)

plot(leaf_lifespan_birth_mean ~ as.factor(age), LeafLLSummary_av)
plot(leaf_lifespan_death_mean ~ as.factor(age), LeafLLSummary_av)

plot(leaf_lifespan_db_mean ~ as.factor(species), LeafLLSummary_av, data=subset(LeafLLSummary_av,age>2))
plot(leaf_lifespan_birth_mean ~ as.factor(species), LeafLLSummary_av, data=subset(LeafLLSummary_av,age>2))
plot(leaf_lifespan_death_mean ~ as.factor(species), LeafLLSummary_av, data=subset(LeafLLSummary_av,age>2))

plot(leaf_lifespan_db_mean ~ as.factor(age), LeafLLSummary_av, data=subset(LeafLLSummary_av,species=="BAER"))
plot(leaf_lifespan_db_mean ~ as.factor(age), LeafLLSummary_av, data=subset(LeafLLSummary_av,species=="BOLE"))
plot(leaf_lifespan_db_mean ~ as.factor(age), LeafLLSummary_av, data=subset(LeafLLSummary_av,species=="COER"))
plot(leaf_lifespan_db_mean ~ as.factor(age), LeafLLSummary_av, data=subset(LeafLLSummary_av,species=="GRBU"))
plot(leaf_lifespan_db_mean ~ as.factor(age), LeafLLSummary_av, data=subset(LeafLLSummary_av,species=="GRSP"))
plot(leaf_lifespan_db_mean ~ as.factor(age), LeafLLSummary_av, data=subset(LeafLLSummary_av,species=="HATE"))
plot(leaf_lifespan_db_mean ~ as.factor(age), LeafLLSummary_av, data=subset(LeafLLSummary_av,species=="HEPU"))


plot(leaf_lifespan_db ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="BAER"))
plot(leaf_lifespan_db ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="BOLE"))
plot(leaf_lifespan_db ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="COER"))
plot(leaf_lifespan_db ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="GRBU"))
plot(leaf_lifespan_db ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="GRSP"))
plot(leaf_lifespan_db ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="HATE"))
plot(leaf_lifespan_db ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="HEPU"))
plot(leaf_lifespan_db ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="LEES"))
plot(leaf_lifespan_db ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="PELA"))
plot(leaf_lifespan_db ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="PEPU"))
plot(leaf_lifespan_db ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="PHPH"))
plot(leaf_lifespan_db ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="PILI"))
plot(leaf_lifespan_db ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="PUTU"))
mod <- lm(leaf_lifespan_db ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="PUTU"))
summary(mod)

