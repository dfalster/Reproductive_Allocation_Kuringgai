#load files
LMA <- read.csv("~/RA schedules - Kuringgai research/Reproductive_Allocation_Kuringgai/data/LMA_short.csv",header=TRUE,as.is=TRUE,stringsAsFactors=FALSE)

# load packages
library(remake)
library(dplyr)
library(tidyr)

#define functions
se <- function(x) sd(x)/sqrt(length(x))
con95 <- function(x) (sd(x)/sqrt(length(x)))*1.96

#summarize
#leaf lifespan summary by species, age
LMA_summary <- LMA %>%
  group_by(species, age) %>%
  summarise_each(funs(mean, se, length), LMA)

#merge with LL data
LeafLLSumary_av <- merge(LeafLLSumary_av, LMA, by.x=c("species","age"), by.y=c("species","age"))

#plotting
plot(mean ~ as.factor(species), LMA_summary)

