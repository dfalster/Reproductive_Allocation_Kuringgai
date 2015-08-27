accessoryList <- read.csv("~/RA schedules - Kuringgai research/Reproductive_Allocation_Kuringgai/data/accessoryList.csv")
FD <- Investment_FD_all

###variant1###
SummaryInvest <- 
  FD %>%
  group_by(part,species) %>%
  summarise_each(funs(sum),weight)

SummaryInvest <- merge(SummaryInvest,select(accessoryList,-sort), by=c("species","part"))
for(v in c("weight")) {
  i <- is.na(SummaryInvest[[v]])
  SummaryInvest[[v]][i] <- 0
}
###

###variant2###
SummaryInvest <- 
  FD %>%
  group_by(parts_fewer,species) %>%
  summarise_each(funs(sum),weight)

SummaryInvest <- merge(SummaryInvest,select(accessoryList,-sort), by=c("species","part"))
for(v in c("weight")) {
  i <- is.na(SummaryInvest[[v]])
  SummaryInvest[[v]][i] <- 0
}
###


data <- SummaryInvest
data <- split(data, data$species)

for(spp in names(data)) {
  pie(data$weight,labels=data$part,main=spp)
}

barplot(height=Invest$weight,names.arg=Invest$part,cex.names=.8,las=2)
View(SummaryInvest)

pdf("figs2/Flower_parts.pdf", height=6, width=9)
plot


Invest <- subset(SummaryInvest, species=="BAER")
pie(Invest$weight,labels=Invest$part,main="BAER",cex.lab=.4)

Invest <- subset(SummaryInvest, species=="BOLE")
pie(Invest$weight,labels=Invest$part,main="BOLE")

Invest <- subset(SummaryInvest, species=="COER")
pie(Invest$weight,labels=Invest$part,main="COER")

Invest <- subset(SummaryInvest, species=="EPMI")
pie(Invest$weight,labels=Invest$part,main="EPMI")

Invest <- subset(SummaryInvest, species=="GRBU")
pie(Invest$weight,labels=Invest$part,main="GRBU")

Invest <- subset(SummaryInvest, species=="GRSP")
pie(Invest$weight,labels=Invest$part,main="GRSP")

Invest <- subset(SummaryInvest, species=="HATE")
pie(Invest$weight,labels=Invest$part,main="HATE")

Invest <- subset(SummaryInvest, species=="HEPU")
pie(Invest$weight,labels=Invest$part,main="HEPU")

Invest <- subset(SummaryInvest, species=="LEES")
pie(Invest$weight,labels=Invest$part,main="LEES")

Invest <- subset(SummaryInvest, species=="PELA")
pie(Invest$weight,labels=Invest$part,main="PELA")

Invest <- subset(SummaryInvest, species=="PEPU")
pie(Invest$weight,labels=Invest$part,main="PEPU")

Invest <- subset(SummaryInvest, species=="PHPH")
pie(Invest$weight,labels=Invest$part,main="PHPH")

Invest <- subset(SummaryInvest, species=="PILI")
pie(Invest$weight,labels=Invest$part,main="PILI")

Invest <- subset(SummaryInvest, species=="PUTU")
pie(Invest$weight,labels=Invest$part,main="PUTU")

dev.off()