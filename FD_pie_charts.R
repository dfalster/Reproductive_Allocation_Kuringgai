accessoryList <- read.csv("~/RA schedules - Kuringgai research/Reproductive_Allocation_Kuringgai/data/accessoryList.csv")
accessoryListShort <- read.csv("~/RA schedules - Kuringgai research/Reproductive_Allocation_Kuringgai/data/accessoryListShort.csv")
InvestmentByPart <- Investment_FD_all

col.barplot <- function(x=NULL){
  ret <- c("cyan4","cyan1","darkslategray3","firebrick","firebrick1","firebrick2","firebrick3","gold","darkorchid","chartreuse2","chartreuse3","chartreuse4","deepskyblue",
           "dodgerblue2","deepskyblue4","darkorange","darkorange1","darkorange2","darkorange3","darkorange4")
  names(ret) <- c("seed","seed_pod","fruit_mature","bud","petals","stigma","style","finished_flower","calyx","fruit_immature","seed_immature","seed_aborted",
                  "bract","pedicel","inflorescence_stalk","cone_aborted","cone_base_brown","cone_base_green","cone_brown","cone_green")
  if(!is.null(x)) {
    ret <- ret[as.character(x)]
  }
  ret
}

labels.barplot <- function(x=NULL){
  names(col.barplot(x))
}

Invest2 <- merge(InvestmentByPart,select(accessoryList,-sort), by=c("species","part"))
  
for(v in c("weight")) {
  i <- is.na(Invest2[[v]])
  Invest2[[v]][i] <- 0
}

SummaryInvest <- 
  Invest2 %>%
  group_by(parts_fewer,species) %>%
  summarise_each(funs(sum),weight)

SummaryInvest <- merge(SummaryInvest,select(accessoryListShort,-sort), by=c("species","parts_fewer"),all.x=TRUE)

SummaryInvestSpp <- 
  Invest2 %>%
  group_by(species) %>%
  summarise_each(funs(sum),weight)

names(SummaryInvestSpp) <- c("species","weight_spp")

SummaryInvest <- merge(SummaryInvest,SummaryInvestSpp,by=c("species"),all.x=TRUE)
SummaryInvest$prop_weight <- round(SummaryInvest$weight / SummaryInvest$weight_spp,digits=3)
SummaryInvest$labels <- paste(SummaryInvest$parts_fewer," (",SummaryInvest$prop_weight,")")
SummaryInvest <- SummaryInvest[order(factor(SummaryInvest$parts_fewer,levels=c(c("seed","seed_pod","fruit_mature","bud","petals","stigma","style","finished_flower","calyx","fruit_immature","seed_immature","seed_aborted",
                                                                                 "bract","pedicel","inflorescence_stalk","cone_aborted","cone_base_brown","cone_base_green","cone_brown","cone_green")))),]

barplot_data <- select(SummaryInvest,species, parts_fewer, prop_weight)
barplot_data <- spread(barplot_data,species,prop_weight,fill=0,drop=FALSE)
barplot_data <- barplot_data[order(factor(barplot_data$parts_fewer,levels=c(c(
  "seed","seed_pod","fruit_mature","bud","petals","stigma","style","finished_flower","calyx","fruit_immature","seed_immature","seed_aborted",
  "bract","pedicel","inflorescence_stalk","cone_aborted","cone_base_brown","cone_base_green","cone_brown","cone_green")))),]


barplot_data_matrix <- as.matrix(barplot_data[2:15])


pdf("figs2/Flower_parts_barchart.pdf", height=8, width=11)
plot
barplot(barplot_data_matrix,beside=FALSE,width=0.8,cex.axis=.8,cex.names=.8,xlim=c(.5,17.2),col=col.barplot(barplot_data$parts_fewer),ylab="proportion repro investment")
legend(13.5,1,legend=barplot_data$parts_fewer,pch=16, cex=1.2,bty="n",col=col.barplot())
dev.off()
##For pie charts

data <- subset(SummaryInvest,weight>0)
data <- split(data, data$species)

pdf("figs2/Flower_parts_fewer.pdf", height=6, width=9)
plot
for(spp in names(data)) {
  pie(data[[spp]]$weight,labels=data[[spp]]$labels,main=spp,col=col.barplot(data[[spp]]$parts_fewer))
}
dev.off()

