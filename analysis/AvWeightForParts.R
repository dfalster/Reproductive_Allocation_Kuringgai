AvWeightForParts<-function()
{
library(xtable)
#dir.create("output/partWeights", recursive=TRUE, showWarnings=FALSE)
source('analysis/OrderedListsOfParts.R')
source('analysis/DeriveMissingParts.R')


######################### Average weight of pieces, i.e., mass/count - OLD VERSION
FPSummary=read.csv('data/flower_parts.csv',header=TRUE,sep=',', stringsAsFactors = FALSE)
out <- list()
FPSummary=FPSummary[FPSummary$category_use=="used",]
for(species.name in unique(FPSummary$species)){
  Sp.Data=FPSummary[FPSummary$species==species.name,]
  PartList=Maps[[species.name]]
  av.weight <- n <-count <-c()
  for(i in 1: length(PartList)){
    I=(Sp.Data$part==PartList[i])
    if(sum(I)>0)
    {
      DataForPart=Sp.Data[I,]
      DataForPart=DataForPart[!is.na(as.numeric(DataForPart$weight)),]
      if(sum(!is.na(DataForPart$weight)&is.na(DataForPart$count))>0){
        DataForPart[!is.na(DataForPart$weight)&is.na(DataForPart$count),]$count=1
      }
      av.weight[i]=(sum(as.numeric(DataForPart$weight))/sum(DataForPart$count))
      n[i]=length(DataForPart$weight)
      count[i]=sum(DataForPart$count)
    }
    else
    {
      av.weight[i]=NA
      n[i]=NA
      count[i]=NA
    }
  }
  out[[species.name ]]=data.frame(species=species.name,part=PartList,n=n,av.weight=av.weight,stringsAsFactors=FALSE)
}
#
# Add additional parts which are derivative of the existing measurments
out=DeriveMissingParts(out)
#
#
for(species.name in unique(FPSummary$species)){
  print(xtable(out[[species.name ]], include.rownames=FALSE), floating=FALSE, type="latex", file=paste0("output/docs/", species.name,".tex"))
}
x1 <- do.call(rbind, out) #merge output into single dataframe
#x1 <- x1[order(x1$species, x1$part),] #reorder

path <- "output/partWeights"
if(!file.exists(path))
  dir.create(path, recursive=TRUE)
write.csv(x1, file=file.path(path, "AvWeightPerUnit.csv"), quote=FALSE, row.names=FALSE)
}




############################## new version
# library(plyr)
# FPSummary=read.csv('data/flower_parts.csv',header=TRUE,sep=',', stringsAsFactors = FALSE)
#
# summarise.parts <- function(df){
# 	index <- !is.na(df$weight)& is.na(df$count)& !is.na(df$length)
# 	if(sum(index)>0) df$count[index] <- 1  # reset count for length.measurements. Why?
# 	data.frame(n=length(df$weight),av.weight=sum(as.numeric(df$weight))/sum(df$count))
# }
#
# summary.all <- ddply(FPSummary, .(species, part), summarise.parts)
# write.csv(summary.all, file="output/partWeights/all.csv", quote=FALSE, row.names=FALSE)
#
# # print latex tables for each species
# d_ply(summary.all, .(species), 	function(x) print(xtable(x, digits=2), include.rownames=FALSE, floating=FALSE, type="latex", file=paste0("output/partWeights/", x$species[1],".tex")))
#
