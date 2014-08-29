AvCountsPerMM<-function()
{
######################### Average counts per mm for specied that require it. Specie (not part) specific.
library(xtable)
#dir.create("output/partWeights", recursive=TRUE, showWarnings=FALSE)

######################### Average weight of pieces, i.e., mass/count - OLD VERSION
FPSummary=read.csv('data/flower_parts.csv',header=TRUE,sep=',', stringsAsFactors = FALSE)

i=1
sp.name<-av.dens<-tot.count<-c()
for(species.name in unique(FPSummary$species)){
  Sp.Data=FPSummary[FPSummary$species==species.name,]
  CPL=Sp.Data[Sp.Data$part=="count_by_length",]
  if(nrow(CPL)>0)
  {
    sp.name[i]=species.name;
    av.dens[i]=mean(CPL$count/CPL$length);
    tot.count[i]=sum(CPL$count);
    i=i+1;
  }
}
AvCountsPerMM<-data.frame(species=sp.name,tot.count=tot.count,AvCountPerMM=av.dens)

print(xtable(AvCountsPerMM, include.rownames=FALSE), floating=FALSE, type="latex", file="output/docs/AvCountsPerMM.tex")

path <- "output/partWeights"
if(!file.exists(path))
  dir.create(path, recursive=TRUE)

write.csv(AvCountsPerMM,file=file.path(path, "AvCountsPerMM.csv"), quote=FALSE, row.names=FALSE)
}
