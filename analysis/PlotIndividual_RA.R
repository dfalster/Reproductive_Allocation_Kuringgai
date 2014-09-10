PlotIndividual_RA<-function()
{
# Function creating individual pdf RA developenent plots for all the species
source("analysis/OrderedListsOfParts.R")
InvSummary=read.csv('output/InvestmentSummary.csv')
#InvSummary["species"]=str_sub(InvSummary$Tree_ID,1,4)

for(j in 1:14)
{
species=names(Maps)[j]
sp_col=rainbow_hcl(14)[j]
InvBySpecies=InvSummary[InvSummary$species==species,]

InvByAge=data.frame(age=c(),GI=c(),GI_L=c(),GI_U=c(),RI=c(),RI_L=c(),RI_U=c(),RA=c(),RA_L=c(),RA_U=c())

N=c()
Ages=unique(InvBySpecies$age)
for(i in 1:length(Ages))
{
  D=InvBySpecies[InvBySpecies$age==Ages[i],]
  n=nrow(D)
  N[i]=n;
  InvByAge=rbind(InvByAge,data.frame(age=Ages[i],
                                     GI=mean(D$GrowthInv),GI_L=mean(D$GrowthInv)-1.96/sqrt(n)*sd(D$GrowthInv),GI_U=mean(D$GrowthInv)+1.96/sqrt(n)*sd(D$GrowthInv),
                                     RI=mean(D$ReproInv) ,RI_L=mean(D$ReproInv) -1.96/sqrt(n)*sd(D$ReproInv), RI_U=mean(D$ReproInv) +1.96/sqrt(n)*sd(D$ReproInv),
                                     RA=mean(D$RA),RA_L=mean(D$RA) -1.96/sqrt(n)*sd(D$RA),RA_U=mean(D$RA) +1.96/sqrt(n)*sd(D$RA)))
}

pdf(file=paste0('output/docs/RA_',species,'.pdf'),width=15,height=5)
N=N[order(InvByAge$age)]
InvByAge=InvByAge[order(InvByAge$age),]
par(mfrow=c(1,3),cex.main=2,cex.lab=1.5)
plot(InvByAge$age,InvByAge$GI,type="b",lwd=3,pch=as.character(N),cex=1.5,col=sp_col,ylim=range(c(InvBySpecies$ReproInv,InvBySpecies$GrowthInv)),main="Growth Investment",xlab="Age (years)",ylab="Growth Investment (mg)")
points(InvByAge$age,InvByAge$GI_L,pch=2,cex=2,col=sp_col)
points(InvByAge$age,InvByAge$GI_U,pch=6,cex=2,col=sp_col)
points(InvBySpecies$age,InvBySpecies$GrowthInv,pch="o",cex=1.5,col='grey')


plot(InvByAge$age,InvByAge$RI,type="b",lwd=3,pch=as.character(N),cex=1.5,col=sp_col,ylim=range(c(InvBySpecies$ReproInv,InvBySpecies$GrowthInv)),main="Reproductive Investment",xlab="Age (years)",ylab="Reproductive Investment (mg)")
points(InvByAge$age,InvByAge$RI_L,pch=2,cex=2,col=sp_col)
points(InvByAge$age,InvByAge$RI_U,pch=6,cex=2,col=sp_col)
points(InvBySpecies$age,InvBySpecies$ReproInv,pch="o",cex=1.5,col='grey')


plot(InvByAge$age,InvByAge$RA,type="b",lwd=3,pch=as.character(N),cex=1.5,col=sp_col,ylim=range(InvBySpecies$RA),main="Reproductive Allocation",xlab="Age (years)",ylab="Reproductive Allovation")
points(InvByAge$age,InvByAge$RA_L,pch=2,cex=2,col=sp_col)
points(InvByAge$age,InvByAge$RA_U,pch=6,cex=2,col=sp_col)
points(InvBySpecies$age,InvBySpecies$RA,pch="o",cex=1.5,col='grey')

dev.off()
}
}


