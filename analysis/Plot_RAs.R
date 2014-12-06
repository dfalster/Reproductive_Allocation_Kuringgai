Plot_RAs<-function()
{
#Function plotting the comparison plot of RA for all the species.
InvSummary=read.csv('output/InvestmentSummary.csv')
RAo=aggregate(RA~species+age,data=InvSummary,FUN=mean)
ages=unique(RAo$age)
nT=length(ages)
RA=t(reshape(RAo,timevar="age",idvar="species",direction="wide"))
colnames(RA)=RA[1,]
RA=RA[-1,]
RA=RA[,order(colnames(RA))]
RA[is.na(RA)]=0
# Check if plants at given ages are present for the specie (i.e., distinguish between no measurments and zero investments)
DiameterData=read.csv('data/2013_Kuringgai_harvest.csv',stringsAsFactors = FALSE)
#Use only basal diameter of the trees that are alive and have status use.
DiameterData=DiameterData[DiameterData$segment==1,]
DiameterData=DiameterData[DiameterData$use_status=="use",]
DiameterData=DiameterData[DiameterData$plant_status=="alive",]

for(i in 1:14)
{
  for(j in 1:nT)
  {
    species=colnames(RA)[i]
    age=ages[j]
    if((nrow(DiameterData[(DiameterData$species==species) & (DiameterData$age==age),]))==0)
      RA[j,i]=NA
  }
}

Age=matrix(ncol=14,nrow=nT,ages)
pdf(file='output/docs/RA_Comparison.pdf',width=20,height=10)
matplot(Age,RA,type="b",pch='o',main="Reproductive Allovation for 14 species along time",col=rainbow_hcl(14),lty=1,lwd=3,xlim=c(0,35),bty="L")
legend(33,0.7,legend=colnames(RA),col=rainbow_hcl(14),lty=1,lwd=3)
dev.off()
}
