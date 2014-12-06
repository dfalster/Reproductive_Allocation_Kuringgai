Plot_IAT_By_Species<-function()
{
  species_v <- read.csv("data/species_names.csv", stringsAsFactors=FALSE)
  for(species in species_v$Abbreviation)
  {
    Data=read.csv(paste0('output/',species,'_Acc.csv'))
  Data=aggregate(cbind(PrePol_A,PrePol_S,PostPol_A,PD,Prop,Total)~age,data=Data,FUN=mean)
  Ages=Data$age
  DataSt=Data/Data$Total
  DataSt=DataSt[,2:6]
  M_Ages=matrix(Ages,ncol=5,nrow=length(Ages),byrow=F)
  pdf(file=paste0('output/docs/IAT_',species,'.pdf'),width=15,height=5)
  matplot(M_Ages,DataSt,type="l",col=rainbow_hcl(5),lty=c(2,2,2,2,1),lwd=c(2,2,2,2,3),xlab="Age",ylab="Proportion of RI")
  legend('topleft',c("Prepol Abort","Prepol Succ","Postpol Abort","Pack Disp","Propagule" ),col=rainbow_hcl(5),lty=c(2,2,2,2,1),lwd=c(2,2,2,2,3))
  dev.off()
  }
}


Compare_IProp<-function()
{
  pdf(file=paste0('output/docs/IAT_Comparison','.pdf'),width=20,height=10)
  plot(-10,-10,xlim=c(2,36),ylim=c(0,0.5),xlab="Age",ylab="Propotion of RI")
  species_v <- read.csv("data/species_names.csv", stringsAsFactors=FALSE)
  i=1
  for(species in species_v$Abbreviation)
  {
    Data=read.csv(paste0('output/',species,'_Acc.csv'))
    Data=aggregate(cbind(PrePol_A,PrePol_S,PostPol_A,PD,Prop,Total)~age,data=Data,FUN=mean)
    Ages=Data$age
    DataSt=Data/Data$Total
    DataSt=DataSt[,6]
    lines(Ages,DataSt,col=rainbow_hcl(14)[i],lwd=2)
    i=i+1;
  }
  legend('topright',species_v$Abbreviation,col=rainbow_hcl(14),lty=1,lwd=1)
  dev.off()
  
}
