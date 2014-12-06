source('analysis/InvestmentInAccessoryTissues.R')
IAT_Species<-function(species)
{
  #Read and restrict the data to the subset of interest.
  Data=read.csv('data/reproduction.csv',sep=',',header=T,stringsAsFactors = FALSE, na.strings="")
  DiameterData=read.csv('data/2013_Kuringgai_harvest.csv',stringsAsFactors = FALSE)
  DiameterData=DiameterData[DiameterData$segment==1,]
  DiameterData=DiameterData[DiameterData$use_status=="use",]
  DiameterData=DiameterData[DiameterData$plant_status=="alive",]
  AgeData=unique(DiameterData[,c("age","tag_ID")])
  
  ind.list=unique(Data[Data$species==species,]$individual)
  InvDist=data.frame(individual=c() ,PrePol_A=c(), PrePol_S=c(), PostPol_A=c(),  PD=c(), Prop=c(), Total=c(),age=c())
  for(individual in ind.list)
  {
    #print(individual)
    Ind=InvestmentInAccessoryTissues(individual)
    if(length(AgeData[AgeData[,2]==individual,1])==1)
    {Ind=cbind(Ind,age=AgeData[AgeData[,2]==individual,1])
    InvDist=rbind(InvDist,Ind)
    }
    
  }
  write.csv(file=paste0('output/',species,'_Acc.csv'),x=InvDist,row.names=F)
}