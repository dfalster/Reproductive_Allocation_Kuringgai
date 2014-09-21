source('analysis/CalculateInvestmentForIndividualPlant.R')

CalculateInvestmentForSpecies<-function(species)
{ 
  #species="PUTU"
  #Initialize data frames
  InvestmentSpecies=data.frame(Individual=c(), age=c(), FromCensus=c(), ToCensus=c(), From=c(), To=c(),Inv=c(), Count=c(),Total=c())
  ErrorSpecies=data.frame(Individual=c(), Element=c(), Census=c(), Count=c())
  LostSpecies=data.frame(Individual=c(),age=c(),what=c(),Census=c(), count=c(),weight=c())
  FDSpecies=data.frame(Individual=c(),age=c(),what=c(),Census=c(), count=c(),weight=c())
  
  #Read and restrict the data to the subset of interest.
  Data=read.csv('data/reproduction.csv',sep=',',header=T,stringsAsFactors = FALSE, na.strings="")
  Data=Data[Data$census<=18,]
  ind.list=unique(Data[Data$species==species,]$individual)
  #Loop along all individuals
  for(individual in ind.list)
  {
    print(individual)
    tryCatch({R=CalculateInvestmentForIndiviualPlant(individual)
              if(!is.null(R$Inv))
              {
                InvestmentSpecies=rbind(InvestmentSpecies,R$Inv)
              }
              if(!is.null(R$Lost))
              {
                LostSpecies=rbind(LostSpecies,R$Lost)
              }
              if(!is.null(R$FD))
              {
                FDSpecies=rbind(FDSpecies,R$FD)
              }
              if(!is.null(R$Err))
              {
                ErrorSpecies=rbind(ErrorSpecies,R$Err)
              }
    },error=function(e){print(paste0("Error in ",individual))})
  }
#Write output files
write.csv(file=paste0('output/',species,'_Inv.csv'),x=InvestmentSpecies,row.names=F)
write.csv(file=paste0('output/',species,'_Lost.csv'),x=LostSpecies,row.names=F)
write.csv(file=paste0('output/',species,'_FinDev.csv'),x=FDSpecies,row.names=F)
write.csv(file=paste0('output/',species,'_Error.csv'),x=ErrorSpecies,row.names=F)
}  