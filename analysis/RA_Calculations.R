RA_Calculations<-function()
{
# Function that produce the summary of both investment types and calculate RAs.
source('analysis/OrderedListsOfParts.R')
WeightDiameterData=read.csv(file="output/WeightDiameterData.csv",stringsAsFactors = FALSE)
WeightDiameterData=WeightDiameterData[!is.na(WeightDiameterData$total_weight),]
############################################################
#Calculate Regression coefficients based on common slope and intercept by the basal diameter at year 2013
############################################################
RegTable=data.frame(TreeID=c(),Intercept=c(),Slope=c())
for(species in unique(WeightDiameterData$species))
{
WD.species=WeightDiameterData[WeightDiameterData$species==species,]
lm.fit=lm(log(total_weight)~log(dia),data=WD.species)
common_slope=coef(lm.fit)[2]
WD.specie.basal=WD.species[WD.species$node_above==1,]
intercept.ind=log(WD.specie.basal$total_weight)-common_slope*log(WD.specie.basal$dia)
RegTable=rbind(RegTable,data.frame(TreeID=WD.specie.basal$tag_ID,Intercept=intercept.ind,Slope=common_slope))
}
RegTable=RegTable[!is.na(RegTable$Intercept),]
write.csv(RegTable,file='output/WeightDiameter_RegressionCoefficients.csv')
############################################################
# Use the regrsssion to obtain weights year 2012 (and 2013).
############################################################
DiameterData=read.csv('data/2013_Kuringgai_harvest.csv',stringsAsFactors = FALSE)
#Use only basal diameter of the trees that are alive and have status use.
DiameterData=DiameterData[DiameterData$segment==1,]
DiameterData=DiameterData[DiameterData$use_status=="use",]
DiameterData=DiameterData[DiameterData$plant_status=="alive",]
DiameterData=DiameterData[,c("tag_ID","year","diameter_1","diameter_2","diameter_3","total_plant_weight")]
#DiameterData=DiameterData[DiameterData$year,]
for(i in 1:nrow(DiameterData))
{
  individual=DiameterData$tag_ID[i]
  I=(RegTable$TreeID==individual)
  if(sum(I)==1)
  {
    coef=RegTable[I,]
    diams=DiameterData[i,c("diameter_1","diameter_2","diameter_3")]
    diam.av =mean(as.numeric(diams),na.rm=T)
    DiameterData$EstWeight[i]=exp(coef$Intercept+coef$Slope*log(diam.av))
    DiameterData[i,"Basal.Diameter.Av"]=diam.av
  }
}
############################################################
#Calculate change in the weight, i.e., Growth Investement
############################################################
GrowthInv=data.frame(Tree_ID=c(),GrowthInv=c(),StartWeight=c(),FinalWeight=c(),StartBasalDiamAv=c(),FinalBasalDiamAv=c())
for(i in 1:length(unique(DiameterData$tag_ID)))
{
  individual=unique(DiameterData$tag_ID)[i]
  IndData=DiameterData[DiameterData$tag_ID==individual,]
  if(nrow(IndData)!=2)
  {
    print(paste0("Incorrect number of measurments for ",individual))
  }
  if(nrow(IndData)==2)
  {
    GrowthInv[i,"Tree_ID"]=individual
    GrowthInv[i,"GrowthInv"]=IndData[IndData$year==2013,"EstWeight"]-IndData[IndData$year==2012,"EstWeight"]
    GrowthInv[i,"StartWeight"]=IndData[IndData$year==2012,"EstWeight"]
    GrowthInv[i,"FinalWeight"]=IndData[IndData$year==2013,"EstWeight"]
    GrowthInv[i,"StartBasalDiamAv"]=IndData[IndData$year==2012,"Basal.Diameter.Av"]
    GrowthInv[i,"FinalBasalDiamAv"]=IndData[IndData$year==2013,"Basal.Diameter.Av"]
  }
}

############################################################
#Use saved data to calculate total reproduction investment per individual plane
############################################################
RepoInv=data.frame(Tree_ID=c(),age=c(),ReproInv=c())
for(species in names(Maps))
{
  InvSpecies=read.csv(file=paste0('output/',species,'_Inv.csv'),stringsAsFactors = FALSE)
  Individuals=unique(InvSpecies$Individual)
  for(individual in Individuals)
  {
    InvIndividual=InvSpecies[InvSpecies$Individual==individual,]
    RepoInv=rbind(RepoInv,data.frame(Tree_ID=individual,age=InvIndividual$age[1],ReproInv=c(sum(InvIndividual$Total))))  
  }
}

############################################################
#Merge two tables and calculate total investment and RAR
############################################################
InvestmentSummary=merge(RepoInv,GrowthInv,by.y="Tree_ID",all.y=T)
#NA that appeared correspond to zeoro reproductive investment
InvestmentSummary[is.na(InvestmentSummary)]=0 
#Recode ages if at some place there are old time tags used.
InvestmentSummary[str_sub(InvestmentSummary$Tree_ID,6,6)=="0","age"]=7
InvestmentSummary[str_sub(InvestmentSummary$Tree_ID,6,6)=="1","age"]=1.3
InvestmentSummary[str_sub(InvestmentSummary$Tree_ID,6,6)=="4","age"]=5
InvestmentSummary[str_sub(InvestmentSummary$Tree_ID,6,6)=="8","age"]=9
InvestmentSummary[str_sub(InvestmentSummary$Tree_ID,6,6)=="9","age"]=32


InvestmentSummary[["TotalInvestment"]]=InvestmentSummary$ReproInv+InvestmentSummary$GrowthInv
InvestmentSummary[["RA"]]=InvestmentSummary$ReproInv/InvestmentSummary$TotalInvestment
row.names(InvestmentSummary)<-NULL
InvestmentSummary[["species"]]=str_sub(InvestmentSummary$Tree_ID,1,4)
InvestmentSummary=InvestmentSummary[order(InvestmentSummary$species),]

write.csv(x=InvestmentSummary,file='output/InvestmentSummary.csv')
}
