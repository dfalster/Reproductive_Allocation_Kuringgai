InvestmentInAccessoryTissues<-function(individual)
{
  #individual="PUTU_904"
  species=str_sub(individual,1,4)
  
  #Read tree data
  InvestmentCategories=read.csv('data/Accessory_parts_lookup_table.csv',header=T,stringsAsFactors = FALSE, na.strings="")
  Data=read.csv('data/reproduction.csv',sep=',',header=T,stringsAsFactors = FALSE, na.strings="")
  Tree=Data[Data$individual==individual,]
  
  #Transform counts to weights and adjust for multiplicity 
  TreeListOrig=WeightCalculationsForTree(Tree)
  TreeListAdj=AdjustForMultiplicity(TreeList=TreeListOrig)
  
  #Read in the information about lost elements
  SpeciesLoss=read.csv(paste0('output/',species,'_FinDev.csv'),header=T,stringsAsFactors = FALSE, na.strings="")
  Lost=SpeciesLoss[SpeciesLoss$Individual==individual,]
  
  #Find the list of investment categories partition for the species
  IC=InvestmentCategories[,c("flower_part",species)]
  IC=IC[!is.na(IC[,2]),]
  
  ########################################################################*
  #Prepolination_abort 
  ########################################################################*
  PreP_A_parts=IC[IC[,2]=="prepollination_abort",1]
  #check if any of the parts can be found in the data for this individual
  PreP_A_parts=PreP_A_parts[PreP_A_parts%in%Lost$what]
  PrepolinationAbortWeight=0
  if(length(PreP_A_parts)>0)
  {
    PrepolinationAbortWeight=sum(Lost[(Lost$what%in%PreP_A_parts),]$weight)
  }
  ########################################################################*
  #Prepolination_success 
  ########################################################################*
  PreP_S_parts=IC[IC[,2]=="prepollination_success",1]
  #check if any of the parts can be found in the data for this individual
  PreP_S_parts=PreP_S_parts[PreP_S_parts%in%Lost$what]
  PrepolinationSuccessWeight=0
  if(length(PreP_A_parts)>0)
  {
    PrepolinationSuccessWeight=sum(Lost[(Lost$what%in%PreP_S_parts),]$weight)
  }
  ########################################################################*
  #Postpollination_abort 
  ########################################################################*
  PostP_A_parts=IC[IC[,2]=="postpollination_abort",1]
  #check if any of the parts can be found in the data for this individual
  PostP_A_parts=PostP_A_parts[PostP_A_parts%in%Lost$what]
  PostpollinationAbortWeight=0
  if(length(PostP_A_parts)>0)
  {
    PostpollinationAbortWeight=sum(Lost[(Lost$what%in%PostP_A_parts),]$weight)
  }
  
  ########################################################################*
  #Packaging Dispersal 
  ########################################################################*
  PD_parts=IC[IC[,2]=="packaging_dispersal",1]
  #check if any of the parts can be found in the data for this individual
  PD_parts=PD_parts[PD_parts%in%Lost$what]
  PackaginDispersalWeight=0
  if(length(PD_parts)>0)
  {
    PackaginDispersalWeight=sum(Lost[(Lost$what%in%PD_parts),]$weight)
  }
  ########################################################################*
  #Propagule 
  ########################################################################*
  PROP_parts=IC[IC[,2]=="propagule",1]
  #check if any of the parts can be found in the data for this individual
  PROP_parts=PROP_parts[PROP_parts%in%Lost$what]
  PropaguleWeight=0
  if(length(PROP_parts)>0)
  {
    PropaguleWeight=sum(Lost[(Lost$what%in%PROP_parts),]$weight)
  }
TotWeight=PrepolinationAbortWeight+PrepolinationSuccessWeight+PostpollinationAbortWeight+PackaginDispersalWeight+PropaguleWeight;
data.frame(individual=individual,PrePol_A=PrepolinationAbortWeight,PrePol_S=PrepolinationSuccessWeight,
           PostPol_A=PostpollinationAbortWeight,PD=PackaginDispersalWeight,Prop=PropaguleWeight,Total=TotWeight)
}
