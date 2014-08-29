source('analysis/WeightCalculationsForTree.R')
source('analysis/InvestmentCalculations.R')
source('analysis/GraphRepresentation.R')
source('analysis/MultiplicityAdjustment.R')


CalculateInvestmentForIndiviualPlant<-function(individual)
{ #Read and extract data
  Data=read.csv('data/reproduction.csv',sep=',',header=T,stringsAsFactors = FALSE, na.strings="")
  Tree=Data[Data$individual==individual,]
  #Transform counts to weights and adjust for multiplicity 
  TreeListOrig=WeightCalculationsForTree(Tree)
  #TreeListAdj=TreeListOrig;
  TreeListAdj=AdjustForMultiplicity(TreeList=TreeListOrig)
  #Calculate investment
  Res=InvestmentCalculations(TreeListAdj)
  #Extract and reorder the results
  I=Res[["Inv"]]
  if(!is.null(I))
  {
    I["age"]=as.numeric(unique(Tree$age))
    I=I[,c(8,9,1:7)]
  }
  
  Err=Res[["Err"]]
  if(!is.null(Err))
  {
    Err["Individual"]=individual
    Err=Err[,c(4,(1:3))]
  }
  Lost=Res[["Lost"]]
  if(!is.null(Lost))
  {if(nrow(Lost)>0)
  {  
    Lost["Individual"]=individual
    Lost["age"]=as.numeric(unique(Tree$age))
    Lost=Lost[,c(5,6,1:4)]
  }
  if(nrow(Lost)==0)
  {
    Lost=NULL
  }
  }
  list(Inv=I,Lost=Lost,Err=Err)
}