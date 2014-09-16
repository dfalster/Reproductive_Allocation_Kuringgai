library(stringr)
library(colorspace)
library(parallel)
library(smatr)
library(plyr)
library(dplyr)
library(igraph)

source('analysis/OrderedListsOfParts.R')
source('analysis/AvWeightForParts.R')
AvWeightForParts()
source('analysis/AvCountsPerMM.R')
AvCountsPerMM()
source('analysis/RegressionCoefficients.R')
RegressionCoefficients()
source('analysis/CalculateMassAndDiameters.R')
CalculateMassAndDiameters()
source('analysis/IndividualBasedWeightsCalculations.R')
IndividualBasedWeightsCalculations()
source('analysis/CalculateInvestmentForSpecies.R')
source('analysis/TreeListToTotalDataFrame.R')
AnalyzeIndividual<-function(individual)
{
  individual="PELA_903"
  species=str_sub(individual,1,4)
  
  #Read tree data
  InvestmentCategories=read.csv('data/Accessory_parts_lookup_table.csv',header=T,stringsAsFactors = FALSE, na.strings="")
  Data=read.csv('data/reproduction.csv',sep=',',header=T,stringsAsFactors = FALSE, na.strings="")
  Tree=Data[Data$individual==individual,]
  
  #Transform counts to weights and adjust for multiplicity 
  TreeListOrig=WeightCalculationsForTree(Tree)
  TreeListAdj=AdjustForMultiplicity(TreeList=TreeListOrig)
  Weights=TreeListToTotalDataFrame(TreeListAdj)
  Inv=CalculateInvestmentForIndiviualPlant(individual)
  print(Weights)
  print(Inv)
}  
  
  