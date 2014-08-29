# Functions that transform census data to weights
# 1. Count to vector weights
# 2. Length of plant part to count
# 3. Dimension of individual to it's weight.
##################################################
library("stringr")
#read in necessary summary data
AvWeightPerUnit=read.table('output/partWeights/AvWeightPerUnit.csv',sep=',',header=T)
AvCountsPerMM  =read.table('output/partWeights/AvCOuntsPerMM.csv'  ,sep=',',header=T) 
RegressionTable=read.table('output/partWeights/RegressionTable.csv'  ,sep=',',header=T) 




# Weights from counts (vector with homogeneous weights)
WeightFromCount<-function(count,species,part)
{
  if (grepl("0",part))
  {
    part=str_sub(string=part,start=1,end=-4)
  }
  av.weight=AvWeightPerUnit[(AvWeightPerUnit$species==species)&(AvWeightPerUnit$part==part),]$av.weight  
  w=rep(1,count)*av.weight
  w
}

# Weights from length (vector with homogeneous weights)
WeightFromLength<-function(length,species,part)
{
  if (grepl("0",part))
  {
    part=str_sub(string=part,start=1,end=-4)
  }
  count=round(length*AvCountsPerMM[AvCountsPerMM$species==species,]$AvCountPerMM)
  w=WeightFromCount(count,species,part)
  w
}

# Weight from dimensions (for single plant element)
WeightFromRegression<-function(height,diameter=0,species,part)
{ 
  if (grepl("0",part))
  {
    part=str_sub(string=part,start=1,end=-4)
  }
  w=NA
  parameters=RegressionTable[(RegressionTable$species==species)&(RegressionTable$part==part),]
  #Regression equations 
  if(diameter==0)
  {
    w=parameters$intercept+parameters$slope*height^parameters$reg.order
  }
  else
  {#Calculations based on volume (cylinder approximation of cones geometry) and density
    volume=pi/4*diameter^2*height;
    w=parameters$slope*volume;
  }
w
}


