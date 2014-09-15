# Functions that transform census data to weights
# 1. Count to vector weights
# 2. Length of plant part to count
# 3. Dimension of individual to it's weight.
##################################################
library("stringr")
#read in necessary summary data
AvWeightPerUnit=read.table('output/partWeights/AvWeightPerUnit.csv',sep=',',header=T)
AvCountsPerMM  =read.table('output/partWeights/AvCountsPerMM.csv'  ,sep=',',header=T) 
RegressionTable=read.table('output/partWeights/RegressionTable.csv'  ,sep=',',header=T) 
#Read in individual based weights
IBW = read.csv('output/partWeights/IndividualBasedWeights.csv')



# Weights from counts (vector with homogeneous weights)
WeightFromCount<-function(count,species,part,individual,census)
{
  if (grepl("0",part))
  {
    part=str_sub(string=part,start=1,end=-4)
  }
  I=(IBW$individual==individual)&(IBW$part==part)&(IBW$census_to_use==census)
  n_i=sum(I)
  if(n_i==1)
  {
    av.weight=IBW[I,"av_weights"]
    #print(paste("Using exact weight",part,census,av.weight,sep=" "))
  }
  if(n_i==0)
  {
  av.weight=AvWeightPerUnit[(AvWeightPerUnit$species==species)&(AvWeightPerUnit$part==part),]$av.weight  
  }
  w=rep(1,count)*av.weight
  w
}

# Weights from length (vector with homogeneous weights)
WeightFromLength<-function(length,species,part,individual,census)
{
  if (grepl("0",part))
  {
    part=str_sub(string=part,start=1,end=-4)
  }
  count=round(length*AvCountsPerMM[AvCountsPerMM$species==species,]$AvCountPerMM)
  w=WeightFromCount(count,species,part,individual=individual,census=census)
  w
}

# Weight from dimensions (for single plant element)
WeightFromRegression<-function(height,diameter=NA,species,part,individual,census)
{ 
  if (grepl("0",part))
  {
    part=str_sub(string=part,start=1,end=-4)
  }
  w=NA
  
  I=(IBW$individual==individual)&(IBW$part==part)&(IBW$census_to_use==census)&(IBW$dimension_height==height)&((IBW$dimension_diameter==diameter)|(is.na(IBW$dimension_diameter)&(is.na(diameter))))
  n_i=sum(I)
  if(n_i==1)
  {
    w=IBW[I,"av_weights"]
    #print(paste("Using exact weight reg",part,census,w,sep=" "))
  }
  if(n_i==0)
  {
  parameters=RegressionTable[(RegressionTable$species==species)&(RegressionTable$part==part),]
  #Regression equations 
  if(is.na(diameter))
  {
    w=parameters$intercept+parameters$slope*height^parameters$reg.order
  }
  else
  {#Calculations based on volume (cylinder approximation of cones geometry) and density
    volume=pi/4*diameter^2*height;
    w=parameters$slope*volume;
  }
  }
w
}


