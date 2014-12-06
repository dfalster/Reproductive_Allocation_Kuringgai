TreeListToTotalDataFrame<-function(TreeList)
{
  individual=TreeList[[1]]
  TreeTotalDataFrame=data.frame(individual=c(),census=c(),part=c(),count=c(),weight=c())
  for(i in 2:(length(TreeList)))
  {
    CensusTotal=TreeList[[i]]$total
    n.types=length(CensusTotal)
    if(n.types>0)
    {
    for(j in 1:n.types)
    {
      census=i-1;
      type  = CensusTotal[[j]]$type
      count = CensusTotal[[j]]$count
      weight= sum(CensusTotal[[j]]$weight)
      TreeTotalDataFrame=rbind(TreeTotalDataFrame,data.frame(individual=NA,census=census,part=type,count=count,weight=weight))
    }
    }
  }
 TreeTotalDataFrame$individual=individual
 return(TreeTotalDataFrame)
}