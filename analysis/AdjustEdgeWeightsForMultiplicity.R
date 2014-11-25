AdjustEdgeWeightsForMultiplicity<-function(graph,species)
{
MultiplicityTable=read.csv('data/MultiplierTable.csv',header = T)  
EdgeList=get.edgelist(graph)
weights=get.edge.attribute(graph,'weight')
for(i in 1:nrow(EdgeList))
{
  from=EdgeList[i,1]
  to=EdgeList[i,2]
  MultiplierFrom=MultiplicityTable[MultiplicityTable$flower_part==from,species]
  MultiplierTo=MultiplicityTable[MultiplicityTable$flower_part==to,species]
  weights[i]=MultiplierTo/MultiplierFrom*weights[i]
}
graph=set.edge.attribute(graph,name = 'weight',value = weights)
return(graph)
}