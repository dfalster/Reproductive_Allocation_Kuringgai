InvestmentInIndividualPart<-function(Element,TreeList_Pred,Progression,El_weight)
{
  #Determine what time are we at
  N=length(TreeList_Pred)
  #Go back in time
  for(t in (N-1):2)
  {
    #If there is no possible progression for this part add that to error list
    if (length(Progression)==1)
    { 
      ErrList=data.frame(Element=Element,Census=N-1,Count=1)
      return(list(TreeList_Pred=TreeList_Pred,Invest=NA,from=NA,to=NA,FromCensus=NA,ToCensus=NA,Count=NA,ErrList=ErrList))
    }
    #what are the parts avilable at that time
    parts.at.time.t=unlist(sapply(TreeList_Pred[[t]]$total, function(x) x$type))
    #Go backwards on the progression line
    for(Predecessor in Progression[(length(Progression)-1):1])
    { #If you find a predecessor
      if(sum(Predecessor==parts.at.time.t))
      { 
       #Use graphd to determine path between predecessor and ancesstor. 
       Plant.Graph=GraphMaps[[substr(TreeList_Pred[[1]],1,4)]]$graph
       EdgePath=unlist(get.shortest.paths(Plant.Graph,from=Element,to=Predecessor,output="both",weights=NA)$epath)
       #proportion of carbon of the predecessors weight that has been used to produce the ancesstor
       w=prod(get.edge.attribute(Plant.Graph,name="weight",index=EdgePath))
       #The weight of ancesstor is provided,Find the weight of predecessor
       I_predecessor=sapply(TreeList_Pred[[t]]$total,function(x) x$type)==Predecessor
       Pre_weight=TreeList_Pred[[t]]$total[[which(I_predecessor)]]$weight[1]
       #Reduce the count of predecessors, remove the weight
       TreeList_Pred[[t]]$total[[which(I_predecessor)]]$weight=TreeList_Pred[[t]]$total[[which(I_predecessor)]]$weight[-1]
       TreeList_Pred[[t]]$total[[which(I_predecessor)]]$count=TreeList_Pred[[t]]$total[[which(I_predecessor)]]$count-1
       if(TreeList_Pred[[t]]$total[[which(I_predecessor)]]$count==0)
       {
         TreeList_Pred[[t]]$total[[which(I_predecessor)]]=NULL
       }
       ErrList=data.frame(Element=NA,Census=NA,Count=NA)
       return(list(TreeList_Pred=TreeList_Pred,Invest=El_weight-w*Pre_weight,from=Predecessor,to=Element,FromCensus=t-1,ToCensus=N-1,Count=1,ErrList=ErrList))
       #take away the count from the list and remove the element
      }
    }
  }
ErrList=data.frame(Element=Element,Census=N-1,Count=1)
return(list(TreeList_Pred=TreeList_Pred,Invest=NA,from=NA,to=NA,FromCensus=NA,ToCensus=NA,Count=NA,ErrList=ErrList))
}