source('analysis/InvestmentInIndividualPart.R')
source('analysis/GraphRepresentation.R')



InvestmentInAPartType<-function(TreeList,TreeList_Pred,Element,Progression)
{
 #Initialize data frames
 Inv=From=To=FromCensus=ToCensus=Count=c()
 Inv_new=data.frame(FromCensus=FromCensus,ToCensus=ToCensus,From=From,To=To,Inv=Inv,Count=Count)
 Inv_pre.ex=data.frame(FromCensus=FromCensus,ToCensus=ToCensus,From=From,To=To,Inv=Inv,Count=Count)
 Inv_total=data.frame(FromCensus=FromCensus,ToCensus=ToCensus,From=From,To=To,Inv=Inv,Count=Count)

 N=length(TreeList)
 #Are there any new parts of that type? If yes, calculate investment
 I_new=sapply(TreeList[[N]]$new,function(x) x$type)==Element
 if(sum(unlist(I_new)))
 {
     Inv=TreeList[[N]]$new[[which(I_new)]]$weight
     From=rep("0",length(Inv))
     To=rep(Element,length(Inv))
     ToCensus=rep(N-1,length(Inv))
     FromCensus=rep(N-2,length(Inv))
     Count=rep(1,length(Inv))
     Inv_new=data.frame(FromCensus=FromCensus,ToCensus=ToCensus,From=From,To=To,Inv=Inv,Count=Count)
 }
 
 #Are there any pre-existing parts of that type.
 I_pre.ex=sapply(TreeList[[N]]$pre.ex,function(x) x$type)==Element
 Inv=From=To=Census=Count=FromCensus=ToCensus=c()
 ErrList=data.frame(Element=NA,Census=NA,Count=NA)
 #If there are there are pre.existing parts and we are at least at Census 2 we try to find a predecessor.
 if(sum(unlist(I_pre.ex))&N>2)
 {
 #Check how many elements of that type there are
 n_elements=TreeList[[N]]$pre.ex[[which(I_pre.ex)]]$count
 for(i in 1:n_elements)
  {
   #Extract the weight of a single element
   El_weight=TreeList[[N]]$pre.ex[[which(I_pre.ex)]]$weight[i]
   #Calculate investment made into progressiong to the element from its ancestor.
   R=InvestmentInIndividualPart(Element=Element,TreeList_Pred=TreeList_Pred,Progression=Progression,El_weight=El_weight)
   Inv[i]=R[["Invest"]]
   From[i]=R[["from"]]
   To[i]=R[["to"]]
   FromCensus[i]=R[["FromCensus"]]
   ToCensus[i]=R[["ToCensus"]]
   Count[i]=R[["Count"]]
   TreeList_Pred=R[["TreeList_Pred"]];
   ErrList=rbind(ErrList,R[["ErrList"]])
  }
 Inv_pre.ex=data.frame(FromCensus=FromCensus,ToCensus=ToCensus,From=From,To=To,Inv=Inv,Count=Count)
 }
 Inv_total=rbind(Inv_new,Inv_pre.ex)
 
 
 return(list(TreeList_Pred=TreeList_Pred, Inv=Inv_total,Err=ErrList))
}
