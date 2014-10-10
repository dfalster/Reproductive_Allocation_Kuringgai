source('analysis/GraphRepresentation.R')
source('analysis/InvestmentInAPartType.R')

InvestmentCalculations<-function(TreeListOrig)
{
  #Extract species name
  species=substr(TreeListOrig[[1]],1,4)
  #Initialize the variables
  ErrList=data.frame(Element=NA,Census=NA,Count=NA)
  Lost=data.frame(what=c(),Census=c(),count=c(),weight=c())
  FinishedDevelopement=data.frame(what=c(),Census=c(),count=c(),weight=c());
  if((length(sapply(TreeListOrig[[19]]$total,function(x) x$type)>0))){
    FinishedDevelopement=rbind(FinishedDevelopement,data.frame(what=sapply(TreeListOrig[[19]]$total,function(x) x$type),
                           Census=rep(18,length(sapply(TreeListOrig[[19]]$total,function(x) x$type))),
                           count=sapply(TreeListOrig[[19]]$total,function(x) x$count),
                           weight=sapply(TreeListOrig[[19]]$total,function(x) sum(x$weight))))}

  
  Investments=data.frame(FromCensus=c(),ToCensus=c(),From=c(),To=c(),Inv=c(),Count=c())
  Err=c()
  #If tree had no reproduction at all return list of empty data frames
  if (is.null(unlist(TreeListOrig[2:19])))
  {return(list(Inv=c(),Err=Err,Lost=Lost,FinishedDevelopement=FinishedDevelopement))}
  
  #Check how many main paths there are for this plant.
  n.paths=nrow(GraphMaps[[species]]$Paths)
  Paths=GraphMaps[[species]]$Paths
  #For each of the paths
  for( a in 1:n.paths)
  { L=data.frame(what=c(),Census=c(),count=c(),weight=c())
    FD=data.frame(what=c(),Census=c(),count=c(),weight=c())
    #Determine beginning and end of the path.
    BE=(GraphMaps[[species]]$Paths[a,])
    #Read in plant graph
    Plant.Graph=GraphMaps[[species]]$graph
    #Extract the list of vertices for the path
    PATH=get.shortest.paths(Plant.Graph,from=as.character(BE[1,1]),to=as.character(BE[1,2]),weights=NA)
    PATH=unlist(PATH$vpath)
    V_names=get.vertex.attribute(Plant.Graph,"name")
    #Define the progression list
    Progression=V_names[PATH]
    n=length(Progression)
    
    #Duplicate tree structure (Acc= for accesories calculations, Pred=for storing possible predecessors)
    # Will modify below,  TreeLis contains list of lists, one for each census. As we identify, parents for
    # a given object, they are removed from the list of possible predecssors. In this way progressively
    # identify predecssors.
    TreeList=TreeListOrig;
    TreeList_Acc =TreeList;
    TreeList_Pred=TreeList;
    #Go backwards in time i=j -> Census= j-1
    for(i in 19:2)
    {
      #Restrict your data to the time frame of interest
      TreeList=TreeList[1:i];
      TreeList_Pred=TreeList_Pred[1:i];
      TreeList_Acc =TreeList_Acc[1:i]
      #Go backwards in the progression (from most developed to the least developed parts)
      for (j in n:1)
      {
        Element=Progression[j]
        #Calculate the investment for that part (restricting possible progression)
        R=InvestmentInAPartType(TreeList=TreeList,TreeList_Pred=TreeList_Pred,Element=Element,Progression=Progression[1:j])
        #Update the list of possible predecessors (remove the ones that you used)
        TreeList_Pred=R[["TreeList_Pred"]]
        #Store investment and possible errors
        Investments=rbind(Investments,R[["Inv"]])
        ErrList=rbind(ErrList,R[["Err"]])
        
        {
          #Some elements have aborted/empty parts which can be their alternative at that stage of reproduction. Make calculations for them
          #
          #Checking if there are X-OR-s before moving to the lower developement
          #
          dist.to.root             = as.numeric(shortest.paths(Plant.Graph,v=Progression[1],to=Progression[j],weight=NA))
          which.have.the.same.dist = shortest.paths(Plant.Graph,v=Progression[1],weight=NA)==dist.to.root
          #Find the ones that belong to the same group
          which.have.the.same.col  = V(Plant.Graph)$col==get.vertex.attribute(Plant.Graph,index=Progression[1],name="col")
          #Find the ones that are not the original value
          which.are.not.the.orig   = !(V(Plant.Graph)$name==Progression[j])
          
          # The list of the XOR parts (mostly aborted fruits,seeds,a.s.o which should be taken into consideration when calculating what was lost.
          #This parts are ancesstors of the previous part as the one on the main axis is)
          XOR.Part=V(Plant.Graph)[which.have.the.same.dist&which.have.the.same.col&which.are.not.the.orig]
          XOR.Part=XOR.Part$name
          n.xors=length(XOR.Part)
          
          if(n.xors>0)
          {
            for(l in 1:n.xors)
            {  #Determine the XOR progression
            XOR.Progression=get.vertex.attribute(Plant.Graph,name="name",index=unlist(get.shortest.paths(Plant.Graph,from=Progression[1],to=XOR.Part[l])$vpath))
            R=InvestmentInAPartType(TreeList=TreeList,TreeList_Pred=TreeList_Pred,Element=XOR.Part[l],Progression=XOR.Progression)
            TreeList_Pred=R[["TreeList_Pred"]]
            Investments=rbind(Investments,R[["Inv"]])
            ErrList=rbind(ErrList,R[["Err"]])}
          }
        }
      }
# Befor moving to next census (from time t to t-1) check the list of avilable predecessor at time t-1 contain any elements. They will not develop to anything, hence they are lost.
if((i>2)&(i<18))
{if((length(sapply(TreeList_Pred[[i-1]]$total,function(x) x$type)>0))){
  L=rbind(L,data.frame(what=sapply(TreeList_Pred[[i-1]]$total,function(x) x$type),
                       Census=rep(i-2,length(sapply(TreeList_Pred[[i-1]]$total,function(x) x$type))),
                       count=sapply(TreeList_Pred[[i-1]]$total,function(x) x$count),
                       weight=sapply(TreeList_Pred[[i-1]]$total,function(x) sum(x$weight))))}}

if(i>2)
{
  if((length(sapply(TreeList_Pred[[i-1]]$total,function(x) x$type)>0))){
    FD=rbind(FD,data.frame(what=sapply(TreeList_Pred[[i-1]]$total,function(x) x$type),
                           Census=rep(i-2,length(sapply(TreeList_Pred[[i-1]]$total,function(x) x$type))),
                           count=sapply(TreeList_Pred[[i-1]]$total,function(x) x$count),
                           weight=sapply(TreeList_Pred[[i-1]]$total,function(x) sum(x$weight))))}}
    }
  
#Restrict loses to the line that you made your calculations on
Lost=rbind(Lost,L[as.character(L$what)%in%V(Plant.Graph)$name[V(Plant.Graph)$col==get.vertex.attribute(Plant.Graph,"col",index=Progression[1])],])
FinishedDevelopement=rbind(FinishedDevelopement,FD[as.character(FD$what)%in%V(Plant.Graph)$name[V(Plant.Graph)$col==get.vertex.attribute(Plant.Graph,"col",index=Progression[1])],])

###########################################
#Calculating cost of accesorries
###########################################
#
# Check the color of main progression line
Progression_color=get.vertex.attribute(Plant.Graph,index=Progression[1],name="col")
# Accesoried to that line have color 1 higher, moreover choose the last stage of progression for each accesory (degree=1)
Acc.Finals=V(Plant.Graph)[(V(Plant.Graph)$col==(Progression_color+1))&(degree(Plant.Graph)==1)]$name
# If there are accesories
if(length(Acc.Finals)>0)
{
  for(k in 1 :length(Acc.Finals))
  { L=data.frame(what=c(),Census=c(),count=c(),weight=c())
    FD=data.frame(what=c(),Census=c(),count=c(),weight=c())
    
    Accessory=Acc.Finals[k]
    #Reset the lists to the original ones.
    TreeList=TreeListOrig
    TreeList_Acc =TreeListOrig
    #Find the progression to the root
    Aux_Progression_to_root=get.vertex.attribute(Plant.Graph,name="name",unlist(get.shortest.paths(Plant.Graph,to=Accessory,from=Progression[1],weight=NA)$vpath))
    #Find the elements of progression that are not on main progression line
    I_not_on_main=!Aux_Progression_to_root%in%Progression
    Progression_Not_on_Main=Aux_Progression_to_root[I_not_on_main]
    n.aux=length(Progression_Not_on_Main)
    #Perform investment calculations in the same mannes as in the previous case. Only calculations for elements not on the main progression are made, however all the progression is allowed.
    for(i in 19:2)
    {
      TreeList=TreeList[1:i];
      TreeList_Acc =TreeList_Acc[1:i]
      for(l in n.aux:1)
      {
        Element.aux=Progression_Not_on_Main[l]
        R=InvestmentInAPartType(TreeList=TreeList,TreeList_Pred=TreeList_Acc,Element=Element.aux,Progression=Aux_Progression_to_root[1:(length(Aux_Progression_to_root)-n.aux+l)])
        TreeList_Acc=R[["TreeList_Pred"]]
        Investments=rbind(Investments,R[["Inv"]])
        ErrList=rbind(ErrList,R[["Err"]])
      }
      #Calculate lost elements
      if((i>2)&(i<18))
      {if((length(sapply(TreeList_Acc[[i-1]]$total,function(x) x$type)>0))){
        L=rbind(L,data.frame(what=sapply(TreeList_Acc[[i-1]]$total,function(x) x$type),
                             Census=rep(i-2,length(sapply(TreeList_Acc[[i-1]]$total,function(x) x$type))),
                             count=sapply(TreeList_Acc[[i-1]]$total,function(x) x$count),
                             weight=sapply(TreeList_Acc[[i-1]]$total,function(x) sum(x$weight))))}
      }
      if(i>2)
      {if((length(sapply(TreeList_Acc[[i-1]]$total,function(x) x$type)>0))){
        FD=rbind(FD,data.frame(what=sapply(TreeList_Acc[[i-1]]$total,function(x) x$type),
                               Census=rep(i-2,length(sapply(TreeList_Acc[[i-1]]$total,function(x) x$type))),
                               count=sapply(TreeList_Acc[[i-1]]$total,function(x) x$count),
                               weight=sapply(TreeList_Acc[[i-1]]$total,function(x) sum(x$weight))))}
      }
    }
    #Restrict loss calculations to the elements outside main progression line.
    Lost=rbind(Lost,L[as.character(L$what)%in%V(Plant.Graph)$name[V(Plant.Graph)$col==get.vertex.attribute(Plant.Graph,"col",index=Accessory)],])
    FinishedDevelopement=rbind(FinishedDevelopement,FD[as.character(FD$what)%in%V(Plant.Graph)$name[V(Plant.Graph)$col==get.vertex.attribute(Plant.Graph,"col",index=Accessory)],])
    
  }
}

}

#Aggregare and order the result
ErrList=ErrList[complete.cases(ErrList),]
if(nrow(ErrList)>0)
{
  Err=aggregate(Count~Element+Census,data=ErrList,FUN="sum")
  Err=Err[order(Err$Census),]
}
Investments=Investments[complete.cases(Investments),]
if(nrow(Investments)>0)
{
  Inv=aggregate(Count~FromCensus+ToCensus+From+To+Inv,FUN="sum",data=Investments)
  Inv=Inv[order(Inv$ToCensus),]
}
Inv["Total"]=Inv$Inv*Inv$Count
Inv["Individual"]=rep(TreeListOrig[[1]],nrow(Inv))

Lost=unique(Lost)
FinishedDevelopement=unique(FinishedDevelopement)
#Exclude elements of degree 1, they can not develop, not lost
LeavesOfGraph=V(Plant.Graph)$name[degree(Plant.Graph)>1]
#Add roots, if they are not the only elements
NonAtomicPathsBeginning=as.character(Paths[as.character(Paths[,1])!=as.character(Paths[,2]),1])
#Progressable parts are defined by union of the both
Progressable=c(LeavesOfGraph,NonAtomicPathsBeginning)
Lost=Lost[Lost$what%in%Progressable,]

if(nrow(Lost)>0)
{
  Lost=Lost[order(Lost$Census),]
}

if(nrow(FinishedDevelopement)>0)
{
  FinishedDevelopement=FinishedDevelopement[order(FinishedDevelopement$Census),]
}

list(Inv=Inv,Err=Err,Lost=Lost,FinishedDevelopement=FinishedDevelopement)
}
