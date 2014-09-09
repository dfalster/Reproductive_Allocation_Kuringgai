source("analysis/WeightCalculationsAtCensus.R")
source("analysis/RemoveDuplicates.R")
source("analysis/WeightFunctions.R")

#Read data
#Data=read.csv('data/reproduction.csv',sep=',',header=T,stringsAsFactors = FALSE, na.strings="")
#FPSummary=read.csv('data/flower_parts.csv',header=T,sep=',', stringsAsFactors = FALSE)

#Choose specific individual

WeightCalculationsForTree<-function(Tree)
{ #Determine the id for individual and put it as the first element in a list
  TreeID=as.character(unique(Tree$individual))
  TreeList=list(TreeID=TreeID)
  #Choose specific census
  for(i in 1:18)
  {
    #Measurments from Census i
    C_old=Tree[(Tree$census==i)&(Tree$pre.new=="pre-existing"),]
    C_new=Tree[(Tree$census==i)&(Tree$pre.new=="new"),]
    #Transforming measurments to weights
    C_old_list=WeightCalculationsAtCensus(C_old,TreeID,census=i)
    C_new_list=WeightCalculationsAtCensus(C_new,TreeID,census=i)
    C_total=c(C_old_list,C_new_list)
    C_total=RemoveDuplicates(C_total)
    #Create list containing new, pre.existing and total data for the given census
    C=list(pre.ex=C_old_list,new=C_new_list,total=C_total)
    #Add the information about weight structure at time i (place i+1 in the list) to the tree List
    TreeList[[i+1]]=C
  }
  #Return tree List
  TreeList
}
