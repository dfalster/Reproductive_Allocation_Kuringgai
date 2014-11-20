# Function that takes the tree structure returned by Weight calculations
# and transforms it to per/seed counts. The parts are not right now to be understand as separate entities rather their fractions that
# develop to a single seed at the end. Such transformation does not change carbon allocation between the elements with the
# same count, however can allow to adjust for the fact that one fruit can have multiple seeds.
MultiplierTable<-read.csv('data/MultiplierTable.csv',header=T,sep=',',stringsAsFactors=FALSE)

AdjustForMultiplicity_List<-function(List,species)
{
 if(length(List)>0)
 {
 for(i in 1:length(List))
   {
   part=List[[i]]$type
   species.multiplier=MultiplierTable[species]
   par.multiplier=species.multiplier[MultiplierTable[,1]==part,]
   List[[i]]$count=(List[[i]]$count)*par.multiplier;
   List[[i]]$weight=rep((List[[i]]$weight)/par.multiplier,par.multiplier)}
 }
 List
}


AdjustForMultiplicity_Census<-function(Census,species)
{
  Census$pre.ex=AdjustForMultiplicity_List(Census$pre.ex,species=species)
  Census$new=AdjustForMultiplicity_List(Census$new,species=species)
  Census$total =AdjustForMultiplicity_List(Census$total,species=species)
Census
}

AdjustForMultiplicity<-function(TreeList)
{
  species=substr(TreeList[[1]],1,4)
  
  for(i in 2:length(TreeList))
  {
   TreeList[[i]]=AdjustForMultiplicity_Census(TreeList[[i]],species) 
  }
TreeList  
}
