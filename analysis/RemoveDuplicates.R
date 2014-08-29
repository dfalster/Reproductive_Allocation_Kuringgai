####################################################################################
#Removing duplicates from the list
#- Need to be used if a mixed method was used to obtain the weights at one census
#- Need to be used for joining the information from old and new census to a total one. 
####################################################################################
RemoveDuplicates<-function(C)
{
C_new=list()
parts.names=unique(sapply(C,function(x) x$type))
k=length(parts.names)
if(k>0)
{
for( i in 1: k)
{
  part.name=parts.names[i]
  repeated.parts = C[unlist(sapply(C,'[[',"type"))==part.name]
  total.count    = sum(unlist(sapply(repeated.parts,'[[',"count")))
  total.weights  = as.vector(unlist(sapply(repeated.parts,'[[',"weight")))
  methods.used   = as.vector(unlist(sapply(repeated.parts,'[[',"m.type")))
  if(all(methods.used==methods.used[1]))
  {
    m.type=methods.used[1]
  }
  else
  {
    m.type="mixed"
  }
  Element=list(type=part.name,m.type=m.type,count=total.count,weight=total.weights)
  C_new[[i]]=Element
}
}
C_new
}
