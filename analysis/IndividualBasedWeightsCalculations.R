IndividualBasedWeightsCalculations<-function()
{
  FPSummary=read.csv('data/flower_parts.csv',header=TRUE,sep=',', stringsAsFactors = FALSE)
  FPSummary=FPSummary[FPSummary$census_notes_use=="used",c("individual","part","census_to_use","weight","count","dimension_height","dimension_diameter")]
  FPSummary=FPSummary[!is.na(as.numeric(FPSummary$weight)),]
  IndividualBasedWeights=FPSummary[!is.na(as.numeric(FPSummary$census_to_use)),]
  ToDivide=FPSummary[is.na(as.numeric(FPSummary$census_to_use)),]
  for(i in 1:nrow(ToDivide))
  {
    count=as.numeric(unlist(strsplit(as.character(ToDivide[i,"census_to_use"]),split=";")))
    ToAdd=ToDivide[rep(i,length(count)),]
    ToAdd$census_to_use=count 
    IndividualBasedWeights=rbind(IndividualBasedWeights,ToAdd)
  }
  rownames(IndividualBasedWeights)<-NULL
IndividualBasedWeights$weight=as.numeric(IndividualBasedWeights$weight)
IndividualBasedWeights$count=as.numeric(IndividualBasedWeights$count)
IndividualBasedWeights["av_weights"]=IndividualBasedWeights$weight/IndividualBasedWeights$count
IndividualBasedWeights=IndividualBasedWeights[!is.na(IndividualBasedWeights$av_weights),]
IndividualBasedWeights[is.na(IndividualBasedWeights)]=0
IndividualBasedWeights=aggregate(cbind(count,weight)~individual+part+census_to_use+dimension_height+dimension_diameter,data=IndividualBasedWeights,FUN=sum)
IndividualBasedWeights[IndividualBasedWeights==0]=NA
IndividualBasedWeights["av_weights"]=IndividualBasedWeights$weight/IndividualBasedWeights$count
write.csv(IndividualBasedWeights,file='./output/partWeights/IndividualBasedWeights.csv')
}
