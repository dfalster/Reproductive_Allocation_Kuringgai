#Function calculating weight for missing parts, i.e., parts which weigth can not be directly observed although we can determin its weight as a derivative of existing weights
#It alse deletes from the list the elements that are used only for the derivation purposes only.
DeriveMissingParts<-function(out)
{
####### EPMI #########
data=out[["EPMI"]]; 
nr=which(data[,2]=="flower_calyx")
data[nr,3]=NA
data[nr,4]=data[data$part=="flower_all_parts",4]-data[data$part=="flower_petals",4] 
data=data[!data$part=="flower_all_parts",]

out[["EPMI"]]=data;
######################

####### COER #########
data=out[["COER"]];
nr=which(data[,2]=="flower_stigma")
data[nr,3]=NA
data[nr,4]=data[data$part=="flower_all_parts",4]-data[data$part=="flower_petals",4]
data=data[!data$part=="flower_all_parts",]

nr=which(data[,2]=="finished_flower")
data[nr,3]=NA
data[nr,4]=data[data$part=="finished_flower_all_parts",4]-data[data$part=="flower_petals",4]
data=data[!data$part=="finished_flower_all_parts",]

out[["COER"]]=data;
######################

####### PILI #########
data=out[["PILI"]]; 
nr=which(data[,2]=="flower_stigma")
data[nr,1]="PILI"
data[nr,3]=NA
data[nr,4]=data[data$part=="fruit_young",4]*0.1

out[["PILI"]]=data;
######################

####### HEPU #########
data=out[["HEPU"]];
nr=which(data[,2]=="fruit_aborted")
data[nr,3]=NA
data[nr,4]=data[data$part=="calyx_aborted_fruit",4]+data[data$part=="fruit_aborted",4]
data=data[!data$part=="calyx_aborted_fruit",]
out[["HEPU"]]=data;

return(out)
}