#Function that for given Census contruct the
#list that contains the information about the weight of each part
################################
library(stringr)
#source('analysis/OrderedListsOfParts.R')

FloweringCategories=read.csv('data/Flowering_cat_lookup_table.csv',sep=',',header=T, stringsAsFactors = FALSE)

WeightCalculationsAtCensus<-function(C, TreeID,census){
  #Determine species name and allowed plant parts together with their way of measuring
  species=substr(TreeID,1,4)
  Parts=FloweringCategories[,c("flower_part",species)]
  Parts=Parts[!(Parts[,2]==""),]
  n.parts=nrow(Parts)
  C_list=list()
  k=0;
  #For each of the parts check how to transform inform in repro_spreadheet to weight and perform given calculations
  for(i in 1:n.parts)
  {
    if(Parts[i,2]=="count")
    {
      count_ch=C[,Parts[i,1]]
      if(!is.na(count_ch))
      {
        k=k+1
        type=Parts[i,1]
        m.type="count"
        count=sum(as.numeric(unlist(strsplit(as.character(count_ch),split=";"))))
        weight=WeightFromCount(count=count,species=species,part=type,individual=TreeID,census=census)
        #specific for inflorescence in HATE(Lizzy made calculations on scale of inflorescent hence adjustments. Possibly we can move it to Multiplier table, althought it is slightly different reasoning)
        if((species=="HATE")*(grepl("inflorescence",type)))
        {
          count=round(count*5.8696);
          weight=rep(weight[1]/5.8696,count)
        }
        Element=list(type=type,m.type=m.type,count=length(weight),weight=weight)
        C_list[[k]]=Element
      }
    }
    if(Parts[i,2]=="count_by_length")
    {
      length_ch=C[,paste0(Parts[i,1],"_by_length")]
      if(!is.na(length_ch))
      {
        k=k+1
        type=Parts[i,1]
        m.type="length"
        length=sum(as.numeric(unlist(strsplit(as.character(length_ch),split=";"))))
        weight=WeightFromLength(length=length,species=species,part=type,individual=TreeID,census=census)
        Element=list(type=type,m.type=m.type,count=length(weight),weight=weight)
        C_list[[k]]=Element
      }
    }
  
    if(Parts[i,2]=="count; count_by_length")
    {
      count_ch=C[,Parts[i,1]]
      if(!is.na(count_ch))
      {
        k=k+1
        type=Parts[i,1]
        m.type="count"
        count=sum(as.numeric(unlist(strsplit(as.character(count_ch),split=";"))))
        weight=WeightFromCount(count=count,species=species,part=type,individual=TreeID,census=census)
        Element=list(type=type,m.type=m.type,count=count,weight=weight)
        C_list[[k]]=Element
      }
      length_ch=C[,paste0(Parts[i,1],"_by_length")]
      if(!is.na(length_ch))
      {
        k=k+1
        type=Parts[i,1]
        m.type="length"
        length=sum(as.numeric(unlist(strsplit(as.character(length_ch),split=";"))))
        weight=WeightFromLength(length=length,species=species,part=type,individual=TreeID,census=census)
        Element=list(type=type,m.type=m.type,count=length(weight),weight=weight)
        C_list[[k]]=Element
      }
    }  
    
    
    if(Parts[i,2]=="regress_by_dim")
    {
      height_ch=C[,Parts[i,1]]
      if(!is.na(height_ch))
      {
        k=k+1
        type=Parts[i,1]
        m.type="regress_by_dim"
        heights=as.numeric(unlist(strsplit(as.character(height_ch),split=";")))
        count=length(heights)
        weight=c()
        for(j in 1:count)
        {
        weight[j]=WeightFromRegression(height=heights[j],species=species,part=type,individual=TreeID,census=census)
        }
        Element=list(type=type,m.type=m.type,count=length(weight),weight=weight)
        C_list[[k]]=Element
      }     
    }
  
    if(Parts[i,2]=="volume")
    {
      dimension_ch=C[,Parts[i,1]]
      if(!is.na(dimension_ch))
      {
        k=k+1
        type=Parts[i,1]
        m.type="volume"
        heights  =matrix(as.numeric(unlist(strsplit(unlist(strsplit(as.character(dimension_ch),split=";")),split="x"))),ncol=2,byrow=T)[,1]
        diameters=matrix(as.numeric(unlist(strsplit(unlist(strsplit(as.character(dimension_ch),split=";")),split="x"))),ncol=2,byrow=T)[,2]      
        count=length(heights)
        weight=c()
        for(j in 1:count)
        {
          weight[j]=WeightFromRegression(height=heights[j],diameter=diameters[j],species=species,part=type,individual=TreeID,census=census)
        }
        Element=list(type=type,m.type=m.type,count=length(weight),weight=weight)
        C_list[[k]]=Element
      } 
    }
  }
  
  #Checking if we need to merge the results
  if(sum(duplicated(unlist(sapply(C_list,function(x) x[1]))))>0)
    {#there are duplicates from different measurment methods
     #and we need to join them together.
    C_list=RemoveDuplicates(C_list)
    }
  C_list
}
