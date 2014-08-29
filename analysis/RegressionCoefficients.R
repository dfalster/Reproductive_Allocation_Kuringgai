RegressionCoefficients<-function()
{
##############################################################################################
# Calculating the regression coefficients requiered for calculations of weight based on the
# measured dimension of the plant part.
# Calculations are only made for some particular species and are part specific.
###############################################################################################
FPSummary=read.csv('data/flower_parts.csv',header=TRUE,sep=',', stringsAsFactors = FALSE)
out <- list()
FPSummary=FPSummary[FPSummary$category_use=="used",]


species<-part<-n<-intercept<-slope<-reg.type<-reg.order<-c()

###### GRBU ######

i=1
species.name="GRBU"
part.name="fruit_large_immature"
reg.order[i]=2;
reg.type[i] ="quadratic_undecided"

species[i]=species.name; part[i]=part.name
SpeciesData=FPSummary[FPSummary$species==species.name,]
PartData=SpeciesData[SpeciesData$part==part.name,]
height=PartData$dimension_height
weight=as.numeric(PartData$weight)
n[i]=length(height)
lm.res=lm(weight~I(height^reg.order[i]))
pdf(file=paste('output/docs/',species.name,"_",part.name,".pdf",sep=""),width=10,height=10)
plot(height,weight,type="p",main=paste(species.name,part.name,sep=": "))
x.dense=data.frame(height=seq(from=min(height),to=max(height),length.out=100))
lines(x.dense$height,predict.lm(object=lm.res,newdata=x.dense))
dev.off()
intercept[i] <-coef(lm.res)[1]
slope[i]     <-coef(lm.res)[2]


i=2
species.name="GRBU"
part.name="seed_pod"
reg.order[i]=1;
reg.type[i] ="linear"

species[i]=species.name; part[i]=part.name
SpeciesData=FPSummary[FPSummary$species==species.name,]
PartData=SpeciesData[SpeciesData$part==part.name,]
height=PartData$dimension_height
weight=as.numeric(PartData$weight)
n[i]=length(height)
lm.res=lm(weight~I(height^reg.order[i]))
pdf(file=paste('output/docs/',species.name,"_",part.name,".pdf",sep=""),width=10,height=10)
plot(height,weight,type="p",main=paste(species.name,part.name,sep=": "))
x.dense=data.frame(height=seq(from=min(height),to=max(height),length.out=100))
lines(x.dense$height,predict.lm(object=lm.res,newdata=x.dense))
dev.off()
intercept[i] <-coef(lm.res)[1]
slope[i]     <-coef(lm.res)[2]


i=3
species.name="GRSP"
part.name="fruit_large_immature"
reg.order[i]=2;
reg.type[i] ="quadratic"

species[i]=species.name; part[i]=part.name
SpeciesData=FPSummary[FPSummary$species==species.name,]
PartData=SpeciesData[SpeciesData$part==part.name,]
height=PartData$dimension_height
weight=as.numeric(PartData$weight)
n[i]=length(height)
lm.res=lm(weight~I(height^reg.order[i]))
pdf(file=paste('output/docs/',species.name,"_",part.name,".pdf",sep=""),width=10,height=10)
plot(height,weight,type="p",main=paste(species.name,part.name,sep=": "))
x.dense=data.frame(height=seq(from=min(height),to=max(height),length.out=100))
lines(x.dense$height,predict.lm(object=lm.res,newdata=x.dense))
dev.off()
intercept[i] <-coef(lm.res)[1]
slope[i]     <-coef(lm.res)[2]

i=4
species.name="GRSP"
part.name="seed_pod"
reg.order[i]=1;
reg.type[i] ="linear"

species[i]=species.name; part[i]=part.name
SpeciesData=FPSummary[FPSummary$species==species.name,]
PartData=SpeciesData[SpeciesData$part==part.name,]
height=PartData$dimension_height
weight=as.numeric(PartData$weight)
n[i]=length(height)
lm.res=lm(weight~I(height^reg.order[i]))
pdf(file=paste('output/docs/',species.name,"_",part.name,".pdf",sep=""),width=10,height=10)
plot(height,weight,type="p",main=paste(species.name,part.name,sep=": "))
x.dense=data.frame(height=seq(from=min(height),to=max(height),length.out=100))
lines(x.dense$height,predict.lm(object=lm.res,newdata=x.dense))
dev.off()
intercept[i] <-coef(lm.res)[1]
slope[i]     <-coef(lm.res)[2]

###### PELA

i=5
species.name="PELA"
part.name="fruit_large_immature"
reg.order[i]=2;
reg.type[i] ="quadratic"

species[i]=species.name; part[i]=part.name
SpeciesData=FPSummary[FPSummary$species==species.name,]
PartData=SpeciesData[SpeciesData$part==part.name,]
height=PartData$dimension_height
weight=as.numeric(PartData$weight)/PartData$count
n[i]=length(height)
lm.res=lm(weight~I(height^reg.order[i]))
pdf(file=paste('output/docs/',species.name,"_",part.name,".pdf",sep=""),width=10,height=10)
plot(height,weight,type="p",main=paste(species.name,part.name,sep=": "))
x.dense=data.frame(height=seq(from=min(height),to=max(height),length.out=100))
lines(x.dense$height,predict.lm(object=lm.res,newdata=x.dense))
dev.off()
intercept[i] <-coef(lm.res)[1]
slope[i]     <-coef(lm.res)[2]

i=6
species.name="PELA"
part.name="seed_pod"
reg.order[i]=3;
reg.type[i] ="cubic"

species[i]=species.name; part[i]=part.name
SpeciesData=FPSummary[FPSummary$species==species.name,]
PartData=SpeciesData[SpeciesData$part==part.name,]
height=PartData$dimension_height
weight=as.numeric(PartData$weight)/PartData$count
n[i]=length(height)
lm.res=lm(weight~I(height^reg.order[i]))
pdf(file=paste('output/docs/',species.name,"_",part.name,".pdf",sep=""),width=10,height=10)
plot(height,weight,type="p",main=paste(species.name,part.name,sep=": "))
x.dense=data.frame(height=seq(from=min(height),to=max(height),length.out=100))
lines(x.dense$height,predict.lm(object=lm.res,newdata=x.dense))
dev.off()
intercept[i] <-coef(lm.res)[1]
slope[i]     <-coef(lm.res)[2]

i=7
species.name="PELA"
part.name="seed"
reg.order[i]=3;
reg.type[i] ="cubic"

species[i]=species.name; part[i]=part.name
SpeciesData=FPSummary[FPSummary$species==species.name,]
PartData=SpeciesData[SpeciesData$part==part.name,]
height=PartData$dimension_height
weight=as.numeric(PartData$weight)/PartData$count
n[i]=length(height)
lm.res=lm(weight~I(height^reg.order[i]))
pdf(file=paste('output/docs/',species.name,"_",part.name,".pdf",sep=""),width=10,height=10)
plot(height,weight,type="p",main=paste(species.name,part.name,sep=": "))
x.dense=data.frame(height=seq(from=min(height),to=max(height),length.out=100))
lines(x.dense$height,predict.lm(object=lm.res,newdata=x.dense))
dev.off()
intercept[i] <-coef(lm.res)[1]
slope[i]     <-coef(lm.res)[2]


#BAER -  calculating density with respect to volume calculated using cylindric approximation


i=8
species.name="BAER"
part.name="cone_young"
reg.order[i]=2;
reg.type[i] ="quadratic without intercept"

species[i]=species.name; part[i]=part.name
SpeciesData=FPSummary[FPSummary$species==species.name,]
PartData=SpeciesData[SpeciesData$part==part.name,]
height=PartData$dimension_height
weight=as.numeric(PartData$weight)/PartData$count
n[i]=length(height)
lm.res=lm(weight~0+I(height^reg.order[i]))
pdf(file=paste('output/docs/',species.name,"_",part.name,".pdf",sep=""),width=10,height=10)
plot(height,weight,type="p",main=paste(species.name,part.name,sep=": "))
x.dense=data.frame(height=seq(from=min(height),to=max(height),length.out=100))
lines(x.dense$height,predict.lm(object=lm.res,newdata=x.dense))
dev.off()
intercept[i] <-0#coef(lm.res)[1]
slope[i]     <-coef(lm.res)[1]


i=9
species.name="BAER"
part.name="cone_green"
reg.order[i]=1;
reg.type[i] ="volume*density"

species[i]=species.name; part[i]=part.name
SpeciesData=FPSummary[FPSummary$species==species.name,]
PartData=SpeciesData[SpeciesData$part==part.name,]
volume=PartData$dimension_height*(PartData$dimension_diameter/2)^2*pi;
weight=as.numeric(PartData$weight)
n[i]=length(volume)
lm.res=lm(weight~0+volume)
pdf(file=paste('output/docs/',species.name,"_",part.name,".pdf",sep=""),width=10,height=10)
plot(volume,weight,type="p",main=paste(species.name,part.name,sep=": "))
x.dense=data.frame(volume=seq(from=min(volume),to=max(volume),length.out=100))
lines(x.dense$volume,predict.lm(object=lm.res,newdata=x.dense))
dev.off()
intercept[i] <-0
slope[i]     <-coef(lm.res)[1]

i=10
species.name="BAER"
part.name="cone_brown_no_expanded_follicles"
reg.order[i]=1;
reg.type[i] ="volume*density"

species[i]=species.name; part[i]=part.name
SpeciesData=FPSummary[FPSummary$species==species.name,]
PartData=SpeciesData[SpeciesData$part==part.name,]
volume=PartData$dimension_height*(PartData$dimension_diameter/2)^2*pi;
weight=as.numeric(PartData$weight)
n[i]=length(volume)
lm.res=lm(weight~0+volume)
pdf(file=paste('output/docs/',species.name,"_",part.name,".pdf",sep=""),width=10,height=10)
plot(volume,weight,type="p",main=paste(species.name,part.name,sep=": "))
x.dense=data.frame(volume=seq(from=min(volume),to=max(volume),length.out=100))
lines(x.dense$volume,predict.lm(object=lm.res,newdata=x.dense))
dev.off()
intercept[i] <-0
slope[i]     <-coef(lm.res)[1]

i=11
species.name="BAER"
part.name="cone_brown"
reg.order[i]=1;
reg.type[i] ="volume*density"

species[i]=species.name; part[i]=part.name
SpeciesData=FPSummary[FPSummary$species==species.name,]
PartData=SpeciesData[SpeciesData$part==part.name,]
volume=PartData$dimension_height*(PartData$dimension_diameter/2)^2*pi;
weight=as.numeric(PartData$weight)
n[i]=length(volume)
lm.res=lm(weight~0+volume)
pdf(file=paste('output/docs/',species.name,"_",part.name,".pdf",sep=""),width=10,height=10)
plot(volume,weight,type="p",main=paste(species.name,part.name,sep=": "))
x.dense=data.frame(volume=seq(from=min(volume),to=max(volume),length.out=100))
lines(x.dense$volume,predict.lm(object=lm.res,newdata=x.dense))
dev.off()
intercept[i] <-0
slope[i]     <-coef(lm.res)[1]

i=12
species.name="BAER"
part.name="cone_aborted"
reg.order[i]=2;
reg.type[i] ="quadratic"

species[i]=species.name; part[i]=part.name
SpeciesData=FPSummary[FPSummary$species==species.name,]
PartData=SpeciesData[SpeciesData$part==part.name,]
height=PartData$dimension_height
weight=as.numeric(PartData$weight)/PartData$count
n[i]=length(height)
lm.res=lm(weight~0+I(height^reg.order[i]))
pdf(file=paste('output/docs/',species.name,"_",part.name,".pdf",sep=""),width=10,height=10)
plot(height,weight,type="p",main=paste(species.name,part.name,sep=": "))
x.dense=data.frame(height=seq(from=min(height),to=max(height),length.out=100))
lines(x.dense$height,predict.lm(object=lm.res,newdata=x.dense))
dev.off()
intercept[i] <-0#coef(lm.res)[1]
slope[i]     <-coef(lm.res)[1]


i=13
species.name="BAER"
part.name="cone_base_green"
reg.order[i]=1;
reg.type[i] ="volume*density"

species[i]=species.name; part[i]=part.name
SpeciesData=FPSummary[FPSummary$species==species.name,]
PartData=SpeciesData[SpeciesData$part==part.name,]
volume=PartData$dimension_height*(PartData$dimension_diameter/2)^2*pi;
weight=as.numeric(PartData$weight)
n[i]=length(volume)
lm.res=lm(weight~0+volume)
pdf(file=paste('output/docs/',species.name,"_",part.name,".pdf",sep=""),width=10,height=10)
plot(volume,weight,type="p",main=paste(species.name,part.name,sep=": "))
x.dense=data.frame(volume=seq(from=min(volume,na.rm=T),to=max(volume,na.rm=T),length.out=100))
lines(x.dense$volume,predict.lm(object=lm.res,newdata=x.dense))
dev.off()
intercept[i] <-0
slope[i]     <-coef(lm.res)[1]


i=14
species.name="BAER"
part.name="cone_base_brown"
reg.order[i]=1;
reg.type[i] ="volume*density"

species[i]=species.name; part[i]=part.name
SpeciesData=FPSummary[FPSummary$species==species.name,]
PartData=SpeciesData[SpeciesData$part==part.name,]
volume=PartData$dimension_height*(PartData$dimension_diameter/2)^2*pi;
weight=as.numeric(PartData$weight)
n[i]=length(volume)
lm.res=lm(weight~0+volume)
pdf(file=paste('output/docs/',species.name,"_",part.name,".pdf",sep=""),width=10,height=10)
plot(volume,weight,type="p",main=paste(species.name,part.name,sep=": "))
x.dense=data.frame(volume=seq(from=min(volume,na.rm=T),to=max(volume,na.rm=T),length.out=100))
lines(x.dense$volume,predict.lm(object=lm.res,newdata=x.dense))
dev.off()
intercept[i] <-0
slope[i]     <-coef(lm.res)[1]

#PEPU -  calculating density with respect to volume calculated using cylindric approximation


i=15
species.name="PEPU"
part.name="cone_just_starting"
reg.order[i]=1;
reg.type[i] ="No data"

species[i]=species.name; part[i]=part.name
SpeciesData=FPSummary[FPSummary$species==species.name,]
PartData=SpeciesData[SpeciesData$part==part.name,]
height=PartData$dimension_height
weight=as.numeric(PartData$weight)/PartData$count
n[i]=length(height)
lm.res=lm(weight~I(height^reg.order[i]))
pdf(file=paste('output/docs/',species.name,"_",part.name,".pdf",sep=""),width=10,height=10)
plot(height,weight,type="p",main=paste(species.name,part.name,sep=": "))
x.dense=data.frame(height=seq(from=min(height),to=max(height),length.out=100))
lines(x.dense$height,predict.lm(object=lm.res,newdata=x.dense))
dev.off()
intercept[i] <-coef(lm.res)[1]
slope[i]     <-coef(lm.res)[2]

i=16
species.name="PEPU"
part.name="cone_young"
reg.order[i]=1;
reg.type[i] ="No data"

species[i]=species.name; part[i]=part.name
SpeciesData=FPSummary[FPSummary$species==species.name,]
PartData=SpeciesData[SpeciesData$part==part.name,]
height=PartData$dimension_height
weight=as.numeric(PartData$weight)/PartData$count
n[i]=length(height)
lm.res=lm(weight~I(height^reg.order[i]))
pdf(file=paste('output/docs/',species.name,"_",part.name,".pdf",sep=""),width=10,height=10)
plot(height,weight,type="p",main=paste(species.name,part.name,sep=": "))
x.dense=data.frame(height=seq(from=min(height),to=max(height),length.out=100))
lines(x.dense$height,predict.lm(object=lm.res,newdata=x.dense))
dev.off()
intercept[i] <-coef(lm.res)[1]
slope[i]     <-coef(lm.res)[2]

i=17
species.name="PEPU"
part.name="cone_green"
reg.order[i]=1;
reg.type[i] ="volume*density"

species[i]=species.name; part[i]=part.name
SpeciesData=FPSummary[FPSummary$species==species.name,]
PartData=SpeciesData[SpeciesData$part==part.name,]
volume=PartData$dimension_height*(PartData$dimension_diameter/2)^2*pi;
n[i]=length(volume)
if(n[i]>0)
{
  weight=as.numeric(PartData$weight)
  lm.res=lm(weight~0+volume)
  pdf(file=paste('output/docs/',species.name,"_",part.name,".pdf",sep=""),width=10,height=10)
  plot(volume,weight,type="p",main=paste(species.name,part.name,sep=": "))
  x.dense=data.frame(volume=seq(from=min(volume,na.rm=T),to=max(volume,na.rm=T),length.out=100))
  lines(x.dense$volume,predict.lm(object=lm.res,newdata=x.dense))
  dev.off()
  intercept[i] <-0
  slope[i]     <-coef(lm.res)[1]
}

i=18
species.name="PEPU"
part.name="cone_brown"
reg.order[i]=1;
reg.type[i] ="volume*density"

species[i]=species.name; part[i]=part.name
SpeciesData=FPSummary[FPSummary$species==species.name,]
PartData=SpeciesData[SpeciesData$part==part.name,]
volume=PartData$dimension_height*(PartData$dimension_diameter/2)^2*pi;
n[i]=length(volume)
if(n[i]>0)
{
  weight=as.numeric(PartData$weight)
  lm.res=lm(weight~0+volume)
  pdf(file=paste('output/docs/',species.name,"_",part.name,".pdf",sep=""),width=10,height=10)
  plot(volume,weight,type="p",main=paste(species.name,part.name,sep=": "))
  x.dense=data.frame(volume=seq(from=min(volume,na.rm=T),to=max(volume,na.rm=T),length.out=100))
  lines(x.dense$volume,predict.lm(object=lm.res,newdata=x.dense))
  dev.off()
  intercept[i] <-0
  slope[i]     <-coef(lm.res)[1]
}

i=19
species.name="PEPU"
part.name="cone_aborted"
reg.order[i]=1;
reg.type[i] ="volume*density"

species[i]=species.name; part[i]=part.name
SpeciesData=FPSummary[FPSummary$species==species.name,]
PartData=SpeciesData[SpeciesData$part==part.name,]
volume=PartData$dimension_height*(PartData$dimension_diameter/2)^2*pi;
n[i]=length(volume)
if(n[i]>0)
{
  weight=as.numeric(PartData$weight)
  lm.res=lm(weight~0+volume)
  pdf(file=paste('output/docs/',species.name,"_",part.name,".pdf",sep=""),width=10,height=10)
  plot(volume,weight,type="p",main=paste(species.name,part.name,sep=": "))
  x.dense=data.frame(volume=seq(from=min(volume,na.rm=T),to=max(volume,na.rm=T),length.out=100))
  lines(x.dense$volume,predict.lm(object=lm.res,newdata=x.dense))
  dev.off()
  intercept[i] <-0
  slope[i]     <-coef(lm.res)[1]
}



RegressionTable=data.frame(species=species,part=part,n=n,reg.type=reg.type,reg.order=reg.order,intercept=intercept,slope=slope)

print(xtable(RegressionTable, include.rownames=FALSE), floating=FALSE, type="latex", file="output/docs/RegressionTable.tex")

path <- "output/partWeights"
if(!file.exists(path))
  dir.create(path, recursive=TRUE)

write.csv(RegressionTable,file=file.path(path, "RegressionTable.csv"), quote=FALSE, row.names=FALSE)
}
