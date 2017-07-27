
remake::make("export")

SummaryCombine <- readRDS("export/SummaryCombine.rds")
SummarySpp <- readRDS("export/SummarySpp.rds")
SummaryInd <- readRDS("export/SummaryInd.rds")
SummarySppAge <- readRDS("export/SummarySppAge.rds")

HarvestData <- readRDS("export/HarvestData.rds")
InvestmentByPart <- readRDS("export/Investment_FD_all.rds")
PartsSummary <- readRDS("export/PartsSummary_all.rds")

source("R/figures.R")


############################################
#####  Supp - Repro inv by age  ############
############################################

###Shows that for many species, reproductive investment increases 1:1 with total leaf production (replacement + expansion); 
###since pollen attraction costs are so big and flowers at regular intervals along leaf nodes, it makes sense that surplus energy is not what is being
###divided in these species


win.metafile("ms/RA/Figure_repro_inv_by_leaf_inv.wmf", height=16, width=16)
par(mfrow=c(4,4), cex=1, omi=c(.5,.6,.2,.02), mai=c(0.2,.65,.01,0.01)) 

data <- SummaryInd

data <- split(data, data$species)


for(spp in c("BOLE","PILI","HEPU","GRSP","GRBU","COER","XXXX","XXXX","EPMI","LEES","PHPH","PUTU","HATE","BAER","PELA","PEPU")) {
  
  if(spp == "XXXX") {
    plot(1,1, type="n", axes=FALSE, ann=FALSE)
  } else {
    
    y_max <-max(data[[spp]]$repro_inv)
    y_min <-min(subset(data[[spp]]$repro_inv,data[[spp]]$repro_inv>0))
    x_max <-max(subset(data[[spp]]$leaf_inv_gross,!is.na(data[[spp]]$leaf_inv_gross)))
    x_min <-min(subset(data[[spp]]$leaf_inv_gross,data[[spp]]$leaf_inv_gross>0))
    
    plot(repro_inv~leaf_inv_gross,data[[spp]],pch=16,log="xy",xlim=c(x_min*0.5,x_max*2),ylim=c(y_min*0.5,y_max*2),col=col.age(age),xaxt="n",ylab="n",las=1,xaxs="i",yaxs="i")
    
    if(spp=="BAER"|spp=="PEPU"|spp=="PELA"|spp=="HATE") {
      axis(1, at=c(2,5,10,20), labels=c(2,5,10,20),cex.axis=1,las=1)
    }
    
    mtext(labels.spp.full(spp),font=3,outer=FALSE,side=3,adj=0)
    abline(0,1)
  }
}
mtext("all leaf investment (mg)",side=1,outer=TRUE,line=1)
mtext("reproductive investment (mg)",side=2,outer=TRUE,line=1.75)

dev.off()

win.metafile("ms/RA/Figure_ovules_by_leaf_inv.wmf", height=16, width=16)
par(mfrow=c(4,4), cex=1, omi=c(.5,.6,.2,.02), mai=c(0.6,.65,.01,0.01)) 

data <- SummaryInd

data <- split(data, data$species)


for(spp in c("BOLE","PILI","HEPU","GRSP","GRBU","COER","XXXX","XXXX","EPMI","LEES","PHPH","PUTU","HATE","BAER","PELA","PEPU")) {
  
  if(spp == "XXXX") {
    plot(1,1, type="n", axes=FALSE, ann=FALSE)
  } else {
    
    y_max <-max(data[[spp]]$ovule_count)
    y_min <-min(subset(data[[spp]]$ovule_count,data[[spp]]$ovule_count>0))
    x_max <-max(subset(data[[spp]]$all_leaf_inv,!is.na(data[[spp]]$all_leaf_inv)))
    x_min <-min(subset(data[[spp]]$all_leaf_inv,data[[spp]]$all_leaf_inv>0))
    
    plot(ovule_count~all_leaf_inv,data[[spp]],pch=16,log="xy",xlim=c(x_min*0.5,x_max*2),ylim=c(y_min*0.5,y_max*2),col=col.age(age),ylab="n",las=1,xaxs="i",yaxs="i")
    
    if(spp=="BAER"|spp=="PEPU"|spp=="PELA"|spp=="HATE") {
      axis(1, at=c(2,5,10,20), labels=c(2,5,10,20),cex.axis=1,las=1)
    }
    
    mtext(labels.spp.full(spp),font=3,outer=FALSE,side=3,adj=0)
  }
}
mtext("all leaf investment (mg)",side=1,outer=TRUE,line=1)
mtext("ovules initiated (count)",side=2,outer=TRUE,line=1.75)

dev.off()
