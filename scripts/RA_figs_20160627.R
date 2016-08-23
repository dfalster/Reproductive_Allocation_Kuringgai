pdf("figs2/RA/Figure_1_leaf_vs_repro.pdf", height=4.3, width=8)
plot
par(mfrow=c(1,2), cex=1, omi=c(1.1,0.8,.1,.1), mai=c(0.3,.4,.1,0.2)) 
plot(repro_inv_log~leaf_inv_log, subset(SummaryInd), log="", col=col.spp(species),pch=16,xlim=c(-4,6),ylim=c(-1,6),
     xlab="",ylab="",cex=0.8,xaxt="n",yaxt="n")
axis(1, at=c(-4,-2,0,2,4,6), labels=c(-10000,-100,0,100,10000,"1000000"),cex.axis=0.9)
axis(2, at=c(0,2,4,6), labels=c(0,100,10000,"1000000"),cex.axis=0.9)
legend("topleft",legend=labels.spp(),col=col.spp(),pch=16, cex=.6,bty="n")
abline(0,1)
text(5.6,5.8,srt=45,"RA = 0.5",cex=.6)
abline(log10(3),1)
text(5.13,5.8,srt=45,"RA = 0.75",cex=.6)
abline(log10(9),1)
text(4.67,5.8,srt=45,"RA = 0.9",cex=.6)
abline(log10(19),1)
text(4.33,5.8,srt=45,"RA = 0.95",cex=.6)
abline(log10(.111),1)
text(5.6,4.85,srt=45,"RA = 0.1",cex=.6)
abline(log10(.010101),1)
text(5.6,3.8,srt=45,"RA = 0.01",cex=.6)
mtext("reproductive investment (mg)",2,line=1,outer=TRUE)

plot(repro_inv_log~leaf_inv_log, subset(SummaryInd), log="", col=col.age(age),pch=16,xlim=c(-4,6),ylim=c(-1,6),
     xlab="",ylab="",cex=0.8,xaxt="n",yaxt="n")
axis(1, at=c(-4,-2,0,2,4,6), labels=c(-10000,-100,0,100,10000,"1000000"),cex.axis=0.9)
axis(2, at=c(0,2,4,6), labels=c(0,100,10000,"1000000"),cex.axis=0.9)
legend("topleft",legend=labels.age(),col=col.age,pch=16, cex=.6,bty="n")
abline(0,1)
text(5.6,5.8,srt=45,"RA = 0.5",cex=.6)
abline(log10(3),1)
text(5.13,5.8,srt=45,"RA = 0.75",cex=.6)
abline(log10(9),1)
text(4.67,5.8,srt=45,"RA = 0.9",cex=.6)
abline(log10(19),1)
text(4.33,5.8,srt=45,"RA = 0.95",cex=.6)
abline(log10(.111),1)
text(5.6,4.85,srt=45,"RA = 0.1",cex=.6)
abline(log10(.010101),1)
text(5.6,3.8,srt=45,"RA = 0.01",cex=.6)
mtext("leaf investment (mg)",1,line=2,outer=TRUE)
dev.off()
```



#Figure 1 Different species have different RA values. But biggest shift in RA is with age
```{r, echo=FALSE,message=FALSE,fig.width=14, fig.height=7.5}
if (min(SummaryInd$growth_leaf) <= 0) {
  SummaryInd$growth_leaf_positive = SummaryInd$growth_leaf - SummaryInd$growth_leaf_min + 1
} else {
  SummaryInd$growth_leaf_positive = SummaryInd$growth_leaf
}

SummaryInd$repro_inv_log <- 0
for (i in 1:length(SummaryInd$individual)) {
  if (SummaryInd$repro_inv[i] > 0) {
    SummaryInd$repro_inv_log[i] <- log10(SummaryInd$repro_inv[i])
  }else{
    SummaryInd$repro_inv_log[i] <- 0  
  }
}

SummaryInd$leaf_inv_log <- 0
for (i in 1:length(SummaryInd$individual)) {
  if (SummaryInd$growth_leaf[i] > 0) {
    SummaryInd$leaf_inv_log[i] <- log10(SummaryInd$growth_leaf[i])
  }else{
    SummaryInd$leaf_inv_log[i] <- -(log10(-SummaryInd$growth_leaf[i]))  
  }
}

pdf("figs2/RA/Figure_1_leaf_vs_repro.pdf", height=4.3, width=8)
plot
par(mfrow=c(1,2), cex=1, omi=c(1.1,0.8,.1,.1), mai=c(0.3,.4,.1,0.2)) 
plot(repro_inv~growth_leaf_positive, subset(SummaryInd,growth_leaf_positive>1), log="xy", col=col.spp(species),pch=16,xlim=c(1,1000000),ylim=c(1,1000000),
     xlab="",ylab="",yaxt="n",xaxt="n",cex=0.8)
axis(1, at=c(1,100,10000,1000000), labels=c(1,100,10000,"1000000"),cex.axis=0.9)
axis(2, at=c(1,100,10000,1000000), labels=c(1,100,10000,"1000000"),cex.axis=0.9)
legend("topleft",legend=labels.spp(),col=col.spp(),pch=16, cex=.6,bty="n")
abline(0,1)
text(10^5.6,10^5.8,srt=45,"RA = 0.5",cex=.6)
abline(log10(3),1)
text(10^5.13,10^5.8,srt=45,"RA = 0.75",cex=.6)
abline(log10(9),1)
text(10^4.67,10^5.8,srt=45,"RA = 0.9",cex=.6)
abline(log10(19),1)
text(10^4.33,10^5.8,srt=45,"RA = 0.95",cex=.6)
abline(log10(.111),1)
text(10^5.6,10^4.85,srt=45,"RA = 0.1",cex=.6)
abline(log10(.010101),1)
text(10^5.6,10^3.8,srt=45,"RA = 0.01",cex=.6)
mtext("reproductive investment (mg)",2,line=1,outer=TRUE)

plot(repro_inv~growth_leaf_positive, subset(SummaryInd,growth_leaf_positive>1), log="xy", col=col.age(age),pch=16,xlim=c(1,1000000),ylim=c(1,1000000),
     xlab="",ylab="",yaxt="n",xaxt="n",cex=0.8)
axis(1, at=c(1,100,10000,1000000), labels=c(1,100,10000,"1000000"),cex.axis=0.9)
axis(2, at=c(1,100,10000,1000000), labels=c(1,100,10000,"1000000"),cex.axis=0.9)
legend("bottomright",legend=labels.age(),col=col.age(),pch=16, cex=.6,bty="n")
abline(0,1)
text(10^5.6,10^5.8,srt=45,"RA = 0.5",cex=.6)
abline(log10(3),1)
text(10^5.13,10^5.8,srt=45,"RA = 0.75",cex=.6)
abline(log10(9),1)
text(10^4.67,10^5.8,srt=45,"RA = 0.9",cex=.6)
abline(log10(19),1)
text(10^4.33,10^5.8,srt=45,"RA = 0.95",cex=.6)
abline(log10(.111),1)
text(10^5.6,10^4.85,srt=45,"RA = 0.1",cex=.6)
abline(log10(.010101),1)
text(10^5.6,10^3.8,srt=45,"RA = 0.01",cex=.6)
mtext("leaf investment, scaled to be positive (mg)",1,line=2,outer=TRUE)
dev.off()
```

#Figure 3 leaf area declines with age in some species, plateaus in others, and in a few continues to increase
```{r}
pdf("figs2/RA/Figure_3_leaf_area_vs_age.pdf", height=11, width=8)
plot
par(mfrow=c(4,4), cex=1, omi=c(.7,.8,.1,.1), mai=c(.3,.4,.4,0.2)) 
xvar <- "age"
yvar <- "leaf_area"

data <- subset(SummaryInd)
data <- split(data, data$species)
data2 <- subset(SummarySppAge[["mean"]])
data2 <- split(data2, data2$species)

for(spp in names(data)) {
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar,log="xy",xlab=xvar,ylab=yvar,cex=1,col=col.age(data[[spp]]$age),xlim=c(1,35))
  points(leaf_area~age,data2[[spp]],cex=2.5,col=col.age(data2[[spp]]$age),pch="_")
  mtext(labels.spp.full(spp),3, outer=FALSE,font=3)
  if(spp=="BOLE"|spp=="COER"|spp=="GRSP"|spp=="HEPU") {
    rect(14,10^par("usr")[3],10^par("usr")[2],10^par("usr")[4],col="light grey",density=NA)
    rect(10^par("usr")[1],10^par("usr")[3],10^par("usr")[2],10^par("usr")[4],col="black",density=0)
  }
  if(spp=="PILI") {
    rect(10,10^par("usr")[3],10^par("usr")[2],10^par("usr")[4],col="light grey",density=NA)
    rect(10^par("usr")[1],10^par("usr")[3],10^par("usr")[2],10^par("usr")[4],col="black",density=0)
  }
}
mtext("leaf area (mm^2)",outer=TRUE,2,line=1.5)
mtext("age (years)",outer=TRUE,1,line=1.5)
dev.off()
```
par("usr")

if(names(data)=="BOLE") {
  rect(15,10^par("usr")[3],10^par("usr")[2],10^par("usr")[4],col="light grey",density=NA)
  rect(10^par("usr")[1],10^par("usr")[3],10^par("usr")[2],10^par("usr")[4],col="black",density=0)
}

#RA by age
```{r}
par(mfrow=c(4,4), cex=1, omi=c(.7,.2,.1,.1), mai=c(.3,.3,.4,0.2)) 
yvar <- "RA_leaf_area"
xvar <- "age"

data <- subset(SummaryInd,RA_leaf_area<4)
data <- split(data, data$species)
data2 <- subset(SummarySppAge[["mean"]],age!=5)
data2 <- split(data2,data2$species)
lmfits <- lapply(data, function(x) lm((x[[yvar]]) ~  ((x[[xvar]]))))
results <- summarise_all_fits(lmfits)

results$r.squared <- round(results$r.squared,digits=3)
results$p.value <- round(results$p.value,digits=3)
results$a <- round(results$a,digits=2)
results$b <- round(results$b,digits=2)

for(spp in names(data)) {
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar,log="x",xlab="",ylab="",cex=1, 
                    main=spp,col=col.age(data[[spp]]$age),xlim=c(1,32))
  points(data2[[spp]]$RA_leaf_area~data2[[spp]]$age,type="l")
}

mtext("RA", 2, outer=TRUE,cex=1.2)
mtext("age", 1, outer=TRUE,cex=1.2)
```

#RV curves
```{r}
par(mfrow=c(4,4), cex=1, omi=c(.7,.8,.1,.1), mai=c(.3,.4,.4,0.2)) 

yvar <- "repro_inv"
yvar2 <- "RA_leaf_area"
yvar3 <- "leaf_growth"
xvar <- "total_weight_0"

data <- subset(SummaryInd)
data <- split(data, data$species)

data2 <- subset(SummarySppAge[["mean"]])
data2 <- split(data2, data2$species)

for(spp in names(data)) {
  
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar,log="xy",xlab="",ylab="",cex=.8,
                    col=col.age(data[[spp]]$age),main=labels.spp.full(spp),cex.main=0.9)
}
mtext("reproductive investment (mg)", 2, outer=TRUE,cex=1,line=1.8)
mtext("weight", 1, outer=TRUE,cex=1,line=0.5)
```

#paired plots showing reproductive vs leaf investment across plant weights together with RA vs age. 
#Shows clearly how there is a crossover in leaf vs reproductive investment for all species, but magnitude of effect is very different
```{r}
pdf("figs2/RA/leaf_vs_repro.pdf", height=11, width=8)
plot
par(mfrow=c(7,2), cex=1, omi=c(.7,.1,.1,.1), mai=c(.25,.7,.4,0.2)) 

yvar <- "leaf_inv_log"
xvar <- "total_weight_0"

data <- subset(SummaryInd)
data <- split(data, data$species)

data2 <- subset(SummaryInd,RA_leaf_area>-2&RA_leaf_area<3)
data2 <- split(data2, data2$species)

for(spp in names(data)) {
  
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar,log="x",xlab="",ylab="",cex=.8,ylim=c(-4,5),
                    col=col.age(data[[spp]]$age),cex.main=0.9,yaxt="n")
  mtext("investment (mg)", 2, outer=FALSE,cex=.7,line=2.8)
  points(repro_inv_log~total_weight_0,data[[spp]],pch=1,cex=1.2,col=col.age(data[[spp]]$age))
  mtext(labels.spp.full(spp),3, outer=FALSE,font=3)
  #arrows(data[[spp]]$total_weight_0,data[[spp]]$leaf_inv_log,data[[spp]]$total_weight_0,data[[spp]]$repro_inv_log,lwd=1,length=.08,col=col.age(data[[spp]]$age))
  abline(0,0)
  axis(2, at=c(-4,-2,0,2,4), labels=c(-10000,-100,0,100,10000),cex.axis=.8,las=1)
  plot(RA_leaf_area~age,data2[[spp]],col=col.age(data2[[spp]]$age),pch=16,xlab="",ylab="",log="x",xlim=c(1,32))
  mtext("RA (leaf area)", 2, outer=FALSE,cex=.7,line=2)
  if (spp=="HATE"|spp=="PUTU") {
    mtext("plant weight (mg)")                                                                                                                   age", 1, outer=TRUE,cex=.7,line=1.8)
  } else {}
  #plot(repro_inv~leaf_weight_0,data[[spp]],col=col.age(data2[[spp]]$age),pch=16,log="xy")
  #mtext("reproductive investment (mg)", 2, outer=FALSE,cex=.7,line=1.8)
  #plot(repro_inv~total_weight_0,data[[spp]],col=col.age(data2[[spp]]$age),pch=16,log="xy")
}
dev.off()
```

```{r}
par(mfrow=c(4,3), cex=1, omi=c(.7,.8,.1,.1), mai=c(.3,.4,.4,0.2)) 
xvar <- "leaf_area_0"
xvar2 <- "total_weight_0"
xvar3 <- "diameter_0"
yvar <- "repro_inv"

data <- subset(SummaryInd)
data <- split(data, data$species)
lmfits <- lapply(data, function(x) lm((x[[yvar]]) ~  ((x[[xvar]]))))
results <- summarise_all_fits(lmfits)

results$r.squared <- round(results$r.squared,digits=3)
results$p.value <- round(results$p.value,digits=3)
results$a <- round(results$a,digits=2)
results$b <- round(results$b,digits=2)

for(spp in names(data)) {
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar,log="xy",xlab=xvar,ylab=yvar,cex=1,col=col.age(data[[spp]]$age))
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar2,log="xy",xlab=xvar2,ylab="",cex=1,main=spp,col=col.age(data[[spp]]$age),yaxt="n")
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar3,log="xy",xlab=xvar3,ylab="",cex=1,col=col.age(data[[spp]]$age),yaxt="n")
}


if (min(SummaryInd$growth_leaf) <= 0) {
  SummaryInd$growth_leaf_positive = SummaryInd$growth_leaf - SummaryInd$growth_leaf_min + 1
} else {
  SummaryInd$growth_leaf_positive = SummaryInd$growth_leaf
}
```

```{r}
pdf("figs2/RA/leaf_area_age.pdf", height=11, width=8)
plot
par(mfcol=c(7,3), cex=1, omi=c(.1,.4,.01,.01), mai=c(.1,.15,.1,0.1)) 
yvar <- "repro_inv"
xvar <- "age"

data <- subset(SummaryInd,species=="BAER"|species=="BOLE"|species=="COER"|species=="EPMI"|species=="GRBU"|species=="GRSP"|species=="HATE")
data <- split(data, data$species)

data2 <- subset(SummarySppAge[["mean"]])
data2 <- split(data2, data2$species)

for(spp in names(data)) {
  
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar,log="xy",xlab="",ylab="",xlim=c(1,33),cex=.8,yaxt="n",xaxt="n",
                    col=col.age(data[[spp]]$age),main=labels.spp.full(spp),cex.main=0.9)
}
mtext("reproductive investment", 2, outer=TRUE,cex=1,line=1.8)
mtext("age (years)", 1, outer=FALSE,cex=1,line=0.5)

yvar <- "growth_leaf_positive"
xvar <- "age"

data <- subset(SummaryInd,species=="BAER"|species=="BOLE"|species=="COER"|species=="EPMI"|species=="GRBU"|species=="GRSP"|species=="HATE")
data <- split(data, data$species)

data2 <- subset(SummarySppAge[["mean"]])
data2 <- split(data2, data2$species)

for(spp in names(data)) {
  
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar,log="xy",xlab="",ylab="",xlim=c(1,33),cex=.8,yaxt="n",xaxt="n",
                    col=col.age(data[[spp]]$age),main=labels.spp.full(spp),cex.main=0.9)
  if (min(data[[spp]]$growth_leaf_min) < 0 ) {
  abline(log10(-(min(data[[spp]]$growth_leaf_min))),0)
  } else {}
}
mtext("leaf investment", 2, outer=FALSE,cex=1,line=0)
mtext("age (years)", 1, outer=FALSE,cex=1,line=0.5)

yvar <- "leaf_area"
xvar <- "age"

data <- subset(SummaryInd,species=="BAER"|species=="BOLE"|species=="COER"|species=="EPMI"|species=="GRBU"|species=="GRSP"|species=="HATE")
data <- split(data, data$species)

data2 <- subset(SummarySppAge[["mean"]])
data2 <- split(data2, data2$species)

for(spp in names(data)) {
  
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar,log="xy",xlab="",ylab="",xlim=c(1,33),cex=.8,yaxt="n",xaxt="n",
                    col=col.age(data[[spp]]$age),main=labels.spp.full(spp),cex.main=0.9)
}
mtext("leaf weight", 2, outer=FALSE,cex=1,line=0)
mtext("age (years)", 1, outer=FALSE,cex=1,line=0.5)
dev.off()
```

#predicting growth from leaf area versus plant weight
#using leaf area at the start of the year is a better predictor of total investment than is total plant weight
```{r}
par(mfrow=c(7,2), cex=1, omi=c(.7,.8,.1,.1), mai=c(.3,.4,.4,0.2)) 
xvar <- "leaf_area_0"
xvar2 <- "total_weight_0"
yvar <- "leaf_repro_inv"

data <- subset(SummaryInd)
data <- split(data, data$species)
lmfits <- lapply(data, function(x) lm((x[[yvar]]) ~  ((x[[xvar]]))))
results <- summarise_all_fits(lmfits)

results$r.squared <- round(results$r.squared,digits=3)
results$p.value <- round(results$p.value,digits=3)
results$a <- round(results$a,digits=2)
results$b <- round(results$b,digits=2)

data <- subset(SummaryInd)
data <- split(data, data$species)
lmfits <- lapply(data, function(x) lm((x[[yvar]]) ~  ((x[[xvar2]]))))
results2 <- summarise_all_fits(lmfits)

results2$r.squared <- round(results2$r.squared,digits=3)
results2$p.value <- round(results2$p.value,digits=3)
results2$a <- round(results2$a,digits=2)
results2$b <- round(results2$b,digits=2)



for(spp in names(data)) {
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar,log="xy",xlab=xvar,ylab=yvar,cex=1,col=col.age(data[[spp]]$age))
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar2,log="xy",xlab=xvar2,ylab="",cex=1,main=spp,col=col.age(data[[spp]]$age),yaxt="n")
}

#Leaf area is a better predictor of the subsequent years growth than is total weight. 
#result could be quite different if stem weight, even part of it were included
par(mfrow=c(2,2), cex=1, omi=c(.1,.1,.1,.1), mai=c(.9,.9,.4,0.2)) 
compare <- select(results, species, r.squared)
compare2 <- select(results2, r.squared)
compare <- cbind(compare, compare2)
names(compare) <- c("species","leaf_area","total_weight")
plot(leaf_area~total_weight,compare,col=col.spp(species),pch=16)
abline(0,1)

plot(leaf_repro_inv~leaf_area_0,SummaryInd,col=col.age(age),pch=16,log="xy")
abline(0,1)
plot(leaf_repro_inv~total_weight_0,SummaryInd,col=col.age(age),pch=16,log="xy")
abline(0,1)
```

```{r}
par(mfrow=c(4,4), cex=1, omi=c(.7,.2,.1,.1), mai=c(.3,.3,.4,0.2)) 
data <- subset(SummaryInd,growth_leaf_positive>1)
data <- split(data, data$species)

for(spp in names(data)) {
plot(growth_height~growth_leaf_positive,data[[spp]],log="xy",pch=16,col=col.age(age),main=labels.spp.full(spp),cex.main=0.9)
}

par(mfrow=c(4,4), cex=1, omi=c(.7,.2,.1,.1), mai=c(.3,.3,.4,0.2)) 
data <- subset(SummaryInd,growth_leaf_positive>1)
data <- split(data, data$species)

for(spp in names(data)) {
  plot(shoot_growth_leaf_area~age,data[[spp]],log="xy",pch=16,col=col.age(age),main=labels.spp.full(spp),cex.main=0.9)
}


par(mfrow=c(4,4), cex=1, omi=c(.7,.2,.1,.1), mai=c(.3,.3,.4,0.2)) 
data <- subset(SummaryInd)
data <- split(data, data$species)

for(spp in names(data)) {
  plot(growth_shoot_length~shoot_growth_leaf_area,data[[spp]],log="xy",pch=16,col=col.age(age),main=labels.spp.full(spp),cex.main=0.9)
}