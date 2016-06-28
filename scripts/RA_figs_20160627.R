#Different species have different RA values. But biggest shift in RA is with age
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


par(mfrow=c(1,2), cex=1, omi=c(1.1,0.8,.1,.1), mai=c(0.3,.4,.1,0.2)) 
plot(repro_inv~growth_leaf_positive, SummaryInd, log="xy", col=col.spp(species),pch=16,xlim=c(1,1000000),ylim=c(1,1000000),xlab="",ylab="")
legend("bottomright",legend=labels.spp(),col=col.spp(),pch=16, cex=.8,bty="n")
abline(0,1)
text(2.1,3,srt=45,"RA = 0.5")
abline(log10(3),1)
text(2,9,srt=45,"RA = 0.75")
abline(log10(9),1)
text(2,25,srt=45,"RA = 0.9")
abline(log10(19),1)
text(2,60,srt=45,"RA = 0.95")
abline(log10(.111),1)
text(19,3,srt=45,"RA = 0.1")
abline(log10(.010101),1)
text(170,2.4,srt=45,"RA = 0.01")
mtext("reproductive investment (mg)",2,line=1,outer=TRUE)

plot(repro_inv~growth_leaf_positive, SummaryInd, log="xy", col=col.age(age),pch=16,xlim=c(1,1000000),ylim=c(1,1000000),xlab="",ylab="")
legend("bottomright",legend=labels.age(),col=col.age(),pch=16, cex=.8,bty="n")
abline(0,1)
text(2.1,3,srt=45,"RA = 0.5")
abline(log10(3),1)
text(2,9,srt=45,"RA = 0.75")
abline(log10(9),1)
text(2,25,srt=45,"RA = 0.9")
abline(log10(19),1)
text(2,60,srt=45,"RA = 0.95")
abline(log10(.111),1)
text(19,3,srt=45,"RA = 0.1")
abline(log10(.010101),1)
text(170,2.4,srt=45,"RA = 0.01")
mtext("leaf growth, scaled to be positive (mg)",1,line=2,outer=TRUE)
```

RA by age
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
  axis(2, at=c(-4,-2,0,2,4), labels=c(-1000,-10,0,10,1000),cex.axis=.8,las=1)
  plot(RA_leaf_area~age,data2[[spp]],col=col.age(data2[[spp]]$age),pch=16,xlab="",ylab="",log="x",xlim=c(1,32))
  mtext("RA (leaf area)", 2, outer=FALSE,cex=.7,line=2)
  if (spp=="HATE"|spp=="PUTU") {
    mtext("plant weight (mg)                                                                                                                   age", 1, outer=TRUE,cex=.7,line=1.8)
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

plot(growth_inv~growth_stem,SummaryInd,log="",pch=16,col=col.spp(species))
check <- subset(SummaryInd,growth_inv<0)
View(check)
abline(0,1)
