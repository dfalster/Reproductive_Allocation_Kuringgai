###RV curves by species
```{r, echo=FALSE,message=FALSE, warning=FALSE}
par(mfrow=c(4,4), cex=1, omi=c(.7,.7,.1,.1), mai=c(.7,.9,.4,0.2)) 
yvar <- "ReproInv"
xvar <- "total_weight"

data <- subset(SummaryInd,RA>0)
data <- split(data, data$species)
lmfits <- lapply(data, function(x) lm(log(x[[yvar]]) ~  (log(x[[xvar]]))))
results <- summarise_all_fits(lmfits)

results$r.squared <- round(results$r.squared,digits=3)
results$p.value <- round(results$p.value,digits=3)
results$a <- round(results$a,digits=2)
results$b <- round(results$b,digits=2)

results

for(spp in names(data)) {
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar,log="xy",
                    main=spp)
}

par(mfrow=c(4,4), cex=1, omi=c(.7,.7,.1,.1), mai=c(.7,.9,.4,0.2)) 
yvar <- "total_repro_inv"
xvar <- "total_weight"

data <- subset(SummaryInd,RA>0)
data <- split(data, data$species)
lmfits <- lapply(data, function(x) lm(log(x[[yvar]]) ~  (log(x[[xvar]]))))
results <- summarise_all_fits(lmfits)

results$r.squared <- round(results$r.squared,digits=3)
results$p.value <- round(results$p.value,digits=3)
results$a <- round(results$a,digits=2)
results$b <- round(results$b,digits=2)

results

for(spp in names(data)) {
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar,log="xy",
                    main=spp)
}


mtext("Plant size (mg)", 1, outer=TRUE,cex=1.2)
mtext("Reproductive investment (mg)", 2, outer=TRUE,cex=1.2)
```
###Vegetative growth by plant size
```{r, echo=FALSE,message=FALSE, warning=FALSE}
par(mfrow=c(4,4), cex=1, omi=c(.7,.7,.1,.1), mai=c(.7,.9,.4,0.2)) 
yvar <- "GrowthInv"
xvar <- "total_weight"

data <- subset(SummaryInd,RA>0)
data <- split(data, data$species)
lmfits <- lapply(data, function(x) lm(log(x[[yvar]]) ~  (log(x[[xvar]]))))
results <- summarise_all_fits(lmfits)

results$r.squared <- round(results$r.squared,digits=3)
results$p.value <- round(results$p.value,digits=3)
results$a <- round(results$a,digits=2)
results$b <- round(results$b,digits=2)

results

for(spp in names(data)) {
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar,log="xy",
                    main=spp)
}

mtext("Plant size (mg)", 1, outer=TRUE,cex=1.2)
mtext("Vegetative investment (mg)", 2, outer=TRUE,cex=1.2)
```

###RA schedule - against plant size
```{r, echo=FALSE,message=FALSE, warning=FALSE}
par(mfrow=c(4,4), cex=1, omi=c(.7,.7,.1,.1), mai=c(.7,.9,.4,0.2)) 
yvar <- "RA"
xvar <- "total_weight"

data <- subset(SummaryInd,RA>0)
data <- split(data, data$species)
lmfits <- lapply(data, function(x) lm(log(x[[yvar]]) ~  (log(x[[xvar]]))))
results <- summarise_all_fits(lmfits)

results$r.squared <- round(results$r.squared,digits=3)
results$p.value <- round(results$p.value,digits=3)
results$a <- round(results$a,digits=2)
results$b <- round(results$b,digits=2)

results

for(spp in names(data)) {
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar,log="xy",
                    main=spp)
}

mtext("Plant size (mg)", 1, outer=TRUE,cex=1.2)
mtext("RA", 2, outer=TRUE,cex=1.2)
```


###RA schedule - against plant age
```{r, echo=FALSE,message=FALSE, warning=FALSE}
par(mfrow=c(4,4), cex=1, omi=c(.7,.7,.1,.1), mai=c(.7,.9,.4,0.2)) 
yvar <- "RA"
xvar <- "age"

data <- subset(SummaryInd,RA>0)
data <- split(data, data$species)
lmfits <- lapply(data, function(x) lm(log(x[[yvar]]) ~  (log(x[[xvar]]))))
results <- summarise_all_fits(lmfits)

results$r.squared <- round(results$r.squared,digits=3)
results$p.value <- round(results$p.value,digits=3)
results$a <- round(results$a,digits=2)
results$b <- round(results$b,digits=2)

results

for(spp in names(data)) {
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar,log="xy",
                    main=spp)
}

mtext("Plant age (years)", 1, outer=TRUE,cex=1.2)
mtext("RA", 2, outer=TRUE,cex=1.2)
```


###"RV curve" using plant height
```{r, echo=FALSE,message=FALSE, warning=FALSE}
par(mfrow=c(4,4), cex=1, omi=c(.7,.7,.1,.1), mai=c(.7,.9,.4,0.2)) 
yvar <- "ReproInv"
xvar <- "height"

data <- subset(SummaryInd,RA>0)
data <- split(data, data$species)
lmfits <- lapply(data, function(x) lm(log(x[[yvar]]) ~  (log(x[[xvar]]))))
results <- summarise_all_fits(lmfits)

results$r.squared <- round(results$r.squared,digits=3)
results$p.value <- round(results$p.value,digits=3)
results$a <- round(results$a,digits=2)
results$b <- round(results$b,digits=2)

results

for(spp in names(data)) {
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar,log="xy",
                    main=spp)
}

mtext("Plant height (mm)", 1, outer=TRUE,cex=1.2)
mtext("Reproductive investment (mg)", 2, outer=TRUE,cex=1.2)
```

###Vegetative investment using plant height
```{r, echo=FALSE,message=FALSE, warning=FALSE}
par(mfrow=c(4,4), cex=1, omi=c(.7,.7,.1,.1), mai=c(.7,.9,.4,0.2)) 
yvar <- "GrowthInv"
xvar <- "height"

data <- subset(SummaryInd,RA>0)
data <- split(data, data$species)
lmfits <- lapply(data, function(x) lm(log(x[[yvar]]) ~  (log(x[[xvar]]))))
results <- summarise_all_fits(lmfits)

results$r.squared <- round(results$r.squared,digits=3)
results$p.value <- round(results$p.value,digits=3)
results$a <- round(results$a,digits=2)
results$b <- round(results$b,digits=2)

results

for(spp in names(data)) {
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar,log="xy",
                    main=spp)
}

mtext("Plant height (mm)", 1, outer=TRUE,cex=1.2)
mtext("Growth investment (mg)", 2, outer=TRUE,cex=1.2)
```



###RA by shoot extension
```{r, echo=FALSE,message=FALSE, warning=FALSE}
par(mfrow=c(4,4), cex=1, omi=c(.7,.7,.1,.1), mai=c(.7,.9,.4,0.2)) 
yvar <- "RA"
xvar <- "growth_shoot_diam"

data <- subset(SummaryInd,RA>0)
data <- split(data, data$species)
lmfits <- lapply(data, function(x) lm(log(x[[yvar]]) ~  (log(x[[xvar]]))))
results <- summarise_all_fits(lmfits)

results$r.squared <- round(results$r.squared,digits=3)
results$p.value <- round(results$p.value,digits=3)
results$a <- round(results$a,digits=2)
results$b <- round(results$b,digits=2)

results

for(spp in names(data)) {
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar,log="xy",
                    main=spp)
}

mtext("Shoot diameter growth", 1, outer=TRUE,cex=1.2)
mtext("RA", 2, outer=TRUE,cex=1.2)

par(mfrow=c(2,2), cex=1, omi=c(.4,.,.1,.1), mai=c(1,1,.4,0.2)) 
plot(RA~growth_shoot_diam,data=subset(SummaryInd,RA>0),log="xy", col=col.spp[as.factor(species)],pch=16)
mod <- lm(log(growth_shoot_diam)~asin(sqrt(RA)),data=subset(SummaryInd,RA>0))
words.bottom.left.logxy(mod)

plot(RA~new_length,data=subset(SummaryInd,RA>0),log="xy", col=col.spp[as.factor(species)],pch=16)
mod <- lm(log(new_length)~asin(sqrt(RA)),data=subset(SummaryInd,RA>0))
words.bottom.left.logxy(mod)

plot(RA~shoot_leaf_area_growth,data=subset(SummaryInd,RA>0),log="xy", col=col.spp[as.factor(species)],pch=16)
mod <- lm(log(shoot_leaf_area_growth)~asin(sqrt(RA)),data=subset(SummaryInd,RA>0&shoot_leaf_area_growth>0))
words.bottom.left.logxy(mod)


par(mfrow=c(4,4), cex=1, omi=c(.7,.7,.1,.1), mai=c(.7,.9,.4,0.2)) 
yvar <- "RA"
xvar <- "shoot_leaf_area_growth"

data <- subset(SummaryInd,RA>0)
data <- split(data, data$species)
lmfits <- lapply(data, function(x) lm(log(x[[yvar]]) ~  (log(x[[xvar]]))))
results <- summarise_all_fits(lmfits)

results$r.squared <- round(results$r.squared,digits=3)
results$p.value <- round(results$p.value,digits=3)
results$a <- round(results$a,digits=2)
results$b <- round(results$b,digits=2)

results

for(spp in names(data)) {
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar,log="xy",
                    main=spp)
}


par(mfrow=c(4,4), cex=1, omi=c(.7,.7,.1,.1), mai=c(.7,.9,.4,0.2)) 
yvar <- "RA"
xvar <- "new_length"

data <- subset(SummaryInd,RA>0)
data <- split(data, data$species)
lmfits <- lapply(data, function(x) lm(log(x[[yvar]]) ~  (log(x[[xvar]]))))
results <- summarise_all_fits(lmfits)

results$r.squared <- round(results$r.squared,digits=3)
results$p.value <- round(results$p.value,digits=3)
results$a <- round(results$a,digits=2)
results$b <- round(results$b,digits=2)

results

for(spp in names(data)) {
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar,log="xy",
                    main=spp)
}

```


