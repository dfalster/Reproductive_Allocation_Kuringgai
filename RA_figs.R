##Summary  


##1. RA schedules, RV curves, and VV curves across all species




##1. RA shifts enormously across species  
* There is also an age effect, with increasing RA with age

```{r, echo=FALSE,message=FALSE,fig.width=14, fig.height=7.5}
par(mfrow=c(1,2), cex=1, omi=c(.1,.1,.1,.1), mai=c(1.1,1.1,.1,0.2)) 
mod <- lm(log(growth_inv)~log(repro_inv),data=subset(SummaryInd,repro_inv>0))
plot((1+repro_inv)~growth_inv, SummaryInd, log="xy", col=col.spp(species),pch=16,xlim=c(1,1000000),ylim=c(1,1000000))
words.top.left.logxy(mod)
legend("bottomright",legend=labels.spp(),col=col.spp,pch=16, cex=.8,bty="n")
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

plot((1+repro_inv)~growth_inv, SummaryInd, log="xy", col=col.age(age),pch=16,xlim=c(1,1000000),ylim=c(1,1000000))
legend("bottomright",legend=labels.age(),col=col.age,pch=16, cex=.8,bty="n")
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
```

##2. RA, RV, and "Veg investment by size" (VV) curves for each species  
* These plots show a lot of scatter...I think it is real  
* Interestingly, different species have a "good" RV vs VV curve  
    * BOLE, COER, EPMI, GRSP, HATE, LEES, sort-of PHPH show consistent increase in reproductive investment with increasing size
    * BAER, GRSP, HATE, HEPU, PEPU, PHPH, PUTU, and sort-of BOLE, COER, EPMI show a consistent increase in vegetative investment with size
* Not surprisingly, COER, EPMI and HATE which have some of the best regressions for both RV and VV-curves have the cleanest RA schedules
* if VV-curves plotted including non-reproducing individuals, get beautiful curves - wonderfully consistent when young, but becoming scattered once reproducing
    ***it would be nice to plot the VV-curves, with different colors for reproducing and non-reproducing individuals***

###RA curves by species
```{r, echo=FALSE,message=FALSE,warning=FALSE, fig.width=20, fig.height=20}
par(mfrow=c(4,4), cex=1, omi=c(.7,.7,.1,.1), mai=c(.7,.9,.4,0.2)) 
yvar <- "RA"
xvar <- "total_weight"

data <- subset(SummaryInd,RA>0)
data <- split(data, data$species)
lmfits <- lapply(data, function(x) lm(asin(sqrt(x[[yvar]])) ~  (log(x[[xvar]]))))
results <- summarise_all_fits(lmfits)

results$r.squared <- round(results$r.squared,digits=3)
results$p.value <- round(results$p.value,digits=3)
results$a <- round(results$a,digits=2)
results$b <- round(results$b,digits=2)

for(spp in names(data)) {
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar,log="xy",
                    main=spp,col=col.age(data[[spp]]$age)
}

mtext("RA", 2, outer=TRUE,cex=1.2)
mtext("Total plant weight (mg)", 1, outer=TRUE,cex=1.2)
```

###RV curves by species
```{r, echo=FALSE,message=FALSE,warning=FALSE, fig.width=20, fig.height=20}
par(mfrow=c(4,4), cex=1, omi=c(.7,.7,.1,.1), mai=c(.7,.9,.4,0.2)) 
yvar <- "repro_inv"
xvar <- "total_weight"

data <- subset(SummaryInd,RA>0)
data <- split(data, data$species)
lmfits <- lapply(data, function(x) lm(log(x[[yvar]]) ~  (log(x[[xvar]]))))
results <- summarise_all_fits(lmfits)

results$r.squared <- round(results$r.squared,digits=3)
results$p.value <- round(results$p.value,digits=3)
results$a <- round(results$a,digits=2)
results$b <- round(results$b,digits=2)

for(spp in names(data)) {
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar,log="xy",
                    main=spp,col=col.age(data[[spp]]$age)
}

mtext("Reproductive investment (mg)", 2, outer=TRUE,cex=1.2)
mtext("Total plant weight (mg)", 1, outer=TRUE,cex=1.2)
```

###Vegetative invesment by size curves by species, only including reproducing individuals  
* colored by age, but legend not correct, because "age 1" color used for youngest age with reproducing individuals
```{r, echo=FALSE,message=FALSE,warning=FALSE, fig.width=14, fig.height=7.5}
par(mfrow=c(4,4), cex=1, omi=c(.7,.7,.1,.1), mai=c(.7,.9,.4,0.2)) 
yvar <- "growth_inv"
xvar <- "total_weight"

data <- subset(SummaryInd, RA>0)
data <- split(data, data$species)
lmfits <- lapply(data, function(x) lm(log(x[[yvar]]) ~  (log(x[[xvar]]))))
results <- summarise_all_fits(lmfits)

results$r.squared <- round(results$r.squared,digits=3)
results$p.value <- round(results$p.value,digits=3)
results$a <- round(results$a,digits=2)
results$b <- round(results$b,digits=2)

for(spp in names(data)) {
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar,log="xy",
                    main=spp,col=col.age(data[[spp]]$age)
}

mtext("Vegetative investment (mg)", 2, outer=TRUE,cex=1.2)
mtext("Total plant weight (mg)", 1, outer=TRUE,cex=1.2)
```
###Vegetative invesment by size curves by species, including all individuls
* 
```{r, echo=FALSE,message=FALSE,warning=FALSE, fig.width=14, fig.height=7.5}
par(mfrow=c(4,4), cex=1, omi=c(.7,.7,.1,.1), mai=c(.7,.9,.4,0.2)) 
yvar <- "growth_inv"
xvar <- "total_weight"

data <- subset(SummaryInd)
data <- split(data, data$species)
lmfits <- lapply(data, function(x) lm(log(x[[yvar]]) ~  (log(x[[xvar]]))))
results <- summarise_all_fits(lmfits)

results$r.squared <- round(results$r.squared,digits=3)
results$p.value <- round(results$p.value,digits=3)
results$a <- round(results$a,digits=2)
results$b <- round(results$b,digits=2)

for(spp in names(data)) {
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar,log="xy",
                    main=spp,col=col.age(data[[spp]]$age)
}

mtext("Vegetative investment (mg)", 2, outer=TRUE,cex=1.2)
mtext("Total plant weight (mg)", 1, outer=TRUE,cex=1.2)
```

##3. RA is a population level parameter, so noise expected across individuals
* None of these species is known to mast, but of course environmental variation year-to-year
* Within a population, some individuals invest more or less in repro in a given year, but overall population should change with time
* This indicates it is informative to consider site level means - which would show an increase in RA with age in many species
  
```{r, echo=FALSE,message=FALSE, warning=FALSE}
par(mfrow=c(4,4), cex=1, omi=c(.7,.7,.1,.1), mai=c(.7,.9,.4,0.2)) 
yvar <- "RA"
xvar <- "age"

data <- subset(SummaryInd,RA>0)
data <- split(data, data$species)

for(spp in names(data)) {
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar,log="x",
                    main=spp,xlim=c(1,35))
}

mtext("RA", 1, outer=TRUE,cex=1.2)
mtext("age", 2, outer=TRUE,cex=1.2)
```

