#Growth rates increase with increasing LMA in adult plants
But there is no relationship in seedlings (age=1.4) or saplings (age=2.4)
This is not predicted by Daniel's model or Anais' review
```{r, echo=FALSE,results='hide', message=FALSE}
plot(RGR_mean ~ LL_birth_mean,data=subset(SummarySppAge,age==1.4),log="xy",col=col.age[as.factor(age)],pch=16,cex=1.2,xlab="leaf lifespan (leaf births)",ylab="relative growth rate (per yr)",main="Seedlings")
text(RGR_mean ~ LL_birth_mean, data=subset(SummarySppAge,age==1.4), labels=species, pos=1, offset=-1.1, cex=0.9,col="blue")

plot(RGR_mean ~ LL_bd_mean,data=subset(SummarySppAge,age==2.4),log="xy",col="pink",pch=16,cex=1.2,xlab="leaf lifespan (leaf births)",ylab="relative growth rate (per yr)",main="Saplings")
text(RGR_mean ~ LL_bd_mean, data=subset(SummarySppAge,age==2.4), labels=species, pos=1, offset=-1.1, cex=0.9,col="blue")

plot(RGR_mean ~ LL_birth_mean,data=subset(SummarySppAge,age>3),log="xy",col=col.age3[as.factor(age)],pch=16,cex=1.2,xlab="leaf lifespan (leaf births)",ylab="relative growth rate (per yr)",main="Adults")
text(RGR_mean ~ LL_birth_mean, data=subset(SummarySppAge,age>3), labels=species, pos=1, offset=-1.1, cex=0.6,col="blue")
legend("bottomright",col=col.age,labels.age,pch=16)

#looking at individuals, not site means
plot(RGR ~ LL_birth,data=subset(SummaryInd,age==1.4),log="xy",col=col.age[as.factor(age)],pch=16,cex=1.2,xlab="leaf lifespan (leaf births)",ylab="relative growth rate (per yr)",main="Seedlings (looking at individuals)")
plot(RGR ~ LL_birth,data=subset(SummaryInd,age==2.4),log="xy",col=col.age[as.factor(age)],pch=16,cex=1.2,xlab="leaf lifespan (leaf births)",ylab="relative growth rate (per yr)",main="Saplings (looking at individuals)")
plot(RGR ~ LL_birth,data=subset(SummaryInd,age>3),log="xy",col=col.age3[as.factor(age)],pch=16,cex=1.2,xlab="leaf lifespan (leaf births)",ylab="relative growth rate (per yr)",main="Adults (looking at individuals)")


```

#Species with greater wood density have higher growth rates
Again there is no relationship in younger plants
```{r, echo=FALSE,results='hide', message=FALSE}
plot(RGR_mean ~ WD_mean,data=subset(SummarySppAge,age>3),log="xy",col=col.age3[as.factor(age)],pch=16,cex=1,xlab="wood density",ylab="RGR (per year)",main="Relationship between wood density and growth rates")
text(RGR_mean ~ WD_mean, data=subset(SummarySppAge,age==5), labels=species, pos=2, offset=-2.4, cex=0.9,col="blue")
legend("bottomright",col=col.age, labels.age, pch=16, cex=1)
```

#Species with larger seeds have higher growth rates
The same basic relationship exists across all ages, so combined all data here
```{r, echo=FALSE,results='hide', message=FALSE}
plot(RGR_mean ~ seed_size,data=subset(SummarySppAge,age>.5),log="xy",col=col.age[as.factor(age)],pch=16,cex=1,xlab="seed size (mg)",ylab="RGR (per year)",main="Effect of seed size on growth rates")
legend("topleft",col=col.age, labels.age, pch=16, cex=1)
text(RGR_mean ~ seed_size, data=subset(SummarySppAge,age==9), labels=species, pos=2, offset=-2.4, cex=0.9,col="blue")
```

#Species with greater maximum height have higher growth rates
For younger plants there is no relationship with *SPECIES* overall maximum H, but there is a positive correlation with the maximum height the species reaches at a given age -i.e. fast growing young plants are tall when young.
```{r, echo=FALSE,results='hide', message=FALSE}
plot(RGR_mean ~ maxH_spp,data=subset(SummarySppAge,age>3),log="xy",col=col.age3[as.factor(age)],pch=16,cex=1.2,xlab="maximum height (mm)",ylab="RGR (per year)",xlim=c(500,3200),main="Relationship between growth rates on maximum H")
legend("bottomright",col=col.age, labels.age, pch=16, cex=1)
text(RGR_mean ~ maxH_spp, data=subset(SummarySppAge,age==9), labels=species, pos=2, offset=-2.4, cex=0.9,col="blue")

plot(RGR_mean ~ maxH_spp,data=subset(SummarySppAge,age<3),log="xy",col=col.age[as.factor(age)],pch=16,cex=1.2,xlab="maximum height (mm)",ylab="RGR (per year)",xlim=c(500,3200),main="Relationship between growth rates on maximum H")
legend("bottomright",col=col.age, labels.age, pch=16, cex=1)
text(RGR_mean ~ maxH_spp, data=subset(SummarySppAge,age==2.4), labels=species, pos=2, offset=-2.4, cex=0.9,col="blue")
mod <- lm(RGR_mean ~ maxH_spp,data=subset(SummarySppAge,age==2.4))

summary(mod)
mod <- lm(RGR_mean ~ maxH_spp,data=subset(SummarySppAge,age==1.4))
summary(mod)


mod <- lm(RGR ~ asin(sqrt(RA)),SummaryInd)
summary(mod)
plot(RGR ~ asin(sqrt(RA)),SummaryInd)

plot(GrowthInv ~ ReproInv,SummaryInd, log="xy")

plot(asin(sqrt(RA)) ~ age,SummaryInd, log="x")
plot(asin(sqrt(RA)) ~ WD_mean,SummaryInd)
plot(asin(sqrt(RA)) ~ height,SummaryInd, log="x")
plot(asin(sqrt(RA)) ~ LL_bd,data=subset(SummaryInd,LL_bd!=Inf&RA>0),col=col.spp[is.factor(species)])

mod <- lm(asin(sqrt(RA)) ~ age + LL_bd + height + WD_mean + maxH_spp,data=subset(SummaryInd,LL_birth!=Inf))
summary(mod)
plot(mod)


