#Growth rates increase with increasing LMA in adult plants
But there is no relationship in seedlings (age=1.4) or saplings (age=2.4)
This is not predicted by Daniel's model or Anais' review
```{r, echo=FALSE,results='hide', message=FALSE}
plot(new_length_mean ~ LL_birth_mean,data=subset(SummarySppAge,age==1.4),log="xy",col=col.age[as.factor(age)],pch=16,cex=1.2,xlab="leaf lifespan (leaf births)",ylab="shoot extension (mm per year)",main="Seedlings")
text(new_length_mean ~ LL_birth_mean, data=subset(SummarySppAge,age==1.4), labels=species, pos=1, offset=-1.1, cex=0.9,col="blue")

plot(new_length_mean ~ LL_bd_mean,data=subset(SummarySppAge,age==2.4),log="xy",col="pink",pch=16,cex=1.2,xlab="leaf lifespan (leaf births)",ylab="shoot extension (mm per year)",main="Saplings")
text(new_length_mean ~ LL_bd_mean, data=subset(SummarySppAge,age==2.4), labels=species, pos=1, offset=-1.1, cex=0.9,col="blue")

plot(new_length_mean ~ LL_birth_mean,data=subset(SummarySppAge,age>3),log="xy",col=col.age3[as.factor(age)],pch=16,cex=1.2,xlab="leaf lifespan (leaf births)",ylab="shoot extension (mm per year)",main="Adults")
text(new_length_mean ~ LL_birth_mean, data=subset(SummarySppAge,age>3), labels=species, pos=1, offset=-1.1, cex=0.6,col="blue")
legend("bottomright",col=col.age,labels.age,pch=16)


plot(new_length ~ LL_birth,data=subset(SummaryInd,age==1.4),log="xy",col=col.age[as.factor(age)],pch=16,cex=1.2,xlab="leaf lifespan (leaf births)",ylab="shoot extension (mm per year)",main="Seedlings (looking at individuals)")
plot(new_length ~ LL_birth,data=subset(SummaryInd,age==2.4),log="xy",col=col.age[as.factor(age)],pch=16,cex=1.2,xlab="leaf lifespan (leaf births)",ylab="shoot extension (mm per year)",main="Saplings (looking at individuals)")
plot(new_length ~ LL_birth,data=subset(SummaryInd,age>3),log="xy",col=col.age3[as.factor(age)],pch=16,cex=1.2,xlab="leaf lifespan (leaf births)",ylab="shoot extension (mm per year)",main="Adults (looking at individuals)")
mod <- lm(new_length ~ LL_birth,data=subset(SummaryInd,age>3))
summary(mod)
```

#Species with greater wood density have higher growth rates
Again there is no relationship in younger plants
```{r, echo=FALSE,results='hide', message=FALSE}
plot(new_length_mean ~ WD_mean,data=subset(SummarySppAge,age>3 &new_length_mean>20),log="xy",col=col.age3[as.factor(age)],pch=16,cex=1,xlab="wood density",ylab="shoot extension (mm per year)",xlim=c(0.57,0.9),main="Relationship between wood density and growth rates")
text(new_length_mean ~ WD_mean, data=subset(SummarySppAge,age==5), labels=species, pos=2, offset=-2.4, cex=0.9,col="blue")
legend("bottomright",col=col.age, labels.age, pch=16, cex=1)
```

#Species with larger seeds have higher growth rates
The same basic relationship exists across all ages, so combined all data here
```{r, echo=FALSE,results='hide', message=FALSE}
plot(new_length_mean ~ seed_size,data=subset(SummarySppAge,age>.5),log="xy",col=col.age[as.factor(age)],pch=16,cex=1,xlab="seed size (mg)",ylab="shoot extension (mm per year)",main="Effect of seed size on growth rates")
legend("topleft",col=col.age, labels.age, pch=16, cex=1)
text(new_length_mean ~ seed_size, data=subset(SummarySppAge,age==9), labels=species, pos=2, offset=-2.4, cex=0.9,col="blue")
```

#Species with greater maximum height have higher growth rates
For younger plants there is no relationship with *SPECIES* overall maximum H, but there is a positive correlation with the maximum height the species reaches at a given age -i.e. fast growing young plants are tall when young.
```{r, echo=FALSE,results='hide', message=FALSE}
plot(new_length_mean ~ maxH_spp,data=subset(SummarySppAge,age==2.4),log="xy",col=col.age3[as.factor(age)],pch=16,cex=1.2,xlab="maximum height (mm)",ylab="shoot extension (mm per year)",xlim=c(500,3200),main="Relationship between growth rates on maximum H")
legend("bottomright",col=col.age, labels.age, pch=16, cex=1)
text(new_length_mean ~ maxH_spp, data=subset(SummarySppAge,age==9), labels=species, pos=2, offset=-2.4, cex=0.9,col="blue")

plot(new_length_mean ~ maxH_spp,data=subset(SummarySppAge,age<3),log="xy",col=col.age[as.factor(age)],pch=16,cex=1.2,xlab="maximum height (mm)",ylab="shoot extension (mm per year)",xlim=c(500,3200),main="Relationship between growth rates on maximum H")
legend("bottomright",col=col.age, labels.age, pch=16, cex=1)
text(new_length_mean ~ maxH_spp, data=subset(SummarySppAge,age==2.4), labels=species, pos=2, offset=-2.4, cex=0.9,col="blue")
mod <- lm(new_length_mean ~ maxH_spp,data=subset(SummarySppAge,age==2.4))

summary(mod)
mod <- lm(new_length_mean ~ maxH_spp,data=subset(SummarySppAge,age==1.4))
