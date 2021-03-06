# file names

- leaf_counts is file with calculated leaf counts and leaf lifespan data
- SummarySppAge is file with means of LL - and other variables - by species and age; LMA data is merged to this file
- subsets of SummarySppAge are created to omit babies or HATE for various analyses

#What causes RA to shift with age in species?
Theoretical models indicate that there need to be size (or age) related shifts in *some* trade-off to create graded RA schedules.

For instance, shifts in a variable such as leaf lifespan, fecundity per RA, photosynthetic rates with size (or other variables) will change the RA schedule.

I have both RA schedules and lots of other data across multiple ages:
  - leaf lifespa
- LMA
- accessory tissue costs

#Shifts in RA with age

Showing just a few of the RA schedules from my last lab meeting.

They show that species differ greatly in RA, that there is a general increase with age, but also that the data are noisy.

```{r, echo=FALSE,results='hide', message=FALSE}
plot_RAbyAge <- function(spp) {
  plot(RA~age, data=subset(SummaryInd,species==spp), xlab="age (years)",ylab="reproductive allocation (%)", log="x", main=labels.spp[spp],xlim=c(1,35),ylim=c(0,1))
  points(RA~age, data=subset(SummarySppAge, species==spp),pch=16,cex=1.4)
}


for(n in labels.spp()) {
  plot_RAbyAge(n)
}
```
#Individual shoots looked decidedly "less leafy" in many older plants
This led me to wonder how leaf lifespan - or some other variable - was shifting with age

The number of leaves along a leader is overall uneffected by age, although some species show a marked decline
```{r, echo=FALSE,results='hide', message=FALSE}
plot(shoot_leaf_count~age,SummaryInd,log="xy", xlab="age",ylab="final leaf count along shoot",col=col.spp(species))
legend("bottomright",col=col.spp(), labels.spp.full(), pch=16, cex=1)
```

#LMA versus age
The plot below is a bulked LMA sample from each site, of leaves from the second most recent cohort

There is a significant increase with age (p=0.002) and many species differ from one another, despite the very few points
Age is only borderline significant (p=0.04) when the seedlings are removed

```{r, echo=FALSE,results='hide', message=FALSE}
plot(LMA~age,data=subset(LMA,branch_age=="mid"),log="xy", xlab="age",ylab="LMA",col=col.spp(species),pch=16)
legend("bottomright",col=col.spp(), labels.spp.full(), pch=16, cex=1)
```

#Leaf lifespan with age
Leaf lifespan increases from seedlings (1.4 yrs) to saplings (2.4 years) to adults (>4 years), but there are no differences between the different adult populations
```{r, echo=FALSE,results='hide', message=FALSE}
plot(LL_birth~age,SummaryInd,log="xy", xlab="age",ylab="leaf lifespan (based on leaf births)",col=col.spp(species))
legend("bottomright",col=col.spp(), labels.spp.full(), pch=16, cex=1)
```

# Plots of individual species show that no species has a significant change either - although several show slight declines at the end
```{r, echo=FALSE,results='hide', message=FALSE}
plot_LLvAge <- function(spp) {
  data2 <- filter(SummaryInd, species==spp)
  plot(LL_birth ~ age, data=data2, xlab="age", ylab="leaf lifespan (leaf births)", cex.axis=.8, las=1,
       main=labels.spp[spp], xlim=c(1,35), ylim=c(0,8), log="x", pch=16)
  points(LL_birth ~ age, data=subset(SummarySppAge, species==spp), cex=1.3, pch=8)
  points((LL_birth+LL_birth_se) ~ age, data=subset(SummarySppAge, species==spp), cex=1.5, pch=25)
  points((LL_birth-LL_birth_se) ~ age, data=subset(SummarySppAge, species==spp), cex=1.5, pch=24)
  
  # fit <- lm(LL_birth ~ log10(age), data=data2)
  # abline(fit)
}

for(n in labels.spp()) {
  plot_LLvAge(n)
  #  mod_LMAvAge(n)
  #  summary(mod_LMAvAge(n))
}
```

#Several species show significant declines in shoot extension with age
```{r, echo=FALSE,results='hide', message=FALSE}
plot_newlengthvAge <- function(spp) {
  plot(growth_shoot_length ~ age, data=subset(SummaryInd,species==spp), xlab="age", ylab="leader shoot extention", cex.axis=.8, las=1, pch=16, log="x",
       main=labels.spp[spp])
  points(growth_shoot_length ~ age, data=subset(SummarySppAge, species==spp), cex=1.3, pch=8)
  points((growth_shoot_length+growth_shoot_length_se) ~ age, data=subset(SummarySppAge, species==spp), cex=1.5, pch=25)
  points((growth_shoot_length-growth_shoot_length_se) ~ age, data=subset(SummarySppAge, species==spp), cex=1.5, pch=24)
}

mod_newlengthvAge <- function(spp) {
  lm(growth_shoot_length ~ age, data=subset(SummaryInd,age>2.4 & species==spp))
}

for(n in labels.spp()) {
  plot_newlengthvAge(n)
  mod_newlengthvAge(n)
  summary(mod_newlengthvAge(n))
}
```

#Had expected a stronger pattern either in leaf lifespan or leaf count on a shoot
Apparently it is simply that older plants have more stem - plotting total leaf weight against final weight, shows bigger plants have relatively fewer leaves
(Yes, I know the 2.4 yr olds are missing...)

```{r, echo=FALSE,results='hide', message=FALSE}
plot(total_leaf_weight~FinalWeight,SummaryInd, log="xy",col=col.age(age), pch=16, cex=0.8)
legend("bottomright",col=col.age, labels.age(), pch=16, cex=1)
```
#Next steps?
As I begin to analyze RA patterns, think more specifically about the species that do show a distinct change with age in one of hte "leaf" parameters.

For instance, is there some way to capture "change with age" as a variabe of its own that is worth comparing across species.

Afterall, the energy allocation models suggest that different RA schedules may be due to different amounts of "shifting" across species - some species might be predicted to shift more than others.

#LMA vs Leaf lifespan
This correlation is as expected, except for the babies

```{r, echo=FALSE,results='hide', message=FALSE}
col.LMA <-c("purple", "pink", "light green", "orange","blue", "light blue","red")
labels.LMA <-c("age 1.4", "age 2.4", "age 5", "age 7", "age 9", "age 32", "HATE")
plot(LL_birth ~ LMA, SummarySppAge, type="n", xlab="LMA", ylab="leaf lifespan (years)", main="leaf lifespan versus LMA (all species and ages)", las=1,pch=17, log="xy")
points(LL_birth ~ LMA, data=subset(SummarySppAge, species!="HATE"), pch=16, cex=1.2, col=col.age(age)
points(LL_birth ~ LMA, col="red", data=subset(SummarySppAge, species=="HATE"), pch=17)
legend("bottomright",col=col.LMA, labels.LMA, pch=16, cex=1.4)
```

#LMA vs LL without young plants
The pattern becomes more obvious when young plants (and Hakea) are omitted

```{r, echo=FALSE,results='hide', message=FALSE}
plot(LL_bd ~ LMA, data=subset(SummarySppAge, species!="HATE" & age>1.5), pch=16, cex=1.2, col=col.age(age)
legend("bottomright",col=col.age,labels.age(),pch=16)
```

#Growth rates increase with increasing LMA in adult plants
But there is no relationship in seedlings (age=1.4) or saplings (age=2.4)
This is not predicted by Daniel's model or Anais' review
```{r, echo=FALSE,results='hide', message=FALSE}
plot(growth_inv ~ LMA,data=subset(SummarySppAge,age==1.4),log="xy",col=col.age(age),pch=16,cex=1.2,xlab="LMA",ylab="growth investment (mg per year)",main="Seedlings")
text(growth_inv ~ LMA, data=subset(SummarySppAge,age==1.4), labels=species, pos=1, offset=-1.1, cex=0.9,col="blue")

plot(growth_inv ~ LMA,data=subset(SummarySppAge,age==2.4),log="xy",col="pink",pch=16,cex=1.2,xlab="LMA",ylab="growth investment (mg per year)",main="Saplings")
text(growth_inv ~ LMA, data=subset(SummarySppAge,age==2.4), labels=species, pos=1, offset=-1.1, cex=0.9,col="blue")

plot(growth_inv ~ LMA,data=subset(SummarySppAge,age>3),log="xy",col=col.age(age),pch=16,cex=1.2,xlab="LMA",ylab="growth investment (mg per year)",main="Adults")
text(growth_inv ~ LMA, data=subset(SummarySppAge,age>3), labels=species, pos=1, offset=-1.1, cex=0.6,col="blue")
legend("bottomright",col=col.age,labels.age(),pch=16)
```

#Species with greater wood density have higher growth rates
Again there is no relationship in younger plants
```{r, echo=FALSE,results='hide', message=FALSE}
plot(growth_inv ~ wood_density,data=subset(SummarySppAge,age>3),log="xy",col=col.age(age),pch=16,cex=1,xlab="wood density",ylab="growth investment (mg per year)",xlim=c(0.57,0.9),main="Relationship between wood density and growth rates")
text(growth_inv ~ wood_density, data=subset(SummarySppAge,age==5), labels=species, pos=2, offset=-2.4, cex=0.9,col="blue")
legend("bottomright",col=col.age, labels.age(), pch=16, cex=1)
```

#Species with larger seeds have higher growth rates
The same basic relationship exists across all ages, so combined all data here
```{r, echo=FALSE,results='hide', message=FALSE}
plot(growth_inv ~ seed_size,data=subset(SummarySppAge,age>.5),log="xy",col=col.age(age),pch=16,cex=1,xlab="seed size (mg)",ylab="growth investment (mg per year)",main="Effect of seed size on growth rates")
legend("topleft",col=col.age, labels.age(), pch=16, cex=1)
text(growth_inv ~ seed_size, data=subset(SummarySppAge,age==9), labels=species, pos=2, offset=-2.4, cex=0.9,col="blue")
```

#Species with greater maximum height have higher growth rates
For younger plants there is no relationship with *SPECIES* overall maximum H, but there is a positive correlation with the maximum height the species reaches at a given age -i.e. fast growing young plants are tall when young.
```{r, echo=FALSE,results='hide', message=FALSE}
plot(growth_inv ~ maxH,data=subset(SummarySppAge,age>3),log="xy",col=col.age(age),pch=16,cex=1.2,xlab="maximum height (mm)",ylab="growth investment (mg per year)",xlim=c(500,3200),main="Relationship between growth rates on maximum H")
legend("bottomright",col=col.age, labels.age(), pch=16, cex=1)
text(growth_inv ~ maxH, data=subset(SummarySppAge,age==9), labels=species, pos=2, offset=-2.4, cex=0.9,col="blue")
```



#Correlations between different growth measures
Increased cross-sectional area of a leader and the entire plant are fairly correlated and uneffected by age (once not baby)
```{r, echo=FALSE,results='hide', message=FALSE}
plot(change_shoot_area~change_basal_area,SummaryInd, log="xy",col=col.age(age), pch=16, cex=.8,xlab="change in basal stem area",ylab="change in shoot area")
legend("bottomright",col=col.age, labels.age(), pch=16, cex=1)
```
