#plots of reproductive investment and RA against various size, age, and growth measures
#RA by age and RA by RGR explain the most variance

plot_RA_by_size <- function(spp) {
  plot(RA ~ FinalWeight, data=subset(SummaryInd,species==spp),main=labels.spp[spp],xlab="plant biomass (mg)", ylab="RA",col=col.age[as.factor(age)], pch=16, log="x")
  legend("topleft",col=col.age, labels.age, pch=16, cex=1)
  mod <- lm(RA ~ FinalWeight, data=subset(SummaryInd,species==spp))
  summary(mod)
  anova(mod)
}

mod <- lm(RA ~ FinalWeight + age + species + age*species, SummaryInd)
summary(mod)

for(n in names(labels.spp)) {
  plot_RA_by_size(n)
} 



plot_RVcurves <- function(spp) {
  plot(ReproInv ~ FinalWeight, data=subset(SummaryInd,species==spp),main=labels.spp[spp],xlab="plant biomass (mg)", ylab="reproductive investment (mg)",col=col.age[as.factor(age)], pch=16, log="x")
  legend("topleft",col=col.age, labels.age, pch=16, cex=1)
  mod <- lm(ReproInv ~ FinalWeight, data=subset(SummaryInd,species==spp))
  summary(mod)
  anova(mod)
}

mod <- lm(ReproInv ~ FinalWeight + age + species + age*species, SummaryInd)
summary(mod)

for(n in names(labels.spp)) {
  plot_RVcurves(n)
} 


plot_RA_by_RGR <- function(spp) {
  plot(RA ~ RGR, data=subset(SummaryInd,species==spp),main=labels.spp[spp],xlab="RGR", ylab="RA",col=col.age[as.factor(age)], pch=16, log="x")
  legend("topleft",col=col.age, labels.age, pch=16, cex=1)
  mod <- lm(RA ~ RGR, data=subset(SummaryInd,species==spp))
  summary(mod)
  anova(mod)
}

mod <- lm(RA ~ RGR + age + species + age*species, SummaryInd)
summary(mod)

for(n in names(labels.spp)) {
  plot_RA_by_RGR(n)
} 


plot_RA_by_age <- function(spp) {
  plot(RA ~ age, data=subset(SummaryInd,species==spp),main=labels.spp[spp],xlab="age (years)", ylab="RA",col=col.age[as.factor(age)], pch=16, log="x")
  legend("topleft",col=col.age, labels.age, pch=16, cex=1)
  mod <- lm(age ~ RA, data=subset(SummaryInd,species==spp))
  summary(mod)
  anova(mod)
}

for(n in names(labels.spp)) {
  plot_RA_by_age(n)
} 

plot_RA_by_height <- function(spp) {
  plot(RA ~ height, data=subset(SummaryInd,species==spp),main=labels.spp[spp],xlab="height (mm)", ylab="RA",col=col.age[as.factor(age)], pch=16, log="x")
  legend("topleft",col=col.age, labels.age, pch=16, cex=1)
  mod <- lm(age ~ height, data=subset(SummaryInd,species==spp))
  summary(mod)
  anova(mod)
}

for(n in names(labels.spp)) {
  plot_RA_by_height(n)
} 
#model with untransformed variables
mod <- lm(RA ~ RGR + GrowthInv + FinalWeight + height + age + species + age*species, SummaryInd)
summary(mod)
plot(mod)
step(mod,direction="both")

#model with transformed variables, but just growth variables
mod <- lm(asin(sqrt(RA)) ~ RGR + log(GrowthInv+1) + log(FinalWeight) + height + age + species + age*species, SummaryInd)
summary(mod)
plot(mod)
step(mod,direction="both")

#model with both growth and functional trait values
mod <- lm(asin(sqrt(RA)) ~ log(RGR+1) + log(GrowthInv+1) + log(FinalWeight) + log(new_length+1) + log(height) + log(seed_size) + 
            LL_bd + WD_mean + species + age + species*age, data=subset(SummaryInd))
summary(mod)
plot(mod)
step(mod,direction="both")

#running a stepwise regression shows that removing seedsize, leaf lifespan, wood density do not decrease - and even seem to increase - the fit of the model
#order variables removed: wood density, seed size, leaf lifespan
#in other words, RA is predicted by species, age, and most importantly various size and growth measures, but not by functional trait values

mod <- lm(asin(sqrt(RA)) ~ log(GrowthInv+1) + log(FinalWeight) + log(new_length+1) + log(height) + species + age + species*age, data=SummaryInd)
summary(mod)
step(mod)

#I also ran the model using only RA>0 - so that data normally distributed. When I do that exactly the same variables are identified as significant, but the overall model exaplains less of the variance.



#plotting against "ReproInv" instead of "RA"
#model with both growth and functional trait values
mod <- lm(log(ReproInv+1) ~ log(RGR+1) + log(GrowthInv+1) + log(FinalWeight) + log(new_length) + log(height) + log(seed_size) + LL_bd + WD_mean + species + age + species*age, SummaryInd)
summary(mod)
plot(mod)
step(mod,direction="both")

#this model has an overall similar outcome, but explains much less of the variation (65% vs 74%)

#checking distributions of variables once transformed
hist(asin(sqrt(SummaryInd$RA)),main="")

#GrowthInv, ReproInv, etc - most growth variables good with basic log transformation
hist(log(SummaryInd$RGR),main="")

hist(log(SummaryInd$GrowthInv),main="")

hist(log(SummaryInd$ReproInv),main="")

hist(log(SummaryInd$FinalWeight),main="")

hist(log(SummaryInd$height),main="")

hist(log(SummaryInd$new_length),main="")

hist(log(SummaryInd$LL_bd),main="")
#leaf lifespan should have log transformation, but not in models right now, because lots of NaNs result
