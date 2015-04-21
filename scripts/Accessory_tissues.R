#Looking at how the relative proportion of various accessory costs shifts as RA shifts. Looked at 1 at a time, 
#higher RA=
# - lower pre-pollination aborted (or higher, depending on model)
# - higher pre-pollination successful
# - higher post-pollination aborted
# - higher packaging and dispersal
# - higher investment in propagules

#When all combined into a single model with PerPrePol_A or PerPropagule become much less significant because they are inversely correlated. But which is significant depends on the order in which I add the terms 

#how can I tell R to figure out for itself the correct order to add variables to the model - depending on order, different variables become significant

mod <- lm(log(Total)~asin(sqrt(PerPropagule))+asin(sqrt(PerPrePol_A))+ asin(sqrt(PerPackDisp))+ asin(sqrt(PerPrePol_S))+ asin(sqrt(PerPostPol_A)),data=subset(SummaryInd,RA>0))
summary(mod)
plot(mod)
step(mod)
hist(log(SummaryInd$Total))


hist(SummaryInd$PerPropagule)
hist(asin(sqrt(SummaryInd$PerPropagule)))

hist(SummaryInd$PerPropagule)
hist(asin(sqrt(SummaryInd$PerPropagule)))


mod <- lm(log(Total)~asin(sqrt(PerAcc)),data=subset(SummaryInd,RA>0))
summary(mod)
#As Total investment in reproduction changes, effect of lots of reproductive components, but Propagule % (and hence total accesory %) doesn't matter
Y<-cbind(asin(sqrt(SummaryInd$PerPropagule)),asin(sqrt(SummaryInd$PerPrePol_A)),asin(sqrt(SummaryInd$PerPackDisp)),asin(sqrt(SummaryInd$PerPrePol_S)),asin(sqrt(SummaryInd$PerPostPol_A)))
mod <- lm(asin(sqrt(RA))~asin(sqrt(PerPropagule))+asin(sqrt(PerPrePol_A))+ asin(sqrt(PerPackDisp))+ asin(sqrt(PerPrePol_S))+ asin(sqrt(PerPostPol_A)),data=subset(SummaryInd,RA>0))

mod <- manova(Y~asin(sqrt(RA))*log(Total),data=subset(SummaryInd,RA>0))
summary(mod)
summary.aov(mod)
anova(mod)


summary(mod)
anova(mod)
plot(mod)
step(mod)

mod <- lm(RA~PerPropagule+PerPrePol_A+ PerPrePol_S+ PerPostPol_A+ PerPackDisp,data=subset(SummaryInd,RA>0))
summary(mod)
plot(mod)
step(mod)

mod <- lm(asin(sqrt(RA))~asin(sqrt(PerPrePol_A)),data=subset(SummaryInd,RA>0))
summary(mod)

mod <- lm(asin(sqrt(RA))~asin(sqrt(PerPrePol_S)),data=subset(SummaryInd,RA>0))
summary(mod)

mod <- lm(asin(sqrt(RA))~asin(sqrt(PerPostPol_A)),data=subset(SummaryInd,RA>0))
summary(mod)

mod <- lm(asin(sqrt(RA))~asin(sqrt(PerPackDisp)),data=subset(SummaryInd,RA>0))
summary(mod)

mod <- lm(asin(sqrt(RA))~asin(sqrt(PerPropagule)),data=subset(SummaryInd,RA>0))
summary(mod)

plot(PerPrePol_A~PerPropagule,data=subset(SummaryInd,RA>0))
plot(RA~PerPropagule,data=subset(SummaryInd,RA>0))

#looking at shifts in allocation to different accessory categories with RA and age

#shifts in pre-pollination abortion rates
#Plants with higher RA have lower pre-pollination abortion
plot(asin(sqrt(RA))~asin(sqrt(PerPrePol_A)),data=subset(SummaryInd,RA>0))
mod <- lm(asin(sqrt(RA))~asin(sqrt(PerPrePol_A)),data=subset(SummaryInd,RA>0))
summary(mod)

#older plants have lower pre-pollination abortion
plot(asin(sqrt(PerPrePol_A))~age,data=subset(SummaryInd,RA>0), log="x")
mod <- lm(age~asin(sqrt(PerPrePol_A)),data=subset(SummaryInd,RA>0))
summary(mod)

#shifts in percent investment in propagules
#Plants with higher RA invest propotionally more in propagules
plot(asin(sqrt(RA))~asin(sqrt(PerPropagule)),data=subset(SummaryInd,RA>0))
mod <- lm(asin(sqrt(RA))~asin(sqrt(PerPropagule))+species+species*asin(sqrt(PerPropagule)),data=subset(SummaryInd,RA>0))
summary(mod)
anova(mod)
mod <- lm(asin(sqrt(RA))~asin(sqrt(PerPropagule)),data=subset(SummaryInd,RA>0&species=="GRSP"))
summary(mod)
mod <- lm(asin(sqrt(RA))~asin(sqrt(PerPropagule)),data=subset(SummaryInd,RA>0&species=="COER"))
summary(mod)
mod <- lm(asin(sqrt(RA))~asin(sqrt(PerPropagule)),data=subset(SummaryInd,RA>0&species=="BAER"))
summary(mod)
mod <- lm(asin(sqrt(RA))~asin(sqrt(PerPropagule)),data=subset(SummaryInd,RA>0&species=="HATE"))
summary(mod)
mod <- lm(asin(sqrt(RA))~asin(sqrt(PerPropagule)),data=subset(SummaryInd,RA>0&species=="GRBU"))
summary(mod)
mod <- lm(asin(sqrt(RA))~asin(sqrt(PerPropagule)),data=subset(SummaryInd,RA>0&species=="PELA"))
summary(mod)


mod <- lm(asin(sqrt(RA))~asin(sqrt(PerPropagule)),data=subset(SummaryInd,RA>0&species=="PILI"))
summary(mod)
mod <- lm(asin(sqrt(RA))~asin(sqrt(PerPropagule)),data=subset(SummaryInd,RA>0&species=="BOLE"&age>2))
summary(mod)


mod <- lm(asin(sqrt(RA))~asin(sqrt(PerPropagule)),data=subset(SummaryInd,RA>0&species=="EPMI"))
summary(mod)
mod <- lm(asin(sqrt(RA))~asin(sqrt(PerPropagule)),data=subset(SummaryInd,RA>0&species=="HEPU"))
summary(mod)
mod <- lm(asin(sqrt(RA))~asin(sqrt(PerPropagule)),data=subset(SummaryInd,RA>0&species=="PUTU"))
summary(mod)

#look at same shifts with total size
plot(log(FinalWeight)~asin(sqrt(PerPropagule)),data=subset(SummaryInd,RA>0))
mod <- lm(log(FinalWeight)~asin(sqrt(PerPropagule))+species+species*asin(sqrt(PerPropagule)),data=subset(SummaryInd,RA>0))
summary(mod)
anova(mod)
mod <- lm(log(FinalWeight)~asin(sqrt(PerPropagule)),data=subset(SummaryInd,RA>0&species=="GRSP"))
summary(mod)
mod <- lm(log(FinalWeight)~asin(sqrt(PerPropagule)),data=subset(SummaryInd,RA>0&species=="COER"))
summary(mod)
mod <- lm(log(FinalWeight)~asin(sqrt(PerPropagule)),data=subset(SummaryInd,RA>0&species=="BAER"))
summary(mod)
mod <- lm(log(FinalWeight)~asin(sqrt(PerPropagule)),data=subset(SummaryInd,RA>0&species=="HATE"))
summary(mod)
mod <- lm(log(FinalWeight)~asin(sqrt(PerPropagule)),data=subset(SummaryInd,RA>0&species=="GRBU"))
summary(mod)
mod <- lm(log(FinalWeight)~asin(sqrt(PerPropagule)),data=subset(SummaryInd,RA>0&species=="PELA"))
summary(mod)


mod <- lm(log(FinalWeight)~asin(sqrt(PerPropagule)),data=subset(SummaryInd,RA>0&species=="PILI"))
summary(mod)
mod <- lm(log(FinalWeight)~asin(sqrt(PerPropagule)),data=subset(SummaryInd,RA>0&species=="BOLE"&age>2))
summary(mod)


mod <- lm(log(FinalWeight)~asin(sqrt(PerPropagule)),data=subset(SummaryInd,RA>0&species=="EPMI"))
summary(mod)
mod <- lm(log(FinalWeight)~asin(sqrt(PerPropagule)),data=subset(SummaryInd,RA>0&species=="HEPU"))
summary(mod)
mod <- lm(log(FinalWeight)~asin(sqrt(PerPropagule)),data=subset(SummaryInd,RA>0&species=="PUTU"))
summary(mod)


#investment in propagules does not shift with age
plot(asin(sqrt(PerPropagule))~age,data=subset(SummaryInd,RA>0), log="x")
mod <- lm(age~asin(sqrt(PerPropagule)),data=subset(SummaryInd,RA>0))
summary(mod)

#overall shifts in accessory costs - inverse of propagule relationships
plot(asin(sqrt(RA))~asin(sqrt(PerAcc)),data=subset(SummaryInd,RA>0&age>2))
mod <- lm(asin(sqrt(RA))~asin(sqrt(PerAcc))+species+asin(sqrt(PerAcc))*species,data=subset(SummaryInd,RA>0&age>2))
summary(mod)
#Higher RA = only a small shift in accessory costs, with no species*RA interactions

mod_RA_by_Acc <- function(spp) {
  plot(PerAcc ~ RA, data=subset(SummaryInd,RA>0 & species==spp),ylab="percent reproduction for accessory tissues",main=labels.spp[spp])
}
  
  for(n in names(labels.spp)) {
    mod_RA_by_Acc(n)
  }

#looking individually through species, only a handful of species have a significant relationship between RA and PerAcc, 
#but always, with higher RA = lower PerAcc and a number of other species have p-value between 0.05 and 0.10.


plot(PerAcc~age,data=subset(SummaryInd,RA>0&age>2), log="x")
mod <- lm(PerAcc~age + species + age*species,data=subset(SummaryInd,RA>0&age>2))
summary(mod)

#full model indicates that for two species, accessory costs do decline with age:
mod <- lm(PerAcc~age,data=subset(SummaryInd,RA>0 & species=="BOLE"&age>2))
summary(mod)

mod <- lm(PerAcc~age,data=subset(SummaryInd,RA>0 & species=="COER"))
summary(mod)


#once the effect of species is accounted for, height has no effect
plot(asin(sqrt(PerAcc))~log(height),data=subset(SummaryInd,RA>0&age>2),log="x",col=col.spp[species])
mod <- lm(asin(sqrt(PerAcc))~log(height)+species+log(height)*species,data=subset(SummaryInd,RA>0&age>2))
summary(mod)
hist(log(SummaryInd$height))

plot(PerAcc~FinalWeight,data=subset(SummaryInd,RA>0&age>2),log="x",col=col.spp[species])
mod <- lm(PerAcc~FinalWeight+species+height*species,data=subset(SummaryInd,RA>0&age>2))
summary(mod)

plot(seed_size~PerAcc_mean,SummarySpp,log="xy")
mod <- lm(seed_size~PerAcc_mean,SummarySpp)
summary(mod)
#species with larger seed have higher accessory costs

plot(seed_size~PerAcc_mean,data=subset(SummarySppAge,age>2.4),log="xy")
mod <- lm(PerAcc_mean~seed_size+age+seed_size*age,data=subset(SummarySppAge,age>2.4))
summary(mod)
#species with larger seed have higher accessory costs


plot(PerPrePol_A~PerPropagule,data=subset(SummaryInd,RA>0))
mod <- lm(PerPrePol_A~PerPropagule,data=subset(SummaryInd,RA>0))
summary(mod)

plot((PerPropagule+.01) ~ age, SummaryInd, log="xy", col=col.spp[is.factor(species)])

plot((PerPrePol_A+.01) ~ age, SummaryInd, log="xy", col=col.spp[is.factor(species)])

mod <- lm((PerPrePol_A +.01) ~ age+ species +age*species, SummaryInd)
summary(mod)

plot((PerPrePol_S+.01) ~ age, SummaryInd, log="x", col=col.spp[is.factor(species)])
mod <- lm((PerPrePol_S +.01) ~ age+ species +age*species + species + age*species, SummaryInd)
summary(mod)

plot((PerPostPol_A+.01) ~ age, SummaryInd, log="xy", col=col.spp[is.factor(species)])
mod <- lm((PerPostPol_A +.01) ~ age+ species +age*species, SummaryInd)
summary(mod)

plot((PerPackDisp+.01) ~ age, SummaryInd, log="xy", col=col.spp[is.factor(species)])

plot_packdisp <- function(spp) {
  points((PerPackDisp_mean+.01) ~ age,data=subset(SummarySppAge,species==spp), col=col.spp[as.factor(species)], pch=16, cex=1.5)
}

for(n in names(labels.spp)) {
  plot_packdisp(n)
}



pairs(SummaryInd[c("PerPrePol_A", "PerPrePol_S", "PerPostPol_A", "PerPackDisp", "PerPropagule")])
hist(asin(sqrt(SummaryInd$PerPropagule)))

hist(asin(sqrt(SummaryInd$PerPrePol_A)))

hist(asin(sqrt(SummaryInd$PerPrePol_S)))

hist(asin(sqrt(SummaryInd$PerPostPol_A)))

hist(asin(sqrt(SummaryInd$PerPackDisp)))


mod <- lm(asin(sqrt(RA))~asin(sqrt(PerPropagule))+asin(sqrt(PerPrePol_A))+ asin(sqrt(PerPrePol_S))+ asin(sqrt(PerPostPol_A))+ asin(sqrt(PerPackDisp)),SummaryInd)
summary(mod)
plot(mod)
step(mod)


plot(PerPropagule_mean ~ FinalWeight_mean, SummarySppAge, log="xy", col=col.spp[as.factor(species)], pch=16)

plot(PerPropagule ~ FinalWeight, SummaryInd, log="xy", col=col.spp[as.factor(species)], pch=16)
legend("bottomleft",col=col.spp, labels.spp, pch=16, cex=1)

mod <- lm(log(FinalWeight)~ asin(sqrt(PerPropagule)) + species,SummaryInd)
summary(mod)


repro <- c("Propagules", "Aborted pre-pollination","Successful pre-pollination","Aborted post-pollination","Packaging and dispersal")
pch.repro <- c(16,17,18,4,5)
plot(PerPropagule_mean~species,SummarySpp,ylab="percent total reproductive investment",cex.axis=.8,ylim=c(0,1), pch=16)
legend("topright",repro,pch=pch.repro)
points(PerPrePol_A_mean~species,SummarySpp, pch=17)
points(PerPrePol_S_mean~species,SummarySpp, pch=18)
points(PerPostPol_A_mean~species,SummarySpp, pch=4)
points(PerPackDisp_mean~species,SummarySpp, pch=5)



