#Looking at how the relative proportion of various accessory costs shifts as RA shifts. Looked at 1 at a time, 
#higher RA=
# - lower pre-pollination aborted (or higher, depending on model)
# - higher pre-pollination successful
# - higher post-pollination aborted
# - higher packaging and dispersal
# - higher investment in propagules

#When all combined into a single model with PerPrePol_A or PerPropagule become much less significant because they are inversely correlated. But which is significant depends on the order in which I add the terms 

#how can I tell R to figure out for itself the correct order to add variables to the model - depending on order, different variables become significant

mod <- lm(asin(sqrt(RA))~asin(sqrt(PerPropagule))+asin(sqrt(PerPrePol_A))+ asin(sqrt(PerPrePol_S))+ asin(sqrt(PerPostPol_A))+ asin(sqrt(PerPackDisp)),SummaryInd)
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

