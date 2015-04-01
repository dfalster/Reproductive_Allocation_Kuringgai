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

