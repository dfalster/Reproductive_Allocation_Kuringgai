#correlations between the three "growth variables"

plot(RGR ~ GrowthInv, data=subset(SummaryInd), log="x",col=col.age[as.factor(age)])

plot(new_length ~ GrowthInv, SummaryInd, log="xy", col=col.age[as.factor(age)])

plot(RGR ~ new_length, SummaryInd, log="xy", col=col.age[as.factor(age)])


pairs(SummaryInd[c("RGR", "GrowthInv", "new_length", "change_shoot_area", "change_basal_area","maxH_spp","height","lvs_end_total","FinalWeight")])

mod <- lm(new_length~height + maxH_spp + WD_mean + LL_bd + RA + seed_size + age, SummaryInd)
summary(mod)
anova(mod)
step(mod)
