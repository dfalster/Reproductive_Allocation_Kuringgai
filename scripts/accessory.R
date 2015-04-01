


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
###can't get colors to work here

plot((PerPrePol_A+.01) ~ age, SummaryInd, log="xy", col=col.spp[is.factor(species)])

plot_prepolA <- function(spp) {
  points((PerPrePol_A_mean+.01) ~ age,data=subset(SummarySppAge,species==spp), col=col.spp[is.factor(species)], pch=16, cex=1.5)
}

for(n in names(labels.spp)) {
  plot_prepolA(n)
}

plot(PerPropagule_mean ~ FinalWeight_mean, SummarySppAge, log="xy", col=col.spp[as.factor(species)], pch=16)



points((PerPackDisp_mean+.01) ~ age,data=subset(SummarySppAge,species=="PILI"), col="yellow", pch=16, cex=2)
points((PerPackDisp_mean+.01) ~ age,data=subset(SummarySppAge,species=="BOLE"), col="salmon", pch=16, cex=2)


mod <- lm((PerPackDisp +.01) ~ age+ species +age*species, SummaryInd)
summary(mod)

plot((PerPropagule+.01) ~ age, SummaryInd, log="x", col=col.spp[is.factor(species)])
mod <- lm((PerPropagule +.01) ~ age + species +age*species, data=subset(SummaryInd,PerPropagule!=0))
summary(mod)

plot((PerAcc+.01) ~ RA,data=subset(SummaryInd,RA>0), log="x",col=col.spp[is.factor(species)])

plot((PerPropagule+.01) ~ RA,data=subset(SummaryInd,RA>0), log="x",col=col.spp[is.factor(species)])
mod <- lm((PerPropagule+.01) ~ RA +age,data=subset(SummaryInd,RA>0))
summary(mod)

plot((PerPrePol_A+.01) ~ RA,data=subset(SummaryInd,RA>0), log="x",col=col.spp[is.factor(species)])
mod <- lm((PerPrePol_A+.01) ~ RA +age,data=subset(SummaryInd,RA>0))
summary(mod)

plot((PerPrePol_S+.01) ~ RA,data=subset(SummaryInd,RA>0), log="x",col=col.spp[is.factor(species)])
mod <- lm((PerPrePol_S+.01) ~ RA +age,data=subset(SummaryInd,RA>0))
summary(mod)

plot((PerPackDisp+.01) ~ RA,data=subset(SummaryInd,RA>0), log="x",col=col.spp[is.factor(species)])
mod <- lm((PerPackDisp+.01) ~ RA +age,data=subset(SummaryInd,RA>0))
summary(mod)

plot((PerPostPol_A+.01) ~ RA,data=subset(SummaryInd,RA>0), log="x",col=col.spp[is.factor(species)])
mod <- lm((PerPostPol_A+.01) ~ RA +age,data=subset(SummaryInd,RA>0))
summary(mod)


mod <- lm(RA~asin(sqrt(PerPrePol_A))+ asin(sqrt(PerPrePol_S))+ asin(sqrt(PerPostPol_A))+ asin(sqrt(PerPackDisp))+ asin(sqrt(PerPropagule)),data=subset(SummaryInd,RA>0))
summary(mod)
plot(mod)

mod <- lm(RA~PerPrePol_A+ PerPrePol_S+ PerPostPol_A + PerPackDisp+PerPropagule,data=subset(SummaryInd,RA>0))
summary(mod)
plot(mod)



plot((PerAcc+.01) ~ RA,data=subset(SummaryInd,RA>0), log="x",col=col.spp[is.factor(species)])


plot((PerAcc+.01) ~ RA,data=subset(SummaryInd,RA>0), log="x",col=col.age[is.factor(age)])

