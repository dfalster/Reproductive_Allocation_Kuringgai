
LMA$age
LL_noHate$age


plot(LL_birth_mean ~ LMA_mean, LL_noHate, xlab="LMA", ylab="leaf lifespan (years)", main="leaf lifespan versus LMA (without HATE)", las=1)
mod_no_HATE <- lm(LL_birth_mean ~ LMA_mean, LL_noHate)
abline(mod_no_HATE)
####how could I label this line as "regression with Hakea omitted" ; i.e. place words on diagonal above line
summary(mod_no_HATE)



plot(LL_birth_mean ~ LMA_mean, LL_nobabies, xlab="LMA", ylab="leaf lifespan (years)", main="leaf lifespan versus LMA (no baby plants)", las=1)
mod <- lm(LL_birth_mean ~ LMA_mean, LL_nobabies)
abline(mod)
summary(mod)



plot(LL_birth_mean ~ LMA_mean, LL_noHatebabies, xlab="LMA", ylab="leaf lifespan (years)", main="leaf lifespan versus LMA (without HATE)", las=1)
mod <- lm(LL_birth_mean ~ LMA_mean, LL_noHatebabies)
abline(mod)
summary(mod)

#model with species included, since HATE so different from others
plot(LL_birth_mean ~ LMA_mean, LLSummary, xlab="LMA", ylab="leaf lifespan (years)", main="leaf lifespan versus LMA (all species and ages)", las=1)
mod <- lm(LL_birth_mean ~ LMA_mean + species, LLSummary)
summary(mod)


#LL by age, species, without youngest plants - graphs
par(mar = c(10, 6, 4, 1) + 0.1)

LLplot <- plot(LL_bd ~ as.factor(species), leaf_counts_nobabies, xlab="species", ylab="leaf lifespan", cex.axis=.8, las=1, xaxt="n")
LLplot <- plot(LL_bd ~ as.factor(species), leaf_counts_nobabies, xlab="species", ylab="leaf lifespan", cex.axis=.8, las=1)

text(LLplot, par("usr")[3] - 0.25, srt=45, adj=1, labels=labels.spp, xpd=TRUE)

axis(1,1:14,labels=labels.spp,col.axis="black", cex.axis=.4, tck=0)


#LMA versus LL
plot(LL_bd_mean ~ LMA_mean, LLSummary, xlab="LMA",ylab="leaf lifespans (years)", main="leaf lifespan versus LMA")
legend("bottomright", c("Banksia","Boronia","Conospermum","Epacris", "Grevillea buxifolia","Grevillea speciosa","Hakea","Hemigenia","Leucopogon","Persoonia", "Petrophile", "Phyllota", "Pimelea", "Pultanaea"), col = col.spp, pch = pch.spp, text.col = "black", border = "black", title="species")

#the above commands work, but if I try to color points by species, it fails
#plot(LL_bd_mean ~ mean , LLSummary, col=col.spp[species], pch=pch.spp[species])

#when I try and plot by age, some ages show up and others don't
plot(LL_bd_mean ~ mean, LLSummary, col=col.age[age], pch=pch.age[age], xlab="LMA",ylab="leaf lifespans (years)", main="leaf lifespan versus LMA")
legend("bottomright", c("1.35","2.4","5","7","9","32"), col = col.age, pch = pch.age, text.col = "black", border = "black", title="age categories")

#can't get syntax correct for moving this legend to locations specified by x,y coordinates




mod <- lm(LL_bd_mean ~ mean + age, LLSummary)
no_HATE <- lm(LL_bd_mean ~ mean + age, LLSummary,data=subset(LLSummary, species != "HATE"))
#this doesn't work
abline(mod)
summary(mod)

summary(lmer(log(leaf_new_total) ~ log(change_basal_diam+1) + (1|species), leaf_counts))
#plots looking at growth of shoots versus whole plant

plot(new_length ~ GrowthInv, leaf_counts, log="xy", col=col.age[as.factor(age)], pch=16)
legend("bottomright",col=col.age, labels.age, pch=16, cex=1)
mod <- lm(new_length ~ GrowthInv +age + GrowthInv*age, leaf_counts)
summary(mod)

plot(new_length ~ GrowthInv, leaf_counts, log="xy", col=col.spp[as.factor(species)], pch=16, xlab="total growth investment", ylab="shoot extension (mm)", main="Comparison of shoot extension to total plant growth")
legend("bottomright",col=col.spp, labels.spp, pch=16, cex=1)
mod <- lm(new_length ~ GrowthInv +age + GrowthInv*age, leaf_counts)
summary(mod)

plot(new_length ~ lvs_end_count, leaf_counts, log="xy", col=col.age[as.factor(age)], pch=16, xlab="final leaf count", ylab="shoot extension (mm)", main="Comparison of shoot extension to total plant growth")
legend("bottomright",col=col.age, labels.age, pch=16, cex=1)


#playing with other information

plot(LL_bd ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="BAER"))
plot(LL_bd ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="BOLE"))
plot(LL_bd ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="COER"))
plot(LL_bd ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="GRBU"))
plot(LL_bd ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="GRSP"))
plot(LL_bd ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="HATE"))
plot(LL_bd ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="HEPU"))
plot(LL_bd ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="LEES"))
plot(LL_bd ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="PELA"))
plot(LL_bd ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="PEPU"))
plot(LL_bd ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="PHPH"))
plot(LL_bd ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="PILI"))
plot(LL_bd ~ as.factor(age), leaf_counts, data=subset(leaf_counts,species=="PUTU"))


plot(change_shoot_diam ~ change_basal_diam, leaf_counts, log="xy", col=col.age[as.factor(age)], pch=16)
legend("bottomright",col=col.age, labels.age, pch=16, cex=1)
mod <- lm(new_length ~ GrowthInv +age + GrowthInv*age, leaf_counts)
summary(mod)

plot(change_shoot_area ~ change_basal_area, leaf_counts, log="xy", col=col.age[as.factor(age)], pch=16)
legend("bottomright",col=col.age, labels.age, pch=16, cex=1)
mod <- lm(new_length ~ GrowthInv +age + GrowthInv*age, leaf_counts)
summary(mod)

plot(change_shoot_diam ~ new_length, leaf_counts, log="xy", col=col.spp[as.factor(species)], pch=16)
legend("bottomright",col=col.spp, labels.spp, pch=16, cex=1)
mod <- lm(new_length ~ GrowthInv +age + GrowthInv*age, leaf_counts)
summary(mod)



#####Use next 6 lines of code for LL-LMA relationship
plot(LL_birth_mean ~ LMA_mean, LLSummary, xlab="LMA", ylab="leaf lifespan (years)", main="leaf lifespan versus LMA (all species and ages)", las=1)
mod <- lm(LL_birth_mean ~ LMA_mean, LLSummary)
abline(mod, col="red", lty=2)
mod_no_HATE <- lm(LL_birth_mean ~ LMA_mean, LL_noHate)
abline(mod_no_HATE)
summary(mod)