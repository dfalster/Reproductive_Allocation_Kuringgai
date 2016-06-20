pdf("figs2/new_leaf_growth.pdf", height=11.69, width=8.27)
plot

par(mfrow=c(2,2), cex=1, omi=c(.1,.1,.1,.1), mai=c(1.2,1.1,.4,0.2))

data <- SummaryInd
data <- split(data, data$species)

for(spp in names(data)) {
data[[spp]]$leaf_adjust <- data[[spp]]$leaf_area_0
data[[spp]]$age_0 = data[[spp]]$age-1
data[[spp]]$growth_leaf_adjust <- data[[spp]]$growth_leaf - min(data[[spp]]$growth_leaf) +1
data2 <- subset(SummarySppAge[["mean"]],species==spp)

plot(leaf_area~diameter,data[[spp]],pch=16,col=col.age(age),log="xy",ylim=c(1,max(leaf_area*1.1)),xlim=c(min(diameter_0*0.9),max(diameter*1.1)))
points(leaf_adjust~diameter_0,data[[spp]],pch=1,col=col.age(age))
arrows(data[[spp]]$diameter_0,data[[spp]]$leaf_adjust,data[[spp]]$diameter,data[[spp]]$leaf_area,code=0,length=0,col=col.age(data[[spp]]$age))
abline(log10(min(data[[spp]]$leaf_area_0)),0)
title(main=spp)

plot(leaf_area~age,data[[spp]],pch=16,col=col.age(age),log="xy",ylim=c(1,max(leaf_area*1.1)),xlim=c(0.3,max(age)+1))
points(leaf_adjust~age_0,data[[spp]],pch=1,col=col.age(age))
arrows(data[[spp]]$age_0,data[[spp]]$leaf_adjust,data[[spp]]$age,data[[spp]]$leaf_area,code=0,length=0,col=col.age(data[[spp]]$age))
abline(log10(min(data[[spp]]$leaf_area_0)),0)
title(main=spp)


#plot(growth_leaf_adjust~repro_inv,data[[spp]],pch=16,col=col.age(age),log="xy")
if (min(data[[spp]]$growth_leaf) < 0) {
  plot(growth_leaf_adjust~repro_inv,data[[spp]],pch=16,col=col.age(age),log="xy")
  abline(log10(-min(data[[spp]]$growth_leaf)),0)
} else {
  plot(growth_leaf~repro_inv,data[[spp]],pch=16,col=col.age(age),log="xy")
  abline(log10(min(data[[spp]]$growth_leaf)),0)
  }
title(main=spp)

plot(RA_leaf_area~age,data[[spp]],pch=16,col=col.age(age),log="x")
points(RA_leaf_area~age,data2,pch=16,cex=1.5,col=col.age(data2$age))
points(RA_leaf_area~age,data2,pch=1,cex=1.5,col="black")
title(main=spp)
}

dev.off()
