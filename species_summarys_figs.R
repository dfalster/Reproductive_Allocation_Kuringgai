yvar <- "growth_inv"
yvar2 <- "repro_inv"
yvar3 <- "total_inv"
yvar4 <- "RA"
yvar5 <- "growth_height_log"
yvar6 <- "growth_leaf_area_log"
yvar7 <- "shoot_leaf_area"
xvar <- "leaf_area_0"
xvar2 <- "total_weight_0"
xvar3 <- "prop_maxH"

data <- subset(SummaryInd)
data <- split(data, data$species)

# pdf("figs2/species_summary_plots.pdf", height=12, width=8)
# plot

for(spp in names(data)) {
  par(mfcol=c(7,3), omi=c(.5,.1,.5,.03), mai=c(.3,.5,.03,0.03))
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar,log="xy",xlab="",ylab="veg investment",cex=1.1, cex.axis=0.9,cex.lab=0.9,col=col.age(data[[spp]]$age))
  legend("bottomright",legend=labels.age(),col=col.age(),pch=16, cex=1, bty="n")
  plot_yvar_vs_xvar(data[[spp]], yvar2, xvar,log="xy",xlab="",ylab="repro investment",cex=1.1, cex.axis=0.9,cex.lab=0.9,col=col.age(data[[spp]]$age))
  plot_yvar_vs_xvar(data[[spp]], yvar3, xvar,log="xy",xlab="",ylab="total investment (mg)",cex=1.1, cex.axis=0.9,cex.lab=0.9,col=col.age(data[[spp]]$age))
  arrows(data[[spp]]$leaf_area_0,data[[spp]]$growth_inv,data[[spp]]$leaf_area_0,data[[spp]]$total_inv,length = 0, angle = 30, code = 2, col = "black",lwd=1)
  plot_yvar_vs_xvar(data[[spp]], yvar5, xvar,log="x",xlab="",ylab="height increase (log-log)",cex=1.1, cex.axis=0.9,cex.lab=0.9,col=col.age(data[[spp]]$age))
  plot_yvar_vs_xvar(data[[spp]], yvar6, xvar,log="x",xlab="",ylab="total leaf area increase (log-log)",cex=1.1, cex.axis=0.9,cex.lab=0.9,col=col.age(data[[spp]]$age))
  plot_yvar_vs_xvar(data[[spp]], yvar7, xvar,log="xy",xlab="",ylab="total shoot leaf area",cex=1.1, cex.axis=0.9,cex.lab=0.9,col=col.age(data[[spp]]$age))
  plot_yvar_vs_xvar(data[[spp]], yvar4, xvar,log="x",xlab="initial leaf area",ylab="RA",cex=1.1, cex.axis=0.9,cex.lab=0.9,col=col.age(data[[spp]]$age))
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar2,log="xy",xlab=spp,ylab="",cex=1.1, cex.axis=0.9,cex.lab=0.9,col=col.age(data[[spp]]$age))
  plot_yvar_vs_xvar(data[[spp]], yvar2, xvar2,log="xy",xlab="",ylab="",cex=1.1, cex.axis=0.9,cex.lab=0.9,col=col.age(data[[spp]]$age))
  plot_yvar_vs_xvar(data[[spp]], yvar3, xvar2,log="xy",xlab="",ylab="",cex=1.1, cex.axis=0.9,cex.lab=0.9,col=col.age(data[[spp]]$age))
  arrows(data[[spp]]$total_weight_0,data[[spp]]$growth_inv,data[[spp]]$total_weight_0,data[[spp]]$total_inv,length = 0, angle = 30, code = 2, col = "black",lwd=1)
  plot_yvar_vs_xvar(data[[spp]], yvar5, xvar2,log="x",xlab=spp,ylab="",cex=1.1, cex.axis=0.9,cex.lab=0.9,col=col.age(data[[spp]]$age))
  plot_yvar_vs_xvar(data[[spp]], yvar6, xvar2,log="x",xlab="",ylab="",cex=1.1, cex.axis=0.9,cex.lab=0.9,col=col.age(data[[spp]]$age))
  plot_yvar_vs_xvar(data[[spp]], yvar7, xvar2,log="xy",xlab="",ylab="",cex=1.1, cex.axis=0.9,cex.lab=0.9,col=col.age(data[[spp]]$age))
  plot_yvar_vs_xvar(data[[spp]], yvar4, xvar2,log="x",xlab="initial plant weight",ylab="",cex=1.1, cex.axis=0.9,cex.lab=0.9,col=col.age(data[[spp]]$age))
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar3,log="y",xlab="",ylab="",cex=1.1, cex.axis=0.9,cex.lab=0.9,col=col.age(data[[spp]]$age))
  plot_yvar_vs_xvar(data[[spp]], yvar2, xvar3,log="y",xlab="",ylab="",cex=1.1, cex.axis=0.9,cex.lab=0.9,col=col.age(data[[spp]]$age))
  plot_yvar_vs_xvar(data[[spp]], yvar3, xvar3,log="y",xlab="",ylab="",cex=1.1, cex.axis=0.9,cex.lab=0.9,col=col.age(data[[spp]]$age))
  plot_yvar_vs_xvar(data[[spp]], yvar5, xvar3,log="",xlab="",ylab="",cex=1.1, cex.axis=0.9,cex.lab=0.9,col=col.age(data[[spp]]$age))
  plot_yvar_vs_xvar(data[[spp]], yvar6, xvar3,log="",xlab="",ylab="",cex=1.1, cex.axis=0.9,cex.lab=0.9,col=col.age(data[[spp]]$age))
  plot_yvar_vs_xvar(data[[spp]], yvar7, xvar3,log="y",xlab="",ylab="",cex=1.1, cex.axis=0.9,cex.lab=0.9,col=col.age(data[[spp]]$age))
  plot_yvar_vs_xvar(data[[spp]], yvar4, xvar3,log="",xlab="prop maxH",ylab="",cex=1.1, cex.axis=0.9,cex.lab=0.9,col=col.age(data[[spp]]$age))
  mtext("Initial leaf area           initial plant weight                  prop maxH", 1, line=1,outer=TRUE,cex=1.2)
  mtext(spp, 3, outer=TRUE,cex=1.2)   
}
# dev.off()