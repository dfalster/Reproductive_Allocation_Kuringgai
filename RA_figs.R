# pdf('ms/RA/Figure_1_stacked_lines.pdf', height=11.5, width=8) plot

data <- select(SummarySppAge[["mean"]], species, age, repro_inv, growth_leaf, leaf_replacement,leaf_area_0,leaf_weight_0,
  growth_stem, leaf_shed,prop_stem,prop_leaf_replacement,prop_leaf_expand,prop_repro,RA_max_1,lifespan,height, RA_vs_all_leaf)


par(mfrow = c(7,4), cex = 1, oma = c(5, 5, 1, 1), mar = c(0, 3, 2, 1))

all_species <- unique(data$species)

for (i in seq_along(all_species)) {

  # Todo: this should be calculated at individual level before taking means by species
  data_sp <- filter(data, species == all_species[i])
  data_sp_no5 <- subset(data_sp,age!=5)
  m <- max(data_sp$age)
  n <- length(data_sp$age)
  curves <- as.data.frame(c("determinate: gradual","determinate: gradual","indeterminate: asymptotic","indeterminate: asymptotic",
                            "determinate: bang-bang","determinate: bang-bang","indeterminate: gradual","determinate: bang-bang",
                            "determinate: gradual","determinate: gradual","indeterminate: declining","determinate: gradual",
                            "determinate: bang-bang","determinate: gradual"))
  
  #leaf area
  plot(NA, log = "x", xlim = c(1.4, 32), ylim=c(1,(max(data_sp$leaf_weight_0)*1.1)),xaxs = "i", yaxs = "i", xaxt="n", ylab="n")
  box()
  axis(1, labels = (i %in% c(7,14)))
  polygon(x = data_sp$age[c(1:n, n, 1)], y = c(data_sp$leaf_weight_0, 0, 0), col = "darkseagreen4", density = NA)
  mtext(labels.spp.full(all_species[i]), 3, 0)
  
  if (i %in% c(4,11)) {
    mtext("Standing leaf biomass (mg)", 2, line=2.5, outer=FALSE)
  }
  
  # #reproductive investment
  # plot(NA,xaxt="n",xlim = c(1.4, 32), ylim=c(1,(max(data_sp$repro_inv)*1.1)),log="x",xaxs = "i", yaxs = "i", xaxt="n", ylab="n")
  # polygon(x = data_sp$age[c(1:n, n, 1)], y = c(data_sp$repro_inv, 0, 0), col = "coral3", density = NA)
  # axis(1, labels = (i %in% c(7,14)))
  # 
  # if (i %in% c(4,11)) {
  #   mtext("reproductive investment (mg)", 2, line=2.5, outer=FALSE)
  # }
  
  #RA leaf area, all leaf
  plot(NA, log = "x", ylim = c(0, 1),  xlim = c(1.4, 32), xaxs = "i", yaxs = "i", axes= FALSE, ann=FALSE)
  box()
  axis(1, labels = (i %in% c(7,14)))
  axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = TRUE, las=1)
  polygon(x = c(1,m,m,1), y = c(1,1, 0, 0), col = "darkseagreen4", density = NA)
  polygon(x = data_sp$age[c(1:n, n, 1)], y = c(data_sp$RA_vs_all_leaf, 0, 0), col = "coral3", density = NA)
  
  if (i %in% c(4,11)) {
    mtext("Proportional allocation to reproduction and leaf growth", 2, line=2.5, outer=FALSE)
  }
  
  #RA leaf area, max 1
  plot(NA, log = "x", ylim = c(0, 1),  xlim = c(1.4, 32), xaxs = "i", yaxs = "i", axes= FALSE, ann=FALSE)
  box()
  axis(1, labels = (i %in% c(7,14)))
  axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = TRUE, las=1)
  polygon(x = c(1,m,m,1), y = c(1,1, 0, 0), col = "darkseagreen2", density = NA)
  polygon(x = data_sp$age[c(1:n, n, 1)], y = c(data_sp$RA_max_1, 0, 0), col = "coral3", density = NA)
  
  if (i %in% c(4,11)) {
    mtext("Proportional allocation to reproduction and leaf growth", 2, line=2.5, outer=FALSE)
  }
  
  #proportion different energy
  plot(NA, log = "x", ylim = c(0, 1),  xlim = c(1.4, 32), xaxs = "i", yaxs = "i", axes= FALSE, ann=FALSE)
  box()
  axis(1, labels = (i %in% c(7,14)))
  axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = TRUE, las=1)

  
  polygon(x = data_sp$age[c(1:n, n, 1)], y = c(data_sp$prop_stem, 0, 0), col = "burlywood4", density = NA)
  polygon(x = data_sp$age[c(1:n, n, 1)], y = c(data_sp$prop_leaf_replacement, 0, 0), col = "darkseagreen4", density = NA)
  polygon(x = data_sp$age[c(1:n, n, 1)], y = c(data_sp$prop_leaf_expand, 0, 0), col = "darkseagreen2", density = NA)
  polygon(x = data_sp$age[c(1:n, n, 1)], y = c(data_sp$prop_repro, 0, 0), col = "coral3", density = NA)
  
  if (i %in% c(4,11)) {
    mtext("Proportional allocation to all energy sinks", 2, line=2.5, outer=FALSE)
  }
  



}
#mtext("Proportion energy", 2, line=3, outer=TRUE)
mtext("Age (yrs)", 1, line=3, outer=TRUE)

# ---------------------------------------------

