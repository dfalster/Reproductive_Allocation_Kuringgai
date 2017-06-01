```{r}
# library(digest)
# library(Rcpp)
# library(reshape2)
# library(tibble)
library(smatr)
library(broom)
library(dplyr)
# library(stats)
# library(scales)
# library(colorspace)

remake::make("export")

SummaryCombine <- readRDS("export/SummaryCombine.rds")
SummarySpp <- readRDS("export/SummarySpp.rds")
SummarySppAge <- readRDS("export/SummarySppAge.rds")

HarvestData <- readRDS("export/HarvestData.rds")
InvestmentByPart <- readRDS("export/Investment_FD_all.rds")
PartsSummary <- readRDS("export/PartsSummary_all.rds")

source("R/figures.R")

data <- select(SummarySppAge[["mean"]], species, age, repro_inv, growth_leaf, leaf_replacement,leaf_area_0,leaf_weight_0,total_inv,prop_surplus,seed_count,
               growth_stem, leaf_shed,prop_stem,prop_leaf_replacement,prop_leaf_expand,prop_repro,RA_max_1,lifespan,height, RA_vs_all_leaf,
               prop_leaf_loss,height_0,total_weight_0,all_leaf_inv,surplus_inv,all_leaf_and_repro_inv,propagule_inv,ovule_count,success_inv)

data_ind <- select(SummaryCombine,species,individual,age, prop_leaf_loss,height_0,leaf_shed,leaf_weight_0,propagule_inv,ovule_count,
                   growth_leaf_pos,repro_inv,total_weight_0,all_leaf_inv,total_inv,surplus_inv,all_leaf_and_repro_inv,age,success_inv,
                   seed_count)

```

```{r}
#pdf('ms/RA/Figure_1_stacked_lines.pdf', height=11.5, width=8) 
plot
par(mfrow = c(7,4), cex = 1, oma = c(5, 5, 1, 1), mar = c(0, 3, 2, 1))

all_species <- unique(data$species)

for (i in seq_along(all_species)) {
  
  # Todo: this should be calculated at individual level before taking means by species
  data_sp <- filter(data, species == all_species[1])
  data_sp_ind <- filter(data_ind, species == all_species[1])
  data_sp_no5 <- subset(data_sp,age!=5)
  m <- max(data_sp$age)
  n <- length(data_sp$age)
  curves <- as.data.frame(c("determinate: gradual","determinate: gradual","indeterminate: asymptotic","indeterminate: asymptotic",
                            "determinate: bang-bang","determinate: bang-bang","indeterminate: gradual","determinate: bang-bang",
                            "determinate: gradual","determinate: gradual","indeterminate: declining","determinate: gradual",
                            "determinate: bang-bang","determinate: gradual"))
  
  
  #height
  plot(NA,log = "x", ylim=c(1,(max(data_sp$height_0)*1.1)), xlim = c(1.2, 37), xaxs = "i", yaxs = "i", xaxt="n", ylab="n")
  box()
  axis(1, labels = (i %in% c(7,14)))
  points(x = data_sp$age, y = data_sp$height_0, col = "darkseagreen4", lwd=2,type="l")
  points(x = data_sp_ind$age, y = data_sp_ind$height_0, col = "black")
  
  if (i %in% c(4,11)) {
    mtext("Height (mm)", 2, line=2, outer=FALSE)
  }
  
  #leaf area
  plot(NA, log = "x", xlim = c(1.4, 32), ylim=c(0,1),xaxs = "i", yaxs = "i", xaxt="n", ylab="n")
  box()
  axis(1, labels = (i %in% c(7,14)))
  points(x = data_sp$age, y = data_sp$prop_leaf_loss, col = "darkseagreen4", lwd=2,type="l")
  points(x = data_sp_ind$age, y = data_sp_ind$prop_leaf_loss, col = "black")
  #points(x = data_sp_ind$age, y = data_sp_ind$leaf_weight_0, col = "black",cex=0.5,pch=16)
  #points(x = data_sp_ind$age, y = data_sp_ind$leaf_shed, col = "darkseagreen4")
  #points(x = data_sp$age, y = data_sp$growth_leaf, col = "darkseagreen2", lwd=2,type="l")
  mtext(labels.spp.full(all_species[i]), 3, 0)
  
  if (i %in% c(4,11)) {
    mtext("Standing leaf biomass (mg)", 2, line=2.5, outer=FALSE)
  }
  
  plot(NA, log = "x", xlim = c(1.4, 32), ylim=c(1,(max(data_sp$total_inv)*0.7)),xaxs = "i", yaxs = "i", xaxt="n", ylab="n")
  box()
  axis(1, labels = (i %in% c(7,14)))
  points(x = data_sp$age, y = data_sp$repro_inv, col = "coral3", lwd=2,type="l")
  #points(x = data_sp_ind$age, y = data_sp_ind$leaf_weight_0, col = "black",cex=0.5,pch=16)
  points(x = data_sp_ind$age, y = data_sp_ind$leaf_shed, col = "darkseagreen4")
  points(x = data_sp$age, y = data_sp$growth_leaf, col = "darkseagreen2", lwd=2,type="l")
  mtext(labels.spp.full(all_species[i]), 3, 0)
  
  if (i %in% c(4,11)) {
    mtext("Yearly investment (mg)", 2, line=2.5, outer=FALSE)
  }
  
  plot(NA, log = "xy", xlim = c((min(data_sp_ind$total_weight_0)*0.9),(max(data_sp_ind$total_weight_0)*1.1)), ylim=c(1,(max(data_sp_ind$total_inv)*0.7)),xaxs = "i", yaxs = "i", xaxt="n", ylab="n")
  box()
  axis(1, labels = (i %in% c(7,14)))
  points(x = data_sp_ind$leaf_weight_0, y = data_sp_ind$all_leaf_inv, col = "darkseagreen4", lwd=2,type="p",pch=1,cex=1.4)
  points(x = data_sp_ind$leaf_weight_0, y = data_sp_ind$repro_inv, col = "coral3", lwd=2,type="p",pch=1,cex=1.4)
  points(x = data_sp_ind$leaf_weight_0, y = data_sp_ind$growth_leaf_pos, col = "darkseagreen2", lwd=2,type="p",pch=1,cex=1.4)
  mtext(labels.spp.full(all_species[i]), 3, 0)
  
  if (i %in% c(4,11)) {
    mtext("Yearly investment (mg)", 2, line=2.5, outer=FALSE)
  }
  
  # #reproductive investment
  # plot(NA,xaxt="n",xlim = c(1.4, 32), ylim=c(1,(max(data_sp$repro_inv)*1.1)),log="x",xaxs = "i", yaxs = "i", xaxt="n", ylab="n")
  # polygon(x = data_sp$age[c(1:n, n, 1)], y = c(data_sp$repro_inv, 0, 0), col = "coral3", density = NA)
  # axis(1, labels = (i %in% c(7,14)))
  # 
  # if (i %in% c(4,11)) {
  #   mtext("reproductive investment (mg)", 2, line=2.5, outer=FALSE)
  # }
  
  # #leaf lifespan
  # plot(NA,log = "x", ylim = c(0, 1),  xlim = c(1.2, 37), xaxs = "i", yaxs = "i", axes= FALSE, ann=FALSE)
  # box()
  # axis(1, labels = (i %in% c(7,14)))
  # axis(2, at = c(0,2,4,6), labels = TRUE, las=1)
  # points(x = data_sp$age, y = data_sp$prop_leaf_loss, col = "darkseagreen4", lwd=2,type="l")
  # points(x = data_sp_ind$age, y = data_sp_ind$prop_leaf_loss, col = "black")
  # 
  # if (i %in% c(4,11)) {
  #   mtext("Leaf lifespan (yrs), based on leaf death", 2, line=2, outer=FALSE)
  # }
}
```

```{r}
#FIGURE 2
par(mfrow = c(7,4), cex = 1, oma = c(5, 5, 1, 1), mar = c(0, 3, 2, 1))

all_species <- unique(data$species)

for (i in seq_along(all_species)) {
  
  data_sp <- filter(data, species == all_species[i])
  m <- max(data_sp$age)
  n <- length(data_sp$age)
  
  #RA leaf area, max 1
  plot(NA, log = "x", ylim = c(0, 1),  xlim = c(1.4, 32), xaxs = "i", yaxs = "i", axes= FALSE, ann=FALSE)
  box()
  axis(1, labels = (i %in% c(7,14)))
  axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0,"","","","",1), las=1)
  polygon(x = c(1,m,m,1), y = c(1,1, 0, 0), col = "darkseagreen2", density = NA)
  polygon(x = data_sp$age[c(1:n, n, 1)], y = c(data_sp$RA_max_1, 0, 0), col = "coral3", density = NA)
  
  if (i %in% c(4,11)) {
    mtext("Proportional allocation to reproduction vs leaf growth", 2, line=2, outer=FALSE)
  }
  
  #RA leaf area, all leaf
  plot(NA, log = "x", ylim = c(0, 1),  xlim = c(1.4, 32), xaxs = "i", yaxs = "i", axes= FALSE, ann=FALSE)
  box()
  axis(1, labels = (i %in% c(7,14)))
  axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0,"","","","",1), las=1)
  polygon(x = c(1,m,m,1), y = c(1,1, 0, 0), col = "darkseagreen4", density = NA)
  polygon(x = data_sp$age[c(1:n, n, 1)], y = c(data_sp$prop_surplus, 0, 0), col = "darkseagreen2", density = NA)
  polygon(x = data_sp$age[c(1:n, n, 1)], y = c(data_sp$RA_vs_all_leaf, 0, 0), col = "coral3", density = NA)
  mtext(labels.spp.full(all_species[i]), 3, 0)
  
  if (i %in% c(4,11)) {
    mtext("Proportional allocation to reproduction vs leaf growth + REPLACEMENT", 2, line=2, outer=FALSE)
  }
  
  #proportion different energy
  # plot(NA, log = "x", ylim = c(0, 1),  xlim = c(1.4, 32), xaxs = "i", yaxs = "i", axes= FALSE, ann=FALSE)
  # box()
  # axis(1, labels = (i %in% c(7,14)))
  # axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0,"","","","",1), las=1)
  # 
  # 
  # polygon(x = data_sp$age[c(1:n, n, 1)], y = c(data_sp$prop_stem, 0, 0), col = "burlywood4", density = NA)
  # polygon(x = data_sp$age[c(1:n, n, 1)], y = c(data_sp$prop_leaf_replacement, 0, 0), col = "darkseagreen4", density = NA)
  # polygon(x = data_sp$age[c(1:n, n, 1)], y = c(data_sp$prop_leaf_expand, 0, 0), col = "darkseagreen2", density = NA)
  # polygon(x = data_sp$age[c(1:n, n, 1)], y = c(data_sp$prop_repro, 0, 0), col = "coral3", density = NA)
  # 
  # if (i %in% c(4,11)) {
  #   mtext("Proportional allocation to all energy sinks", 2, line=2, outer=FALSE)
  # }
}
```



```{r}
#FIGURE 3
par(mfrow = c(7,4), cex = 1, oma = c(5, 5, 1, 1), mar = c(0, 3, 2, 1))

all_species <- unique(data$species)

for (i in seq_along(all_species)) {
  
  data_sp <- filter(data, species == all_species[i])
  data_sp_ind <- filter(data_ind, species == all_species[i])
  data_sp_no5 <- subset(data_sp,age!=5)
  m <- max(data_sp$age)
  n <- length(data_sp$age)
  
  #panel 1
  plot(repro_inv ~ leaf_weight_0,data_sp_ind,log="xy",pch=16,xaxt="n")
  points(repro_inv ~ growth_leaf_pos,data_sp_ind,log="xy",pch=16,xaxt="n",col="darkseagreen4")
  points(repro_inv ~ all_leaf_inv,data_sp_ind,log="xy",pch=16,xaxt="n",col="darkseagreen2")
  abline(0,1)
  mtext(labels.spp.full(all_species[i]), 3, 0)
  
  
  plot(ovule_count ~ leaf_weight_0,data_sp_ind,log="xy",pch=16,xaxt="n",col=col.age(age))
  #points(leaf_weight_0 ~ ovule_count,data_sp_ind,log="xy",pch=1,xaxt="n",col=col.age(age))
  #points(surplus_inv ~ leaf_weight_0,data_sp_ind,log="xy",pch=14,xaxt="n",col=col.age(age))
  #points(repro_inv ~ growth_leaf_pos,data_sp_ind,log="xy",pch=16,xaxt="n",col="darkseagreen4")
  #points(repro_inv ~ all_leaf_inv,data_sp_ind,log="xy",pch=16,xaxt="n",col="darkseagreen2")
  abline(0,1)
  mtext(labels.spp.full(all_species[i]), 3, 0)
  
  #panel 2
  #  plot(all_leaf_and_repro_inv ~ leaf_weight_0,data_sp_ind,log="xy",pch=16,xaxt="n")
  # points(surplus_inv ~ total_weight_0,data_sp_ind,log="xy",pch=16,xaxt="n",col="darkseagreen2")
  # abline(0,1)
  # mtext(labels.spp.full(all_species[i]), 3, 0)
  
}
```


```{r}
yvar <- "repro_inv"
xvar <- "leaf_weight_0"
data <- subset(SummaryCombine,SummaryCombine[[xvar]]>0&SummaryCombine[[yvar]]>0)

fit <- sma(data[[yvar]] ~ data[[xvar]]*data[["species"]], method="SMA", log="xy",slope.test=1)
results <- fit$groupsummary
results$r2 <- format(results$r2,digits=2)
results$pval <- format(results$pval,digits=3)
acc_slopes <- dplyr::select(results,group,n,r2,pval)
names(acc_slopes) <- c("species","n_leaf_weight","r2_leaf_weight","p_leaf_weight")

yvar <- "repro_inv"
xvar <- "growth_leaf_pos"
data <- subset(SummaryCombine,SummaryCombine[[xvar]]>0&SummaryCombine[[yvar]]>0)

fit <- sma(data[[yvar]] ~ data[[xvar]]*data[["species"]], method="SMA",  log="xy",slope.test=1)
results <- fit$groupsummary
results$r2 <- format(results$r2,digits=2)
results$pval <- format(results$pval,digits=3)
acc_slopes2 <- dplyr::select(results,group,n,r2,pval)
names(acc_slopes2) <- c("species","n_growth_leaf","r2_growth_leaf","p_growth_leaf")
acc_slopes <- dplyr::left_join(acc_slopes, acc_slopes2, by ="species")

yvar <- "repro_inv"
xvar <- "all_leaf_inv"
data <- subset(SummaryCombine,SummaryCombine[[xvar]]>0&SummaryCombine[[yvar]]>0)

fit <- sma(data[[yvar]] ~ data[[xvar]]*data[["species"]], method="SMA",  log="xy",slope.test=1)
results <- fit$groupsummary
results$r2 <- format(results$r2,digits=2)
results$pval <- format(results$pval,digits=3)
acc_slopes2 <- dplyr::select(results,group,n,r2,pval)
names(acc_slopes2) <- c("species","n_all_leaf_inv","r2_all_leaf_inv","p_all_leaf_inv")
acc_slopes <- dplyr::left_join(acc_slopes, acc_slopes2, by ="species")


acc_slopes$species <- c("Banksia ericifolia","Boronia ledifolia","Conospermum ericifolium","Epacris microphylla", "Grevillea buxifolia","Grevillea speciosa","Hakea teretifolia", "Hemigenia purpurea","Leucopogon esquamatus","Persoonia lanceolata", "Petrophile pulchella", "Phyllota phylicoides", "Pimelea linifolia", "Pultenaea tuberculata")

write.csv(acc_slopes, file="ms/RA/Table_XX_best_predictor_repro_inv.csv",row.names=FALSE)
```

```{r}
yvar <- "surplus_inv"
xvar <- "leaf_weight_0"
data <- subset(SummaryCombine,SummaryCombine[[xvar]]>0&SummaryCombine[[yvar]]>0)

fit <- sma(data[[yvar]] ~ data[[xvar]]*data[["species"]], method="SMA", log="xy",slope.test=1)
results <- fit$groupsummary
results$r2 <- format(results$r2,digits=2)
results$pval <- format(results$pval,digits=3)
acc_slopes <- dplyr::select(results,group,n,r2,pval)
names(acc_slopes) <- c("species","n_lsurplus","r2_surplus","p_surplus")

yvar <- "all_leaf_and_repro_inv"
xvar <- "leaf_weight_0"
data <- subset(SummaryCombine,SummaryCombine[[xvar]]>0&SummaryCombine[[yvar]]>0)

fit <- sma(data[[yvar]] ~ data[[xvar]]*data[["species"]], method="SMA",  log="xy",slope.test=1)
results <- fit$groupsummary
results$r2 <- format(results$r2,digits=2)
results$pval <- format(results$pval,digits=3)
acc_slopes2 <- dplyr::select(results,group,n,r2,pval)
names(acc_slopes2) <- c("species","n_leaf_and_repro","r2_leaf_and_repro","p_leaf_and_repro")
acc_slopes <- dplyr::left_join(acc_slopes, acc_slopes2, by ="species")

yvar <- "all_leaf_inv"
xvar <- "leaf_weight_0"
data <- subset(SummaryCombine,SummaryCombine[[xvar]]>0&SummaryCombine[[yvar]]>0)

fit <- sma(data[[yvar]] ~ data[[xvar]]*data[["species"]], method="SMA",  log="xy",slope.test=1)
results <- fit$groupsummary
results$r2 <- format(results$r2,digits=2)
results$pval <- format(results$pval,digits=3)
acc_slopes2 <- dplyr::select(results,group,n,r2,pval)
names(acc_slopes2) <- c("species","n_all_leaf_inv","r2_all_leaf_inv","p_all_leaf_inv")
acc_slopes <- dplyr::left_join(acc_slopes, acc_slopes2, by ="species")


acc_slopes$species <- c("Banksia ericifolia","Boronia ledifolia","Conospermum ericifolium","Epacris microphylla", "Grevillea buxifolia","Grevillea speciosa","Hakea teretifolia", "Hemigenia purpurea","Leucopogon esquamatus","Persoonia lanceolata", "Petrophile pulchella", "Phyllota phylicoides", "Pimelea linifolia", "Pultenaea tuberculata")

write.csv(acc_slopes, file="ms/RA/Table_XX_best_predictor_leaf_weight.csv",row.names=FALSE)
```

yvar <- "ovule_count"
xvar <- "leaf_weight_0"
data <- subset(SummaryCombine,SummaryCombine[[xvar]]>0&SummaryCombine[[yvar]]>0)

fit <- sma(data[[yvar]] ~ data[[xvar]]*data[["species"]], method="SMA",  log="xy",slope.test=1)
results <- fit$groupsummary
results
