library(smatr)
library(broom)
library(dplyr)
remake::make("export")

SummaryInd <- readRDS("export/SummaryInd.rds")
SummarySpp <- readRDS("export/SummarySpp.rds")
SummarySppAge <- readRDS("export/SummarySppAge.rds")

source("R/figures.R")

dir.create("ms/Accessory/output/", FALSE, TRUE)

# Data 0. means, counts to include in manuscript number of plants in study
count(subset(SummaryInd, age > 1))

# number of reproducing individuals in study
count(subset(SummaryInd, repro_inv > 0))

# number of individuals in study that produce seed
count(subset(SummaryInd, seed_count > 0))

# average proportion energy going to failed tissues (among reproducing
# individuals)
1 - mean(subset(SummaryInd$prop_success, SummaryInd$seed_count > 0))
mean(subset(SummaryInd$seedset, SummaryInd$seed_count > 0))

# average proportion of energy going to accessory costs (based on propagules,
# not seeds) (among individuals that produce seed)
mean(subset(SummaryInd$prop_accessory_vs_embryo_endo, SummaryInd$seed_count > 0))
mean(subset(SummaryInd$prop_accessory_vs_propagule, SummaryInd$seed_count > 0))
SummarySpp[["mean"]]$prop_accessory_vs_embryo_endo
SummarySpp[["mean"]]$species

# individual with lowest accessory costs
check <- subset(SummaryInd, prop_embryo_endo_vs_all_repro > 0.1)
check <- dplyr::select(check, individual, prop_embryo_endo_vs_all_repro)
max(check$prop_embryo_endo_vs_all_repro)

# r2 for relationship between propagule investment and repro investment across
# all individuals
mod <- sma(propagule_inv ~ repro_inv, subset(SummaryInd, SummaryInd$seedset > 0),
  log = "xy", method = "SMA")

# individual with minimum & maximum accessory costs
min(SummaryInd$prop_accessory_vs_embryo_endo)
max(SummaryInd$prop_embryo_endo_vs_all_repro)

# relationship between plant weight and number of ovules initiated - bigger
# plants produce proportionally fewer buds
mod <- sma(ovule_count ~ total_weight, SummaryInd, method = "SMA", log = "xy",
  slope = 1)
summary(mod)

# RE per seed unrelated to seedsize
mod <- sma(repro_costs ~ embryo_endo_costs, SummarySpp[["mean"]], method = "SMA",
  log = "xy", slope.test = 1)
summary(mod)
plot(repro_costs ~ embryo_endo_costs, SummarySpp[["mean"]], log = "xy", pch = 16,
  col = col.spp(species))

# energy invested in accessory tissues unrelated to seedsize
mod <- sma(accessory_costs_using_ee ~ embryo_endo_costs, data = subset(SummarySpp[["mean"]]),
  method = "SMA", log = "xy", slope.test = 1)
summary(mod)
plot(accessory_costs_using_ee ~ embryo_endo_costs, SummarySpp[["mean"]], log = "xy",
  pch = 16, col = col.spp(species))

# total seed costs correlated with seedsize
mod <- sma(success_costs ~ embryo_endo_costs, SummarySpp[["mean"]], method = "SMA",
  log = "xy")
summary(mod)
plot(success_costs ~ embryo_endo_costs, SummarySpp[["mean"]], log = "xy", pch = 16,
  col = col.spp(species))

# DISCUSSION: lower seedset = higher proportional investment in discarded
# tissues
plot(seedset ~ prop_discarded_vs_all_repro, data = subset(SummarySpp[["mean"]]),
  log = "", pch = 16, col = col.spp(species))
mod <- sma(seedset ~ prop_discarded_vs_all_repro, data = subset(SummarySpp[["mean"]],
  species != "BAER"), log = "", method = "SMA")
summary(mod)

# RESULTS - seed set vs seed size
mod <- sma(seedset ~ embryo_endo_costs, data = subset(SummarySpp[["mean"]]), method = "SMA",
  log = "xy", slope.test = 1)
summary(mod)
plot(seedset ~ embryo_endo_costs, SummarySpp[["mean"]], log = "xy", pch = 16, col = col.spp(species))

# seed size by propagule investment - all species
mod <- sma(propagule_inv ~ embryo_endo_costs, SummarySpp[["mean"]], method = "SMA",
  log = "xy", slope.test = 1)
summary(mod)
plot(propagule_inv ~ embryo_endo_costs, SummarySpp[["mean"]], log = "xy", pch = 16,
  col = col.spp(species))

# seed size by propagule investment - without EPMI
mod <- sma(propagule_inv ~ embryo_endo_costs, data = subset(SummarySpp[["mean"]],
  species != "EPMI"), method = "SMA", log = "xy", slope.test = 1)
summary(mod)
plot(propagule_inv ~ embryo_endo_costs, data = subset(SummarySpp[["mean"]], species !=
  "EPMI"), log = "xy", pch = 16, col = col.spp(species))



mod <- sma(repro_inv ~ embryo_endo_inv, SummaryInd, method = "SMA", log = "xy",
  slope.test = 1)
summary(mod)
plot(repro_inv ~ embryo_endo_inv, SummaryInd, log = "xy", pch = 16, col = col.spp(species))


mod <- sma(repro_inv ~ flower_inv, SummaryInd, method = "SMA", log = "xy", slope.test = 1)
summary(mod)
plot(repro_inv ~ flower_inv, SummaryInd, log = "xy", pch = 16, col = col.spp(species))


# DISCUSSION SECTION ON ESTIMATING REPRODUCTIVE EFFORT: seed size by
# reproductive investment - all species
mod <- sma(repro_inv ~ embryo_endo_costs, SummarySpp[["mean"]], method = "SMA",
  log = "xy", slope.test = 1)
summary(mod)
plot(repro_inv ~ embryo_endo_costs, SummarySpp[["mean"]], log = "xy", pch = 16,
  col = col.spp(species))

# DISCUSSION SECTION ON ESTIMATING REPRODUCTIVE EFFORT: seed size by
# reproductive investment - without EPMI
mod <- sma(repro_inv ~ embryo_endo_costs, data = subset(SummarySpp[["mean"]], species !=
  "EPMI"), method = "SMA", log = "xy", slope.test = 1)
summary(mod)
plot(repro_inv ~ embryo_endo_costs, data = subset(SummarySpp[["mean"]], species !=
  "EPMI"), log = "xy", pch = 16, col = col.spp(species))

# DISCUSSION SECTION ON ESTIMATING REPRODUCTIVE EFFORT: seed size by total
# weight - all species
mod <- sma(total_weight_0 ~ embryo_endo_costs, SummarySpp[["mean"]], method = "SMA",
  log = "xy", slope.test = 1)
summary(mod)
plot(total_weight_0 ~ embryo_endo_costs, SummarySpp[["mean"]], log = "xy", pch = 16,
  col = col.spp(species))


# DISCUSSION - slope of seed size vs repro costs not different from values
# reported in Lord; i.e. testing slope against the 1.1 she had.
slopes <- sma(repro_costs ~ embryo_endo_costs, data = subset(SummarySpp[["mean"]]),
  slope.test = 1.1173, method = "SMA", log = "xy")

checking_slope <- dplyr::select(slopes$groupsummary, r2, pval, Slope, Slope_lowCI,
  Slope_highCI, Int, Int_lowCI, Int_highCI, Slope_test_p)



yvar <- "ovule_count"
xvar <- "repro_inv"
data <- subset(SummaryInd, SummaryInd[[xvar]] > 0 & SummaryInd[[yvar]] > 0)

fit <- sma(data[[yvar]] ~ data[[xvar]] * data[["species"]], method = "SMA", log = "xy")
plot(fit, xlab = xvar, ylab = yvar, pch = 16, col = col.spp(fit$groupsummary$group),
  lty = "solid", ylim = c(1, 3e+05), xlim = c(1, 30000))
fit$groupsummary

yvar <- "ovule_count"
xvar <- "leaf_area_0"
data <- subset(SummaryInd, SummaryInd[[xvar]] > 0 & SummaryInd[[yvar]] > 0)

fit <- sma(data[[yvar]] ~ data[[xvar]] * data[["species"]], method = "SMA", log = "xy",
  slope.test = 1)
plot(fit, xlab = xvar, ylab = yvar, pch = 16, col = col.spp(fit$groupsummary$group),
  lty = "solid", ylim = c(1, 3e+05), xlim = c(1, 30000))
fit$groupsummary

yvar <- "propagule_inv"
xvar <- "leaf_area_0"
data <- subset(SummaryInd, SummaryInd[[xvar]] > 0 & SummaryInd[[yvar]] > 0)

fit <- sma(data[[yvar]] ~ data[[xvar]] * data[["species"]], method = "SMA", log = "xy",
  slope.test = 1)
plot(fit, xlab = xvar, ylab = yvar, pch = 16, col = col.spp(fit$groupsummary$group),
  lty = "solid", ylim = c(1, 3e+05), xlim = c(1, 30000))
fit$groupsummary

xvar <- "propagule_inv"
yvar <- "repro_inv"
data <- subset(SummaryInd, SummaryInd[[xvar]] > 0 & SummaryInd[[yvar]] > 0)

fit <- sma(data[[yvar]] ~ data[[xvar]] * data[["species"]], method = "SMA", log = "xy",
  slope.test = 1)
plot(fit, xlab = xvar, ylab = yvar, pch = 16, col = col.spp(fit$groupsummary$group),
  lty = "solid", ylim = c(1, 3e+05), xlim = c(1, 30000))
fit$groupsummary



# Table 1 showing accessory cost values for the species
acc_costs_spp <- dplyr::select(SummarySpp[["mean"]], species, embryo_endo_costs,
  seedset, repro_costs, prop_prepollen_discarded_vs_all_repro, prop_postpollen_discarded_vs_all_repro,
  prop_pollen_attract_vs_all_repro, prop_pack_disp_vs_all_repro, prop_embryo_endo_vs_all_repro,
  success_costs, prop_pollen_attract_vs_all_repro, prop_pollen_attract_vs_success,
  prop_provisioning_vs_success, embryo_endo_costs, packaging_dispersal_costs,
  pollen_attract_costs, repro_inv, seed_count)
acc_costs_spp$species <- labels.spp.full()
acc_costs_spp_format <- acc_costs_spp
for (i in c("embryo_endo_costs", "seedset", "repro_costs", "prop_prepollen_discarded_vs_all_repro",
  "prop_postpollen_discarded_vs_all_repro", "prop_pollen_attract_vs_all_repro",
  "prop_pack_disp_vs_all_repro", "prop_embryo_endo_vs_all_repro", "success_costs",
  "prop_pollen_attract_vs_success", "prop_provisioning_vs_success", "packaging_dispersal_costs",
  "pollen_attract_costs", "repro_inv")) {
  acc_costs_spp_format[[i]] <- round(acc_costs_spp_format[[i]], digits = 3)
}

for (i in c("seed_count")) {
  acc_costs_spp_format[[i]] <- round(acc_costs_spp_format[[i]], digits = 1)
}

for (i in c("prop_prepollen_discarded_vs_all_repro", "prop_postpollen_discarded_vs_all_repro",
  "prop_pack_disp_vs_all_repro", "prop_embryo_endo_vs_all_repro", "prop_pollen_attract_vs_all_repro",
  "prop_pollen_attract_vs_success", "prop_provisioning_vs_success")) {
  acc_costs_spp_format[[i]] <- round(100 * acc_costs_spp_format[[i]], digits = 2)
}

acc_costs_spp_format <- acc_costs_spp_format[order(acc_costs_spp_format$embryo_endo_costs),
  ]

write.csv(acc_costs_spp_format, file = "ms/Accessory/output/Table01_AccCosts_by_Species.csv",
  row.names = FALSE)



# Table 2. Slopes for various embryo size by acc cost regressions

par(mfrow = c(2, 3), cex = 1, omi = c(0.6, 0.1, 0.05, 0.05), mai = c(0.1, 0.8,
  0.05, 0.05))
options(scipen = 5)

library(smatr)

# repro costs
slopes <- sma(repro_costs ~ embryo_endo_costs, data = subset(SummarySpp[["mean"]]),
  slope.test = 1, method = "SMA", log = "xy")

acc_by_species <- dplyr::select(slopes$groupsummary, r2, pval, Slope, Slope_lowCI,
  Slope_highCI, Int, Int_lowCI, Int_highCI, Slope_test_p)

# accessory costs
slopes <- sma(accessory_costs ~ embryo_endo_costs, data = subset(SummarySpp[["mean"]]),
  slope.test = 1, method = "SMA", log = "xy")

acc_by_species <- dplyr::bind_rows(acc_by_species, dplyr::select(slopes$groupsummary,
  r2, pval, Slope, Slope_lowCI, Slope_highCI, Int, Int_lowCI, Int_highCI, Slope_test_p))

# success costs
slopes <- sma(success_costs ~ embryo_endo_costs, data = subset(SummarySpp[["mean"]]),
  slope.test = 1, method = "SMA", log = "xy")

acc_by_species <- dplyr::bind_rows(acc_by_species, dplyr::select(slopes$groupsummary,
  r2, pval, Slope, Slope_lowCI, Slope_highCI, Int, Int_lowCI, Int_highCI, Slope_test_p))

# pollen attraction costs
slopes <- sma(pollen_attract_costs ~ embryo_endo_costs, data = subset(SummarySpp[["mean"]]),
  slope.test = 1, method = "SMA", log = "xy")

acc_by_species <- dplyr::bind_rows(acc_by_species, dplyr::select(slopes$groupsummary,
  r2, pval, Slope, Slope_lowCI, Slope_highCI, Int, Int_lowCI, Int_highCI, Slope_test_p))

# provisioning costs
slopes <- sma(provisioning_costs ~ embryo_endo_costs, data = subset(SummarySpp[["mean"]]),
  slope.test = 1, log = "xy", method = "SMA")

acc_by_species <- dplyr::bind_rows(acc_by_species, dplyr::select(slopes$groupsummary,
  r2, pval, Slope, Slope_lowCI, Slope_highCI, Int, Int_lowCI, Int_highCI, Slope_test_p))

# discarded costs
slopes <- sma(discarded_costs ~ embryo_endo_costs, data = subset(SummarySpp[["mean"]]),
  slope.test = 1, method = "SMA", log = "xy")

acc_by_species <- dplyr::bind_rows(acc_by_species, dplyr::select(slopes$groupsummary,
  r2, pval, Slope, Slope_lowCI, Slope_highCI, Int, Int_lowCI, Int_highCI, Slope_test_p))

# discarded prepollen costs
slopes <- sma(prepollen_discarded_costs ~ embryo_endo_costs, data = subset(SummarySpp[["mean"]]),
  slope.test = 1, log = "xy", method = "SMA")

acc_by_species <- dplyr::bind_rows(acc_by_species, dplyr::select(slopes$groupsummary,
  r2, pval, Slope, Slope_lowCI, Slope_highCI, Int, Int_lowCI, Int_highCI, Slope_test_p))

# discarded postpollen costs
slopes <- sma(postpollen_aborted_costs ~ embryo_endo_costs, data = subset(SummarySpp[["mean"]]),
  slope.test = 1, log = "xy", method = "SMA")

acc_by_species <- dplyr::bind_rows(acc_by_species, dplyr::select(slopes$groupsummary,
  r2, pval, Slope, Slope_lowCI, Slope_highCI, Int, Int_lowCI, Int_highCI, Slope_test_p))

acc_by_species$variable <- c("total reproductive costs", "total accessory costs",
  "seed costs", "pollen attraction costs", "provisioning costs", "failed tissues costs",
  "failed pre-pollination costs", "failed provisioning costs")

acc_by_species$Slope <- format(acc_by_species$Slope, nsmall = 2, digits = 2)
acc_by_species$Slope_lowCI <- format(acc_by_species$Slope_lowCI, nsmall = 2, digits = 2)
acc_by_species$Slope_highCI <- format(acc_by_species$Slope_highCI, nsmall = 2,
  digits = 2)

acc_by_species$slope2 <- paste(acc_by_species$Slope, " (", acc_by_species$Slope_lowCI,
  " - ", acc_by_species$Slope_highCI, ")", sep = "")

acc_by_species$r2 <- format(acc_by_species$r2, digits = 2)
acc_by_species$pval <- format_p(acc_by_species$pval)
acc_by_species$Slope_test_p <- format(acc_by_species$Slope_test_p, digits = 2)
acc_slopes <- dplyr::select(acc_by_species, variable, r2, slope2, Slope_test_p)

write.csv(acc_slopes, file = "ms/Accessory/output/Table02_acc_cost_embryo_size_slopes.csv",
  row.names = FALSE)


# Table 3. correlations with total repro inv
results <- as.data.frame(matrix(vector(), 0, 9, dimnames = list(c(), c("xvar",
  "variable", "r2", "pval", "Slope", "Slope_lowCI", "Slope_highCI", "Int", "Slope_test_p"))),
  stringsAsFactors = F)

for (i in c("total_weight", "embryo_endo_inv", "propagule_inv", "fruit_inv", "flower_inv",
  "flower_inv2", "success_inv", "prepollen_success_inv", "pack_disp_success_inv",
  "discarded_inv")) {
  for (j in c("repro_inv")) {
    yvar <- i
    xvar <- j
    data <- subset(SummaryInd, SummaryInd[[xvar]] > 0 & SummaryInd[[yvar]] >
      0)
    fit <- sma(data[[yvar]] ~ data[[xvar]], method = "OLS", log = "xy", slope.test = 1)
    temp <- as.data.frame(c(xvar, yvar, dplyr::select(fit$groupsummary, r2,
      pval, Slope, Slope_lowCI, Slope_highCI, Int, Slope_test_p, n)))
    colnames(temp)[2] <- "variable"
    colnames(temp)[1] <- "xvar"
    results <- dplyr::bind_rows(results, temp)
  }
}

results$slope2 <- paste(format(results$Slope, digits = 3), " (", format(results$Slope_lowCI,
  digits = 3), " - ", format(results$Slope_highCI, digits = 3), ")", sep = "")

results$r2 <- format(results$r2, digits = 3)
results$pval <- format(results$pval, digits = 3)
results$Slope_test_p <- format(results$Slope_test_p, digits = 3)
results <- dplyr::select(results, variable, n, r2, pval)


write.csv(results, file = "ms/Accessory/output/Table03_correlations_with_repro_inv.csv",
  row.names = FALSE)


# Figure 01. predictions

png("ms/Accessory/output/Figure01b_predictions.png", height = 2.5, width = 7.3,
  units = "in", res = 300)

windowsFonts(Myriad = windowsFont("Myriad Pro"))


par(mfrow = c(1, 3), cex = 1, omi = c(0.1, 0.35, 0.05, 0.05), mai = c(0.7, 0.5,
  0.05, 0.15))
plot(0, 0, ylim = c(0, 1), yaxt = "n", xaxt = "n", xlim = c(0, 1), col = "white",
  xlab = "", ylab = "")
segments(x0 = 0.2, x1 = 0.9, y0 = 0.9, y1 = 0.2, lty = 1)
# axis(3, at=c(0,1),labels=c('lower','higher'),family='Myriad',cex.axis=0.875)
# axis(2,
# at=c(0,1),labels=c('lower','higher'),family='Myriad',cex.axis=0.875,las=2)
# mtext(text='Trade-off 1:',side=3,outer=FALSE,adj=0,line=1.6) mtext(text='Seed
# count-reproductive costs',side=3,outer=FALSE,adj=0,line=0.7)
mtext(text = "seed count,", side = 2, line = 1.3, outer = FALSE, family = "Myriad",
  cex = 0.875)
mtext(text = "scaled to plant size", side = 2, line = 0.4, outer = FALSE, family = "Myriad",
  cex = 0.875)
mtext(text = "reproductive costs", side = 1, line = 0.3, outer = FALSE, family = "Myriad",
  cex = 0.875)
mtext(text = "(mg/seed)", side = 1, line = 1.2, outer = FALSE, family = "Myriad",
  cex = 0.875)

plot(0, 0, ylim = c(0, 1), yaxt = "n", xaxt = "n", xlim = c(0, 1), col = "white",
  xlab = "", ylab = "")
segments(x0 = 0.1, x1 = 0.8, y0 = 0.8, y1 = 0.1, lty = 1)
# axis(3, at=c(0,1),labels=c('lower','higher'),family='Myriad',cex.axis=0.875)
# axis(2, at=c(0,1),labels=c('lower','higher'),cex.axis=1,las=2)
# mtext(text='Trade-off 2.',side=3,outer=FALSE,adj=0,line=1.6)
# mtext(text='Ovule count-pollen attraction
# costs',side=3,outer=FALSE,adj=0,line=0.7)
mtext(text = "mature ovules count,", side = 2, line = 1.3, outer = FALSE, family = "Myriad",
  cex = 0.875)
mtext(text = "scaled to plant size", side = 2, line = 0.4, outer = FALSE, family = "Myriad",
  cex = 0.875)
mtext(text = "pollen attraction costs", side = 1, line = 0.3, outer = FALSE, family = "Myriad",
  cex = 0.875)
mtext(text = "(mg/seed)", side = 1, line = 1.2, outer = FALSE, family = "Myriad",
  cex = 0.875)

plot(0, 0, ylim = c(0, 1), yaxt = "n", xaxt = "n", xlim = c(0, 1), col = "white",
  xlab = "", ylab = "")
segments(x0 = 0.1, x1 = 0.9, y0 = 0.9, y1 = 0.1, lty = 1)
# axis(3, at=c(0,1),labels=c('lower','higher'),family='Myriad',cex.axis=0.875)
# axis(2, at=c(0,1),labels=c('lower','higher'),cex.axis=1,las=2)
# mtext(text='Trade-off 3.',side=3,outer=FALSE,adj=0,line=1.6)
# mtext(text='Choosiness-pollen attraction
# costs',side=3,outer=FALSE,adj=0,line=0.7)
mtext(text = "choosiness", side = 2, line = 1.3, outer = FALSE, family = "Myriad",
  cex = 0.875)
mtext(text = "(ovule:seed)", side = 2, line = 0.4, outer = FALSE, family = "Myriad",
  cex = 0.875)
mtext(text = "pollen attraction costs,", side = 1, line = 0.3, family = "Myriad",
  cex = 0.875)
mtext(text = "scaled to plant size", side = 1, line = 1.2, family = "Myriad", cex = 0.875)
mtext(text = "(mg/seed)", side = 1, line = 2.1, outer = FALSE, family = "Myriad",
  cex = 0.875)

dev.off()

# Figure 2

png("ms/Accessory/output/Figure02.png", height = 9, width = 6, units = "in", res = 300)

par(mfrow = c(3, 2), cex = 1, omi = c(0.1, 0.1, 0.05, 0.05), mai = c(0.7, 0.8,
  0.05, 0.05))
options(scipen = 5)

# panel a
plot(scaled_seed_count ~ repro_costs, SummarySpp[["mean"]], pch = 16, log = "xy",
  col = venetian_red, xlim = c(0.01, 5000), ylim = c(5e-04, 500), yaxt = "n",
  xaxt = "n", ylab = "", xlab = "", cex = 1)
points(scaled_reach_flowering_count ~ pollen_attract_costs, SummarySpp[["mean"]],
  pch = 16, col = cerulean, cex = 1)

axis(2, at = c(0.01, 1, 100), labels = c(0.01, 1, 100), cex.axis = 0.8, las = 2)
axis(1, at = c(0.1, 10, 1000), labels = c(0.1, 10, 1000), cex.axis = 0.8)
mtext("costs to make a successful unit", 1, line = 1.8, cex = 0.7)
mtext("count, scaled to leaf area", 2, line = 2.3, cex = 0.7)

mod_seed <- sma(scaled_seed_count ~ repro_costs, data = SummarySpp[["mean"]], log = "xy",
  slope = -1, method = "SMA")
mod_ovule <- sma(scaled_reach_flowering_count ~ pollen_attract_costs, data = SummarySpp[["mean"]],
  log = "xy", method = "SMA", slope.test = mod_seed$groupsummary["Slope"][[1]],
  elev.test = mod_seed$coef[[1]][1, 1])

line_function_log_xy(SummarySpp[["mean"]]$repro_costs, mod_seed, 1, col = venetian_red)
line_function_log_xy(SummarySpp[["mean"]]$pollen_attract_costs, mod_ovule, 1, col = cerulean)
legend("bottomleft", legend = c("seed count by reproductive costs", "mature ovules count by",
  "pollen-attraction costs"), col = c(venetian_red, cerulean, "white"), pch = c(16,
  16, 16), bty = "n", cex = 0.7)
extra.top.left.logxy("a", 2, 1)

mod_seed$groupsummary$r2
mod_ovule$groupsummary$r2

# panel b
plot(prop_prepollen_success~ choosiness,  SummarySpp[["mean"]], log = "x")
,
  col = cerulean, pch = 16, xlim = c(0.002, 0.3), ylim = c(3, 800), yaxt = "n",
  cex = 1, ylab = "", xlab = "", xaxt = "n")
axis(2, at = c(10, 100, 1000), labels = c(10, 100, 1000), cex.axis = 0.8, las = 2)
axis(1, at = c(0.01, 0.1), labels = c(0.01, 0.1), cex.axis = 0.8)
mtext("pollen-attractions costs, scaled to leaf area", 1, line = 1.7, cex = 0.7)
mtext("choosiness (ovules:seed)", 2, line = 2.3, cex = 0.7)
# mod_choosy <-
# sma(choosiness2~scaled_pollen_attract_costs,data=subset(SummarySpp[['mean']]),log='xy',slope=-1,method='SMA')
mod_choosy2 <- sma(choosiness2 ~ scaled_pollen_attract_costs, data = subset(SummarySpp[["mean"]],
  species != "EPMI"), log = "xy", slope = -1, method = "SMA")
line_function_log_xy(SummarySpp[["mean"]]$scaled_pollen_attract_costs, mod_choosy2,
  1, col = cerulean)
extra.top.left.logxy("b", 2, 1)

mod_choosy2$groupsummary$r2

# panel c - success proportions
plot(prop_prepollen_success ~ embryo_endo_costs, SummarySpp[["mean"]], pch = 16,
  log = "x", col = cerulean, ylim = c(-0.1, 1.1), xlim = c(0.01, 100), xlab = "",
  ylab = "", xaxt = "n", yaxt = "n")
points(prop_prepollen_discarded ~ embryo_endo_costs, SummarySpp[["mean"]], pch = 16,
  col = venetian_red)
extra.top.left.logx("c", 2, 1)
axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1),
  cex.axis = 0.8, las = 2)
axis(1, at = c(0.01, 1, 100), labels = c(0.01, 1, 100), cex.axis = 0.8)
mtext("seed size (mg)", 1, line = 1.8, cex = 0.7)
mtext("prop. total pollen-attraction investment", 2, line = 2.3, cex = 0.7)
legend("bottomleft", legend = c("successful ovules", "discarded ovules"), col = c(cerulean,
  venetian_red), pch = c(16, 16), bty = "n", cex = 0.7)

mod_prop_prepollen_success <- sma(prop_prepollen_success ~ embryo_endo_costs, SummarySpp[["mean"]],
  log = "x", method = "OLS")
mod_prop_prepollen_discarded <- sma(prop_prepollen_discarded ~ embryo_endo_costs,
  SummarySpp[["mean"]], log = "x", method = "OLS")

line_function_log_x(SummarySpp[["mean"]]$embryo_endo_costs, mod_prop_prepollen_success,
  1, col = cerulean)
line_function_log_x(SummarySpp[["mean"]]$embryo_endo_costs, mod_prop_prepollen_discarded,
  1, col = venetian_red)

# panel d - pollen-attraction proportions
plot(prop_postpollen_success ~ embryo_endo_costs, SummarySpp[["mean"]], pch = 16,
  log = "x", col = cerulean, ylim = c(-0.1, 1.1), xlim = c(0.01, 100), xlab = "",
  ylab = "", xaxt = "n", yaxt = "n")
points(prop_postpollen_discarded ~ embryo_endo_costs, SummarySpp[["mean"]], pch = 16,
  col = venetian_red)
extra.top.left.logx("d", 2, 1)
axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1),
  cex.axis = 0.8, las = 2)
axis(1, at = c(0.01, 1, 100), labels = c(0.01, 1, 100), cex.axis = 0.8)
mtext("seed size (mg)", 1, line = 1.8, cex = 0.7)
mtext("prop. total provisioning investment", 2, line = 2.3, cex = 0.7)
legend("bottomleft", legend = c("successful zygotes", "aborted zygotes"), col = c(cerulean,
  venetian_red), pch = c(16, 16), bty = "n", cex = 0.7)

mod_prop_postpollen_success <- sma(prop_postpollen_success ~ embryo_endo_costs,
  SummarySpp[["mean"]], log = "x", method = "OLS")
mod_prop_postpollen_discarded <- sma(prop_postpollen_discarded ~ embryo_endo_costs,
  SummarySpp[["mean"]], log = "x", method = "OLS")

line_function_log_x(SummarySpp[["mean"]]$embryo_endo_costs, mod_prop_postpollen_success,
  1, col = cerulean)
line_function_log_x(SummarySpp[["mean"]]$embryo_endo_costs, mod_prop_postpollen_discarded,
  1, col = venetian_red)

# panel e - provisioning proportions
plot(prop_pollen_attract_vs_success ~ embryo_endo_costs, SummarySpp[["mean"]],
  pch = 16, log = "x", col = cerulean, ylim = c(-0.1, 1.1), xlim = c(0.01, 100),
  xlab = "", ylab = "", xaxt = "n", yaxt = "n")
points(prop_provisioning_vs_success ~ embryo_endo_costs, SummarySpp[["mean"]],
  pch = 16, col = venetian_red)
extra.top.left.logx("e", 2, 1)
axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(0, 0.2, 0.4, 0.6, 0.8, 1),
  cex.axis = 0.8, las = 2)
axis(1, at = c(0.01, 1, 100), labels = c(0.01, 1, 100), cex.axis = 0.8)
mtext("seed size (mg)", 1, line = 1.8, cex = 0.7)
mtext("prop. successful investment", 2, line = 2.3, cex = 0.7)
legend("bottomleft", legend = c("pollen attraction costs", "provisioning costs"),
  col = c(cerulean, venetian_red), pch = c(16, 16), bty = "n", cex = 0.7)

mod_prop_pollen_attract_vs_success <- sma(prop_pollen_attract_vs_success ~ embryo_endo_costs,
  SummarySpp[["mean"]], log = "x", method = "OLS")
mod_prop_provisioning_vs_success <- sma(prop_provisioning_vs_success ~ embryo_endo_costs,
  SummarySpp[["mean"]], log = "x", method = "OLS")

line_function_log_x(SummarySpp[["mean"]]$embryo_endo_costs, mod_prop_pollen_attract_vs_success,
  1, col = cerulean)
line_function_log_x(SummarySpp[["mean"]]$embryo_endo_costs, mod_prop_provisioning_vs_success,
  1, col = venetian_red)

# panel f - change in slope of 'success costs'
plot(pollen_attract_costs ~ embryo_endo_costs, SummarySpp[["mean"]], pch = 16,
  log = "xy", col = cerulean, xlim = c(0.01, 100), ylim = c(0.01, 20000), xlab = "",
  ylab = "", xaxt = "n", yaxt = "n")
points(provisioning_costs ~ embryo_endo_costs, SummarySpp[["mean"]], pch = 16,
  col = venetian_red)
extra.top.left.logxy("f", 2, 1)
axis(2, at = c(0.01, 1, 100, 10000), labels = c(0.01, 1, 100, 10000), cex.axis = 0.8,
  las = 2)
axis(1, at = c(0.01, 1, 100), labels = c(0.01, 1, 100), cex.axis = 0.8)
mtext("seed size (mg)", 1, line = 1.8, cex = 0.7)
mtext("success cost (mg/seed)", 2, line = 2.3, cex = 0.7)
legend("bottomright", legend = c("pollen attraction", "provisioning"), col = c(cerulean,
  venetian_red), pch = c(16, 16), bty = "n", cex = 0.7)


mod_repro_costs <- sma(repro_costs ~ embryo_endo_costs, SummarySpp[["mean"]], log = "xy",
  method = "SMA")
mod_pollen_attract_costs <- sma(pollen_attract_costs ~ embryo_endo_costs, SummarySpp[["mean"]],
  log = "xy", method = "SMA", slope.test = mod_repro_costs$groupsummary$Slope_lowCI)
mod_provisioning_costs <- sma(provisioning_costs ~ embryo_endo_costs, SummarySpp[["mean"]],
  log = "xy", method = "SMA", slope.test = mod_repro_costs$groupsummary$Slope)

line_function_log_xy(SummarySpp[["mean"]]$embryo_endo_costs, mod_pollen_attract_costs,
  1, col = cerulean)
line_function_log_xy(SummarySpp[["mean"]]$embryo_endo_costs, mod_provisioning_costs,
  1, col = venetian_red)

dev.off()

Figure2_summary <- dplyr::select(mod_seed$groupsummary, r2, pval, Slope, Slope_lowCI,
  Slope_highCI)
Figure2_summary <- dplyr::bind_rows(Figure2_summary, dplyr::select(mod_ovule$groupsummary,
  r2, pval, Slope, Slope_lowCI, Slope_highCI))
Figure2_summary <- dplyr::bind_rows(Figure2_summary, dplyr::select(mod_choosy2$groupsummary,
  r2, pval, Slope, Slope_lowCI, Slope_highCI))
Figure2_summary <- dplyr::bind_rows(Figure2_summary, dplyr::select(mod_prop_prepollen_success$groupsummary,
  r2, pval, Slope, Slope_lowCI, Slope_highCI))
Figure2_summary <- dplyr::bind_rows(Figure2_summary, dplyr::select(mod_prop_postpollen_success$groupsummary,
  r2, pval, Slope, Slope_lowCI, Slope_highCI))
Figure2_summary <- dplyr::bind_rows(Figure2_summary, dplyr::select(mod_prop_pollen_attract_vs_success$groupsummary,
  r2, pval, Slope, Slope_lowCI, Slope_highCI))
Figure2_summary$names <- c("seed count by reproductive costs", "mature ovule count by pollen attraction costs",
  "choosiness by pollen-attraction costs", "prop of pollen attraction to success by seed size",
  "prop of provisioning to success by seed size", "prop of successful investment to pollen attraction by seed size")


# Table with above model results
mod_prop_pollen_attract_vs_all_repro <- sma(prop_pollen_attract_vs_all_repro ~
  embryo_endo_costs, SummarySpp[["mean"]], log = "x", method = "OLS")
mod_prop_provisioning_vs_success <- sma(prop_provisioning_vs_success ~ embryo_endo_costs,
  SummarySpp[["mean"]], log = "x", method = "OLS")
mod_prop_prepollen_success <- sma(prop_prepollen_success ~ embryo_endo_costs, SummarySpp[["mean"]],
  log = "x", method = "OLS")
mod_prop_prepollen_success_SMA <- sma(prop_prepollen_success ~ embryo_endo_costs,
  SummarySpp[["mean"]], log = "x", method = "SMA")
mod_prop_prepollen_discarded <- sma(prop_prepollen_discarded ~ embryo_endo_costs,
  SummarySpp[["mean"]], log = "x", method = "OLS")
mod_prop_postpollen_success <- sma(prop_postpollen_success ~ embryo_endo_costs,
  SummarySpp[["mean"]], log = "x", method = "OLS")
mod_prop_postpollen_discarded <- sma(prop_postpollen_discarded ~ embryo_endo_costs,
  SummarySpp[["mean"]], log = "x", method = "OLS")

energy_prop <- dplyr::select(mod_prop_pollen_attract_vs_all_repro$groupsummary,
  r2, pval, Slope, Slope_lowCI, Slope_highCI, Int, Int_lowCI, Int_highCI)
energy_prop$test[1] <- "Proportion sucess costs to prepollen"

temp <- dplyr::select(mod_prop_prepollen_success$groupsummary, r2, pval, Slope,
  Slope_lowCI, Slope_highCI, Int, Int_lowCI, Int_highCI)
temp$test[1] <- "Proportion prepollen costs to success"
energy_prop <- rbind(energy_prop, temp)

temp <- dplyr::select(mod_prop_postpollen_success$groupsummary, r2, pval, Slope,
  Slope_lowCI, Slope_highCI, Int, Int_lowCI, Int_highCI)
temp$test[1] <- "Proportion postpollen costs to success"
energy_prop <- rbind(energy_prop, temp)



# Table S1.correlations between repro investment and flower investment by
# species (noted in Discussion)


yvar <- "repro_inv"
xvar <- "embryo_endo_inv"
data <- subset(SummaryInd, SummaryInd[[xvar]] > 0 & SummaryInd[[yvar]] > 0)

fit <- sma(data[[yvar]] ~ data[[xvar]] * data[["species"]], method = "SMA", log = "xy")
results <- fit$groupsummary
results$r2 <- format(results$r2, digits = 2)
results$pval <- format(results$pval, digits = 3)
acc_slopes <- dplyr::select(results, group, n, r2, pval)

yvar <- "repro_inv"
xvar <- "flower_inv"
data <- subset(SummaryInd, SummaryInd[[xvar]] > 0 & SummaryInd[[yvar]] > 0)

fit <- sma(data[[yvar]] ~ data[[xvar]] * data[["species"]], method = "SMA", log = "xy")
results <- fit$groupsummary
results$r2 <- format(results$r2, digits = 2)
results$pval <- format(results$pval, digits = 3)
acc_slopes2 <- dplyr::select(results, group, n, r2, pval)

acc_slopes <- merge(acc_slopes, acc_slopes2, by = "group")

names(acc_slopes) <- c("species", "n_propagule", "r2_propagule", "p_propagule",
  "n_flower", "r2_flower", "p_flower")

write.csv(acc_slopes, file = "ms/Accessory/output/Table_S1_extra_flower_invest_vs_all_repro_inv.csv",
  row.names = FALSE)


# Table 7 extra. correlations with total repro inv


results <- as.data.frame(matrix(vector(), 0, 9, dimnames = list(c(), c("xvar",
  "variable", "r2", "pval", "Slope", "Slope_lowCI", "Slope_highCI", "Int", "Slope_test_p"))),
  stringsAsFactors = F)

for (i in c("total_weight", "repro_inv", "discarded_inv", "success_inv", "propagule_inv",
  "packaging_dispersal_inv", "prepollen_success_inv", "fruit_inv", "flower_inv")) {
  for (j in c("total_weight", "leaf_area_0", "diameter_0", "repro_inv", "propagule_inv")) {
    yvar <- i
    xvar <- j
    data <- subset(SummaryInd, SummaryInd[[xvar]] > 0 & SummaryInd[[yvar]] >
      0)
    fit <- sma(data[[yvar]] ~ data[[xvar]], method = "SMA", log = "xy", slope.test = 1)
    temp <- as.data.frame(c(xvar, yvar, dplyr::select(fit$groupsummary, r2,
      pval, Slope, Slope_lowCI, Slope_highCI, Int, Slope_test_p, n)))
    colnames(temp)[2] <- "variable"
    colnames(temp)[1] <- "xvar"
    results <- bind_rows(results, temp)
  }
}

write.csv(results, file = "ms/Accessory/output/output/Table07_EXTRA_correlations_with_repro_inv.csv",
  row.names = FALSE)


### GOOD VISUALS - not currently in manuscript


xvar <- "leaf_area_0"
yvar <- "flower_inv"
data <- subset(SummaryInd, SummaryInd[[xvar]] > 0 & SummaryInd[[yvar]] > 0)

fit <- sma(data[[yvar]] ~ data[[xvar]] * data[["species"]], method = "SMA", log = "xy")
plot(fit, col = col.spp(fit$groupsummary$group), log = "xy", ylim = c(1, 3e+05),
  xlim = c(1, 1e+07), xlab = "", ylab = "", lty = "solid", pch = 16)
fit$groupsummary

plot(propagule_costs ~ total_weight_0, SummaryInd, log = "xy", pch = 16, col = col.spp(species))
plot(propagule_costs ~ total_weight_0, SummarySpp[["mean"]], log = "xy", pch = 16,
  col = col.spp(species))



# Although very little energy goes into propagules (or embryos) a notable
# amount of energy is required for successful seeds to form


png("ms/Accessory/output/FigureXX_success_vs_discarded.png", height = 6, width = 8,
  units = "in", res = 300)


par(mfrow = c(1, 2), cex = 1, omi = c(0.8, 0.1, 0.1, 0.1), mai = c(0.1, 0.8, 0.1,
  0.1))

xvar <- "repro_inv"
yvar <- "prop_propagule"

data <- subset(SummaryInd, SummaryInd[[xvar]] > 0 & SummaryInd[[yvar]] > 0)

fit <- sma(data[[yvar]] ~ data[[xvar]] * data[["species"]], method = "SMA", log = "xy")
plot(fit, col = col.spp(fit$groupsummary$group), pch = 16, lty = "solid", ylim = c(0.001,
  1), xlab = "", ylab = "prop repro energy to propagules")
abline(-0.30103, 0, lwd = 2)
abline(-1, 0, lwd = 2)

xvar <- "repro_inv"
yvar <- "prop_success"

data <- subset(SummaryInd, SummaryInd[[xvar]] > 0 & SummaryInd[[yvar]] > 0)

fit <- sma(data[[yvar]] ~ data[[xvar]] * data[["species"]], method = "SMA", log = "xy")
plot(fit, col = col.spp(fit$groupsummary$group), pch = 16, lty = "solid", ylim = c(0.001,
  1), xlab = "", ylab = "prop repro energy to success")
abline(-0.30103, 0, lwd = 2)
abline(-1, 0, lwd = 2)
mtext(text = "reproductive investment (mg)", side = 1, line = 2, outer = TRUE)

dev.off()


# Neither within nor across species, does the proportion of reproductive energy
# spent on 'success' correlate with total reproductive investment


par(mfrow = c(1, 1), cex = 1, omi = c(0.1, 0.1, 0.1, 0.05), mai = c(1.2, 1.2, 0.1,
  0.05))

xvar <- "repro_inv"
yvar <- "prop_success"
data <- subset(SummaryInd, SummaryInd[[xvar]] > 0 & SummaryInd[[yvar]] > 0)

fit <- sma(data[[yvar]] ~ data[[xvar]] * data[["species"]], log = "x", method = "SMA")
plot(fit, col = col.spp(fit$groupsummary$group), xlab = xvar, ylab = yvar, lty = "solid",
  pch = 16, log = "x")


# when reproductive investment is scaled by plant size there is no significant
# difference by seed size - so increase in repro investment with seed size
# simply because species with bigger seeds are bigger plants


plot(scaled_repro_inv ~ propagule_costs, SummaryInd, col = col.spp(species), pch = 16,
  log = "xy")
points(scaled_repro_inv ~ propagule_costs, SummarySpp[["mean"]], cex = 2, col = col.spp(species),
  pch = 16)
mod <- sma(scaled_repro_inv ~ propagule_costs, SummarySpp[["mean"]], log = "xy",
  method = "SMA")
summary(mod)


# This effect is true across species, since larger-seeded species produce fewer
# seeds and have higher per seed accessory costs and true within species, since
# absolute accessory investment is a function of plant size not seedset


png("ms/Accessory/output/FigureXX_EXTRA_acc_cost_by_seed_count.png", height = 5,
  width = 5, units = "in", res = 300)

par(mfrow = c(1, 1), cex = 1, omi = c(0.1, 0.1, 0.1, 0.1), mai = c(1, 1, 0.5, 0.5))

xvar <- "seed_count"
yvar <- "accessory_costs"
data <- subset(SummaryInd, SummaryInd[[xvar]] > 0 & SummaryInd[[yvar]] > 0)

fit <- sma(data[[yvar]] ~ data[[xvar]] * data[["species"]], method = "SMA", log = "xy")
plot(fit, col = col.spp(fit$groupsummary$group), pch = 16, xlab = xvar, ylab = yvar,
  lty = "solid")

points(accessory_costs ~ seed_count, data = subset(SummarySpp[["mean"]], accessory_costs >
  0 & seed_count > 0), col = "black", pch = 16, cex = 2.5)
points(accessory_costs ~ seed_count, data = subset(SummarySpp[["mean"]], accessory_costs >
  0 & seed_count > 0), col = col.spp(species), pch = 16, cex = 2)
mod <- sma(accessory_costs ~ seed_count, data = subset(SummarySpp[["mean"]]), method = "SMA",
  log = "xy")
# words.top.right.logxy(mod)
legend_with_r2(results, "bottomleft")

write.csv(results, file = "ms/Accessory/output/TableXX_seedcount_vs_accessory.csv",
  row.names = FALSE)

dev.off()

