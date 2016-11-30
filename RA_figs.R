
library(dplyr)
source("R/figures.R")
SummarySppAge <- readRDS("export/SummarySppAge.rds")


# pdf('figs2/RA/Figure_1_stacked_lines.pdf', height=11.5, width=8) plot

data <- select(SummarySppAge[["mean"]], species, age, repro_inv, growth_leaf, leaf_replacement,
  growth_stem, leaf_shed)

# replace zeroes
for (v in c("repro_inv", "growth_leaf", "leaf_replacement", "growth_stem", "leaf_shed")) {
  data[[v]][is.na(data[[v]])] <- 0
}
for (v in c("growth_leaf")) {
  data[[v]][data[[v]] < 0] <- 0
}

par(mfrow = c(5, 3), cex = 1, oma = c(5, 5, 1, 1), mar = c(0, 1, 2, 1))

all_species <- unique(data$species)

for (i in seq_along(all_species)) {

  # Todo: this should be calculated at individual level before taking means by species
  data_sp <- filter(data, species == all_species[i]) %>%
            mutate(
                total_inv = repro_inv + growth_leaf + leaf_replacement + growth_stem,
                prop_repro = repro_inv/total_inv,
                prop_leaf_expand = (repro_inv + growth_leaf)/total_inv,
                prop_leaf_replacement = (leaf_replacement + repro_inv + growth_leaf)/total_inv,
                prop_stem = total_inv/total_inv)

  plot(NA, log = "x", ylim = c(0, 1),  xlim = c(1.4, 32), xaxs = "i", yaxs = "i", axes= FALSE, ann=FALSE)
  box()
  axis(1, labels = (i >12))
  axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = (i %% 3 == 1), las=1)

  n <- length(data_sp$age)
  polygon(x = data_sp$age[c(1:n, n, 1)], y = c(data_sp$prop_stem, 0, 0), col = "burlywood4", density = NA)
  polygon(x = data_sp$age[c(1:n, n, 1)], y = c(data_sp$prop_leaf_replacement, 0, 0), col = "darkseagreen4", density = NA)
  polygon(x = data_sp$age[c(1:n, n, 1)], y = c(data_sp$prop_leaf_expand, 0, 0), col = "darkseagreen2", density = NA)
  polygon(x = data_sp$age[c(1:n, n, 1)], y = c(data_sp$prop_repro, 0, 0), col = "coral3", density = NA)
  mtext(labels.spp.full(all_species[i]), 3, 0)
}
mtext("Proportion energy", 2, line=3, outer=TRUE)
mtext("Age (yrs)", 1, line=3, outer=TRUE)

# ---------------------------------------------

