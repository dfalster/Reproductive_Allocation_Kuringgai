#!/usr/bin/env Rscript
library(whisker)

allspecies <- c("BAER", "BOLE", "COER", "EPMI", "GRBU", "GRSP", "HATE", "HEPU", "LEES", "PELA", "PEPU", "PHPH", "PILI", "PUTU")
#allspecies <- c("EPMI", "PILI")

vals <- list(allspecies=iteratelist(allspecies, value="species"))
str <- whisker.render(readLines("maker_species_data.yml.whisker"), vals)
writeLines(str, "maker_species_data.yml")

str <- whisker.render(readLines("maker_species_figures.yml.whisker"), vals)
writeLines(str, "maker_species_figures.yml")

str <- whisker.render(readLines("species_data.R.whisker"), vals)
writeLines(str, "R/species_data.R")

