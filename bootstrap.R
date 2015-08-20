#!/usr/bin/env Rscript
library(whisker)


allspecies <- c("BAER", "BOLE", "COER", "EPMI", "GRBU", "GRSP", "HATE", "HEPU", "LEES", "PELA", "PEPU", "PHPH", "PILI", "PUTU")
#allspecies <- c("COER", "PUTU")

vals <- list(allspecies=iteratelist(allspecies, value="species"))
str <- whisker.render(readLines("x_remake_species_data.yml.whisker"), vals)
writeLines(str, "remake_species_data.yml")

str <- whisker.render(readLines("x_species_data.R.whisker"), vals)
writeLines(str, "R/species_data.R")
