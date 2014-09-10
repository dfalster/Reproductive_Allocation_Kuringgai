library(stringr)
library(colorspace)
library(parallel)
library(smatr)
library(plyr)
library(dplyr)
library(igraph)

source('analysis/OrderedListsOfParts.R')

source('analysis/AvWeightForParts.R')
AvWeightForParts()

source('analysis/AvCountsPerMM.R')
AvCountsPerMM()

source('analysis/RegressionCoefficients.R')
RegressionCoefficients()

source('analysis/CalculateMassAndDiameters.R')
CalculateMassAndDiameters()

source('analysis/IndividualBasedWeightsCalculations.R')
IndividualBasedWeightsCalculations()

source('analysis/CalculateInvestmentForSpecies.R')
species <- read.csv("data/species_names.csv", stringsAsFactors=FALSE)
mclapply(species$Abbreviation, CalculateInvestmentForSpecies,  mc.cores=detectCores()-1 )

source('analysis/RA_Calculations.R')
RA_Calculations()

source('analysis/PlotIndividual_RA.R')
PlotIndividual_RA()

source('analysis/Plot_RAs.R')
Plot_RAs()

source('analysis/PlotsRA_vs_Weight_Diameter.R')
PlotRAs_vs_Diameter()
PlotRAs_vs_Weight()
PlotIndividualRAs_vs_Weight_Diam()


