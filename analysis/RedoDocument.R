

source("analysis/OrderedListsOfParts.R")

source("analysis/AvWeightForParts.R")
AvWeightForParts()

source("analysis/AvCountsPerMM.R")
AvCountsPerMM()

source("analysis/RegressionCoefficients.R")
RegressionCoefficients()

source("analysis/CalculateMassAndDiameters.R")
CalculateMassAndDiameters()

source("analysis/IndividualBasedWeightsCalculations.R")
IndividualBasedWeightsCalculations()

source("analysis/CalculateInvestmentForSpecies.R")
species <- read.csv("data/species_names.csv", stringsAsFactors = FALSE)
# Korad & daniel
mclapply(species$Abbreviation, CalculateInvestmentForSpecies, mc.cores = detectCores() - 1)
# Lizzy - cannot rubn multicore
lapply(species$Abbreviation, CalculateInvestmentForSpecies)

# Rerun a spp if needed CalculateInvestmentForSpecies('BAER')

source("analysis/RA_Calculations.R")
RA_Calculations()

source("analysis/PlotIndividual_RA.R")
PlotIndividual_RA()

source("analysis/Plot_RAs.R")
Plot_RAs()

source("analysis/PlotsRA_vs_Weight_Diameter.R")
PlotRAs_vs_Diameter()
PlotRAs_vs_Weight()
PlotIndividualRAs_vs_Weight_Diam()

# Investments in diffeernet types of accesorry tissues
source("analysis/IAT.R")
IAT()

source("analysis/Plot_IAT_By_Species.R")
Plot_IAT_By_Species()
Compare_IProp()
