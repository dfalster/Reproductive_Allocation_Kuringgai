remake::make("export")


SummaryInd <- readRDS("export/SummaryInd.rds")
SummarySpp <- readRDS("export/SummarySpp.rds")
SummarySppAge <- readRDS("export/SummarySppAge.rds")
HarvestData <- readRDS("export/HarvestData.rds")
InvestmentByPart <- readRDS("export/Investment_FD_all.rds")
PartsSummary <- readRDS("export/PartsSummary_all.rds")
library(smatr)

source("R/figures.R")


remake::make("BAER_Investment")
remake::make("EPMI_Investment")
remake::make("HATE_Investment")



remake::make("BOLE_Investment")
remake::make("COER_Investment")
remake::make("GRBU_Investment")
remake::make("GRSP_Investment")
remake::make("HEPU_Investment")
remake::make("LEES_Investment")
remake::make("PELA_Investment")
remake::make("PEPU_Investment")
remake::make("PHPH_Investment")
remake::make("PILI_Investment")
remake::make("PUTU_Investment")
