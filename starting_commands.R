remake::make("export")


SummaryInd <- readRDS("export/SummaryInd.rds")
SummarySpp <- readRDS("export/SummarySpp.rds")
SummarySppAge <- readRDS("export/SummarySppAge.rds")
HarvestData <- readRDS("export/HarvestData.rds")
InvestmentByPart <- readRDS("export/Investment_FD_all.rds")
PartsSummary <- readRDS("export/PartsSummary_all.rds")

source("R/figures.R")
