RA_Calculations <- function(thisSpecies, Species_Investment, HarvestData, Maps, IndividualsList) {


  ### Calculate Regression coefficients based on common slope and intercept by the basal diameter at year 2013

  WD.species <- CalculateMassAndDiameters(HarvestData)

  lm.fit <- lm(log(total_weight) ~ log(dia), data = WD.species)
  common_slope <- as.numeric(coef(lm.fit)[2])

  WD.specie.basal <- WD.species[WD.species$node_above == 1, ]
  intercept.ind <- log(WD.specie.basal$total_weight) - common_slope * log(WD.specie.basal$dia)
  RegTable <- data.frame(TreeID = WD.specie.basal$individual, Intercept = intercept.ind, Slope = common_slope)

  ### Use the regrsssion to obtain weights year 2012 (and 2013).
  # Use only basal diameter of the trees that are alive and have status use.
  DiameterData <- HarvestData %>%
    filter(individual %in% IndividualsList$individual[IndividualsList$use_for_allocation_calculations]) %>%
    filter(segment == 1, use_status == "use", plant_status == "alive")  %>%
    select(individual, age, start_end, diameter_1, diameter_2, diameter_3, total_plant_weight)

  for (i in seq_len(nrow(DiameterData))) {
    individual <- DiameterData$individual[i]
    I <- (RegTable$TreeID == individual)
    if (sum(I) == 1) {
      coef <- RegTable[I, ]
      diams <- DiameterData[i, c("diameter_1", "diameter_2", "diameter_3")]
      diam.av <- mean(as.numeric(diams), na.rm = T)
      DiameterData$EstWeight[i] <- exp(coef$Intercept + coef$Slope * log(diam.av))
      DiameterData[i, "Basal.Diameter.Av"] <- diam.av
    }
  }
  DiameterData <- DiameterData[!is.na(DiameterData$EstWeight), ]

  ### Calculate change in the weight, i.e., Growth Investement
  GrowthInv <- data.frame(individual = c(), GrowthInv = c(), StartWeight = c(), FinalWeight = c(), StartBasalDiamAv = c(), FinalBasalDiamAv = c(), age = c())
  for (i in 1:length(unique(DiameterData$individual))) {
    end <- "end"
    individual <- unique(DiameterData$individual)[i]
    IndData <- DiameterData[DiameterData$individual == individual, ]
    if (nrow(IndData) != 2) {
      print(paste0("Incorrect number of measurments for ", individual))
    }
    if (nrow(IndData) == 2) {
      GrowthInv[i, "individual"] <- individual
      GrowthInv[i, "age"] <- unique(IndData$age)
      GrowthInv[i, "GrowthInv"] <- IndData[IndData$start_end == end, "EstWeight"] - IndData[IndData$start_end == "start", "EstWeight"]
      GrowthInv[i, "StartWeight"] <- IndData[IndData$start_end == "start", "EstWeight"]
      GrowthInv[i, "FinalWeight"] <- IndData[IndData$start_end == end, "EstWeight"]
      GrowthInv[i, "StartBasalDiamAv"] <- IndData[IndData$start_end == "start", "Basal.Diameter.Av"]
      GrowthInv[i, "FinalBasalDiamAv"] <- IndData[IndData$start_end == end, "Basal.Diameter.Av"]

    }
  }
  GrowthInv <- GrowthInv[complete.cases(GrowthInv), ]

  ### Use saved data to calculate total reproduction investment per individual plant
  InvSpecies <- Species_Investment$Investment

  RepoInv <- data.frame(individual = c(), ReproInv = c())
  for (individual in unique(InvSpecies$Individual)) {
      InvIndividual <- InvSpecies[InvSpecies$Individual == individual, ]
      RepoInv <- rbind(RepoInv, data.frame(individual = individual, ReproInv = c(sum(InvIndividual$Total))))
    }



  ### Merge two tables and calculate total investment and RAR
  InvestmentSummary <- merge(RepoInv, GrowthInv, by.y = "individual", all.y = T)
  # NA that appeared correspond to zeoro reproductive investment
  InvestmentSummary[is.na(InvestmentSummary)] <- 0
  # Recode ages if at some place there are old time tags used. InvestmentSummary[str_sub(InvestmentSummary$individual,6,6)=='0','age']=7
  # InvestmentSummary[str_sub(InvestmentSummary$individual,6,6)=='1','age']=1.3 InvestmentSummary[str_sub(InvestmentSummary$individual,6,6)=='4','age']=5
  # InvestmentSummary[str_sub(InvestmentSummary$individual,6,6)=='8','age']=9 InvestmentSummary[str_sub(InvestmentSummary$individual,6,6)=='9','age']=32


  InvestmentSummary[["TotalInvestment"]] <- InvestmentSummary$ReproInv + InvestmentSummary$GrowthInv
  InvestmentSummary[["RA"]] <- InvestmentSummary$ReproInv/InvestmentSummary$TotalInvestment
  row.names(InvestmentSummary) <- NULL
  InvestmentSummary[["species"]] <- str_sub(InvestmentSummary$individual, 1, 4)
  InvestmentSummary[order(InvestmentSummary$species), ]
}
