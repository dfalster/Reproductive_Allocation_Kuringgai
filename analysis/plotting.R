PlotIndividual_RA <- function() {
  # Function creating individual pdf RA developenent plots for all the species
  source("analysis/OrderedListsOfParts.R")
  InvSummary <- read.csv("output/InvestmentSummary.csv")
  # InvSummary['species']=str_sub(InvSummary$Tree_ID,1,4)

  for (j in 1:14) {
    species <- sort(names(Maps))[j]
    sp_col <- rainbow_hcl(14)[j]
    InvBySpecies <- InvSummary[InvSummary$species == species, ]

    InvByAge <- data.frame(age = c(), GI = c(), GI_L = c(), GI_U = c(), RI = c(), RI_L = c(), RI_U = c(), RA = c(), RA_L = c(), RA_U = c())

    N <- c()
    Ages <- unique(InvBySpecies$age)
    for (i in seq_len(length(Ages))) {
      D <- InvBySpecies[InvBySpecies$age == Ages[i], ]
      n <- nrow(D)
      N[i] <- n
      InvByAge <- rbind(InvByAge, data.frame(age = Ages[i], GI = mean(D$GrowthInv), GI_L = mean(D$GrowthInv) - 1.96/sqrt(n) * sd(D$GrowthInv), GI_U = mean(D$GrowthInv) +
        1.96/sqrt(n) * sd(D$GrowthInv), RI = mean(D$ReproInv), RI_L = mean(D$ReproInv) - 1.96/sqrt(n) * sd(D$ReproInv), RI_U = mean(D$ReproInv) + 1.96/sqrt(n) *
        sd(D$ReproInv), RA = mean(D$RA), RA_L = mean(D$RA) - 1.96/sqrt(n) * sd(D$RA), RA_U = mean(D$RA) + 1.96/sqrt(n) * sd(D$RA)))
    }

    pdf(file = paste0("output/docs/RA_", species, ".pdf"), width = 15, height = 5)
    N <- N[order(InvByAge$age)]
    InvByAge <- InvByAge[order(InvByAge$age), ]
    par(mfrow = c(1, 3), cex.main = 2, cex.lab = 1.5)
    plot(InvByAge$age, InvByAge$GI, type = "b", lwd = 3, pch = as.character(N), cex = 1.5, col = sp_col, ylim = range(c(InvBySpecies$ReproInv, InvBySpecies$GrowthInv)),
      main = "Growth Investment", xlab = "Age (years)", ylab = "Growth Investment (mg)")
    points(InvByAge$age, InvByAge$GI_L, pch = 2, cex = 2, col = sp_col)
    points(InvByAge$age, InvByAge$GI_U, pch = 6, cex = 2, col = sp_col)
    points(InvBySpecies$age, InvBySpecies$GrowthInv, pch = "o", cex = 1.5, col = "grey")


    plot(InvByAge$age, InvByAge$RI, type = "b", lwd = 3, pch = as.character(N), cex = 1.5, col = sp_col, ylim = range(c(InvBySpecies$ReproInv, InvBySpecies$GrowthInv)),
      main = "Reproductive Investment", xlab = "Age (years)", ylab = "Reproductive Investment (mg)")
    points(InvByAge$age, InvByAge$RI_L, pch = 2, cex = 2, col = sp_col)
    points(InvByAge$age, InvByAge$RI_U, pch = 6, cex = 2, col = sp_col)
    points(InvBySpecies$age, InvBySpecies$ReproInv, pch = "o", cex = 1.5, col = "grey")


    plot(InvByAge$age, InvByAge$RA, type = "b", lwd = 3, pch = as.character(N), cex = 1.5, col = sp_col, ylim = range(InvBySpecies$RA), main = "Reproductive Allocation",
      xlab = "Age (years)", ylab = "Reproductive Allocation")
    points(InvByAge$age, InvByAge$RA_L, pch = 2, cex = 2, col = sp_col)
    points(InvByAge$age, InvByAge$RA_U, pch = 6, cex = 2, col = sp_col)
    points(InvBySpecies$age, InvBySpecies$RA, pch = "o", cex = 1.5, col = "grey")

    dev.off()
  }
}
Plot_IAT_By_Species <- function() {
  species_v <- read.csv("data/species_names.csv", stringsAsFactors = FALSE)
  for (species in species_v$Abbreviation) {
    Data <- read.csv(paste0("output/", species, "_Acc.csv"))
    Data <- aggregate(cbind(PrePol_A, PrePol_S, PostPol_A, PD, Prop, Total) ~ age, data = Data, FUN = mean)
    Ages <- Data$age
    DataSt <- Data/Data$Total
    DataSt <- DataSt[, 2:6]
    M_Ages <- matrix(Ages, ncol = 5, nrow = length(Ages), byrow = F)
    pdf(file = paste0("output/docs/IAT_", species, ".pdf"), width = 15, height = 5)
    matplot(M_Ages, DataSt, type = "l", col = rainbow_hcl(5), lty = c(2, 2, 2, 2, 1), lwd = c(2, 2, 2, 2, 3), xlab = "Age", ylab = "Proportion of RI")
    legend("topleft", c("Prepol Abort", "Prepol Succ", "Postpol Abort", "Pack Disp", "Propagule"), col = rainbow_hcl(5), lty = c(2, 2, 2, 2, 1), lwd = c(2, 2,
      2, 2, 3))
    dev.off()
  }
}


Compare_IProp <- function() {
  pdf(file = paste0("output/docs/IAT_Comparison", ".pdf"), width = 20, height = 10)
  plot(-10, -10, xlim = c(2, 36), ylim = c(0, 0.5), xlab = "Age", ylab = "Propotion of RI")
  species_v <- read.csv("data/species_names.csv", stringsAsFactors = FALSE)
  i <- 1
  for (species in species_v$Abbreviation) {
    Data <- read.csv(paste0("output/", species, "_Acc.csv"))
    Data <- aggregate(cbind(PrePol_A, PrePol_S, PostPol_A, PD, Prop, Total) ~ age, data = Data, FUN = mean)
    Ages <- Data$age
    DataSt <- Data/Data$Total
    DataSt <- DataSt[, 6]
    lines(Ages, DataSt, col = rainbow_hcl(14)[i], lwd = 2)
    i <- i + 1
  }
  legend("topright", species_v$Abbreviation, col = rainbow_hcl(14), lty = 1, lwd = 1)
  dev.off()

}
Plot_RAs <- function() {
  # Function plotting the comparison plot of RA for all the species.
  InvSummary <- read.csv("output/InvestmentSummary.csv")
  RAo <- aggregate(RA ~ species + age, data = InvSummary, FUN = mean)
  ages <- unique(RAo$age)
  nT <- length(ages)
  RA <- t(reshape(RAo, timevar = "age", idvar = "species", direction = "wide"))
  colnames(RA) <- RA[1, ]
  RA <- RA[-1, ]
  RA <- RA[, order(colnames(RA))]
  RA[is.na(RA)] <- 0
  # Check if plants at given ages are present for the specie (i.e., distinguish between no measurments and zero investments)
  DiameterData <- read.csv("data/2013_Kuringgai_harvest.csv", stringsAsFactors = FALSE)
  # Use only basal diameter of the trees that are alive and have status use.
  DiameterData <- DiameterData[DiameterData$segment == 1, ]
  DiameterData <- DiameterData[DiameterData$use_status == "use", ]
  DiameterData <- DiameterData[DiameterData$plant_status == "alive", ]

  for (i in 1:14) {
    for (j in seq_len(nT)) {
      species <- colnames(RA)[i]
      age <- ages[j]
      if ((nrow(DiameterData[(DiameterData$species == species) & (DiameterData$age == age), ])) == 0)
        RA[j, i] <- NA
    }
  }

  Age <- matrix(ncol = 14, nrow = nT, ages)
  pdf(file = "output/docs/RA_Comparison.pdf", width = 20, height = 10)
  matplot(Age, RA, type = "b", pch = "o", main = "Reproductive Allovation for 14 species along time", col = rainbow_hcl(14), lty = 1, lwd = 3, xlim = c(0, 35),
    bty = "L")
  legend(33, 0.7, legend = colnames(RA), col = rainbow_hcl(14), lty = 1, lwd = 3)
  dev.off()
}
PlotRAs_vs_Diameter <- function() {
  InvSummary <- read.csv("output/InvestmentSummary.csv")
  pdf(file = "output/docs/RA_Diam_Comparison.pdf", width = 20, height = 10)
  plot(InvSummary$FinalBasalDiamAv, InvSummary$RA, type = "p", col = rainbow_hcl(14)[as.numeric(InvSummary$species)], xlab = "Basal Diameter, year 2013", ylab = "RA",
    bty = "L")
  legend("topright", legend = as.character(sort(unique(InvSummary$species))), col = rainbow_hcl(14), pch = 1)
  dev.off()
}

PlotRAs_vs_Weight <- function() {
  InvSummary <- read.csv("output/InvestmentSummary.csv")
  pdf(file = "output/docs/RA_Weight_Comparison.pdf", width = 20, height = 10)
  plot(InvSummary$FinalWeight, InvSummary$RA, type = "p", col = rainbow_hcl(14)[as.numeric(InvSummary$species)], xlab = "Tree Weight, year 2013", ylab = "RA",
    bty = "L")
  legend("topright", legend = as.character(sort(unique(InvSummary$species))), col = rainbow_hcl(14), pch = 1)
  dev.off()
}



PlotIndividualRAs_vs_Weight_Diam <- function() {
  InvSummary <- read.csv("output/InvestmentSummary.csv")
  species_v <- as.character(sort(unique(InvSummary$species)))
  for (i in 1:14) {
    species <- species_v[i]
    Data <- InvSummary[InvSummary$species == species, ]
    pdf(file = paste0("output/docs/RA_", species, "_DW.pdf"), width = 14, height = 7)
    par(mfrow = c(1, 2), cex.main = 2, cex.lab = 1.5)
    plot(Data$FinalBasalDiamAv, Data$RA, type = "p", col = rainbow_hcl(14)[i], xlab = "Diameter, year 2013", ylab = "RA", bty = "L", main = "Diameter", cex = 1.5)
    plot(Data$FinalWeight, Data$RA, type = "p", col = rainbow_hcl(14)[i], xlab = "Weight, year 2013", ylab = "RA", bty = "L", main = "Weight", cex = 1.5)
    dev.off()
  }
}
