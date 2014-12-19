
col.table <- function(){
  # genereated via colorspace::: rainbow_hcl(12)
  c(BAER="#E495A5", BOLE="#9CB469", COER="#39BEB1", EPMI="#46BAC8", GRBU="#56BD96",
    GRSP="#6FB3D9", HATE="#99A9E2", HEPU="#D497D3",LEES="#E193BF", PELA="#DD9B89",
    PEPU="#CEA472", PHPH= "#B8AC65", PILI="#7BBA7D", PUTU="#46BAC8")
}

get_RA_means_by_age <- function(RA_all){
  group_by(RA_all, species, age) %.%
    summarise(
      n=length(age),
      GI_M = mean(GrowthInv),
      GI_L = mean(GrowthInv) - 1.96/sqrt(n) * sd(GrowthInv),
      GI_U = mean(GrowthInv) + 1.96/sqrt(n) * sd(GrowthInv),
      RI_M = mean(ReproInv),
      RI_L = mean(ReproInv) - 1.96/sqrt(n) * sd(ReproInv),
      RI_U = mean(ReproInv) + 1.96/sqrt(n) * sd(ReproInv),
      RA_M = mean(RA),
      RA_L = mean(RA) - 1.96/sqrt(n) * sd(RA),
      RA_U = mean(RA) + 1.96/sqrt(n) * sd(RA)) %.%
    arrange(species, age)
}

plot_species_RA_age_panel <- function(InvBySpecies) {

  InvByAge <- get_RA_means_by_age(InvBySpecies)

  cols <- col.table()[InvBySpecies$species]

  par(mfrow = c(1, 3), cex.main = 2, cex.lab = 1.5)
  plot(InvByAge$age, InvByAge$GI_M, type = "b", lwd = 3, pch = as.character(InvByAge$age), cex = 1.5, col = cols, ylim = range(c(InvBySpecies$ReproInv, InvBySpecies$GrowthInv)),
    main = "Growth Investment", xlab = "Age (years)", ylab = "Growth Investment (mg)")
  points(InvByAge$age, InvByAge$GI_L, pch = 2, cex = 2, col = cols)
  points(InvByAge$age, InvByAge$GI_U, pch = 6, cex = 2, col = cols)
  points(InvBySpecies$age, InvBySpecies$GrowthInv, pch = "o", cex = 1.5, col = "grey")

  plot(InvByAge$age, InvByAge$RI_M, type = "b", lwd = 3, pch = as.character(InvByAge$age), cex = 1.5, col = cols, ylim = range(c(InvBySpecies$ReproInv, InvBySpecies$GrowthInv)),
    main = "Reproductive Investment", xlab = "Age (years)", ylab = "Reproductive Investment (mg)")
  points(InvByAge$age, InvByAge$RI_L, pch = 2, cex = 2, col = cols)
  points(InvByAge$age, InvByAge$RI_U, pch = 6, cex = 2, col = cols)
  points(InvBySpecies$age, InvBySpecies$ReproInv, pch = "o", cex = 1.5, col = "grey")


  plot(InvByAge$age, InvByAge$RA_M, type = "b", lwd = 3, pch = as.character(InvByAge$age), cex = 1.5, col = cols, ylim = range(InvBySpecies$RA), main = "Reproductive Allocation",
    xlab = "Age (years)", ylab = "Reproductive Allocation")
  points(InvByAge$age, InvByAge$RA_L, pch = 2, cex = 2, col = cols)
  points(InvByAge$age, InvByAge$RA_U, pch = 6, cex = 2, col = cols)
  points(InvBySpecies$age, InvBySpecies$RA, pch = "o", cex = 1.5, col = "grey")
}

plot_RA_comparison_age <- function(RA_all) {

  InvByAge <- get_RA_means_by_age(RA_all)
  cols <- col.table()
  plot(InvByAge$age, InvByAge$RA_M, type="n", main = "Reproductive Allovation for 14 species along time", lty = 1, lwd = 3, xlim = c(0, 35), bty = "L", xlab = "Age (years)", ylab = "Reproductive Allocation")
  d_ply(InvByAge, "species", function(x) points(x$age, x$RA_M, type = "b", pch = "o", col= cols[x$species]))
  legend("topright", legend = names(cols), col = cols, lty = 1, lwd = 3)

}

plot_RA_comparison_diam <- function(RA_all) {
  cols <- col.table()
  plot(RA_all$FinalBasalDiamAv, RA_all$RA, type = "p", col = cols[RA_all$species], xlab = "Basal Diameter, year 2013", ylab = "RA", bty = "L")
  legend("topright", legend = names(cols), col = cols, pch = 1)
}

plot_RA_comparison_weight <- function(RA_all) {
  cols <- col.table()
  plot(RA_all$FinalWeight, RA_all$RA, type = "p", col = cols[RA_all$species], xlab = "Weight, year 2013", ylab = "RA", bty = "L")
  legend("topright", legend = names(cols), col = cols, pch = 1)
}

plot_species_RA_weight_diam_panel <- function(Data) {
    cols <- col.table()[Data$species]
    par(mfrow = c(1, 3), cex.main = 2, cex.lab = 1.5)
    plot(Data$FinalBasalDiamAv, Data$RA, type = "p", col = cols, xlab = "Diameter, year 2013", ylab = "RA", bty = "L", main = "Diameter", cex = 1.5)

    plot(Data$FinalWeight, Data$RA, type = "p", col = cols, xlab = "Weight, year 2013", ylab = "RA", bty = "L", main = "Weight", cex = 1.5)
}

print_xtable_for_species <- function(thisSpecies, data, file){
  print_xtable(filter(data, species==thisSpecies),file=file)
}

print_xtable <- function(data, file){
 print(xtable(data, include.rownames=FALSE), floating=FALSE, type="latex",
    file=file)
}
