

For document

  print(xtable(AvCountsPerMM, include.rownames = FALSE), floating = FALSE, type = "latex", file = "output/docs/AvCountsPerMM.tex")



  print(xtable(RegressionTable, include.rownames = FALSE), floating = FALSE, type = "latex", file = "output/docs/RegressionTable.tex")

print tex docs within make_AvWeightPerUnit

possible ply replacement for make_AvWeightPerUnit

make_RegressionTable
	- lots of repeated code
	- print pdfs



  # Write output files
  write.csv(file = paste0("output/", species, "_Inv.csv"), x = Investment, row.names = F)
  write.csv(file = paste0("output/", species, "_Lost.csv"), x = LostSpecies, row.names = F)
  write.csv(file = paste0("output/", species, "_FinDev.csv"), x = FDSpecies, row.names = F)
  write.csv(file = paste0("output/", species, "_Error.csv"), x = ErrorSpecies, row.names = F)
