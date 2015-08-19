

col.spp <- function(x=NULL){

  ret <- c(BAER="red", BOLE="darkolivegreen4", COER="blue", EPMI="purple", GRBU="grey",
    GRSP="light green", HATE="gold1", HEPU="cyan2",LEES="#E193BF", PELA="chocolate1",
    PEPU="light blue", PHPH= "brown", PILI="deep pink", PUTU="orchid1")
  
  if(!is.null(x)) {
    ret <- ret[x]
  }
  ret
}

labels.spp <- function(x=NULL){  
  names(col.species(x))
}

labels.spp.full <- function(x=NULL){
  
  ret <- c("Banksia ericifolia","Boronia ledifolia","Conospermum ericifolium","Epacris micraphylla", "Grevillea buxifolia","Grevillea speciosa","Hakea teretifolia","Hemigenia purpurea","Leucopogon esquamatus","Persoonia lanceolata", "Petrophile puchella", "Phyllota phyllicoides", "Pimelea linifolia", "Pultanaea tuberculata")
  names(ret) <- c("BAER","BOLE","COER","EPMI","GRBU","GRSP","HATE","HEPU","LEES","PELA","PEPU","PHPH","PILI","PUTU")

  if(!is.null(x)) {
    ret <- ret[as.character(x)]
  }
  ret
}

col.age <- function(x=NULL){

  ret <- c("orchid1","gold1","darkolivegreen3","cyan2","dodgerblue","purple")
  names(ret) <- c("1.4","2.4","5","7","9","32") 

  if(!is.null(x)) {
    ret <- ret[as.character(x)]
  }
  ret
}

labels.age <- function(x=NULL){
  names(col.age(x))
}

col.mature <- function(x=NULL){

  ret <- c("cyan3","red")
  names(ret) <- c("juvenile","mature")

  if(!is.null(x)) {
    ret <- ret[as.character(x)]
  }
  ret
}

labels.mature <- function(x=NULL){
 names(col.mature(x))
}

col.lots <- function(x) {
  #returns up to 80 unique, nice colors, generated using http://tools.medialab.sciences-po.fr/iwanthue/
  # Starts repeating after 80
  n <- length(unique(x))
  cols<-rep(c("#75954F","#D455E9","#E34423","#4CAAE1","#451431","#5DE737","#DC9B94","#DC3788","#E0A732","#67D4C1","#5F75E2","#1A3125","#65E689","#A8313C","#8D6F96","#5F3819","#D8CFE4","#BDE640","#DAD799","#D981DD","#61AD34","#B8784B","#892870","#445662","#493670","#3CA374","#E56C7F","#5F978F","#BAE684","#DB732A","#7148A8","#867927","#918C68","#98A730","#DDA5D2","#456C9C","#2B5024","#E4D742","#D3CAB6","#946661","#9B66E3","#AA3BA2","#A98FE1","#9AD3E8","#5F8FE0","#DF3565","#D5AC81","#6AE4AE","#652326","#575640","#2D6659","#26294A","#DA66AB","#E24849","#4A58A3","#9F3A59","#71E764","#CF7A99","#3B7A24","#AA9FA9","#DD39C0","#604458","#C7C568","#98A6DA","#DDAB5F","#96341B","#AED9A8","#55DBE7","#57B15C","#B9E0D5","#638294","#D16F5E","#504E1A","#342724","#64916A","#975EA8","#9D641E","#59A2BB","#7A3660","#64C32A"),
            ceiling(n/80))
  cols[as.factor(x)]
}
get_ReproductionAllocation_means_by_age <- function(ReproductionAllocation_all){
  group_by(ReproductionAllocation_all, species, age) %>%
    summarise(
      n=length(age),
      GI_M = mean(growth_inv),
      GI_L = mean(growth_inv) - 1.96/sqrt(n) * sd(growth_inv),
      GI_U = mean(growth_inv) + 1.96/sqrt(n) * sd(growth_inv),
      RI_M = mean(repro_inv),
      RI_L = mean(repro_inv) - 1.96/sqrt(n) * sd(repro_inv),
      RI_U = mean(repro_inv) + 1.96/sqrt(n) * sd(repro_inv),
      RA_M = mean(RA),
      RA_L = mean(RA) - 1.96/sqrt(n) * sd(RA),
      RA_U = mean(RA) + 1.96/sqrt(n) * sd(RA)) %>%
    arrange(species, age)
}

plot_species_ReproductionAllocation_age_panel <- function(InvBySpecies) {

  InvByAge <- get_ReproductionAllocation_means_by_age(InvBySpecies)

  cols <- col.spp()[InvBySpecies$species]

  par(mfrow = c(1, 3), cex.main = 1, cex.lab = 1)
  plot(InvByAge$age, InvByAge$GI_M, type = "b", lwd = 3, pch = as.character(InvByAge$age), cex = 1, col = cols, ylim = range(c(InvBySpecies$repro_inv, InvBySpecies$growth_inv)),
    main = "Growth Investment", xlab = "Age (years)", ylab = "Growth Investment (mg)")
  points(InvByAge$age, InvByAge$GI_L, pch = 2, cex = 1, col = cols)
  points(InvByAge$age, InvByAge$GI_U, pch = 6, cex = 1, col = cols)
  points(InvBySpecies$age, InvBySpecies$growth_inv, pch = "o", cex = 1, col = "grey")

  plot(InvByAge$age, InvByAge$RI_M, type = "b", lwd = 1, pch = as.character(InvByAge$age), cex = 1, col = cols, ylim = range(c(InvBySpecies$repro_inv, InvBySpecies$growth_inv)),
    main = "Reproductive Investment", xlab = "Age (years)", ylab = "Reproductive Investment (mg)")
  points(InvByAge$age, InvByAge$RI_L, pch = 2, cex = 1, col = cols)
  points(InvByAge$age, InvByAge$RI_U, pch = 6, cex = 1, col = cols)
  points(InvBySpecies$age, InvBySpecies$repro_inv, pch = "o", cex = 1, col = "grey")


  plot(InvByAge$age, InvByAge$RA_M, type = "b", lwd = 1, pch = as.character(InvByAge$age), cex = 1, col = cols, ylim = range(InvBySpecies$RA), main = "Reproductive Allocation",
    xlab = "Age (years)", ylab = "Reproductive Allocation")
  points(InvByAge$age, InvByAge$RA_L, pch = 2, cex = 1, col = cols)
  points(InvByAge$age, InvByAge$RA_U, pch = 6, cex = 1, col = cols)
  points(InvBySpecies$age, InvBySpecies$RA, pch = "o", cex = 1, col = "grey")
}

plot_ReproductionAllocation_comparison_age <- function(ReproductionAllocation_all) {

  InvByAge <- get_ReproductionAllocation_means_by_age(ReproductionAllocation_all)
  cols <- col.spp()
  plot(InvByAge$age, InvByAge$RA_M, type="n", main = "Reproductive Allovation for 14 species along time", lty = 1, lwd = 3, xlim = c(0, 35), bty = "L", xlab = "Age (years)", ylab = "Reproductive Allocation")
  d_ply(InvByAge, "species", function(x) points(x$age, x$RA_M, type = "b", pch = "o", col= cols[x$species]))
  legend("topright", legend = names(cols), col = cols, lty = 1, lwd = 3)

}

plot_ReproductionAllocation_comparison_diam <- function(ReproductionAllocation_all) {
  cols <- col.spp()
  plot(ReproductionAllocation_all$diameter, ReproductionAllocation_all$RA, type = "p", col = cols[ReproductionAllocation_all$species], xlab = "Basal Diameter, year 2013", ylab = "RA", bty = "L")
  legend("topright", legend = names(cols), col = cols, pch = 1)
}

plot_ReproductionAllocation_comparison_weight <- function(ReproductionAllocation_all) {
  cols <- col.spp()
  plot(ReproductionAllocation_all$total_weight, ReproductionAllocation_all$RA, type = "p", col = cols[ReproductionAllocation_all$species], xlab = "Weight, year 2013", ylab = "RA", bty = "L")
  legend("topright", legend = names(cols), col = cols, pch = 1)
}

plot_species_ReproductionAllocation_weight_diam_panel <- function(Data) {
    cols <- col.spp()[Data$species]
    par(mfrow = c(1, 2), cex.main = 1, cex.lab = 1)
    plot(Data$diameter, Data$RA, type = "p", col = cols, xlab = "Diameter, year 2013", ylab = "RA", bty = "L", main = "Diameter", cex = 1)

    plot(Data$total_weight, Data$RA, type = "p", col = cols, xlab = "Weight, year 2013", ylab = "RA", bty = "L", main = "Weight", cex = 1)
}

print_xtable_for_species <- function(thisSpecies, data, file){
  print_xtable(filter(data, species==thisSpecies),file=file)
}

print_xtable <- function(data, file){
 print(xtable(data, include.rownames=FALSE), floating=FALSE, type="latex",
    file=file)
}
