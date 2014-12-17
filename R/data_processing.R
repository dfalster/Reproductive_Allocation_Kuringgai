
read_csv <- function(filename) {
  read.csv(filename, header = TRUE, sep = ",", stringsAsFactors = FALSE)
}

filterBySpecies <- function(thisSpecies, data){
  filter(data, species == thisSpecies)
}

filterForAllocation <- function(data, IndividualsList){

 # TODO: change column name in reproduction ro be consistent: individual -> tagID
 keep <- IndividualsList$individual[IndividualsList$use_for_allocation_calculations]
 data %.% filter(individual %in% keep)
 }

make_AvCountsPerMM <- function(FPSummary) {
  ### Average counts per mm for specied that require it. Specie (not part) specific.

  ### Average weight of pieces, i.e., mass/count - OLD VERSION

  i <- 1
  sp.name <- av.dens <- tot.count <- c()
  for (species.name in unique(FPSummary$species)) {
    Sp.Data <- FPSummary[FPSummary$species == species.name, ]
    CPL <- Sp.Data[Sp.Data$part == "count_by_length", ]
    if (nrow(CPL) > 0) {
      sp.name[i] <- species.name
      av.dens[i] <- mean(CPL$count/CPL$length)
      tot.count[i] <- sum(CPL$count)
      i <- i + 1
    }
  }

  data.frame(species = sp.name, tot.count = tot.count, AvCountPerMM = av.dens)
}


### Calculating the regression coefficients requiered for calculations of weight based on the measured dimension of the plant part.  Calculations are only made for
# TODO: lots of reptition here
make_RegressionTable <- function(FPSummary) {
  # some particular species and are part specific.
  out <- list()
  FPSummary <- FPSummary[FPSummary$category_use == "used", ]

  species <- part <- n <- intercept <- slope <- reg.type <- reg.order <- c()

  ###### GRBU ######

  i <- 1
  species.name <- "GRBU"
  part.name <- "fruit_large_immature"
  reg.order[i] <- 2
  reg.type[i] <- "quadratic_undecided"

  species[i] <- species.name
  part[i] <- part.name
  SpeciesData <- FPSummary[FPSummary$species == species.name, ]
  PartData <- SpeciesData[SpeciesData$part == part.name, ]
  height <- PartData$dimension_height
  weight <- as.numeric(PartData$weight)
  n[i] <- length(height)
  lm.res <- lm(weight ~ I(height^reg.order[i]))
  # TODO: make these plots
  # pdf(file = paste("output/docs/", species.name, "_", part.name, ".pdf", sep = ""), width = 10, height = 10)
  # plot(height, weight, type = "p", main = paste(species.name, part.name, sep = ": "))
  # x.dense <- data.frame(height = seq(from = min(height), to = max(height), length.out = 100))
  # lines(x.dense$height, predict.lm(object = lm.res, newdata = x.dense))
  # dev.off()
  intercept[i] <- coef(lm.res)[1]
  slope[i] <- coef(lm.res)[2]


  i <- 2
  species.name <- "GRBU"
  part.name <- "seed_pod"
  reg.order[i] <- 1
  reg.type[i] <- "linear"

  species[i] <- species.name
  part[i] <- part.name
  SpeciesData <- FPSummary[FPSummary$species == species.name, ]
  PartData <- SpeciesData[SpeciesData$part == part.name, ]
  height <- PartData$dimension_height
  weight <- as.numeric(PartData$weight)
  n[i] <- length(height)
  lm.res <- lm(weight ~ I(height^reg.order[i]))
  # pdf(file = paste("output/docs/", species.name, "_", part.name, ".pdf", sep = ""), width = 10, height = 10)
  # plot(height, weight, type = "p", main = paste(species.name, part.name, sep = ": "))
  # x.dense <- data.frame(height = seq(from = min(height), to = max(height), length.out = 100))
  # lines(x.dense$height, predict.lm(object = lm.res, newdata = x.dense))
  # dev.off()
  intercept[i] <- coef(lm.res)[1]
  slope[i] <- coef(lm.res)[2]


  i <- 3
  species.name <- "GRSP"
  part.name <- "fruit_large_immature"
  reg.order[i] <- 2
  reg.type[i] <- "quadratic"

  species[i] <- species.name
  part[i] <- part.name
  SpeciesData <- FPSummary[FPSummary$species == species.name, ]
  PartData <- SpeciesData[SpeciesData$part == part.name, ]
  height <- PartData$dimension_height
  weight <- as.numeric(PartData$weight)
  n[i] <- length(height)
  lm.res <- lm(weight ~ I(height^reg.order[i]))
  # pdf(file = paste("output/docs/", species.name, "_", part.name, ".pdf", sep = ""), width = 10, height = 10)
  # plot(height, weight, type = "p", main = paste(species.name, part.name, sep = ": "))
  # x.dense <- data.frame(height = seq(from = min(height), to = max(height), length.out = 100))
  # lines(x.dense$height, predict.lm(object = lm.res, newdata = x.dense))
  # dev.off()
  # intercept[i] <- coef(lm.res)[1]
  slope[i] <- coef(lm.res)[2]

  i <- 4
  species.name <- "GRSP"
  part.name <- "seed_pod"
  reg.order[i] <- 1
  reg.type[i] <- "linear"

  species[i] <- species.name
  part[i] <- part.name
  SpeciesData <- FPSummary[FPSummary$species == species.name, ]
  PartData <- SpeciesData[SpeciesData$part == part.name, ]
  height <- PartData$dimension_height
  weight <- as.numeric(PartData$weight)
  n[i] <- length(height)
  lm.res <- lm(weight ~ I(height^reg.order[i]))
  # pdf(file = paste("output/docs/", species.name, "_", part.name, ".pdf", sep = ""), width = 10, height = 10)
  # plot(height, weight, type = "p", main = paste(species.name, part.name, sep = ": "))
  # x.dense <- data.frame(height = seq(from = min(height), to = max(height), length.out = 100))
  # lines(x.dense$height, predict.lm(object = lm.res, newdata = x.dense))
  # dev.off()
  intercept[i] <- coef(lm.res)[1]
  slope[i] <- coef(lm.res)[2]

  ###### PELA

  i <- 5
  species.name <- "PELA"
  part.name <- "fruit_large_immature"
  reg.order[i] <- 2
  reg.type[i] <- "quadratic"

  species[i] <- species.name
  part[i] <- part.name
  SpeciesData <- FPSummary[FPSummary$species == species.name, ]
  PartData <- SpeciesData[SpeciesData$part == part.name, ]
  height <- PartData$dimension_height
  weight <- as.numeric(PartData$weight)/PartData$count
  n[i] <- length(height)
  lm.res <- lm(weight ~ I(height^reg.order[i]))
  # pdf(file = paste("output/docs/", species.name, "_", part.name, ".pdf", sep = ""), width = 10, height = 10)
  # plot(height, weight, type = "p", main = paste(species.name, part.name, sep = ": "))
  # x.dense <- data.frame(height = seq(from = min(height), to = max(height), length.out = 100))
  # lines(x.dense$height, predict.lm(object = lm.res, newdata = x.dense))
  # dev.off()
  intercept[i] <- coef(lm.res)[1]
  slope[i] <- coef(lm.res)[2]

  i <- 6
  species.name <- "PELA"
  part.name <- "seed_pod"
  reg.order[i] <- 3
  reg.type[i] <- "cubic"

  species[i] <- species.name
  part[i] <- part.name
  SpeciesData <- FPSummary[FPSummary$species == species.name, ]
  PartData <- SpeciesData[SpeciesData$part == part.name, ]
  height <- PartData$dimension_height
  weight <- as.numeric(PartData$weight)/PartData$count
  n[i] <- length(height)
  lm.res <- lm(weight ~ I(height^reg.order[i]))
  # pdf(file = paste("output/docs/", species.name, "_", part.name, ".pdf", sep = ""), width = 10, height = 10)
  # plot(height, weight, type = "p", main = paste(species.name, part.name, sep = ": "))
  # x.dense <- data.frame(height = seq(from = min(height), to = max(height), length.out = 100))
  # lines(x.dense$height, predict.lm(object = lm.res, newdata = x.dense))
  # dev.off()
  intercept[i] <- coef(lm.res)[1]
  slope[i] <- coef(lm.res)[2]

  i <- 7
  species.name <- "PELA"
  part.name <- "seed"
  reg.order[i] <- 3
  reg.type[i] <- "cubic"

  species[i] <- species.name
  part[i] <- part.name
  SpeciesData <- FPSummary[FPSummary$species == species.name, ]
  PartData <- SpeciesData[SpeciesData$part == part.name, ]
  height <- PartData$dimension_height
  weight <- as.numeric(PartData$weight)/PartData$count
  n[i] <- length(height)
  lm.res <- lm(weight ~ I(height^reg.order[i]))
  # pdf(file = paste("output/docs/", species.name, "_", part.name, ".pdf", sep = ""), width = 10, height = 10)
  # plot(height, weight, type = "p", main = paste(species.name, part.name, sep = ": "))
  # x.dense <- data.frame(height = seq(from = min(height), to = max(height), length.out = 100))
  # lines(x.dense$height, predict.lm(object = lm.res, newdata = x.dense))
  # dev.off()
  intercept[i] <- coef(lm.res)[1]
  slope[i] <- coef(lm.res)[2]


  # BAER - calculating density with respect to volume calculated using cylindric approximation


  i <- 8
  species.name <- "BAER"
  part.name <- "cone_young"
  reg.order[i] <- 2
  reg.type[i] <- "quadratic without intercept"

  species[i] <- species.name
  part[i] <- part.name
  SpeciesData <- FPSummary[FPSummary$species == species.name, ]
  PartData <- SpeciesData[SpeciesData$part == part.name, ]
  height <- PartData$dimension_height
  weight <- as.numeric(PartData$weight)/PartData$count
  n[i] <- length(height)
  lm.res <- lm(weight ~ 0 + I(height^reg.order[i]))
  # pdf(file = paste("output/docs/", species.name, "_", part.name, ".pdf", sep = ""), width = 10, height = 10)
  # plot(height, weight, type = "p", main = paste(species.name, part.name, sep = ": "))
  # x.dense <- data.frame(height = seq(from = min(height), to = max(height), length.out = 100))
  # lines(x.dense$height, predict.lm(object = lm.res, newdata = x.dense))
  # dev.off()
  intercept[i] <- 0  #coef(lm.res)[1]
  slope[i] <- coef(lm.res)[1]


  i <- 9
  species.name <- "BAER"
  part.name <- "cone_green"
  reg.order[i] <- 1
  reg.type[i] <- "volume*density"

  species[i] <- species.name
  part[i] <- part.name
  SpeciesData <- FPSummary[FPSummary$species == species.name, ]
  PartData <- SpeciesData[SpeciesData$part == part.name, ]
  volume <- PartData$dimension_height * (PartData$dimension_diameter/2)^2 * pi
  weight <- as.numeric(PartData$weight)
  n[i] <- length(volume)
  lm.res <- lm(weight ~ 0 + volume)
  # pdf(file = paste("output/docs/", species.name, "_", part.name, ".pdf", sep = ""), width = 10, height = 10)
  # plot(volume, weight, type = "p", main = paste(species.name, part.name, sep = ": "))
  # x.dense <- data.frame(volume = seq(from = min(volume), to = max(volume), length.out = 100))
  # lines(x.dense$volume, predict.lm(object = lm.res, newdata = x.dense))
  # dev.off()
  intercept[i] <- 0
  slope[i] <- coef(lm.res)[1]

  i <- 10
  species.name <- "BAER"
  part.name <- "cone_brown_no_expanded_follicles"
  reg.order[i] <- 1
  reg.type[i] <- "volume*density"

  species[i] <- species.name
  part[i] <- part.name
  SpeciesData <- FPSummary[FPSummary$species == species.name, ]
  PartData <- SpeciesData[SpeciesData$part == part.name, ]
  volume <- PartData$dimension_height * (PartData$dimension_diameter/2)^2 * pi
  weight <- as.numeric(PartData$weight)
  n[i] <- length(volume)
  lm.res <- lm(weight ~ 0 + volume)
  # pdf(file = paste("output/docs/", species.name, "_", part.name, ".pdf", sep = ""), width = 10, height = 10)
  # plot(volume, weight, type = "p", main = paste(species.name, part.name, sep = ": "))
  # x.dense <- data.frame(volume = seq(from = min(volume), to = max(volume), length.out = 100))
  # lines(x.dense$volume, predict.lm(object = lm.res, newdata = x.dense))
  # dev.off()
  intercept[i] <- 0
  slope[i] <- coef(lm.res)[1]

  i <- 11
  species.name <- "BAER"
  part.name <- "cone_brown"
  reg.order[i] <- 1
  reg.type[i] <- "volume*density"

  species[i] <- species.name
  part[i] <- part.name
  SpeciesData <- FPSummary[FPSummary$species == species.name, ]
  PartData <- SpeciesData[SpeciesData$part == part.name, ]
  volume <- PartData$dimension_height * (PartData$dimension_diameter/2)^2 * pi
  weight <- as.numeric(PartData$weight)
  n[i] <- length(volume)
  lm.res <- lm(weight ~ 0 + volume)
  # pdf(file = paste("output/docs/", species.name, "_", part.name, ".pdf", sep = ""), width = 10, height = 10)
  # plot(volume, weight, type = "p", main = paste(species.name, part.name, sep = ": "))
  # x.dense <- data.frame(volume = seq(from = min(volume), to = max(volume), length.out = 100))
  # lines(x.dense$volume, predict.lm(object = lm.res, newdata = x.dense))
  # dev.off()
  intercept[i] <- 0
  slope[i] <- coef(lm.res)[1]

  i <- 12
  species.name <- "BAER"
  part.name <- "cone_aborted"
  reg.order[i] <- 2
  reg.type[i] <- "quadratic"

  species[i] <- species.name
  part[i] <- part.name
  SpeciesData <- FPSummary[FPSummary$species == species.name, ]
  PartData <- SpeciesData[SpeciesData$part == part.name, ]
  height <- PartData$dimension_height
  weight <- as.numeric(PartData$weight)/PartData$count
  n[i] <- length(height)
  lm.res <- lm(weight ~ 0 + I(height^reg.order[i]))
  # pdf(file = paste("output/docs/", species.name, "_", part.name, ".pdf", sep = ""), width = 10, height = 10)
  # plot(height, weight, type = "p", main = paste(species.name, part.name, sep = ": "))
  # x.dense <- data.frame(height = seq(from = min(height), to = max(height), length.out = 100))
  # lines(x.dense$height, predict.lm(object = lm.res, newdata = x.dense))
  # dev.off()
  intercept[i] <- 0  #coef(lm.res)[1]
  slope[i] <- coef(lm.res)[1]


  i <- 13
  species.name <- "BAER"
  part.name <- "cone_base_green"
  reg.order[i] <- 1
  reg.type[i] <- "volume*density"

  species[i] <- species.name
  part[i] <- part.name
  SpeciesData <- FPSummary[FPSummary$species == species.name, ]
  PartData <- SpeciesData[SpeciesData$part == part.name, ]
  volume <- PartData$dimension_height * (PartData$dimension_diameter/2)^2 * pi
  weight <- as.numeric(PartData$weight)
  n[i] <- length(volume)
  lm.res <- lm(weight ~ 0 + volume)
  # pdf(file = paste("output/docs/", species.name, "_", part.name, ".pdf", sep = ""), width = 10, height = 10)
  # plot(volume, weight, type = "p", main = paste(species.name, part.name, sep = ": "))
  # x.dense <- data.frame(volume = seq(from = min(volume, na.rm = T), to = max(volume, na.rm = T), length.out = 100))
  # lines(x.dense$volume, predict.lm(object = lm.res, newdata = x.dense))
  # dev.off()
  intercept[i] <- 0
  slope[i] <- coef(lm.res)[1]


  i <- 14
  species.name <- "BAER"
  part.name <- "cone_base_brown"
  reg.order[i] <- 1
  reg.type[i] <- "volume*density"

  species[i] <- species.name
  part[i] <- part.name
  SpeciesData <- FPSummary[FPSummary$species == species.name, ]
  PartData <- SpeciesData[SpeciesData$part == part.name, ]
  volume <- PartData$dimension_height * (PartData$dimension_diameter/2)^2 * pi
  weight <- as.numeric(PartData$weight)
  n[i] <- length(volume)
  lm.res <- lm(weight ~ 0 + volume)
  # pdf(file = paste("output/docs/", species.name, "_", part.name, ".pdf", sep = ""), width = 10, height = 10)
  # plot(volume, weight, type = "p", main = paste(species.name, part.name, sep = ": "))
  # x.dense <- data.frame(volume = seq(from = min(volume, na.rm = T), to = max(volume, na.rm = T), length.out = 100))
  # lines(x.dense$volume, predict.lm(object = lm.res, newdata = x.dense))
  # dev.off()
  intercept[i] <- 0
  slope[i] <- coef(lm.res)[1]

  # PEPU - calculating density with respect to volume calculated using cylindric approximation


  i <- 15
  species.name <- "PEPU"
  part.name <- "cone_just_starting"
  reg.order[i] <- 1
  reg.type[i] <- "No data"

  species[i] <- species.name
  part[i] <- part.name
  SpeciesData <- FPSummary[FPSummary$species == species.name, ]
  PartData <- SpeciesData[SpeciesData$part == part.name, ]
  height <- PartData$dimension_height
  weight <- as.numeric(PartData$weight)/PartData$count
  n[i] <- length(height)
  lm.res <- lm(weight ~ I(height^reg.order[i]))
  # pdf(file = paste("output/docs/", species.name, "_", part.name, ".pdf", sep = ""), width = 10, height = 10)
  # plot(height, weight, type = "p", main = paste(species.name, part.name, sep = ": "))
  # x.dense <- data.frame(height = seq(from = min(height), to = max(height), length.out = 100))
  # lines(x.dense$height, predict.lm(object = lm.res, newdata = x.dense))
  # dev.off()
  intercept[i] <- coef(lm.res)[1]
  slope[i] <- coef(lm.res)[2]

  i <- 16
  species.name <- "PEPU"
  part.name <- "cone_young"
  reg.order[i] <- 1
  reg.type[i] <- "No data"

  species[i] <- species.name
  part[i] <- part.name
  SpeciesData <- FPSummary[FPSummary$species == species.name, ]
  PartData <- SpeciesData[SpeciesData$part == part.name, ]
  height <- PartData$dimension_height
  weight <- as.numeric(PartData$weight)/PartData$count
  n[i] <- length(height)
  lm.res <- lm(weight ~ I(height^reg.order[i]))
  # pdf(file = paste("output/docs/", species.name, "_", part.name, ".pdf", sep = ""), width = 10, height = 10)
  # plot(height, weight, type = "p", main = paste(species.name, part.name, sep = ": "))
  # x.dense <- data.frame(height = seq(from = min(height), to = max(height), length.out = 100))
  # lines(x.dense$height, predict.lm(object = lm.res, newdata = x.dense))
  # dev.off()
  intercept[i] <- coef(lm.res)[1]
  slope[i] <- coef(lm.res)[2]

  i <- 17
  species.name <- "PEPU"
  part.name <- "cone_green"
  reg.order[i] <- 1
  reg.type[i] <- "volume*density"

  species[i] <- species.name
  part[i] <- part.name
  SpeciesData <- FPSummary[FPSummary$species == species.name, ]
  PartData <- SpeciesData[SpeciesData$part == part.name, ]
  volume <- PartData$dimension_height * (PartData$dimension_diameter/2)^2 * pi
  n[i] <- length(volume)
  if (n[i] > 0) {
    weight <- as.numeric(PartData$weight)
    lm.res <- lm(weight ~ 0 + volume)
    # pdf(file = paste("output/docs/", species.name, "_", part.name, ".pdf", sep = ""), width = 10, height = 10)
    # plot(volume, weight, type = "p", main = paste(species.name, part.name, sep = ": "))
    # x.dense <- data.frame(volume = seq(from = min(volume, na.rm = T), to = max(volume, na.rm = T), length.out = 100))
    # lines(x.dense$volume, predict.lm(object = lm.res, newdata = x.dense))
    # dev.off()
    intercept[i] <- 0
    slope[i] <- coef(lm.res)[1]
  }

  i <- 18
  species.name <- "PEPU"
  part.name <- "cone_brown"
  reg.order[i] <- 1
  reg.type[i] <- "volume*density"

  species[i] <- species.name
  part[i] <- part.name
  SpeciesData <- FPSummary[FPSummary$species == species.name, ]
  PartData <- SpeciesData[SpeciesData$part == part.name, ]
  volume <- PartData$dimension_height * (PartData$dimension_diameter/2)^2 * pi
  n[i] <- length(volume)
  if (n[i] > 0) {
    weight <- as.numeric(PartData$weight)
    lm.res <- lm(weight ~ 0 + volume)
    # pdf(file = paste("output/docs/", species.name, "_", part.name, ".pdf", sep = ""), width = 10, height = 10)
    # plot(volume, weight, type = "p", main = paste(species.name, part.name, sep = ": "))
    # x.dense <- data.frame(volume = seq(from = min(volume, na.rm = T), to = max(volume, na.rm = T), length.out = 100))
    # lines(x.dense$volume, predict.lm(object = lm.res, newdata = x.dense))
    # dev.off()
    intercept[i] <- 0
    slope[i] <- coef(lm.res)[1]
  }

  i <- 19
  species.name <- "PEPU"
  part.name <- "cone_aborted"
  reg.order[i] <- 1
  reg.type[i] <- "volume*density"

  species[i] <- species.name
  part[i] <- part.name
  SpeciesData <- FPSummary[FPSummary$species == species.name, ]
  PartData <- SpeciesData[SpeciesData$part == part.name, ]
  volume <- PartData$dimension_height * (PartData$dimension_diameter/2)^2 * pi
  n[i] <- length(volume)
  if (n[i] > 0) {
    weight <- as.numeric(PartData$weight)
    lm.res <- lm(weight ~ 0 + volume)
    # pdf(file = paste("output/docs/", species.name, "_", part.name, ".pdf", sep = ""), width = 10, height = 10)
    # plot(volume, weight, type = "p", main = paste(species.name, part.name, sep = ": "))
    # x.dense <- data.frame(volume = seq(from = min(volume, na.rm = T), to = max(volume, na.rm = T), length.out = 100))
    # lines(x.dense$volume, predict.lm(object = lm.res, newdata = x.dense))
    # dev.off()
    intercept[i] <- 0
    slope[i] <- coef(lm.res)[1]
  }


  data.frame(species = species, part = part, n = n, reg.type = reg.type, reg.order = reg.order, intercept = intercept, slope = slope)
}





# Function calculating weight for missing parts, i.e., parts which weigth can not be directly observed although we can determin its weight as a derivative of
# existing weights It alse deletes from the list the elements that are used only for the derivation purposes only.
DeriveMissingParts <- function(out) {
  ####### EPMI ######### data=out[['EPMI']]; nr=which(data[,2]=='flower_calyx') data[nr,3]=NA
  ####### data[nr,4]=data[data$part=='flower_all_parts',4]-data[data$part=='flower_petals',4] data=data[!data$part=='flower_all_parts',] out[['EPMI']]=data;

  ####### COER #########
  data <- out[["COER"]]
  nr <- which(data[, 2] == "flower_stigma")
  data[nr, 3] <- NA
  data[nr, 4] <- data[data$part == "flower_all_parts", 4] - data[data$part == "flower_petals", 4]
  data <- data[!data$part == "flower_all_parts", ]

  # nr=which(data[,2]=='finished_flower') data[nr,3]=NA data[nr,4]=data[data$part=='finished_flower_all_parts',4]-data[data$part=='flower_petals',4]
  # data=data[!data$part=='finished_flower_all_parts',]

  out[["COER"]] <- data
  ######################

  ####### PILI #########
  data <- out[["PILI"]]
  nr <- which(data[, 2] == "flower_stigma")
  data[nr, 1] <- "PILI"
  data[nr, 3] <- NA
  data[nr, 4] <- data[data$part == "fruit_young", 4] * 0.1

  out[["PILI"]] <- data

  ####### HEPU ######### data=out[['HEPU']]; nr=which(data[,2]=='fruit_aborted') data[nr,3]=NA
  ####### data[nr,4]=data[data$part=='calyx_aborted_fruit',4]+data[data$part=='fruit_aborted',4] data=data[!data$part=='calyx_aborted_fruit',] out[['HEPU']]=data;

  out
}

make_AvWeightPerUnit <- function(Maps, FPSummary) {

  ### Average weight of pieces, i.e., mass/count - OLD VERSION
  out <- list()
  FPSummary <- FPSummary[FPSummary$category_use == "used", ]
  for (species.name in unique(FPSummary$species)) {
    Sp.Data <- FPSummary[FPSummary$species == species.name, ]
    PartList <- Maps[[species.name]]
    av.weight <- n <- count <- c()
    for (i in seq_len(length(PartList))) {
      I <- (Sp.Data$part == PartList[i])
      if (sum(I) > 0) {
        DataForPart <- Sp.Data[I, ]
        DataForPart <- DataForPart[!is.na(as.numeric(DataForPart$weight)), ]
        if (sum(!is.na(DataForPart$weight) & is.na(DataForPart$count)) > 0) {
          DataForPart[!is.na(DataForPart$weight) & is.na(DataForPart$count), ]$count <- 1
        }
        av.weight[i] <- (sum(as.numeric(DataForPart$weight))/sum(DataForPart$count))
        n[i] <- length(DataForPart$weight)
        count[i] <- sum(DataForPart$count)
      } else {
        av.weight[i] <- NA
        n[i] <- NA
        count[i] <- NA
      }
    }
    out[[species.name]] <- data.frame(species = species.name, part = PartList, n = n, av.weight = av.weight, stringsAsFactors = FALSE)
  }
  # Add additional parts which are derivative of the existing measurments
  out <- DeriveMissingParts(out)
  #
  # path <- "output/docs"
  # if (!file.exists(path))
  #   dir.create(path, recursive = TRUE)
  # for (species.name in unique(FPSummary$species)) {
  #   print(xtable(out[[species.name]], include.rownames = FALSE), floating = FALSE, type = "latex", file = paste0("output/docs/", species.name, ".tex"))
  # }

  #merge output into single dataframe
  do.call(rbind, out)

}



###### new version library(plyr) FPSummary=read.csv('data/flower_parts.csv',header=TRUE,sep=',', stringsAsFactors = FALSE) summarise.parts <- function(df){ index <-
###### !is.na(df$weight)& is.na(df$count)& !is.na(df$length) if(sum(index)>0) df$count[index] <- 1 # reset count for length.measurements. Why?
###### data.frame(n=length(df$weight),av.weight=sum(as.numeric(df$weight))/sum(df$count)) } summary.all <- ddply(FPSummary, .(species, part), summarise.parts)
###### write.csv(summary.all, file='output/partWeights/all.csv', quote=FALSE, row.names=FALSE) # print latex tables for each species d_ply(summary.all, .(species),
###### function(x) print(xtable(x, digits=2), include.rownames=FALSE, floating=FALSE, type='latex', file=paste0('output/partWeights/', x$species[1],'.tex')))


make_IndividualBasedWeights <- function(FPSummary) {

  FPSummary <- FPSummary[FPSummary$census_notes_use == "used", c("individual", "part", "census_to_use", "weight", "count", "dimension_height", "dimension_diameter")]
  FPSummary <- FPSummary[!is.na(FPSummary$weight), ]
  IndividualBasedWeights <- FPSummary[!is.na(FPSummary$census_to_use), ]
  ToDivide <- FPSummary[is.na(FPSummary$census_to_use), ]
  for (i in seq_len(nrow(ToDivide))) {
    count <- as.numeric(unlist(strsplit(as.character(ToDivide[i, "census_to_use"]), split = ";")))
    ToAdd <- ToDivide[rep(i, length(count)), ]
    ToAdd$census_to_use <- count
    IndividualBasedWeights <- rbind(IndividualBasedWeights, ToAdd)
  }
  rownames(IndividualBasedWeights) <- NULL
  IndividualBasedWeights$weight <- as.numeric(IndividualBasedWeights$weight)
  IndividualBasedWeights$count <- as.numeric(IndividualBasedWeights$count)
  IndividualBasedWeights["av_weights"] <- IndividualBasedWeights$weight/IndividualBasedWeights$count
  IndividualBasedWeights <- IndividualBasedWeights[!is.na(IndividualBasedWeights$av_weights), ]
  IndividualBasedWeights[is.na(IndividualBasedWeights)] <- 0
  IndividualBasedWeights <- aggregate(cbind(count, weight) ~ individual + part + census_to_use + dimension_height + dimension_diameter, data = IndividualBasedWeights,
    FUN = sum)
  IndividualBasedWeights[IndividualBasedWeights == 0] <- NA
  IndividualBasedWeights["av_weights"] <- IndividualBasedWeights$weight/IndividualBasedWeights$count
  IndividualBasedWeights

}


combine_RA <- function(..., d=list(...), IndividualsList) {
    ret <- ldply(d, function(x) x)
    if(!all(ret$Tree_ID %in% IndividualsList$individual[IndividualsList$use_for_allocation_calculations]))
      stop("problem with individuals list")
    ret
 }
