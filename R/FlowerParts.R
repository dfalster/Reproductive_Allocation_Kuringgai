# Defines functions that calculates the unit weights of all reproductive
# tissues for each individual, including special calculations for determining,
# where applicable:
#   i)  weights based on dimensions; and
#   ii) individual based weights.


make_PartsSummary <- function(FlowerParts, PartsList) {
  list(IndividualBasedWeights = make_IndividualBasedWeights(FlowerParts), AvCountsPerMM = make_AvCountsPerMM(FlowerParts),
    RegressionTable = make_RegressionTable(FlowerParts), AvWeightPerUnit = make_AvWeightPerUnit(FlowerParts,
      PartsList))
}

# Average counts per mm for specified that require it. Specie (not part)
# specific.
make_AvCountsPerMM <- function(FPSummary) {
  data <- FPSummary %>% filter(part == "count_by_length")
  ret <- NULL

  if (nrow(data) > 0) {
    ret <- group_by(data, species) %>% summarise(AvCountPerMM = mean(count/length),
      tot.count = sum(count))
  }
  ret
}

get_volume <- function(diameter, height) {
  height * (diameter/2)^2 * pi
}

# Calculating the regression coefficients required for calculations of weight
# based on the measured dimension of the plant part.  Calculations are only
# made for TODO: make these plots pdf(file = paste('output/docs/',
# species.name, '_', part.name, '.pdf', sep = ''), width = 10, height = 10)
# plot(height, weight, type = 'p', main = paste(species.name, part.name, sep =
# ': ')) x.dense <- data.frame(height = seq(from = min(height), to =
# max(height), length.out = 100)) lines(x.dense$height, predict.lm(object =
# lm.res, newdata = x.dense)) dev.off()


# TODO: Would it be better to save these as lm objects? could then use predict
# function?

make_RegressionTable <- function(FPSummary) {

  fit_regression <- function(PartData, reg.order, reg.type) {
    ret <- data.frame(n = c(), intercept = c(), slope = c())
    if (reg.type == "height") {
      height <- PartData$dimension_height
      weight <- as.numeric(PartData$weight)
      ## What is the I here?
      lmFit <- lm(weight ~ I(height^reg.order))
      ret <- data.frame(n = length(height), intercept = coef(lmFit)[1], slope = coef(lmFit)[2])
    } else if (reg.type == "height_noIntercept") {
      height <- PartData$dimension_height
      weight <- as.numeric(PartData$weight)/PartData$count
      lmFit <- lm(weight ~ 0 + I(height^reg.order))
      ret <- data.frame(n = length(height), intercept = 0, slope = coef(lmFit)[1])
    } else if (reg.type == "averageWeight") {
      height <- PartData$dimension_height
      weight <- as.numeric(PartData$weight)/PartData$count
      lmFit <- lm(weight ~ I(height^reg.order))
      ret <- data.frame(n = length(height), intercept = coef(lmFit)[1], slope = coef(lmFit)[2])
    } else if (reg.type == "volume") {
      volume <- get_volume(PartData$dimension_diameter, PartData$dimension_height)
      n <- length(volume)
      weight <- as.numeric(PartData$weight)
      lmFit <- lm(weight ~ 0 + I(volume^reg.order))
      ret <- data.frame(n = length(volume), intercept = 0, slope = coef(lmFit)[1])
    }
    ret
  }

  regToFit <- data.frame(rbind(c("GRBU", "fruit_large_immature", 2, "height"),
    c("GRBU", "seed_pod", 1, "height"), c("GRSP", "fruit_large_immature", 2,
      "height"), c("GRSP", "seed_pod", 1, "height"), c("PELA", "fruit_large_immature",
      2, "averageWeight"), c("PELA", "seed_pod", 3, "averageWeight"), c("PELA",
      "seed", 3, "averageWeight"), c("BAER", "cone_young", 2, "height_noIntercept"),
    c("BAER", "cone_green", 1, "volume"), c("BAER", "cone_brown_no_expanded_follicles",
      1, "volume"), c("BAER", "cone_brown", 1, "volume"), c("BAER", "cone_aborted",
      2, "height_noIntercept"), c("BAER", "cone_base_green", 1, "volume"),
    c("BAER", "cone_base_brown", 1, "volume"), c("PEPU", "cone_just_starting",
      1, "averageWeight"), c("PEPU", "cone_young", 1, "averageWeight"), c("PEPU",
      "cone_green", 1, "volume"), c("PEPU", "cone_brown", 1, "volume"), c("PEPU",
      "cone_aborted", 1, "height_noIntercept")), stringsAsFactors = FALSE)
  names(regToFit) <- c("species", "part", "reg.order", "reg.type")
  regToFit$reg.order <- as.numeric(regToFit$reg.order)
  regToFit <- filter(regToFit, species %in% unique(FPSummary$species))
  FPSummary <- filter(FPSummary, category_use == "used")
  ddply(regToFit, c("species", "part", "reg.type", "reg.order"), function(x) fit_regression(filter(FPSummary,
    part == x$part, species == x$species), x$reg.order, x$reg.type))
}



# Function calculating weight for missing parts, i.e., parts which weigth can
# not be directly observed although we can determin its weight as a derivative
# of existing weights It alse deletes from the list the elements that are used
# only for the derivation purposes only.
DeriveMissingParts <- function(data) {

  ##### COER #########
  nr <- which(data[["part"]] == "flower_stigma" && data[["species"]] == "COER")
  data[nr, "weight"] <- data[data$part == "flower_all_parts", 4] - data[data$part ==
    "flower_petals", 4]
  data <- data[!data$part == "flower_all_parts", ]

  ##### PILI #########
  nr <- which(data[["part"]] == "flower_stigma" && data[["species"]] == "PILI")
  data[nr, "weight"] <- data[data$part == "fruit_young", 4] * 0.1

  data
}


# Average weight of pieces, i.e., mass/count - OLD VERSION

make_AvWeightPerUnit <- function(FPSummary, PartList) {


  species.name <- FPSummary$species[1]
  Sp.Data <- FPSummary[FPSummary$category_use == "used", ]

  weight <- n <- count <- c()
  for (i in seq_len(length(PartList))) {
    I <- (Sp.Data$part == PartList[i])
    if (sum(I) > 0) {
      DataForPart <- Sp.Data[I, ]
      DataForPart <- DataForPart[!is.na(as.numeric(DataForPart$weight)),
        ]
      if (sum(!is.na(DataForPart$weight) & is.na(DataForPart$count)) > 0) {
        DataForPart[!is.na(DataForPart$weight) & is.na(DataForPart$count),
          ]$count <- 1
      }
      weight[i] <- (sum(as.numeric(DataForPart$weight))/sum(DataForPart$count))
      n[i] <- length(DataForPart$weight)
      count[i] <- sum(DataForPart$count)
    } else {
      weight[i] <- NA
      n[i] <- NA
      count[i] <- NA
    }
  }
  out <- data.frame(species = species.name, part = PartList, weight = weight,
    stringsAsFactors = FALSE)

  # Add additional parts which are derivative of the existing measurements
  DeriveMissingParts(out)

}


make_IndividualBasedWeights <- function(FPSummary) {

  FPSummary <- FPSummary[FPSummary$census_notes_use == "used", c("individual",
    "part", "census_to_use", "weight", "count", "dimension_height", "dimension_diameter")]
  FPSummary <- FPSummary[!is.na(FPSummary$weight), ]
  IndividualBasedWeights <- FPSummary[!is.na(FPSummary$census_to_use), ]
  ToDivide <- FPSummary[is.na(FPSummary$census_to_use), ]
  for (i in seq_len(nrow(ToDivide))) {
    count <- as.numeric(unlist(strsplit(as.character(ToDivide[i, "census_to_use"]),
      split = ";")))
    ToAdd <- ToDivide[rep(i, length(count)), ]
    ToAdd$census_to_use <- count
    IndividualBasedWeights <- rbind(IndividualBasedWeights, ToAdd)
  }
  rownames(IndividualBasedWeights) <- NULL
  IndividualBasedWeights$weight <- as.numeric(IndividualBasedWeights$weight)
  IndividualBasedWeights$count <- as.numeric(IndividualBasedWeights$count)
  IndividualBasedWeights["av_weights"] <- IndividualBasedWeights$weight/IndividualBasedWeights$count
  IndividualBasedWeights <- IndividualBasedWeights[!is.na(IndividualBasedWeights$av_weights),
    ]
  IndividualBasedWeights[is.na(IndividualBasedWeights)] <- 0
  IndividualBasedWeights <- aggregate(cbind(count, weight) ~ individual + part +
    census_to_use + dimension_height + dimension_diameter, data = IndividualBasedWeights,
    FUN = sum)
  IndividualBasedWeights[IndividualBasedWeights == 0] <- NA
  IndividualBasedWeights["av_weights"] <- IndividualBasedWeights$weight/IndividualBasedWeights$count
  IndividualBasedWeights
}


combine_Growth <- function(..., d = list(...)) {
  ldply(d, function(x) x)
}

