# Removing duplicates from the list - Need to be used if a mixed method was
# used to obtain the weights at one census - Need to be used for joining the
# information from old and new census to a total one.
RemoveDuplicates <- function(C) {
  C_new <- list()
  parts.names <- unique(sapply(C, function(x) x$type))
  k <- length(parts.names)
  for (i in seq_len(k)) {
    part.name <- parts.names[i]
    repeated.parts <- C[unlist(sapply(C, "[[", "type")) == part.name]
    total.count <- sum(unlist(sapply(repeated.parts, "[[", "count")))
    total.weights <- as.vector(unlist(sapply(repeated.parts, "[[", "weight")))
    methods.used <- as.vector(unlist(sapply(repeated.parts, "[[", "m.type")))
    if (all(methods.used == methods.used[1])) {
      m.type <- methods.used[1]
    } else {
      m.type <- "mixed"
    }
    Element <- list(type = part.name, m.type = m.type, count = total.count,
      weight = total.weights)
    C_new[[i]] <- Element
  }

  C_new
}

# Choose specific individual

WeightCalculationsForTree <- function(Tree, FloweringCategories, PartsSummary) {
  # Determine the id for individual and put it as the first element in a list
  TreeID <- as.character(unique(Tree$individual))
  TreeList <- list(TreeID = TreeID)
  # Choose specific census
  k <- 1
  for (i in unique(Tree$census)) {
    # Measurments from Census i
    C_old <- Tree[(Tree$census == i) & (Tree$pre.new == "pre-existing"), ]
    C_new <- Tree[(Tree$census == i) & (Tree$pre.new == "new"), ]
    # Transforming measurments to weights
    C_old_list <- WeightCalculationsAtCensus(C_old, TreeID, i, FloweringCategories,
      PartsSummary)
    # Determine species name and allowed plant par

    C_new_list <- WeightCalculationsAtCensus(C_new, TreeID, i, FloweringCategories,
      PartsSummary)
    C_total <- c(C_old_list, C_new_list)
    C_total <- RemoveDuplicates(C_total)
    # Create list containing new, pre.existing and total data for the given census
    C <- list(pre.ex = C_old_list, new = C_new_list, total = C_total)
    # Add the information about weight structure at time i (place i+1 in the list)
    # to the tree List
    TreeList[[k + 1]] <- C
    k <- k + 1
  }
  # Return tree List
  TreeList
}


# Functions that transform census data to weights 1. Count to vector weights 2.
# Length of plant part to count 3. Dimension of individual to it's weight.

# Weights from counts (vector with homogeneous weights)
WeightFromCount <- function(count, species, part, individual, census, AvWeightPerUnit,
  IBW) {
  if (grepl("0", part)) {
    part <- str_sub(string = part, start = 1, end = -4)
  }
  I <- (IBW$individual == individual) & (IBW$part == part) & (IBW$census_to_use ==
    census)
  n_i <- sum(I)
  if (n_i == 1) {
    weight <- IBW[I, "av_weights"]
    # print(paste('Using exact weight',part,census,weight,sep=' '))
  }
  if (n_i == 0) {
    weight <- AvWeightPerUnit[(AvWeightPerUnit$species == species) & (AvWeightPerUnit$part ==
      part), ]$weight
  }
  w <- rep(1, count) * weight
  w
}

# Weights from length (vector with homogeneous weights)
WeightFromLength <- function(length, species, part, individual, census, AvCountsPerMM,
  AvWeightPerUnit, IBW) {
  if (grepl("0", part)) {
    part <- str_sub(string = part, start = 1, end = -4)
  }
  count <- round(length * AvCountsPerMM[AvCountsPerMM$species == species, ]$AvCountPerMM)
  w <- WeightFromCount(count, species, part, individual, census, AvWeightPerUnit,
    IBW)
  w
}

# Weight from dimensions (for single plant element)
WeightFromRegression <- function(height, diameter, species, part, individual, census,
  IBW, RegressionTable) {
  if (grepl("0", part)) {
    part <- str_sub(string = part, start = 1, end = -4)
  }
  w <- NA

  I <- (IBW$individual == individual) & (IBW$part == part) & (IBW$census_to_use ==
    census) & (IBW$dimension_height == height) & ((is.na(IBW$dimension_diameter) &
    (is.na(diameter))) | (IBW$dimension_diameter == diameter))

  n_i <- sum(I)
  if (n_i == 1) {
    w <- IBW[I, "av_weights"]
    # print(paste('Using exact weight reg',part,census,w,sep=' '))
  }
  if (n_i == 0) {
    parameters <- RegressionTable[(RegressionTable$species == species) & (RegressionTable$part ==
      part), ]
    if (parameters[["reg.type"]] == "volume") {
      w <- parameters$intercept + parameters$slope * get_volume(diameter,
        height)
    } else {
      w <- parameters$intercept + parameters$slope * height^parameters$reg.order
    }
  }
  w
}


# Function that for given Census construct the list that contains the
# information about the weight of each part

WeightCalculationsAtCensus <- function(C, TreeID, census, FloweringCategories,
  PartsSummary) {

  RegressionTable <- PartsSummary$RegressionTable
  AvCountsPerMM <- PartsSummary$AvCountsPerMM
  AvWeightPerUnit <- PartsSummary$AvWeightPerUnit
  IBW <- PartsSummary$IndividualBasedWeights

  # Determine species name and allowed plant parts together with their way of
  # measuring
  species <- substr(TreeID, 1, 4)
  Parts <- FloweringCategories[, c("flower_part", species)]
  Parts <- Parts[!(Parts[, species] == ""), ]
  n.parts <- nrow(Parts)
  C_list <- list()
  k <- 0
  # For each of the parts check how to transform inform in repro_spreadheet to
  # weight and perform given calculations
  for (i in seq_len(n.parts)) {
    if (Parts[i, species] == "count") {
      count_ch <- C[, Parts[i, "flower_part"]]
      if (!is.na(count_ch) & count_ch != "") {
        k <- k + 1
        type <- Parts[i, "flower_part"]
        m.type <- "count"
        count <- sum(as.numeric(unlist(strsplit(as.character(count_ch),
          split = ";"))))
        weight <- WeightFromCount(count, species, type, TreeID, census,
          AvWeightPerUnit, IBW)

        # specific for inflorescence in HATE(Lizzy made calculations on scale of
        # inflorescent hence adjustments. Possibly we can move it to Multiplier table,
        # althought it is slightly different reasoning) 15June2016 Lizzy moved to
        # multiplier table, because current method also increasing weight of buds by
        # multiplier, so investment off if ((species == 'HATE') *
        # (grepl('inflorescence', type))) { count <- round(count * 5.8696) weight <-
        # rep(weight[1]/5.8696, count) }
        Element <- list(type = type, m.type = m.type, count = length(weight),
          weight = weight)
        C_list[[k]] <- Element
      }
    }
    if (Parts[i, species] == "count_by_length") {
      length_ch <- C[, paste0(Parts[i, "flower_part"], "_by_length")]
      if (!is.na(length_ch) & length_ch != "") {
        k <- k + 1
        type <- Parts[i, "flower_part"]
        m.type <- "length"
        length <- sum(as.numeric(unlist(strsplit(as.character(length_ch),
          split = ";"))))
        weight <- WeightFromLength(length, species, type, TreeID, census,
          AvCountsPerMM, AvWeightPerUnit, IBW)
        Element <- list(type = type, m.type = m.type, count = length(weight),
          weight = weight)
        C_list[[k]] <- Element
      }
    }

    if (Parts[i, species] == "count; count_by_length") {
      count_ch <- C[, Parts[i, "flower_part"]]
      if (!is.na(count_ch) & count_ch != "") {
        k <- k + 1
        type <- Parts[i, "flower_part"]
        m.type <- "count"
        count <- sum(as.numeric(unlist(strsplit(as.character(count_ch),
          split = ";"))))
        weight <- WeightFromCount(count, species, type, TreeID, census,
          AvWeightPerUnit, IBW)
        Element <- list(type = type, m.type = m.type, count = count, weight = weight)
        C_list[[k]] <- Element
      }
      length_ch <- C[, paste0(Parts[i, "flower_part"], "_by_length")]
      if (!is.na(length_ch) & length_ch != "") {
        k <- k + 1
        type <- Parts[i, "flower_part"]
        m.type <- "length"
        length <- sum(as.numeric(unlist(strsplit(as.character(length_ch),
          split = ";"))))

        weight <- WeightFromLength(length, species, type, TreeID, census,
          AvCountsPerMM, AvWeightPerUnit, IBW)
        Element <- list(type = type, m.type = m.type, count = length(weight),
          weight = weight)
        C_list[[k]] <- Element
      }
    }


    if (Parts[i, species] == "regress_by_dim") {
      height_ch <- C[, Parts[i, "flower_part"]]
      if (!is.na(height_ch) & height_ch != "") {
        k <- k + 1
        type <- Parts[i, "flower_part"]
        m.type <- "regress_by_dim"
        heights <- as.numeric(unlist(strsplit(as.character(height_ch),
          split = ";")))
        count <- length(heights)
        weight <- c()
        for (j in seq_len(count)) {
          weight[j] <- WeightFromRegression(heights[j], NA, species, type,
          TreeID, census, IBW, RegressionTable)
        }
        Element <- list(type = type, m.type = m.type, count = length(weight),
          weight = weight)
        C_list[[k]] <- Element
      }
    }

    if (Parts[i, species] == "volume") {
      dimension_ch <- C[, Parts[i, "flower_part"]]
      if (!is.na(dimension_ch) & dimension_ch != "") {
        k <- k + 1
        type <- Parts[i, "flower_part"]
        m.type <- "volume"
        heights <- matrix(as.numeric(unlist(strsplit(unlist(strsplit(as.character(dimension_ch),
          split = ";")), split = "x"))), ncol = 2, byrow = T)[, 1]
        diameters <- matrix(as.numeric(unlist(strsplit(unlist(strsplit(as.character(dimension_ch),
          split = ";")), split = "x"))), ncol = 2, byrow = T)[, 2]
        count <- length(heights)
        weight <- c()

        for (j in seq_len(count)) {
          weight[j] <- WeightFromRegression(heights[j], diameters[j], species,
          type, TreeID, census, IBW, RegressionTable)
        }
        Element <- list(type = type, m.type = m.type, count = length(weight),
          weight = weight)
        C_list[[k]] <- Element
      }
    }
  }

  # Checking if we need to merge the results
  if (sum(duplicated(unlist(sapply(C_list, function(x) x[1])))) > 0) {
    # there are duplicates from different measurement methods and we need to join
    # them together.
    C_list <- RemoveDuplicates(C_list)
  }
  C_list
}
