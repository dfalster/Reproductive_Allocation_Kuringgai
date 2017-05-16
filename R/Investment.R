# Functions that combine, for each species, the map of parts (`GraphMaps`),
# weights of flower parts (`PartsSummary`), and the census data
# (`Reproduction`) to calculate the actual investment in reproductive parts.

# The output is a list with four dataframes
#  - Lost - parts lost during development - same number lost
#  - Finished Development is everything (including lost parts) - # rename to Halted development
#  - Investment = energy investment in going from one stage to next
#   - Error - amounts that were pre-existing on plant at start of census, i.e. no predecessor - rename to pre-existing

CalculateInvestmentForSpecies <- function(species, Reproduction, FloweringCategories,
  MultiplierTable, GraphMaps, PartsSummary) {

  combine <- function(name, d) {
    ldply(d, function(x) x[[name]])
  }

  R <- llply(unique(Reproduction$individual), function(x) CalculateInvestmentForIndiviualPlant(x,
    filter(Reproduction, individual == x), FloweringCategories, MultiplierTable,
    GraphMaps, PartsSummary))

  FD <- combine("FD", R)

  list(Investment = combine("Investment", R), Lost = combine("Lost", R), FD = FD,
    Error = combine("Error", R))
}


CalculateInvestmentForIndiviualPlant <- function(individual, Reproduction, FloweringCategories,
  MultiplierTable, GraphMaps, PartsSummary) {

  print(individual)

  # Transform counts to weights and adjust for multiplicity Returns a list, one
  # per census Each element of the list is also a list, with possible elements
  # 'pre.ex' 'new' 'total', Each element in those lists is the weight and number
  # of a particular part
  TreeListOrig <- WeightCalculationsForTree(Reproduction, FloweringCategories, PartsSummary)
  TreeListAdj <- AdjustForMultiplicity(TreeListOrig, MultiplierTable)

  # Calculate investment
  Res <- InvestmentCalculations(TreeListAdj, GraphMaps)

  # Extract and reorder the results
  Inv <- Res[["Inv"]]
  if (!is.null(Inv)) {
    Inv["age"] <- as.numeric(unique(Reproduction$age))
    Inv <- Inv[, c("individual", "age", "FromCensus", "ToCensus", "From", "To",
      "Inv", "Count", "Total")]
  }

  Err <- Res[["Err"]]
  if (!is.null(Err)) {
    Err["individual"] <- individual
    Err <- Err[, c("individual", "Element", "Census", "Count")]
  }
  Lost <- Res[["Lost"]]
  if (!is.null(Lost)) {
    if (nrow(Lost) > 0) {
      Lost["individual"] <- individual
      Lost["age"] <- as.numeric(unique(Reproduction$age))
      Lost <- Lost[, c("individual", "age", "part", "Census", "count", "weight")]
    }
    if (nrow(Lost) == 0) {
      Lost <- NULL
    }
  }

  FD <- Res[["FinishedDevelopement"]]

  if (!is.null(FD)) {
    if (nrow(FD) > 0) {
      FD["individual"] <- individual
      FD["age"] <- as.numeric(unique(Reproduction$age))
      FD <- FD[, c("weight", "individual", "species", "part", "Census", "count")]
    }
    if (nrow(FD) == 0) {
      FD <- NULL
    }
  }
  list(Investment = Inv, Lost = Lost, Error = Err, FD = FD)
}


InvestmentCalculations <- function(TreeListOrig, GraphMaps) {
  last.census <- length(TreeListOrig)
  n.censuses <- last.census - 1
  # Extract species name
  species <- substr(TreeListOrig[[1]], 1, 4)
  # Initialize the variables
  ErrList <- data.frame(Element = NA, Census = NA, Count = NA)
  Lost <- data.frame(part = c(), Census = c(), count = c(), weight = c())
  FinishedDevelopement <- data.frame(species = c(), part = c(), Census = c(),
    count = c(), weight = c())
  if ((length(sapply(TreeListOrig[[last.census]]$total, function(x) x$type) >
    0))) {
    FinishedDevelopement <- rbind(FinishedDevelopement, data.frame(species = species,
      part = sapply(TreeListOrig[[last.census]]$total, function(x) x$type),
      Census = rep(n.censuses, length(sapply(TreeListOrig[[last.census]]$total,
        function(x) x$type))), count = sapply(TreeListOrig[[last.census]]$total,
        function(x) x$count), weight = sapply(TreeListOrig[[last.census]]$total,
        function(x) sum(x$weight))))
  }

  Investments <- data.frame(FromCensus = c(), ToCensus = c(), From = c(), To = c(),
    Inv = c(), Count = c(), type = c())
  Err <- c()
  # If tree had no reproduction at all return list of empty data frames
  if (is.null(unlist(TreeListOrig[2:last.census]))) {
    return(list(Inv = c(), Err = Err, Lost = Lost, FinishedDevelopement = FinishedDevelopement))
  }

  # Check how many main paths there are for this plant.
  n.paths <- nrow(GraphMaps$Paths)
  Paths <- GraphMaps$Paths
  # For each of the paths
  for (a in seq_len(n.paths)) {
    L <- data.frame(part = c(), Census = c(), count = c(), weight = c())
    FD <- data.frame(species = c(), part = c(), Census = c(), count = c(),
      weight = c())
    # Determine beginning and end of the path.
    BE <- (GraphMaps$Paths[a, ])
    # Read in plant graph
    Plant.Graph <- GraphMaps$graph
    # Extract the list of vertices for the path
    PATH <- get.shortest.paths(Plant.Graph, from = as.character(BE[1, 1]),
      to = as.character(BE[1, 2]), weights = NA)
    PATH <- unlist(PATH$vpath)
    V_names <- get.vertex.attribute(Plant.Graph, "name")
    # Define the progression list
    Progression <- V_names[PATH]
    n <- length(Progression)

    # Duplicate tree structure (Acc= for accessories calculations, Pred=for storing
    # possible predecessors) Will modify below, TreeLis contains list of lists, one
    # for each census. As we identify, parents for a given object, they are removed
    # from the list of possible predecssors. In this way progressively identify
    # predecessors.
    TreeList <- TreeListOrig
    TreeList_Acc <- TreeList
    TreeList_Pred <- TreeList
    # Go backwards in time i=j -> Census= j-1
    for (i in last.census:2) {
      # Restrict your data to the time frame of interest
      TreeList <- TreeList[seq_len(i)]
      TreeList_Pred <- TreeList_Pred[seq_len(i)]
      TreeList_Acc <- TreeList_Acc[seq_len(i)]
      # Go backwards in the progression (from most developed to the least developed
      # parts)
      for (j in rev(seq_len(n))) {
        Element <- Progression[j]
        # Calculate the investment for that part (restricting possible progression)
        R <- InvestmentInAPartType(TreeList, TreeList_Pred, Element, Progression[1:j],
          GraphMaps)
        # Update the list of possible predecessors (remove the ones that you used)
        TreeList_Pred <- R[["TreeList_Pred"]]
        # Store investment and possible errors
        Investments <- rbind(Investments, R[["Inv"]])
        ErrList <- rbind(ErrList, R[["Err"]])

        {
          # Some elements have aborted/empty parts which can be their alternative at that
          # stage of reproduction. Make calculations for them Checking if there are
          # X-OR-s before moving to the lower development
          dist.to.root <- as.numeric(shortest.paths(Plant.Graph, v = Progression[1],
          to = Progression[j], weight = NA))
          which.have.the.same.dist <- shortest.paths(Plant.Graph, v = Progression[1],
          weight = NA) == dist.to.root
          # Find the ones that belong to the same group
          which.have.the.same.col <- V(Plant.Graph)$col == get.vertex.attribute(Plant.Graph,
          index = Progression[1], name = "col")
          # Find the ones that are not the original value
          which.are.not.the.orig <- !(V(Plant.Graph)$name == Progression[j])

          # The list of the XOR parts (mostly aborted fruits,seeds,a.s.o which should be
          # taken into consideration when calculating part was lost. This parts are
          # ancesstors of the previous part as the one on the main axis is)
          XOR.Part <- V(Plant.Graph)[which.have.the.same.dist & which.have.the.same.col &
          which.are.not.the.orig]
          XOR.Part <- XOR.Part$name
          n.xors <- length(XOR.Part)

          for (l in seq_len(n.xors)) {
          # Determine the XOR progression
          XOR.Progression <- get.vertex.attribute(Plant.Graph, name = "name",
            index = unlist(get.shortest.paths(Plant.Graph, from = Progression[1],
            to = XOR.Part[l])$vpath))
          R <- InvestmentInAPartType(TreeList, TreeList_Pred, XOR.Part[l],
            XOR.Progression, GraphMaps)

          TreeList_Pred <- R[["TreeList_Pred"]]
          Investments <- rbind(Investments, R[["Inv"]])
          ErrList <- rbind(ErrList, R[["Err"]])
          }
        }
      }
      # Befor moving to next census (from time t to t-1) check the list of avilable
      # predecessor at time t-1 contain any elements. They will not develop to
      # anything, hence they are lost.
      if ((i > 2) & (i < n.censuses)) {
        if ((length(sapply(TreeList_Pred[[i - 1]]$total, function(x) x$type) >
          0))) {
          L <- rbind(L, data.frame(part = sapply(TreeList_Pred[[i - 1]]$total,
          function(x) x$type), Census = rep(i - 2, length(sapply(TreeList_Pred[[i -
          1]]$total, function(x) x$type))), count = sapply(TreeList_Pred[[i -
          1]]$total, function(x) x$count), weight = sapply(TreeList_Pred[[i -
          1]]$total, function(x) sum(x$weight))))
        }
      }

      if (i > 2) {
        if ((length(sapply(TreeList_Pred[[i - 1]]$total, function(x) x$type) >
          0))) {
          FD <- rbind(FD, data.frame(species = species, part = sapply(TreeList_Pred[[i -
          1]]$total, function(x) x$type), Census = rep(i - 2, length(sapply(TreeList_Pred[[i -
          1]]$total, function(x) x$type))), count = sapply(TreeList_Pred[[i -
          1]]$total, function(x) x$count), weight = sapply(TreeList_Pred[[i -
          1]]$total, function(x) sum(x$weight))))
        }
      }
    }

    # Restrict loses to the line that you made your calculations on
    Lost <- rbind(Lost, L[as.character(L$part) %in% V(Plant.Graph)$name[V(Plant.Graph)$col ==
      get.vertex.attribute(Plant.Graph, "col", index = Progression[1])],
      ])
    FinishedDevelopement <- rbind(FinishedDevelopement, FD[as.character(FD$part) %in%
      V(Plant.Graph)$name[V(Plant.Graph)$col == get.vertex.attribute(Plant.Graph,
        "col", index = Progression[1])], ])

    ################# Calculating cost of accessories Check the color of main progression line
    Progression_color <- get.vertex.attribute(Plant.Graph, index = Progression[1],
      name = "col")
    # Accesoried to that line have color 1 higher, moreover choose the last stage
    # of progression for each accesory (degree=1)
    Acc.Finals <- V(Plant.Graph)[(V(Plant.Graph)$col == (Progression_color +
      1)) & (degree(Plant.Graph) == 1)]$name
    # If there are accessories
    for (k in seq_len(length(Acc.Finals))) {
      L <- data.frame(part = c(), Census = c(), count = c(), weight = c())
      FD <- data.frame(species = c(), part = c(), Census = c(), count = c(),
        weight = c())

      Accessory <- Acc.Finals[k]
      # Reset the lists to the original ones.
      TreeList <- TreeListOrig
      TreeList_Acc <- TreeListOrig
      # Find the progression to the root
      Aux_Progression_to_root <- get.vertex.attribute(Plant.Graph, name = "name",
        unlist(get.shortest.paths(Plant.Graph, to = Accessory, from = Progression[1],
          weight = NA)$vpath))
      # Find the elements of progression that are not on main progression line
      I_not_on_main <- !Aux_Progression_to_root %in% Progression
      Progression_Not_on_Main <- Aux_Progression_to_root[I_not_on_main]
      n.aux <- length(Progression_Not_on_Main)
      # Perform investment calculations in the same mannes as in the previous case.
      # Only calculations for elements not on the main progression are made, however
      # all the progression is allowed.
      for (i in last.census:2) {
        TreeList <- TreeList[seq_len(i)]
        TreeList_Acc <- TreeList_Acc[seq_len(i)]
        for (l in rev(seq_len(n.aux))) {
          Element.aux <- Progression_Not_on_Main[l]
          R <- InvestmentInAPartType(TreeList, TreeList_Acc, Element.aux,
          Aux_Progression_to_root[seq_len(length(Aux_Progression_to_root) -
            n.aux + l)], GraphMaps)
          TreeList_Acc <- R[["TreeList_Pred"]]
          Investments <- rbind(Investments, R[["Inv"]])
          ErrList <- rbind(ErrList, R[["Err"]])

          # Calculate lost elements
          if ((i > 2) & (i < n.censuses)) {
          if ((length(sapply(TreeList_Acc[[i - 1]]$total, function(x) x$type) >
            0))) {
            L <- rbind(L, data.frame(part = sapply(TreeList_Acc[[i -
            1]]$total, function(x) x$type), Census = rep(i - 2, length(sapply(TreeList_Acc[[i -
            1]]$total, function(x) x$type))), count = sapply(TreeList_Acc[[i -
            1]]$total, function(x) x$count), weight = sapply(TreeList_Acc[[i -
            1]]$total, function(x) sum(x$weight))))
          }
          }
          if (i > 2) {
          if ((length(sapply(TreeList_Acc[[i - 1]]$total, function(x) x$type) >
            0))) {
            FD <- rbind(FD, data.frame(species = species, part = sapply(TreeList_Acc[[i -
            1]]$total, function(x) x$type), Census = rep(i - 2, length(sapply(TreeList_Acc[[i -
            1]]$total, function(x) x$type))), count = sapply(TreeList_Acc[[i -
            1]]$total, function(x) x$count), weight = sapply(TreeList_Acc[[i -
            1]]$total, function(x) sum(x$weight))))
          }
          }
        }
      }
      # Restrict loss calculations to the elements outside main progression line and
      # only in the progression line of question (for k).
      Lost <- rbind(Lost, L[as.character(L$part) %in% Progression_Not_on_Main,
        ])
      FinishedDevelopement <- rbind(FinishedDevelopement, FD[as.character(FD$part) %in%
        Progression_Not_on_Main, ])

    }

  }
  Investments <- Investments[complete.cases(Investments), ]

  # Aggregate and order the result
  ErrList <- ErrList[complete.cases(ErrList), ]
  if (nrow(ErrList) > 0) {
    Err <- aggregate(Count ~ Element + Census, data = ErrList, FUN = "sum")
    Err <- Err[order(Err$Census), ]
  }
  Inv <- NULL
  Investments <- Investments[complete.cases(Investments), ]
  if (nrow(Investments) > 0) {
    # Count number of unique combinations of FromCensus, ToCensus, From, To, Inv
    Inv <- aggregate(Count ~ FromCensus + ToCensus + From + To + Inv, FUN = "sum",
      data = Investments)
    Inv <- Inv[order(Inv$ToCensus), ]
    # Total cost is product of number and per unit cost
    Inv["Total"] <- Inv$Inv * Inv$Count
    Inv["individual"] <- rep(TreeListOrig[[1]], nrow(Inv))
  }
  Lost <- unique(Lost)
  FinishedDevelopement <- unique(FinishedDevelopement)
  # Exclude elements of degree 1, they can not develop, not lost
  LeavesOfGraph <- V(Plant.Graph)$name[degree(Plant.Graph) > 1]
  # Add roots, if they are not the only elements
  NonAtomicPathsBeginning <- as.character(Paths[as.character(Paths[, 1]) != as.character(Paths[,
    2]), 1])
  # Progressable parts are defined by union of the both
  Progressable <- c(LeavesOfGraph, NonAtomicPathsBeginning)
  Lost <- Lost[Lost$part %in% Progressable, ]

  if (nrow(Lost) > 0) {
    Lost <- Lost[order(Lost$Census), ]
  }

  if (nrow(FinishedDevelopement) > 0) {
    FinishedDevelopement <- FinishedDevelopement[order(FinishedDevelopement$Census),
      ]
  }

  list(Inv = Inv, Err = Err, Lost = Lost, FinishedDevelopement = FinishedDevelopement)
}

InvestmentInAPartType <- function(TreeList, TreeList_Pred, Element, Progression,
  GraphMaps) {
  # Initialize data frames
  Inv <- From <- To <- FromCensus <- ToCensus <- Count <- c()
  Inv_new <- data.frame(FromCensus = FromCensus, ToCensus = ToCensus, From = From,
    To = To, Inv = Inv, Count = Count, type = c())
  Inv_pre.ex <- data.frame(FromCensus = FromCensus, ToCensus = ToCensus, From = From,
    To = To, Inv = Inv, Count = Count, type = c())

  N <- length(TreeList)
  # Are there any new parts of that type? If yes, calculate investment
  I_new <- sapply(TreeList[[N]]$new, function(x) x$type) == Element
  if (sum(unlist(I_new))) {
    Inv <- TreeList[[N]]$new[[which(I_new)]]$weight
    From <- rep("0", length(Inv))
    To <- rep(Element, length(Inv))
    ToCensus <- rep(N - 1, length(Inv))
    FromCensus <- rep(N - 2, length(Inv))
    Count <- rep(1, length(Inv))
    Inv_new <- data.frame(FromCensus = FromCensus, ToCensus = ToCensus, From = From,
      To = To, Inv = Inv, Count = Count, type = "new")
  }

  # Are there any pre-existing parts of that type.
  I_pre.ex <- sapply(TreeList[[N]]$pre.ex, function(x) x$type) == Element
  Inv <- From <- To <- Census <- Count <- FromCensus <- ToCensus <- c()
  ErrList <- data.frame(Element = NA, Census = NA, Count = NA)

  # If there are there are pre.existing parts and we are at least at Census 2 we
  # try to find a predecessor.
  if (sum(unlist(I_pre.ex)) & N > 2) {
    # Check how many elements of that type there are
    n_elements <- TreeList[[N]]$pre.ex[[which(I_pre.ex)]]$count
    for (i in seq_len(n_elements)) {
      # Extract the weight of a single element
      El_weight <- TreeList[[N]]$pre.ex[[which(I_pre.ex)]]$weight[i]
      # Calculate investment made into progressiong to the element from its ancestor.
      R <- InvestmentInIndividualPart(Element, TreeList_Pred, Progression,
        El_weight, GraphMaps$graph)
      Inv[i] <- R[["Invest"]]
      From[i] <- R[["from"]]
      To[i] <- R[["to"]]
      FromCensus[i] <- R[["FromCensus"]]
      ToCensus[i] <- R[["ToCensus"]]
      Count[i] <- R[["Count"]]
      TreeList_Pred <- R[["TreeList_Pred"]]
      ErrList <- rbind(ErrList, R[["ErrList"]])
    }
    Inv_pre.ex <- data.frame(FromCensus = FromCensus, ToCensus = ToCensus,
      From = From, To = To, Inv = Inv, Count = Count, type = "preexisting")
  }

  # Errors in the beginning
  if (sum(unlist(I_pre.ex)) & N == 2) {
    Err <- data.frame(Element = Element, Census = 2, Count = TreeList[[N]]$pre.ex[[which(I_pre.ex)]]$count)
    ErrList <- rbind(ErrList, Err)
  }

  list(TreeList_Pred = TreeList_Pred, Inv = rbind(Inv_new, Inv_pre.ex), Err = ErrList)
}


InvestmentInIndividualPart <- function(Element, TreeList_Pred, Progression, El_weight,
  Plant.Graph) {
  # Determine what time are we at
  N <- length(TreeList_Pred)
  # Go back in time
  for (t in (N - 1):2) {
    # If there is no possible progression for this part add that to error list
    if (length(Progression) == 1) {
      ErrList <- data.frame(Element = Element, Census = N - 1, Count = 1)
      return(list(TreeList_Pred = TreeList_Pred, Invest = NA, from = NA,
        to = NA, FromCensus = NA, ToCensus = NA, Count = NA, ErrList = ErrList))
    }
    # what are the parts avilable at that time
    parts.at.time.t <- unlist(sapply(TreeList_Pred[[t]]$total, function(x) x$type))
    # Go backwards on the progression line
    for (Predecessor in Progression[rev(seq_len(length(Progression) - 1))]) {
      # If you find a predecessor
      if (sum(Predecessor == parts.at.time.t)) {
        # Use graphd to determine path between predecessor and ancesstor.
        EdgePath <- unlist(get.shortest.paths(Plant.Graph, from = Element,
          to = Predecessor, output = "both", weights = NA)$epath)
        # proportion of carbon of the predecessors weight that has been used to produce
        # the ancesstor
        w <- prod(get.edge.attribute(Plant.Graph, name = "weight", index = EdgePath))
        # The weight of ancesstor is provided,Find the weight of predecessor
        I_predecessor <- sapply(TreeList_Pred[[t]]$total, function(x) x$type) ==
          Predecessor
        Pre_weight <- TreeList_Pred[[t]]$total[[which(I_predecessor)]]$weight[1]
        # Reduce the count of predecessors, remove the weight
        TreeList_Pred[[t]]$total[[which(I_predecessor)]]$weight <- TreeList_Pred[[t]]$total[[which(I_predecessor)]]$weight[-1]
        TreeList_Pred[[t]]$total[[which(I_predecessor)]]$count <- TreeList_Pred[[t]]$total[[which(I_predecessor)]]$count -
          1
        if (TreeList_Pred[[t]]$total[[which(I_predecessor)]]$count == 0) {
          TreeList_Pred[[t]]$total[[which(I_predecessor)]] <- NULL
        }
        ErrList <- data.frame(Element = NA, Census = NA, Count = NA)
        return(list(TreeList_Pred = TreeList_Pred, Invest = El_weight -
          w * Pre_weight, from = Predecessor, to = Element, FromCensus = t -
          1, ToCensus = N - 1, Count = 1, ErrList = ErrList))
        # take away the count from the list and remove the element
      }
    }
  }
  ErrList <- data.frame(Element = Element, Census = N - 1, Count = 1)
  list(TreeList_Pred = TreeList_Pred, Invest = NA, from = NA, to = NA, FromCensus = NA,
    ToCensus = NA, Count = NA, ErrList = ErrList)
}


# Function that takes the tree structure returned by Weight calculations and
# transforms it to per/seed counts. The parts are not right now to be
# understand as separate entities rather their fractions that develop to a
# single seed at the end. Such transformation does not change carbon allocation
# between the elements with the same count, however can allow to adjust for the
# fact that one fruit can have multiple seeds.

AdjustForMultiplicity <- function(TreeList, MultiplierTable) {
  species <- substr(TreeList[[1]], 1, 4)

  for (i in 2:length(TreeList)) {
    TreeList[[i]] <- AdjustForMultiplicity_Census(TreeList[[i]], species, MultiplierTable)
  }
  TreeList
}

AdjustForMultiplicity_Census <- function(Census, species, MultiplierTable) {
  Census$pre.ex <- AdjustForMultiplicity_List(Census$pre.ex, species, MultiplierTable)
  Census$new <- AdjustForMultiplicity_List(Census$new, species, MultiplierTable)
  Census$total <- AdjustForMultiplicity_List(Census$total, species, MultiplierTable)
  Census
}


AdjustForMultiplicity_List <- function(List, species, MultiplierTable) {
  if (length(List) > 0) {
    for (i in seq_len(length(List))) {
      part <- List[[i]]$type
      species.multiplier <- MultiplierTable[species]
      par.multiplier <- species.multiplier[MultiplierTable[, 1] == part,
        ]
      List[[i]]$count <- (List[[i]]$count) * par.multiplier
      List[[i]]$weight <- rep((List[[i]]$weight)/par.multiplier, par.multiplier)
    }
  }
  List
}
