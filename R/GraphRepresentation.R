# Defines the plant maps for each species
# Also defines the list of reproductive parts for each species.

make_OrderedListsOfParts_LEES <- function() {
  c("bud_tiny", "bud_mid", "bud_aborted", "bud_big", "flower_petals", "flower_calyx",
    "finished_flower", "fruit_just_starting", "fruit_young", "calyx_fruit",
    "fruit_large_immature", "fruit_aborted", "fruit_mature")
}

make_OrderedListsOfParts_GRBU <- function() {
  c("inflorescence_bud_small", "inflorescence_stalk", "bud_tiny", "bud_small",
    "bud_mid", "bud_big", "flower_petals", "flower_stigma", "finished_flower_stigma",
    "fruit_just_starting", "fruit_young", "pedicel", "fruit_large_immature",
    "seed_pod", "seed", "seed_aborted")
}

make_OrderedListsOfParts_GRSP <- function() {
  c("inflorescence_bud_small", "inflorescence_stalk", "bud_tiny", "inflorescence_stalk_in_fruit",
    "bud_small", "bud_mid", "bud_big", "bud_just_opening", "flower_petals",
    "flower_stigma", "finished_flower_stigma", "fruit_just_starting", "fruit_young",
    "pedicel", "fruit_large_immature", "seed_pod", "seed_aborted", "seed")
}

make_OrderedListsOfParts_EPMI <- function() {
  c("bud_tiny", "bud_mid", "bud_big", "flower_petals", "flower_calyx", "finished_flower",
    "fruit_young", "fruit_large_immature", "seed_pod", "seed")
}

make_OrderedListsOfParts_COER <- function() {
  c("inflorescence_bud_tiny", "inflorescence_bud_mid", "inflorescence_stalk",
    "bud_big", "inflorescence_stalk_in_fruit", "inflorescence_stalk_in_fruit_large",
    "inflorescence_stalk_in_fruit_very_large", "flower_petals", "flower_stigma",
    "finished_flower", "fruit_just_starting", "bract_fruit", "fruit_young",
    "fruit_large_immature", "fruit_mature", "flower_all_parts")
}

make_OrderedListsOfParts_PUTU <- function() {
  c("bud_big", "flower_petals", "flower_calyx", "bract_flower_or_finished_flower",
    "flower_stigma", "finished_flower_stigma", "flower_aborted", "fruit_large_immature",
    "fruit_aborted", "seed_pod", "seed", "seed_aborted")
}

make_OrderedListsOfParts_BAER <- function() {
  c("cone_base_green", "cone_base_brown", "cone_green", "cone_brown", "cone_brown_no_expanded_follicles",
    "cone_aborted", "bud_tiny", "bud_small", "bud_mid", "bud_big", "bud_just_opening",
    "flower_petals", "flower_style", "flower_stigma", "finished_flower_stigma",
    "fruit_just_starting", "fruit_young", "fruit_large_immature", "seed_pod",
    "seed", "seed_aborted")
}

make_OrderedListsOfParts_BOLE <- function() {
  c("bud_tiny", "bud_small", "bud_mid", "bud_big", "pedicel", "flower_petals",
    "late_flower_petals", "flower_calyx", "finished_flower", "finished_flower_stigma",
    "late_finished_flower", "fruit_just_starting", "fruit_young", "fruit_large_immature",
    "seed_pod", "seed")
}

make_OrderedListsOfParts_HEPU <- function() {
  c("bud_small", "bud_big", "flower_petals", "flower_calyx", "flower_calyx_aborting",
    "fruit_aborted", "finished_flower", "finished_flower_aborting", "calyx_aborted_fruit",
    "calyx_fruit", "fruit_young", "fruit_young_aborting", "fruit_large_immature",
    "fruit_large_immature_aborting", "fruit_mature")
}

make_OrderedListsOfParts_PILI <- function() {
  c("inflorescence_bud_mid", "inflorescence_stalk", "bract_flower_or_finished_flower",
    "bud_big", "flower_calyx", "flower_stigma", "flower_petals", "fruit_young",
    "fruit_large_immature", "fruit_aborted", "seed_pod", "seed")
}

make_OrderedListsOfParts_PHPH <- function() {
  c("bud_small", "bud_mid", "flower_petals_small", "flower_petals", "bract_flower_or_finished_flower",
    "flower_calyx", "flower_stigma", "flower_aborted", "flower_aborted_without_petals",
    "finished_flower_stigma", "fruit_just_starting", "fruit_young", "fruit_large_immature",
    "fruit_aborted", "seed_pod", "seed", "seed_aborted")
}

make_OrderedListsOfParts_PELA <- function() {
  c("bud_small", "bud_mid", "bud_big", "flower_petals", "pedicel", "flower_stigma",
    "finished_flower_stigma", "fruit_just_starting", "fruit_young", "fruit_large_immature",
    "seed", "seed_pod")
}

make_OrderedListsOfParts_HATE <- function() {
  c("inflorescence_bud_tiny", "inflorescence_bud_small", "inflorescence_bud_mid",
    "inflorescence_bud_big_bracts", "inflorescence_bud_big_flowers", "flower_petals",
    "flower_stigma", "finished_flower_stigma", "fruit_just_starting", "fruit_young",
    "fruit_large_immature", "fruit_aborted", "seed_pod", "seed_immature", "seed_aborted",
    "seed")
}

make_OrderedListsOfParts_PEPU <- function() {
  c("cone_just_starting", "cone_young", "cone_green", "cone_brown", "cone_aborted",
    "bud_tiny", "bud_big", "bud_aborted", "flower_petals", "flower_calyx",
    "flower_stigma", "fruit_just_starting", "fruit_young", "fruit_large_immature",
    "fruit_empty", "fruit_mature", "fruit_aborted")
}

AV_W <- function(species, part, AvWeightPerUnit) {
  AvWeightPerUnit[(AvWeightPerUnit$species == species) & (AvWeightPerUnit$part ==
    part), "weight"]
}

AdjustEdgeWeightsForMultiplicity <- function(graph, species, MultiplierTable) {

  EdgeList <- get.edgelist(graph)
  weights <- get.edge.attribute(graph, "weight")
  for (i in seq_len(nrow(EdgeList))) {
    from <- EdgeList[i, 1]
    to <- EdgeList[i, 2]
    MultiplierFrom <- MultiplierTable[MultiplierTable$flower_part == from,
      species]
    MultiplierTo <- MultiplierTable[MultiplierTable$flower_part == to, species]
    weights[i] <- MultiplierTo/MultiplierFrom * weights[i]
  }
  set.edge.attribute(graph, name = "weight", value = weights)
}

make_LEES_GraphMaps <- function(PartsSummary, MultiplierTable) {

  AvWeightPerUnit <- PartsSummary$AvWeightPerUnit

  LEES.graph <- graph.formula("bud_tiny" - "bud_mid" - "bud_big" - "flower_calyx" -
    "finished_flower" - "fruit_just_starting" - "fruit_young" - "fruit_large_immature_01" -
    "fruit_mature", "bud_tiny" - "bud_aborted", "bud_big" - "flower_petals",
    "fruit_young" - "calyx_fruit", "fruit_young" - "fruit_aborted")
  from <- "bud_tiny"
  to <- "fruit_mature"
  Paths <- data.frame(from = from, to = to)
  # Setting colours indicating main progression line together with its XORs. The
  # accessory tissues are to have different colour, mark only the final element
  # of the accessory line! Accessories to the same line are to be marked in their
  # own colour of value 1 larger than the path itself
  LEES.graph <- set.vertex.attribute(LEES.graph, name = "col", index = V(LEES.graph),
    value = 3)
  LEES.graph <- set.vertex.attribute(LEES.graph, name = "col", index = c(11,
    12), value = 4)
  # Setting edge weight which defines how much of carbon is beeing directed from
  # each part to each part. Especially important for species with accessories All
  # edges get weight 1
  LEES.graph <- set.edge.attribute(LEES.graph, name = "weight", value = 1)
  # Multiplicity adjust weights
  LEES.graph <- AdjustEdgeWeightsForMultiplicity(graph = LEES.graph, species = "LEES",
    MultiplierTable)
  # We modify the edges for those parts which are result of branching on the
  # plant map
  AM <- get.adjacency(LEES.graph, edges = T)
  # Flower_petals, flower_calyx which edges
  E1 <- AM["bud_big", "flower_calyx"]
  E2 <- AM["bud_big", "flower_petals"]
  # av. weights of the parts
  W1 <- AV_W("LEES", "flower_calyx", AvWeightPerUnit)
  W2 <- AV_W("LEES", "flower_petals", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, W2)
  w2 <- W2/sum(W1, W2)
  LEES.graph <- set.edge.attribute(LEES.graph, name = "weight", index = c(E1,
    E2), value = c(w1, w2))
  # Flower_petals, flower_calyx which edges
  E1 <- AM["fruit_young", "calyx_fruit"]
  E2 <- AM["fruit_young", "fruit_large_immature_01"]
  # av. weights of the parts
  W1 <- AV_W("LEES", "calyx_fruit", AvWeightPerUnit)
  W2 <- AV_W("LEES", "fruit_large_immature", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, W2)
  w2 <- W2/sum(W1, W2)
  LEES.graph <- set.edge.attribute(LEES.graph, name = "weight", index = c(E1,
    E2), value = c(w1, w2))
  list(graph = LEES.graph, Paths = Paths)
}

make_GRBU_GraphMaps <- function(PartsSummary, MultiplierTable) {

  AvWeightPerUnit <- PartsSummary$AvWeightPerUnit

  GRBU.graph <- graph.formula("bud_tiny" - "bud_small" - "bud_mid" - "bud_big" -
    "flower_stigma" - "finished_flower_stigma" - "fruit_just_starting" - "fruit_young" -
    "fruit_large_immature_01" - "fruit_large_immature_02" - "fruit_large_immature_03" -
    "fruit_large_immature_04" - "fruit_large_immature_05" - "fruit_large_immature_06" -
    "seed", "inflorescence_bud_small" - "inflorescence_stalk", "bud_big" -
    "flower_petals", "fruit_young" - "pedicel", "fruit_large_immature_06" -
    "seed_pod", "fruit_large_immature_06" - "seed_aborted")
  from <- c("bud_tiny", "inflorescence_bud_small")
  to <- c("seed", "inflorescence_stalk")
  Paths <- data.frame(from = from, to = to)
  GRBU.graph <- set.vertex.attribute(GRBU.graph, name = "col", index = c(1:15,
    21), value = 3)
  GRBU.graph <- set.vertex.attribute(GRBU.graph, name = "col", index = 18:20,
    value = 4)
  GRBU.graph <- set.vertex.attribute(GRBU.graph, name = "col", index = 16:17,
    value = 5)
  # Setting edge weight which defines how much of carbon is being directed from
  # each part to each part. Especially important for species with accessories
  GRBU.graph <- set.edge.attribute(GRBU.graph, name = "weight", value = 1)
  # Multiplicity adjust weights
  GRBU.graph <- AdjustEdgeWeightsForMultiplicity(graph = GRBU.graph, species = "GRBU",
    MultiplierTable)
  # We modify the edges for those parts which are result of branching on the
  # plant map
  AM <- get.adjacency(GRBU.graph, edges = T)
  # Flower_petals, flower_stigma which edges
  E1 <- AM["bud_big", "flower_stigma"]
  E2 <- AM["bud_big", "flower_petals"]
  # av. weights of the parts
  W1 <- AV_W("GRBU", "flower_stigma", AvWeightPerUnit)
  W2 <- AV_W("GRBU", "flower_petals", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, W2)
  w2 <- W2/sum(W1, W2)
  GRBU.graph <- set.edge.attribute(GRBU.graph, name = "weight", index = c(E1,
    E2), value = c(w1, w2))
  # pedicel, fruit_large_immature_01 which edges
  E1 <- AM["fruit_young", "pedicel"]
  E2 <- AM["fruit_young", "fruit_large_immature_01"]
  # av. weights of the parts
  W1 <- AV_W("GRBU", "pedicel", AvWeightPerUnit)
  W2 <- AV_W("GRBU", "fruit_large_immature", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, W2)
  w2 <- W2/sum(W1, W2)
  GRBU.graph <- set.edge.attribute(GRBU.graph, name = "weight", index = c(E1,
    E2), value = c(w1, w2))
  # seed_pod,seed,seed_aborted which edges
  E1 <- AM["fruit_large_immature_06", "seed_pod"]
  E2 <- AM["fruit_large_immature_06", "seed"]
  E3 <- AM["fruit_large_immature_06", "seed_aborted"]
  # av. weights of the parts
  W1 <- AV_W("GRBU", "seed_pod", AvWeightPerUnit)
  W2 <- AV_W("GRBU", "seed_aborted", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, 2 * W2)
  w2 <- W2/sum(W1, 2 * W2)
  GRBU.graph <- set.edge.attribute(GRBU.graph, name = "weight", index = c(E1,
    E2, E3), value = c(w1, w2, w2))
  list(graph = GRBU.graph, Paths = Paths)
}

make_GRSP_GraphMaps <- function(PartsSummary, MultiplierTable) {

  AvWeightPerUnit <- PartsSummary$AvWeightPerUnit

  GRSP.graph <- graph.formula("bud_tiny" - "bud_small" - "bud_mid" - "bud_big" -
    "bud_just_opening" - "flower_stigma" - "finished_flower_stigma" - "fruit_just_starting" -
    "fruit_young" - "fruit_large_immature_01" - "fruit_large_immature_02" -
    "fruit_large_immature_03" - "fruit_large_immature_04" - "fruit_large_immature_05" -
    "fruit_large_immature_06" - "seed", "inflorescence_bud_small" - "inflorescence_stalk" -
    "inflorescence_stalk_in_fruit", "bud_just_opening" - "flower_petals", "fruit_young" -
    "pedicel", "fruit_large_immature_06" - "seed_pod", "fruit_large_immature_06" -
    "seed_aborted")
  from <- c("bud_tiny", "inflorescence_bud_small")
  to <- c("seed", "inflorescence_stalk_in_fruit")
  Paths <- data.frame(from = from, to = to)
  # Set colors
  GRSP.graph <- set.vertex.attribute(GRSP.graph, name = "col", index = c(1:16,
    23), value = 3)
  GRSP.graph <- set.vertex.attribute(GRSP.graph, name = "col", index = 20:22,
    value = 4)
  GRSP.graph <- set.vertex.attribute(GRSP.graph, name = "col", index = 17:19,
    value = 5)
  # Set weights
  GRSP.graph <- set.edge.attribute(GRSP.graph, name = "weight", value = 1)
  # Multiplicity adjust weights
  GSP.graph <- AdjustEdgeWeightsForMultiplicity(graph = GRSP.graph, species = "GRSP",
    MultiplierTable)
  # We modify the edges for those parts which are result of branching on the
  # plant map
  AM <- get.adjacency(GRSP.graph, edges = T)
  # Flower_petals, flower_stigma which edges
  E1 <- AM["bud_just_opening", "flower_stigma"]
  E2 <- AM["bud_just_opening", "flower_petals"]
  # av. weights of the parts
  W1 <- AV_W("GRSP", "flower_stigma", AvWeightPerUnit)
  W2 <- AV_W("GRSP", "flower_petals", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, W2)
  w2 <- W2/sum(W1, W2)
  GRSP.graph <- set.edge.attribute(GRSP.graph, name = "weight", index = c(E1,
    E2), value = c(w1, w2))

  # pedicel, fruit_large_immature_01 which edges
  E1 <- AM["fruit_young", "pedicel"]
  E2 <- AM["fruit_young", "fruit_large_immature_01"]
  # av. weights of the parts
  W1 <- AV_W("GRSP", "pedicel", AvWeightPerUnit)
  W2 <- AV_W("GRSP", "fruit_large_immature", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, W2)
  w2 <- W2/sum(W1, W2)
  GRSP.graph <- set.edge.attribute(GRSP.graph, name = "weight", index = c(E1,
    E2), value = c(w1, w2))

  # seed_pod,seed,seed_aborted from fruit_large_immature_06 which edges
  E1 <- AM["fruit_large_immature_06", "seed_pod"]
  E2 <- AM["fruit_large_immature_06", "seed"]
  E3 <- AM["fruit_large_immature_06", "seed_aborted"]
  # av. weights of the parts
  W1 <- AV_W("GRSP", "seed_pod", AvWeightPerUnit)
  W2 <- AV_W("GRSP", "seed_aborted", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, 2 * W2)
  w2 <- W2/sum(W1, 2 * W2)
  GRSP.graph <- set.edge.attribute(GRSP.graph, name = "weight", index = c(E1,
    E2, E3), value = c(w1, w2, w2))
  list(graph = GRSP.graph, Paths = Paths)
}

make_EPMI_GraphMaps <- function(PartsSummary, MultiplierTable) {

  AvWeightPerUnit <- PartsSummary$AvWeightPerUnit

  EPMI.graph <- graph.formula("bud_tiny" - "bud_mid" - "bud_big" - "flower_calyx" -
    "finished_flower" - "fruit_young" - "fruit_large_immature_01" - "seed",
    "bud_big" - "flower_petals", "fruit_large_immature_01" - "seed_pod")
  from <- "bud_tiny"
  to <- "seed"
  Paths <- data.frame(from = from, to = to)
  # Set vertex color
  EPMI.graph <- set.vertex.attribute(EPMI.graph, name = "col", index = c(1:8),
    value = 3)
  EPMI.graph <- set.vertex.attribute(EPMI.graph, name = "col", index = c(9:10),
    value = 4)
  # Set edge color
  EPMI.graph <- set.edge.attribute(EPMI.graph, name = "weight", value = 1)
  AM <- get.adjacency(EPMI.graph, edges = T)
  # Flower_petals, flower_calyx which edges
  E1 <- AM["bud_big", "flower_calyx"]
  E2 <- AM["bud_big", "flower_petals"]

  # av. weights of the parts
  W1 <- AV_W("EPMI", "flower_calyx", AvWeightPerUnit)
  W2 <- AV_W("EPMI", "flower_petals", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, W2)
  w2 <- W2/sum(W1, W2)
  EPMI.graph <- set.edge.attribute(EPMI.graph, name = "weight", index = c(E1,
    E2), value = c(w1, w2))

  # seed_pod,seed from fruit_large_immature_01 which edges
  E1 <- AM["fruit_large_immature_01", "seed_pod"]
  E2 <- AM["fruit_large_immature_01", "seed"]
  # av. weights of the parts
  W1 <- AV_W("EPMI", "seed_pod", AvWeightPerUnit)

  #
  w1 <- W1/sum(W1, 2 * W2)

  EPMI.graph <- set.edge.attribute(EPMI.graph, name = "weight", index = c(E1,
    E2), value = c(w1, w2))

  list(graph = EPMI.graph, Paths = Paths)
}

make_COER_GraphMaps <- function(PartsSummary, MultiplierTable) {

  AvWeightPerUnit <- PartsSummary$AvWeightPerUnit

  COER.graph <- graph.formula("bud_big" - "flower_stigma" - "finished_flower" -
    "fruit_just_starting" - "fruit_young" - "fruit_large_immature_01" - "fruit_mature",
    "inflorescence_bud_tiny" - "inflorescence_bud_mid" - "inflorescence_stalk" -
      "inflorescence_stalk_in_fruit", "inflorescence_stalk" - "inflorescence_stalk_in_fruit_large",
    "inflorescence_stalk" - "inflorescence_stalk_in_fruit_very_large", "bud_big" -
      "flower_petals", "fruit_just_starting" - "bract_fruit")
  from <- c("bud_big", "inflorescence_bud_tiny")
  to <- c("fruit_mature", "inflorescence_stalk_in_fruit_very_large")
  Paths <- data.frame(from = from, to = to)
  # First progression with accessories
  COER.graph <- set.vertex.attribute(COER.graph, name = "col", index = c(1:7),
    value = 3)
  COER.graph <- set.vertex.attribute(COER.graph, name = "col", index = 14:15,
    value = 4)
  # Second progression without accessories
  COER.graph <- set.vertex.attribute(COER.graph, name = "col", index = 8:13,
    value = 5)
  # Set the weights for all edges
  COER.graph <- set.edge.attribute(COER.graph, name = "weight", value = 1)
  AM <- get.adjacency(COER.graph, edges = T)
  # Flower_petals, flower_stigma which edges
  E1 <- AM["bud_big", "flower_stigma"]
  E2 <- AM["bud_big", "flower_petals"]
  # av. weights of the parts
  W1 <- AV_W("COER", "flower_stigma", AvWeightPerUnit)
  W2 <- AV_W("COER", "flower_petals", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, W2)
  w2 <- W2/sum(W1, W2)
  COER.graph <- set.edge.attribute(COER.graph, name = "weight", index = c(E1,
    E2), value = c(w1, w2))
  # bract_fruit, fruit_young from fruit_just_starting which edges
  E1 <- AM["fruit_just_starting", "bract_fruit"]
  E2 <- AM["fruit_just_starting", "fruit_young"]
  # av. weights of the parts
  W1 <- AV_W("COER", "bract_fruit", AvWeightPerUnit)
  W2 <- AV_W("COER", "fruit_young", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, W2)
  w2 <- W2/sum(W1, W2)
  COER.graph <- set.edge.attribute(COER.graph, name = "weight", index = c(E1,
    E2), value = c(w1, w2))

  list(graph = COER.graph, Paths = Paths)
}

make_PUTU_GraphMaps <- function(PartsSummary, MultiplierTable) {

  AvWeightPerUnit <- PartsSummary$AvWeightPerUnit

  PUTU.graph <- graph.formula("bud_big" - "flower_stigma" - "finished_flower_stigma" -
    "fruit_large_immature_01" - "seed", "bud_big" - "flower_petals", "bud_big" -
    "flower_calyx", "bud_big" - "bract_flower_or_finished_flower", "flower_aborted",
    "flower_stigma" - "fruit_aborted", "fruit_large_immature_01" - "seed_pod",
    "fruit_large_immature_01" - "seed_aborted")
  from <- c("bud_big", "flower_aborted")
  to <- c("seed_pod", "flower_aborted")
  Paths <- data.frame(from = from, to = to)
  # Set colors to vertices
  PUTU.graph <- set.vertex.attribute(PUTU.graph, name = "col", index = c(1:4,
    11, 10), value = 3)
  PUTU.graph <- set.vertex.attribute(PUTU.graph, name = "col", index = c(6:8,
    5, 12), value = 4)
  PUTU.graph <- set.vertex.attribute(PUTU.graph, name = "col", index = c(9),
    value = 5)
  # Set weights to all edges
  PUTU.graph <- set.edge.attribute(PUTU.graph, name = "weight", value = 1)
  # PUTU.graph=set.edge.attribute(PUTU.graph,name='weight',index=c(1,2,3,4,8,9,10),value=c(0.1596,0.5122,0.1557,0.1725,0.19,0.81,0.19))
  AM <- get.adjacency(PUTU.graph, edges = T)
  # flower_petals, flower_calyx, flower_stigma,bract_flower_or_finished_flower
  # from bud big which edges
  E1 <- AM["bud_big", "flower_petals"]
  E2 <- AM["bud_big", "flower_calyx"]
  E3 <- AM["bud_big", "flower_stigma"]
  E4 <- AM["bud_big", "bract_flower_or_finished_flower"]

  # av. weights of the parts
  W1 <- AV_W("PUTU", "flower_petals", AvWeightPerUnit)
  W2 <- AV_W("PUTU", "flower_calyx", AvWeightPerUnit)
  W3 <- AV_W("PUTU", "flower_stigma", AvWeightPerUnit)
  W4 <- AV_W("PUTU", "bract_flower_or_finished_flower", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, W2, W3, W4)
  w2 <- W2/sum(W1, W2, W3, W4)
  w3 <- W3/sum(W1, W2, W3, W4)
  w4 <- W4/sum(W1, W2, W3, W4)
  PUTU.graph <- set.edge.attribute(PUTU.graph, name = "weight", index = c(E1,
    E2, E3, E4), value = c(w1, w2, w3, w4))

  # seed_pod,seed,seed_aborted from fruit_large_immature_01 which edges
  E1 <- AM["fruit_large_immature_01", "seed_pod"]
  E2 <- AM["fruit_large_immature_01", "seed"]
  E3 <- AM["fruit_large_immature_01", "seed_aborted"]
  # av. weights of the parts
  W1 <- AV_W("PUTU", "seed_pod", AvWeightPerUnit)
  W2 <- AV_W("PUTU", "seed_aborted", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, 2 * W2)
  w2 <- W2/sum(W1, 2 * W2)
  PUTU.graph <- set.edge.attribute(PUTU.graph, name = "weight", index = c(E1,
    E2, E3), value = c(w1, w2, w2))
  list(graph = PUTU.graph, Paths = Paths)
}

make_BAER_GraphMaps <- function(PartsSummary, MultiplierTable) {

  AvWeightPerUnit <- PartsSummary$AvWeightPerUnit

  BAER.graph <- graph.formula("cone_base_green_01" - "cone_base_green_02" - "cone_base_green_03" -
    "cone_base_green_04" - "cone_base_brown", "cone_young_01" - "cone_young_02" -
    "cone_young_03" - "cone_young_04" - "cone_green_01" - "cone_green_02" -
    "cone_green_03" - "cone_green_04" - "cone_brown", "cone_green_04" - "cone_brown_no_expanded_follicles",
    "cone_young_04" - "cone_aborted", "bud_tiny" - "bud_small" - "bud_mid" -
      "bud_big" - "bud_just_opening" - "flower_stigma" - "finished_flower_stigma" -
      "fruit_just_starting" - "fruit_young" - "fruit_large_immature_01" -
      "seed", "bud_just_opening" - "flower_petals", "bud_just_opening" -
      "flower_style", "fruit_large_immature_01" - "seed_pod", "fruit_large_immature_01" -
      "seed_aborted")
  from <- c("bud_tiny", "cone_young_01", "cone_base_green_01")
  to <- c("seed", "cone_brown", "cone_base_brown")
  Paths <- data.frame(from = from, to = to)
  # Set colors
  BAER.graph <- set.vertex.attribute(BAER.graph, name = "col", index = c(1:5),
    value = 1)
  BAER.graph <- set.vertex.attribute(BAER.graph, name = "col", index = c(6:16),
    value = 5)
  BAER.graph <- set.vertex.attribute(BAER.graph, name = "col", index = c(17:27,
    31), value = 3)
  BAER.graph <- set.vertex.attribute(BAER.graph, name = "col", index = c(28:30),
    value = 4)
  # Set weights
  BAER.graph <- set.edge.attribute(BAER.graph, name = "weight", value = 1)
  # BAER.graph=set.edge.attribute(BAER.graph,name='weight',index=c(19,20,21,26,27,28),value=c(0.28,0.44,0.28,0.07,0.93,0.07))

  AM <- get.adjacency(BAER.graph, edges = T)
  # flower_petals, flower_style, flower_stigma, from bud_just_opening which edges
  E1 <- AM["bud_just_opening", "flower_petals"]
  E2 <- AM["bud_just_opening", "flower_style"]
  E3 <- AM["bud_just_opening", "flower_stigma"]

  # av. weights of the parts
  W1 <- AV_W("BAER", "flower_petals", AvWeightPerUnit)
  W2 <- AV_W("BAER", "flower_style", AvWeightPerUnit)
  W3 <- AV_W("BAER", "flower_stigma", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, W2, W3)
  w2 <- W2/sum(W1, W2, W3)
  w3 <- W3/sum(W1, W2, W3)
  BAER.graph <- set.edge.attribute(BAER.graph, name = "weight", index = c(E1,
    E2, E3), value = c(w1, w2, w3))

  # seed_pod,seed,seed_aborted from fruit_large_immature_01 which edges
  E1 <- AM["fruit_large_immature_01", "seed_pod"]
  E2 <- AM["fruit_large_immature_01", "seed"]
  E3 <- AM["fruit_large_immature_01", "seed_aborted"]
  # av. weights of the parts
  W1 <- AV_W("BAER", "seed_pod", AvWeightPerUnit)
  W2 <- AV_W("BAER", "seed_aborted", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, 2 * W2)
  w2 <- W2/sum(W1, 2 * W2)
  BAER.graph <- set.edge.attribute(BAER.graph, name = "weight", index = c(E1,
    E2, E3), value = c(w1, w2, w2))
  list(graph = BAER.graph, Paths = Paths)
}

make_BOLE_GraphMaps <- function(PartsSummary, MultiplierTable) {

  AvWeightPerUnit <- PartsSummary$AvWeightPerUnit

  BOLE.graph <- graph.formula("bud_tiny" - "bud_small" - "bud_mid" - "bud_big" -
    "flower_calyx" - "finished_flower_stigma" - "fruit_just_starting" - "fruit_young" -
    "fruit_large_immature_01" - "seed", "bud_big" - "pedicel", "bud_big" -
    "flower_petals", "flower_calyx" - "finished_flower" - "late_finished_flower",
    "fruit_large_immature_01" - "seed_pod", "flower_petals" - "late_flower_petals")
  from <- c("bud_tiny")
  to <- c("seed")
  Paths <- data.frame(from = from, to = to)
  # Set vertex colors to define paths and their accessories
  BOLE.graph <- set.vertex.attribute(BOLE.graph, name = "col", index = c(1:10),
    value = 3)
  BOLE.graph <- set.vertex.attribute(BOLE.graph, name = "col", index = c(11,
    12, 13, 14, 15, 16), value = 4)
  # Set edge weight to define carbon flow
  BOLE.graph <- set.edge.attribute(BOLE.graph, name = "weight", value = 1)
  # BOLE.graph=set.edge.attribute(BOLE.graph,name='weight',index=c(4,5,6,7,8,12,13),value=c(0.486,0.116,0.4,0.03,0.97,0.48,0.52))

  AM <- get.adjacency(BOLE.graph, edges = T)
  # flower_petals, pedicel, flower_calyx, from bud_big which edges
  E1 <- AM["bud_big", "flower_petals"]
  E2 <- AM["bud_big", "pedicel"]
  E3 <- AM["bud_big", "flower_calyx"]

  # av. weights of the parts
  W1 <- AV_W("BOLE", "flower_petals", AvWeightPerUnit)
  W2 <- AV_W("BOLE", "pedicel", AvWeightPerUnit)
  W3 <- AV_W("BOLE", "flower_calyx", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, W2, W3)
  w2 <- W2/sum(W1, W2, W3)
  w3 <- W3/sum(W1, W2, W3)
  BOLE.graph <- set.edge.attribute(BOLE.graph, name = "weight", index = c(E1,
    E2, E3), value = c(w1, w2, w3))

  # finished_flower, finished_flower_stigma from flower_calyx which edges
  E1 <- AM["flower_calyx", "finished_flower"]
  E2 <- AM["flower_calyx", "finished_flower_stigma"]

  # av. weights of the parts
  W1 <- AV_W("BOLE", "finished_flower", AvWeightPerUnit)
  W2 <- AV_W("BOLE", "finished_flower_stigma", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, 4 * W2)
  w2 <- W2/sum(W1, 4 * W2)
  BOLE.graph <- set.edge.attribute(BOLE.graph, name = "weight", index = c(E1,
    E2), value = c(w1, w2))

  # seed_pod, seed from fruit_large_immature which edges
  E1 <- AM["fruit_large_immature_01", "seed_pod"]
  E2 <- AM["fruit_large_immature_01", "seed"]

  # av. weights of the parts
  W1 <- AV_W("BOLE", "seed_pod", AvWeightPerUnit)
  W2 <- AV_W("BOLE", "seed", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, W2)
  w2 <- W2/sum(W1, W2)
  BOLE.graph <- set.edge.attribute(BOLE.graph, name = "weight", index = c(E1,
    E2), value = c(w1, w2))
  list(graph = BOLE.graph, Paths = Paths)
}

make_HEPU_GraphMaps <- function(PartsSummary, MultiplierTable) {

  AvWeightPerUnit <- PartsSummary$AvWeightPerUnit

  HEPU.graph <- graph.formula("bud_small" - "bud_big" - "flower_calyx" - "finished_flower" -
    "fruit_young" - "fruit_large_immature_01" - "fruit_mature", "bud_big" -
    "flower_petals", "flower_calyx" - "fruit_aborted", "finished_flower" -
    "calyx_fruit", "bud_big" - "flower_calyx_aborting", "flower_calyx" - "finished_flower_aborting",
    "finished_flower" - "fruit_young_aborting", "fruit_young" - "fruit_large_immature_aborting")
  from <- c("bud_small")
  to <- c("fruit_mature")
  Paths <- data.frame(from = from, to = to)
  # Set vertex colors
  HEPU.graph <- set.vertex.attribute(HEPU.graph, name = "col", index = c(1:7,
    9, 11:14), value = 3)
  HEPU.graph <- set.vertex.attribute(HEPU.graph, name = "col", index = c(8, 10),
    value = 4)
  # Set egde weights
  HEPU.graph <- set.edge.attribute(HEPU.graph, name = "weight", value = 1)

  AM <- get.adjacency(HEPU.graph, edges = T)
  ############################ flower_petals, flower_calyx from bud_big which edges
  E1 <- AM["bud_big", "flower_petals"]
  E2 <- AM["bud_big", "flower_calyx"]

  # av. weights of the parts
  W1 <- AV_W("HEPU", "flower_petals", AvWeightPerUnit)
  W2 <- AV_W("HEPU", "flower_calyx", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, W2)
  w2 <- W2/sum(W1, W2)
  HEPU.graph <- set.edge.attribute(HEPU.graph, name = "weight", index = c(E1,
    E2), value = c(w1, w2))


  ############################ flower_petals, flower_calyx_aborting from bud_big which edges
  E1 <- AM["bud_big", "flower_petals"]
  E2 <- AM["bud_big", "flower_calyx_aborting"]

  # av. weights of the parts
  W1 <- AV_W("HEPU", "flower_petals", AvWeightPerUnit)
  W2 <- AV_W("HEPU", "flower_calyx_aborting", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, W2)
  w2 <- W2/sum(W1, W2)
  HEPU.graph <- set.edge.attribute(HEPU.graph, name = "weight", index = c(E1,
    E2), value = c(w1, w2))

  ############################ calyx_fruit, fruit_young from finished_flower which edges
  E1 <- AM["finished_flower", "calyx_fruit"]
  E2 <- AM["finished_flower", "fruit_young"]

  # av. weights of the parts
  W1 <- AV_W("HEPU", "calyx_fruit", AvWeightPerUnit)
  W2 <- AV_W("HEPU", "fruit_young", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, W2)
  w2 <- W2/sum(W1, W2)
  HEPU.graph <- set.edge.attribute(HEPU.graph, name = "weight", index = c(E1,
    E2), value = c(w1, w2))


  ############################ calyx_fruit, fruit_young_aborting from finished_flower which edges
  E1 <- AM["finished_flower", "calyx_fruit"]
  E2 <- AM["finished_flower", "fruit_young_aborting"]

  # av. weights of the parts
  W1 <- AV_W("HEPU", "calyx_fruit", AvWeightPerUnit)
  W2 <- AV_W("HEPU", "fruit_young_aborting", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, W2)
  w2 <- W2/sum(W1, W2)
  HEPU.graph <- set.edge.attribute(HEPU.graph, name = "weight", index = c(E1,
    E2), value = c(w1, w2))

  list(graph = HEPU.graph, Paths = Paths)
}

make_PILI_GraphMaps <- function(PartsSummary, MultiplierTable) {

  AvWeightPerUnit <- PartsSummary$AvWeightPerUnit

  PILI.graph <- graph.formula("bud_big" - "flower_stigma" - "fruit_young" - "fruit_large_immature_01" -
    "seed", "inflorescence_bud_mid" - "inflorescence_stalk", "inflorescence_bud_mid" -
    "bract_flower_or_finished_flower", "bud_big" - "flower_calyx", "bud_big" -
    "flower_petals", "fruit_young" - "fruit_aborted", "fruit_large_immature_01" -
    "seed_pod")
  from <- c("bud_big", "inflorescence_bud_mid")
  to <- c("seed", "inflorescence_stalk")
  Paths <- data.frame(from = from, to = to)
  # Set vertex colors
  PILI.graph <- set.vertex.attribute(PILI.graph, name = "col", index = c(1:5,
    11), value = 2)
  PILI.graph <- set.vertex.attribute(PILI.graph, name = "col", index = c(9, 10,
    12), value = 3)
  PILI.graph <- set.vertex.attribute(PILI.graph, name = "col", index = c(6:7),
    value = 4)
  PILI.graph <- set.vertex.attribute(PILI.graph, name = "col", index = 8, value = 5)
  # Set egde weights
  PILI.graph <- set.edge.attribute(PILI.graph, name = "weight", value = 1)
  # PILI.graph=set.edge.attribute(PILI.graph,name='weight',index=c(1,2,3,7,8,9,10),value=c(0.02,0.2,0.78,0.61,0.39,0.22,0.78))
  AM <- get.adjacency(PILI.graph, edges = T)
  # flower_petals, flower_calyx, flower_stigma, from bud_big which edges
  E1 <- AM["bud_big", "flower_petals"]
  E2 <- AM["bud_big", "flower_calyx"]
  E3 <- AM["bud_big", "flower_stigma"]

  # av. weights of the parts
  W1 <- AV_W("PILI", "flower_petals", AvWeightPerUnit)
  W2 <- AV_W("PILI", "flower_calyx", AvWeightPerUnit)
  W3 <- AV_W("PILI", "flower_stigma", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, W2, W3)
  w2 <- W2/sum(W1, W2, W3)
  w3 <- W3/sum(W1, W2, W3)
  PILI.graph <- set.edge.attribute(PILI.graph, name = "weight", index = c(E1,
    E2, E3), value = c(w1, w2, w3))

  # seed_pod, seed from fruit_large_immature which edges
  E1 <- AM["fruit_large_immature_01", "seed_pod"]
  E2 <- AM["fruit_large_immature_01", "seed"]

  # av. weights of the parts
  W1 <- AV_W("PILI", "seed_pod", AvWeightPerUnit)
  W2 <- AV_W("PILI", "seed", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, W2)
  w2 <- W2/sum(W1, W2)
  PILI.graph <- set.edge.attribute(PILI.graph, name = "weight", index = c(E1,
    E2), value = c(w1, w2))

  # inflorescence_stalk, bract_flower_or_finished_flower from
  # inflorescence_bud_mid which edges
  E1 <- AM["inflorescence_bud_mid", "inflorescence_stalk"]
  E2 <- AM["inflorescence_bud_mid", "bract_flower_or_finished_flower"]

  # av. weights of the parts
  W1 <- AV_W("PILI", "inflorescence_stalk", AvWeightPerUnit)
  W2 <- AV_W("PILI", "bract_flower_or_finished_flower", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, W2)
  w2 <- W2/sum(W1, W2)
  PILI.graph <- set.edge.attribute(PILI.graph, name = "weight", index = c(E1,
    E2), value = c(w1, w2))
  list(graph = PILI.graph, Paths = Paths)
}


make_PHPH_GraphMaps <- function(PartsSummary, MultiplierTable) {

  AvWeightPerUnit <- PartsSummary$AvWeightPerUnit

  PHPH.graph <- graph.formula("bud_small" - "bud_mid" - "flower_stigma" - "finished_flower_stigma" -
    "fruit_just_starting" - "fruit_young" - "fruit_large_immature_01" - "seed",
    "bud_mid" - "flower_petals_small" - "flower_petals", "bud_mid" - "bract_flower_or_finished_flower",
    "bud_mid" - "flower_calyx", "bud_small" - "flower_aborted", "flower_aborted_without_petals",
    "fruit_young" - "fruit_aborted", "fruit_large_immature_01" - "seed_pod",
    "fruit_large_immature_01" - "seed_aborted")
  from <- c("bud_small", "flower_aborted_without_petals")
  to <- c("seed_pod", "flower_aborted_without_petals")
  Paths <- data.frame(from = from, to = to)
  # Set vertex color
  PHPH.graph <- set.vertex.attribute(PHPH.graph, name = "col", index = c(1:7,
    16, 13, 15), value = 3)
  PHPH.graph <- set.vertex.attribute(PHPH.graph, name = "col", index = c(8, 9,
    10, 11, 12, 17), value = 4)
  PHPH.graph <- set.vertex.attribute(PHPH.graph, name = "col", index = c(14),
    value = 5)
  # Set egde weights
  PHPH.graph <- set.edge.attribute(PHPH.graph, name = "weight", value = 1)
  # PHPH.graph=set.edge.attribute(PHPH.graph,name='weight',index=c(3,4,5,6,12,13,14),value=c(0.1269,0.3441,0.3413,0.1876,0.24,0.76,0.24))
  AM <- get.adjacency(PHPH.graph, edges = T)
  # flower_petals_small, bract_flower_or_finished_flower,
  # flower_stigma,flower_calyx from bud_mid which edges
  E1 <- AM["bud_mid", "flower_petals_small"]
  E2 <- AM["bud_mid", "bract_flower_or_finished_flower"]
  E3 <- AM["bud_mid", "flower_stigma"]
  E4 <- AM["bud_mid", "flower_calyx"]

  # av. weights of the parts
  W1 <- AV_W("PHPH", "flower_petals_small", AvWeightPerUnit)
  W2 <- AV_W("PHPH", "bract_flower_or_finished_flower", AvWeightPerUnit)
  W3 <- AV_W("PHPH", "flower_stigma", AvWeightPerUnit)
  W4 <- AV_W("PHPH", "flower_calyx", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, W2, W3, W4)
  w2 <- W2/sum(W1, W2, W3, W4)
  w3 <- W3/sum(W1, W2, W3, W4)
  w4 <- W4/sum(W1, W2, W3, W4)
  PHPH.graph <- set.edge.attribute(PHPH.graph, name = "weight", index = c(E1,
    E2, E3, E4), value = c(w1, w2, w3, w4))


  # seed_pod,seed,seed_aborted from fruit_large_immature_01 which edges
  E1 <- AM["fruit_large_immature_01", "seed_pod"]
  E2 <- AM["fruit_large_immature_01", "seed"]
  E3 <- AM["fruit_large_immature_01", "seed_aborted"]
  # av. weights of the parts
  W1 <- AV_W("PHPH", "seed_pod", AvWeightPerUnit)
  W2 <- AV_W("PHPH", "seed_aborted", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, 2 * W2)
  w2 <- W2/sum(W1, 2 * W2)
  PHPH.graph <- set.edge.attribute(PHPH.graph, name = "weight", index = c(E1,
    E2, E3), value = c(w1, w2, w2))

  list(graph = PHPH.graph, Paths = Paths)
}


make_PELA_GraphMaps <- function(PartsSummary, MultiplierTable) {

  AvWeightPerUnit <- PartsSummary$AvWeightPerUnit

  PELA.graph <- graph.formula("bud_small" - "bud_mid" - "bud_big" - "flower_stigma" -
    "finished_flower_stigma" - "fruit_just_starting" - "fruit_young" - "fruit_large_immature_01" -
    "fruit_large_immature_02" - "fruit_large_immature_03" - "fruit_large_immature_04" -
    "fruit_large_immature_05" - "fruit_large_immature_06" - "seed", "bud_big" -
    "flower_petals", "bud_big" - "pedicel", "fruit_large_immature_06" - "seed_pod")
  from <- c("bud_small")
  to <- c("seed")
  Paths <- data.frame(from = from, to = to)
  # Set vertex colors
  PELA.graph <- set.vertex.attribute(PELA.graph, name = "col", index = c(1:14),
    value = 3)
  PELA.graph <- set.vertex.attribute(PELA.graph, name = "col", index = 15:17,
    value = 4)
  # Set edge color
  PELA.graph <- set.edge.attribute(PELA.graph, name = "weight", value = 1)
  # PELA.graph=set.edge.attribute(PELA.graph,name='weight',index=c(3,4,5,15,16),value=c(0.08,0.78,0.14,0.42,0.58))
  AM <- get.adjacency(PELA.graph, edges = T)
  # flower_petals, pedicel, flower_stigma, from bud_big which edges
  E1 <- AM["bud_big", "flower_petals"]
  E2 <- AM["bud_big", "pedicel"]
  E3 <- AM["bud_big", "flower_stigma"]

  # av. weights of the parts
  W1 <- AV_W("PELA", "flower_petals", AvWeightPerUnit)
  W2 <- AV_W("PELA", "pedicel", AvWeightPerUnit)
  W3 <- AV_W("PELA", "flower_stigma", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, W2, W3)
  w2 <- W2/sum(W1, W2, W3)
  w3 <- W3/sum(W1, W2, W3)
  PELA.graph <- set.edge.attribute(PELA.graph, name = "weight", index = c(E1,
    E2, E3), value = c(w1, w2, w3))

  # seed_pod, seed from fruit_large_immature which edges
  E1 <- AM["fruit_large_immature_06", "seed_pod"]
  E2 <- AM["fruit_large_immature_06", "seed"]

  # av. weights of the parts
  W1 <- AV_W("PELA", "seed_pod", AvWeightPerUnit)
  W2 <- AV_W("PELA", "seed", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, W2)
  w2 <- W2/sum(W1, W2)
  PELA.graph <- set.edge.attribute(PELA.graph, name = "weight", index = c(E1,
    E2), value = c(w1, w2))

  list(graph = PELA.graph, Paths = Paths)
}


make_HATE_GraphMaps <- function(PartsSummary, MultiplierTable) {

  AvWeightPerUnit <- PartsSummary$AvWeightPerUnit

  HATE.graph <- graph.formula("inflorescence_bud_tiny" - "inflorescence_bud_small" -
    "inflorescence_bud_mid" - "inflorescence_bud_big_flowers" - "flower_stigma" -
    "finished_flower_stigma" - "fruit_just_starting" - "fruit_young" - "fruit_large_immature_01" -
    "seed_immature" - "seed", "inflorescence_bud_mid" - "inflorescence_bud_big_bracts",
    "inflorescence_bud_big_flowers" - "flower_petals", "fruit_young" - "fruit_aborted",
    "fruit_large_immature_01" - "seed_pod", "fruit_large_immature_01" - "seed_aborted")
  from <- c("inflorescence_bud_tiny")
  to <- c("seed")
  Paths <- data.frame(from = from, to = to)
  # Set vertex colors
  HATE.graph <- set.vertex.attribute(HATE.graph, name = "col", index = c(1:11,
    14, 16), value = 3)
  HATE.graph <- set.vertex.attribute(HATE.graph, name = "col", index = c(12,
    13, 15), value = 4)
  # Set edge color
  HATE.graph <- set.edge.attribute(HATE.graph, name = "weight", value = 1)
  # HATE.graph=set.edge.attribute(HATE.graph,name='weight',index=c(3,4,5,6,12,13,14),value=c(0.44,0.56,0.45,0.55,0.01,0.99,0.01))
  AM <- get.adjacency(HATE.graph, edges = T)
  # inflorescence_bud_big_bracts, inflorescence_bud_big_flowers from
  # inflorescence_bud_mid which edges
  E1 <- AM["inflorescence_bud_mid", "inflorescence_bud_big_bracts"]
  E2 <- AM["inflorescence_bud_mid", "inflorescence_bud_big_flowers"]

  # av. weights of the parts
  W1 <- AV_W("HATE", "inflorescence_bud_big_bracts", AvWeightPerUnit)
  W2 <- AV_W("HATE", "inflorescence_bud_big_flowers", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, W2)
  w2 <- W2/sum(W1, W2)
  HATE.graph <- set.edge.attribute(HATE.graph, name = "weight", index = c(E1,
    E2), value = c(w1, w2))
  # flower_petals,flower_stigma from inflorescence_bud_big_flowers which edges
  E1 <- AM["inflorescence_bud_big_flowers", "flower_petals"]
  E2 <- AM["inflorescence_bud_big_flowers", "flower_stigma"]

  # av. weights of the parts
  W1 <- AV_W("HATE", "flower_petals", AvWeightPerUnit)
  W2 <- AV_W("HATE", "flower_stigma", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, W2)
  w2 <- W2/sum(W1, W2)
  HATE.graph <- set.edge.attribute(HATE.graph, name = "weight", index = c(E1,
    E2), value = c(w1, w2))

  # seed_pod,seed_immature,seed_aborted from fruit_large_immature_01 which edges
  E1 <- AM["fruit_large_immature_01", "seed_pod"]
  E2 <- AM["fruit_large_immature_01", "seed_immature"]
  E3 <- AM["fruit_large_immature_01", "seed_aborted"]
  # av. weights of the parts
  W1 <- AV_W("HATE", "seed_pod", AvWeightPerUnit)
  W2 <- AV_W("HATE", "seed_aborted", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, 2 * W2)
  w2 <- W2/sum(W1, 2 * W2)
  HATE.graph <- set.edge.attribute(HATE.graph, name = "weight", index = c(E1,
    E2, E3), value = c(w1, w2, w2))

  list(graph = HATE.graph, Paths = Paths)
}

make_PEPU_GraphMaps <- function(PartsSummary, MultiplierTable) {

  AvWeightPerUnit <- PartsSummary$AvWeightPerUnit

  PEPU.graph <- graph.formula("bud_tiny" - "bud_big" - "flower_stigma" - "fruit_just_starting" -
    "fruit_young" - "fruit_large_immature_01" - "fruit_mature", "cone_just_starting_01" -
    "cone_just_starting_02" - "cone_just_starting_03" - "cone_just_starting_04" -
    "cone_just_starting_05" - "cone_young_01" - "cone_young_02" - "cone_young_03" -
    "cone_young_04" - "cone_green_01" - "cone_green_02" - "cone_green_03" -
    "cone_green_04" - "cone_brown", "cone_green_04" - "cone_aborted", "bud_tiny" -
    "bud_aborted", "bud_big" - "flower_petals", "bud_big" - "flower_calyx",
    "fruit_large_immature_01" - "fruit_empty", "fruit_just_starting" - "fruit_aborted")
  from <- c("bud_tiny", "cone_just_starting_01")
  to <- c("fruit_mature", "cone_brown")
  Paths <- data.frame(from = from, to = to)
  # Set vertex colors
  PEPU.graph <- set.vertex.attribute(PEPU.graph, name = "col", index = c(1:7,
    23, 26, 27), value = 3)
  PEPU.graph <- set.vertex.attribute(PEPU.graph, name = "col", index = c(24,
    25), value = 4)
  PEPU.graph <- set.vertex.attribute(PEPU.graph, name = "col", index = c(8:22),
    value = 5)
  # Set edge color
  PEPU.graph <- set.edge.attribute(PEPU.graph, name = "weight", value = 1)
  PEPU.graph <- set.edge.attribute(PEPU.graph, name = "weight", index = c(3,
    4, 5), value = c(0.22, 0.66, 0.12))
  AM <- get.adjacency(PEPU.graph, edges = T)
  # flower_petals,flower_calyx, flower_stigma, from bud_big which edges
  E1 <- AM["bud_big", "flower_petals"]
  E2 <- AM["bud_big", "flower_calyx"]
  E3 <- AM["bud_big", "flower_stigma"]

  # av. weights of the parts
  W1 <- AV_W("PEPU", "flower_petals", AvWeightPerUnit)
  W2 <- AV_W("PEPU", "flower_calyx", AvWeightPerUnit)
  W3 <- AV_W("PEPU", "flower_stigma", AvWeightPerUnit)
  #
  w1 <- W1/sum(W1, W2, W3)
  w2 <- W2/sum(W1, W2, W3)
  w3 <- W3/sum(W1, W2, W3)
  PEPU.graph <- set.edge.attribute(PEPU.graph, name = "weight", index = c(E1,
    E2, E3), value = c(w1, w2, w3))

  list(graph = PEPU.graph, Paths = Paths)
}
