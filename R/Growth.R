preprocessHarvest <- function(HarvestData_raw, IndividualsList) {

  keep <- filter(IndividualsList, use_for_allocation_calculations)$individual

  data <- filter(HarvestData_raw, individual %in% keep) %>%
          group_by(species, individual, site, age, start_end)

  segments <- list()
  segments[["1"]] <- c(as.character(1:8), "1.2", "1.3", "1.2.1", "1.2.1.1", "1.1.2", "1.1.2.1", "1.1.1.2")
  segments[["2"]] <- c(as.character(2:8), "1.1.2", "1.1.2.1", "1.1.1.2")
  segments[["3"]] <- c(as.character(3:8), "1.1.1.2")
  segments[["4"]] <- as.character(4:8)
  segments[["5"]] <- as.character(5:8)
  segments[["6"]] <- as.character(6:8)
  segments[["7"]] <- as.character(7:8)
  segments[["8"]] <- as.character(8)
  segments[["1.2"]] <- c("1.2", "1.2.1", "1.2.1.1")
  segments[["1.3"]] <- c("1.3")
  segments[["1.2.1"]] <- c("1.2.1", "1.2.1.1")
  segments[["1.2.1.1"]] <- c("1.2.1.1")
  segments[["1.1.2"]] <- c("1.1.2", "1.1.2.1")
  segments[["1.1.2.1"]] <- c("1.1.2.1")
  segments[["1.1.1.2"]] <- c("1.1.1.2")

  ## Get total mass of all material subtended by given node
  get.mass.at.node <- function(segment_name, data) {
    data %>%
      filter(segment %in% segments[[segment_name]]) %>%
      summarise(
          segment = segment_name,
          leaf_weight = sum(leaf_weight),
          stem_weight = sum(stem_weight),
          total_weight = sum(leaf_weight, stem_weight))
  }

  get.mass.at.node.by.segment <- function(data) {
    lapply(names(segments), get.mass.at.node, data=data)  %>% rbind_all()
  }

  # Get average diameter for base of segment. This is given by diameter readings for segment with names in list above
  get.diam.node.above <- function(level, data) {
    data %>%
      filter(segment == level) %>%
      group_by(individual) %>%
      summarise(
        segment = level,
        diameter = mean(c(diameter_1, diameter_2, diameter_3), na.rm = TRUE),
        stem_area = diameter^2 * pi/4,
        height = ifelse(segment==1, height, NaN))
  }

  get.diam.node.above.by.segment <- function(data) {
    lapply(names(segments),  get.diam.node.above, data=data)  %>% rbind_all()
  }

  mass <-  do(data, get.mass.at.node.by.segment(.))
  diameters <- do(data, get.diam.node.above.by.segment(.))

  # Merge mass and diameter measurements
  merge(diameters, mass, by = c("species", "site", "individual", "age", "segment", "start_end")) %>%
   arrange(species, site, individual, age, segment, desc(start_end))
}
