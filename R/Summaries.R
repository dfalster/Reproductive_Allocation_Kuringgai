

make_summary_species <- function() {

	
}

process_seed_costs <- function(seedSize_raw, PartsSummary_all) {

  seedsize <- seedSize_raw %>%
      select(species,seed_size)

  # adding info on all costs to produce 1 seed
  # Todo: Lizzy you need to replace numbers below with actual names, see examples
  seedcosts <- list() 
  seedcosts[["BAER"]] <- list(parts=NULL, scale=1)  #w[2]/10+w[4]/10+w[13]+w[12]+w[19]+w[20]
  seedcosts[["BOLE"]] <- list(parts=NULL, scale=1) #w[5]+w[6]+w[11]+w[15]+w[16]
  seedcosts[["COER"]] <- list(parts=c("inflorescence_stalk_in_fruit","flower_petals", "bract_fruit","fruit_mature"), scale=c(1/6,1,1,1))
  seedcosts[["EPMI"]] <- list(parts=c("flower_petals", "fruit_mature"), scale=1)
  seedcosts[["GRBU"]] <- list(parts=NULL, scale=1) #w[2]+w[7]+w[12]+w[14]+w[15]
  seedcosts[["GRSP"]] <- list(parts=NULL, scale=1) #w[4]+w[9]+w[14]+w[16]+w[18]
  seedcosts[["HATE"]] <- list(parts=NULL, scale=1) #w[4]+w[6]+w[13]+w[16]
  seedcosts[["HEPU"]] <- list(parts=NULL, scale=1) #w[3]+w[10]+w[15]
  seedcosts[["LEES"]] <- list(parts=NULL, scale=1) #w[5]+w[10]+w[13]
  seedcosts[["PELA"]] <- list(parts=NULL, scale=1) #w[4]+w[5]+w[11]+w[12]
  seedcosts[["PEPU"]] <- list(parts=NULL, scale=1) #w[4]/30+w[9]+w[16]
  seedcosts[["PHPH"]] <- list(parts=NULL, scale=1) #w[4]+w[5]+w[6]+w[15]+w[16]
  seedcosts[["PILI"]] <- list(parts=NULL, scale=1) #w[2]/10+w[3]+w[5]+w[7]+w[11]+w[12]
  seedcosts[["PUTU"]] <- list(parts=NULL, scale=1) # w[2]+w[3]+w[4]+w[10]+w[11]

  species <- names(seedcosts)

  costs <- data.frame(species = species,
    seedcosts = sapply(names(seedcosts), function(sp) {
                      x <- filterBySpecies(sp, PartsSummary_all)
                      sum(x$weight[x$part %in% seedcosts[[sp]][["parts"]]] 
                          * seedcosts[[sp]][["scale"]]) 
                    }))
  seedsize <- merge(seedsize, costs, by="species", all.x=TRUE)
  seedsize$costs_per_seed <- (seedsize$seedcosts-seedsize$seed_size)/(seedsize$seedcosts)
  seedsize
}

porcess_LMA <- function(LMA_raw) {
  LMA <- filter(LMA_raw, species!="" & species!=" ") %>%
    select(species, age, LMA, branch_age, leaf_number, leaf_area)
  LMA$leaf_size <- LMA$leaf_area/LMA$leaf_number
  LMA
}