

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

process_LMA <- function(LMA_raw) {
  LMA <- filter(LMA_raw, species!="" & species!=" ") %>%
    select(species, age, LMA, branch_age, leaf_number, leaf_area)
  LMA$leaf_size <- LMA$leaf_area/LMA$leaf_number
  LMA
}

process_leaves_per_length <- function(leavesPerLength_raw) {
    # get average leaves per length for spp
  filter(leavesPerLength_raw,use=="yes") %>%
    mutate(count_per_length = leaf_count/length_mm) %>%
    group_by(species) %>%
    summarise(count_per_length = mean(count_per_length, na.rm=TRUE))
}

process_leaf_lifespan <- function(leafLifespan_raw, leavesPerLength) {
  leafLifespan <- filter(leafLifespan_raw, dont_use!="dead" & dont_use!="dont_use") %>%
    select(species,age,individual,d_start,d_end,length_start,new_length,lvs_start_length,lvs_start_count,lvs_end_length,lvs_end_count,lvs_new_length,lvs_new_count,new_but_shed_count,mm_lvs_spec,count_lvs_spec)

  leafLifespan$individual <- as.factor(leafLifespan$individual)
  leafLifespan$species <- as.factor(leafLifespan$species)

  #loading this column as character, so using this for now

  # Calculating Leaf Lifespan

  # where counts did not previously exists, set as 0; CHECK WITH DANIEL THAT "V" IS A SET "NAME" IN THESE FUNCTIONS

  leaf_lifespan_temp <- leafLifespan %>%
    select(species,age,individual,d_start,d_end,length_start,new_length,lvs_start_count,lvs_end_count,lvs_new_count,new_but_shed_count)

  for(v in c("lvs_start_count","lvs_end_count","lvs_new_count","new_but_shed_count")) {
    i <- is.na(leaf_lifespan_temp[[v]])
    leaf_lifespan_temp[[v]][i] <- 0
  }

  # For individuals with "length" measures, translate into count


  # get spp average for all individuals
  leafLifespan$count_per_length <- leavesPerLength$count_per_length[match(leafLifespan$species, leavesPerLength$species)]

  # if possible use data for that individual, if it exists; this code substitutes in species count per length #s for individuals where this data is available
  i <- !is.na(leafLifespan$mm_lvs)
  leafLifespan$count_per_length[i] <- (leafLifespan$count_lvs_spec/leafLifespan$mm_lvs)[i]

  #this is the end of the section Daniel helped me rewrite - next section is mine

  #get total leaf counts
  for(v in c("lvs_start_length","lvs_start_count","lvs_end_length","lvs_end_count","lvs_new_length","lvs_new_count","new_but_shed_count")) {
    i <- is.na(leafLifespan[[v]])
    leafLifespan[[v]][i] <- 0
  }
  leafLifespan$lvs_start <- leafLifespan$lvs_start_count + (leafLifespan$lvs_start_length * leafLifespan$count_per_length)
  leafLifespan$lvs_end <- leafLifespan$lvs_end_count + (leafLifespan$lvs_end_length * leafLifespan$count_per_length)
  leafLifespan$lvs_new <- leafLifespan$lvs_new_count + (leafLifespan$lvs_new_length * leafLifespan$count_per_length)

  #calculating leaf lifespan
  leafLifespan$LL_bd <- (leafLifespan$lvs_start)/(((leafLifespan$lvs_start)-(leafLifespan$lvs_end))+(leafLifespan$lvs_new)/2)
  leafLifespan$LL_death <- (leafLifespan$lvs_start)/((leafLifespan$lvs_start)-(leafLifespan$lvs_end))
  leafLifespan$LL_birth <- (leafLifespan$lvs_start)/(leafLifespan$lvs_new)

  leafLifespan$LL_death[which(is.infinite(leafLifespan$LL_death))] <- NA
  leafLifespan$LL_bd[which(is.infinite(leafLifespan$LL_death))] <- NA
  leafLifespan$LL_birth[which(is.infinite(leafLifespan$LL_death))] <- NA
}
