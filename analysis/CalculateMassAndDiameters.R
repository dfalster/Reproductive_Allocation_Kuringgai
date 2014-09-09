CalculateMassAndDiameters<-function()
{
source("analysis/plot-utils.R")

data <- tbl_df(read.csv("data/2013_Kuringgai_harvest.csv", stringsAsFactors=FALSE)) %.%
  filter(year==2013) %.%
  arrange(tag_ID, date, segment)

data$species[data$species=="BAER "] <- "BAER"

segments <- list()
segments[["1"]] <- c(as.character(1:8,"1.2","1.3","1.2.1","1.2.1.1","1.1.2","1.1.2.1","1.1.1.2"))
segments[["2"]] <- c(as.character(2:8,"1.1.2","1.1.2.1","1.1.1.2"))
segments[["3"]] <- c(as.character(3:8,"1.1.1.2"))
segments[["4"]] <- as.character(4:8)
segments[["5"]] <- as.character(5:8)
segments[["6"]] <- as.character(6:8)
segments[["7"]] <- as.character(7:8)
segments[["8"]] <- as.character(8)
segments[["9"]] <- c("1.2","1.2.1","1.2.1.1")
segments[["10"]] <- c("1.3")
segments[["11"]] <- c("1.2.1","1.2.1.1")
segments[["12"]] <- c("1.2.1.1")
segments[["13"]] <- c("1.1.2","1.1.2.1")
segments[["14"]] <- c("1.1.2.1")
segments[["15"]] <- c("1.1.1.2")

## Get total mass of all material subteneded by given node

get.mass.at.node <- function(data, segments){
  data %.%  filter(segment %in% segments) %.%
    group_by(tag_ID) %.%
    summarise(
      species = species[1],
      site= site[1],
      age=age[1],
      leaf_weight=sum(leaf_weight),
      stem_weight=sum(stem_weight),
      total_weight=sum(leaf_weight, stem_weight))
}

mass <- ldply(segments,  function(x) get.mass.at.node(data, x)) %.%
  rename(c('.id'='node_above')) %.%  # rename plyr name to somewthing more meaningful
  arrange(tag_ID, node_above)

# Get avergae diameter for base of segment. This is given by diameter readings for segment
# with names in list above
get.diam.node.above <- function(data, level){
  data %.%  filter(node_above == level) %.%
    group_by(tag_ID) %.%
    summarise(
      node_above=level,
      dia=mean(c(diameter_1,diameter_2, diameter_3), na.rm=TRUE),
      stem.area=dia^2*pi/4
    )
}

diameters <- ldply(names(segments),  function(x) get.diam.node.above(data, x)) %.%
  arrange(tag_ID, node_above)

# Merge mass and diameter measurements
merged <- merge(mass, diameters, by=c("tag_ID", "node_above"))
write.csv(x=merged,file="output/WeightDiameterData.csv")
}
