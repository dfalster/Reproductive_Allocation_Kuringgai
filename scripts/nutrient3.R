#starting commands
nutrient <- read.csv("~/R/Reproductive_Allocation_Kuringgai/data/nutrient_data.csv")
soil <- read.csv("~/R/Reproductive_Allocation_Kuringgai/data/nutrient_soil.csv")
accessoryList <- read.csv("~/R/Reproductive_Allocation_Kuringgai/data/accessoryList.csv")

library(plyr)
library(dplyr)
library(tidyr)

pch.tissue <- function(x=NULL){
  ret <- c(wood_from_tip=16, wood_from_middle=10,wood_from_base=7,shed_sapwood=1,bark=15,shed_bark=0,leaves=17,senescent_leaves=2,seed=8,reproductive=4)
  
  names(ret) <- c("wood_from_tip","wood_from_middle","wood_from_base","shed_sapwood","bark","shed_bark","leaves","senescent_leaves","seed","reproductive")
  if(!is.null(x)) {
    ret <- ret[as.character(x)]
  }
  ret
}

pch.tissue2 <- function(x=NULL){
  ret <- c(sapwood=16, bark=15,leaves=17)
  names(ret) <- c("sapwood","bark","leaves")
  if(!is.null(x)) {
    ret <- ret[as.character(x)]
  }
  ret
}

plot_abline <- function(add_lines) {
  abline(log(1),1)
  abline(log10(.1),1,col="light gray")
  abline(log10(10),1,col="light gray")
  abline(log10(.5),1,col="gray")
  abline(log10(2),1,,col="gray")
  abline(log10(.01),1,col="light blue")
  abline(log10(100),1,col="light blue")
}

polygon2 <- function(...) polygon(..., density=NA)

ypoly <- function(data, n=6) {c(data[seq_len(n)],0,0)}

#SUMMARIZING AND MERGING NP DATA INTO SUMMARYIND

#summarize nutrient data by species, age
nutrient_spp_age <- subset(nutrient,live_dead=="live"|live_dead=="dead")
mean_values <- function(nutrient_spp_age) {
  dplyr::select(nutrient_spp_age,species,tissue,age,live_dead,K,P,N) %>%
  dplyr::group_by(species,tissue,age,live_dead) %>%
  dplyr::summarise_at(c("N","P","K"),mean)
}

nutrient_sum <- mean_values(nutrient_spp_age)

nutrient_sum_N <- dplyr::select(nutrient_sum,species,tissue,age,live_dead,N)
nutrient_sum_N <- tidyr::spread(nutrient_sum_N,live_dead,N)
nutrient_sum_N <- as.data.frame(nutrient_sum_N)
names(nutrient_sum_N) <- c("species","tissue","age","N_dead","N_live")

nutrient_sum_P <- dplyr::select(nutrient_sum,species,tissue,age,live_dead,P)
nutrient_sum_P <- tidyr::spread(nutrient_sum_P,live_dead,P)
nutrient_sum_P <- as.data.frame(nutrient_sum_P)
names(nutrient_sum_P) <- c("species","tissue","age","P_dead","P_live")

nutrient_sum_K <- dplyr::select(nutrient_sum,species,tissue,age,live_dead,K)
nutrient_sum_K <- tidyr::spread(nutrient_sum_K,live_dead,K)
nutrient_sum_K <- as.data.frame(nutrient_sum_K)
names(nutrient_sum_K) <- c("species","tissue","age","K_dead","K_live")

nutrient_sum_spp_age <- join(nutrient_sum_N, nutrient_sum_P, by = c("species","tissue","age"),type="full")
nutrient_sum_spp_age <- plyr::join(nutrient_sum_spp_age, nutrient_sum_K, by = c("species","tissue","age"),type="full")

nutrient_sum_spp_age <- nutrient_sum_spp_age %>%
  mutate(
    NUE = 1 - (N_dead/N_live),
    PUE = 1 - (P_dead/P_live),
    KUE = 1 - (K_dead/K_live)
  )

#summarize nutrient data by species
nutrient_spp <- subset(nutrient,live_dead=="live"|live_dead=="dead")
mean_values <- function(nutrient_spp) {
  dplyr::select(nutrient_spp_age,species,tissue,live_dead,K,P,N) %>%
  dplyr::group_by(species,tissue,live_dead) %>%
    dplyr::summarise_at(c("N","P","K"),mean)
}

nutrient_sum <- mean_values(nutrient_spp)

nutrient_sum_N <- dplyr::select(nutrient_sum,species,tissue,live_dead,N)
nutrient_sum_N <- tidyr::spread(nutrient_sum_N,live_dead,N)
nutrient_sum_N <- as.data.frame(nutrient_sum_N)
names(nutrient_sum_N) <- c("species","tissue","N_dead","N_live")

nutrient_sum_P <- dplyr::select(nutrient_sum,species,tissue,live_dead,P)
nutrient_sum_P <- tidyr::spread(nutrient_sum_P,live_dead,P)
nutrient_sum_P <- as.data.frame(nutrient_sum_P)
names(nutrient_sum_P) <- c("species","tissue","P_dead","P_live")

nutrient_sum_K <- dplyr::select(nutrient_sum,species,tissue,live_dead,K)
nutrient_sum_K <- tidyr::spread(nutrient_sum_K,live_dead,K)
nutrient_sum_K <- as.data.frame(nutrient_sum_K)
names(nutrient_sum_K) <- c("species","tissue","K_dead","K_live")

nutrient_sum_spp <- plyr::join(nutrient_sum_N, nutrient_sum_P, by = c("species","tissue"),type="full")
nutrient_sum_spp <- plyr::join(nutrient_sum_spp, nutrient_sum_K, by = c("species","tissue"),type="full")


nutrient_sum_spp <- nutrient_sum_spp %>%
  mutate(
    NUE = 1 - (N_dead/N_live),
    PUE = 1 - (P_dead/P_live),
    KUE = 1 - (K_dead/K_live)
  )

#merge dataframe with species means for N,P,K content with SummaryInd
temp <- subset(nutrient_sum_spp,tissue=="leaves")
temp <- dplyr::select(temp,species,N_dead,N_live,P_dead,P_live,NUE,PUE)
names(temp) <- c("species","dead_leaf_N","live_leaf_N","dead_leaf_P","live_leaf_P","NUE","PUE")

SummaryInd2 <- plyr::join(SummaryInd,temp, by="species")

#merging in reproduction data
InvestIndPart <- merge(InvestmentByPart,dplyr::select(accessoryList,-sort), by=c("species","part"))
  
for(v in c("weight")) {
  i <- is.na(InvestIndPart[[v]])
  InvestIndPart[[v]][i] <- 0
}

SummaryInvest <- 
  InvestIndPart %>%
  dplyr::group_by(parts_to_match,individual) %>%
  dplyr::summarise_at("weight",sum)

SummaryInvest$species <- SummaryInvest$individual
SummaryInvest$species <- substr(SummaryInvest$species,1,nchar(SummaryInvest$species)-4)
names(SummaryInvest) <- c("parts_to_match","individual","weight","species")

nutrient_subset <- dplyr::select(nutrient_spp,species,parts_to_match,N,P)

SummaryInvest <- merge(SummaryInvest,nutrient_subset, by=c("species","parts_to_match"),all.x=TRUE)
names(SummaryInvest)

SummaryInvest$repro_N <- SummaryInvest$weight*SummaryInvest$N
SummaryInvest$repro_P <- SummaryInvest$weight*SummaryInvest$P

nutrientRepro <- 
  SummaryInvest %>%
  dplyr::group_by(individual) %>%
  dplyr::summarise_at(c("repro_N","repro_P"),sum)

SummaryInd2 <- plyr::join(SummaryInd2,nutrientRepro, by="individual")

#determine leaf N,P in various tissue pools
SummaryNut <- SummaryInd2

SummaryNut <- SummaryInd2 %>% mutate(
  leaf_N_lost = leaf_shed*dead_leaf_N,
  leaf_P_lost = leaf_shed*dead_leaf_P,
  leaf_N_resorbed = leaf_shed*(live_leaf_N-dead_leaf_N),
  leaf_P_resorbed = leaf_shed*(live_leaf_P-dead_leaf_P),
  leaf_N_new = leaf_inv_gross*live_leaf_N,
  leaf_P_new = leaf_inv_gross*live_leaf_P, #P used by plant for leaves - includes resorbed P being reused
  leaf_N_net = leaf_N_new - leaf_N_resorbed, #new N required from soil
  leaf_P_net = leaf_P_new - leaf_P_resorbed, #new P required from soil
  leaf_N_for_RA = growth_leaf*live_leaf_N,
  leaf_P_for_RA = growth_leaf*live_leaf_P,
  leaf_N_for_replacement = leaf_N_lost,
  leaf_P_for_replacement = leaf_P_lost
)


for (i in 1:nrow(SummaryNut)) {
  for (v in c("leaf_N_for_RA","leaf_P_for_RA")) {
    if (SummaryNut[[v]][i] < 0) {
      SummaryNut[[v]][i] = 0
    }
  }
}

for (i in 1:nrow(SummaryNut)) {
  for (v in c("repro_N","repro_P")) {
    i <- is.na(SummaryNut[[v]])
    SummaryNut[[v]][i] <- 0
  }
}

SummaryNut <- SummaryNut %>% mutate(
  NP_live_leaf = live_leaf_N/live_leaf_P,
  NP_dead_leaf = dead_leaf_N/dead_leaf_P,
  RA_N = repro_N / (repro_N + leaf_N_for_RA),
  RA_P = repro_P / (repro_P + leaf_P_for_RA),
  total_N_per_year = leaf_N_new + repro_N,
  total_P_per_year = leaf_P_new + repro_P,
  prop_recycled_N = leaf_N_resorbed / leaf_N_new,
  prop_recycled_P = leaf_P_resorbed / leaf_P_new,
  prop_required_N = leaf_N_resorbed / total_N_per_year,
  prop_required_P = leaf_P_resorbed / total_P_per_year
)

data <- dplyr::select(SummaryNut,species,age,RA_N,RA_P,RA_max_1)

  for (v in c("RA_N","RA_P","RA_max_1")) {
    i <- is.na(data[[v]])
    data[[v]][i] <- 0
  }

data_spp_age <- dplyr::select(data,species,age,RA_N,RA_P,RA_max_1)
mean_values <- function(data_spp_age) {
  dplyr::group_by(data_spp_age, species,age) %>%
  dplyr::summarise_at(c("RA_N","RA_P","RA_max_1"),mean)
}

RA_Nutrient <- mean_values(data_spp_age)

stem_segments <- nutrient %>%
    subset(tissue_complete %in% c("wood_from_base","wood_from_middle","wood_from_tip"))%>%
    dplyr::select(species,age,P,N,mean_without_bark_diam_mm,tissue_complete,use_for_N,use_for_P) 
stem_segments$diameter <- as.numeric(stem_segments$mean_without_bark_diam_mm)
#stem_segments$area <- ((stem_segments$diameter/2)^2)*3.14159

all_species <- unique(stem_segments$species)

stem_segments$category <- 1

for (i in 1:nrow(stem_segments)) {
  if(stem_segments$tissue_complete[i] == "wood_from_middle") {
  stem_segments$category[i] <- 2
  }
  if(stem_segments$tissue_complete[i] == "wood_from_base") {
    stem_segments$category[i] <- 3
  }
}

  
plot(P~diameter,stem_segments,log="xy",col=col.spp(species),pch=16)

stem_segments$diameter <- as.numeric(stem_segments$diameter)


use <- subset(nutrient,nutrient$with_bark_diam_mm!="")

for (i in 1:length(use$individual_rep1)) {
  if (use$with_bark_diam_mm[i]=="") {
    use$with_bark_diam <- 3
    use$with_bark_diam <- mean(as.numeric(unlist(strsplit(as.character(use$with_bark_diam_mm), split = ";"))))
  } else {
    nutrient$with_bark_diam <- 4
  }
}



subset(nutrient$with_bark_diam_mm,nutrient$with_bark_diam_mm!="")
#looking at sequential discs from stems
```{r}
wood_discs_base <- nutrient %>%
    subset(tissue_complete %in% c("wood_from_base"))%>%
    select(individual_rep1,species,P) 
names(wood_discs_base) <- c("individual","species","P_base")

wood_discs_middle <- nutrient %>%
    subset(tissue_complete %in% c("wood_from_middle"))%>%
    select(individual_rep1,P) 
names(wood_discs_middle) <- c("individual","P_mid")

wood_discs_tip <- nutrient %>%
    subset(tissue_complete %in% c("wood_from_tip"))%>%
    select(individual_rep1,P) 
names(wood_discs_tip) <- c("individual","P_tip")

wood_discs <- merge(wood_discs_base, wood_discs_middle, by="individual")
wood_discs <- merge(wood_discs, wood_discs_tip, by="individual")



```
names(nutrient)

#PLOTS

#soils data
```{r}
pdf("figs2/Nutrients/soil.pdf", height=6, width=6)
plot
par(mfrow=c(1,2), cex=1, omi=c(.1,.1,.6,.05), mai=c(.8,.8,.05,.05))
plot(N~P,soil,col=col.age(age),pch=16,cex=replicate)
dev.off()
```

#various plots using NP data in leaves, reproductive material
```{r}
pdf("output/plots_of_NP_use.pdf", height=7.5, width=11)
plot
par(mfrow=c(1,2), cex=1, omi=c(.1,.1,.6,.05), mai=c(.8,.8,.05,.05))
plot(leaf_N_net ~ repro_N,SummaryNut, col=col.spp(species),pch=16,log="xy",xlim=c(1,10^7),ylim=c(1,10^7),xlab="",ylab="")
mtext("N in reproductive material (mg)",1, outer=FALSE,line=2)
mtext("new N from soil required for leaves (mg)",2, outer=FALSE,line=2)
plot_abline(add_lines)

plot(leaf_P_net ~ repro_P,SummaryNut, col=col.spp(species),pch=16,log="xy",xlim=c(100,10^8),ylim=c(100,10^8),xlab="",ylab="")
mtext("P in reproductive material (ppm)",1, outer=FALSE,line=2)
mtext("new P from soil required for leaves (ppm)",2, outer=FALSE,line=2)
plot_abline(add_lines)

plot(total_N_per_year ~ leaf_N_resorbed,SummaryNut, col=col.spp(species),pch=16,log="xy",xlim=c(1,10^7),ylim=c(1,10^7),xlab="",ylab="")
mtext("N resorbed from shed leaves (mg)",1, outer=FALSE,line=2)
mtext("total N used by plant for repro + all leaf growth (mg)",2, outer=FALSE,line=2)
plot_abline(add_lines)

plot(total_P_per_year ~ leaf_P_resorbed,SummaryNut, col=col.spp(species),pch=16,log="xy",xlim=c(100,10^8),ylim=c(100,10^8),xlab="",ylab="")
mtext("P resorbed from shed leaves (ppm)",1, outer=FALSE,line=2)
mtext("total P used by plant for repro + all leaf growth (ppm)",2, outer=FALSE,line=2)
plot_abline(add_lines)

plot(leaf_N_net ~leaf_N_resorbed,subset(SummaryNut,age>3), col=col.spp(species),pch=16,log="xy",xlim=c(1,10^7),ylim=c(1,10^7),xlab="",ylab="")
mtext("N resorbed from shed leaves (ppm)",1, outer=FALSE,line=2)
mtext("new N from soil required for leaves (ppm)",2, outer=FALSE,line=2)
plot_abline(add_lines)

plot(leaf_P_net~leaf_P_resorbed,subset(SummaryNut,age>3), col=col.spp(species),pch=16,log="xy",xlim=c(100,10^8),ylim=c(100,10^8),xlab="",ylab="")
mtext("P resorbed from shed leaves (ppm)",1, outer=FALSE,line=2)
mtext("new P from soil required for leaves (ppm)",2, outer=FALSE,line=2)
plot_abline(add_lines)

plot(NUE ~height,subset(SummaryNut,age>3), col=col.spp(species),pch=16,log="x")
plot(PUE ~height,subset(SummaryNut,age>3), col=col.spp(species),pch=16,log="x")

dev.off()
```


#simple N vs P plot
```{r}
pdf("figs2/Nutrients/NvsP.pdf", height=11, width=8.5)
plot
par(mfrow=c(2,1), cex=1, omi=c(.1,.1,.05,.05), mai=c(.8,.8,.05,.05))
plot(P~N,data=subset(nutrient),col=col.spp(species),pch=pch.tissue(tissue_simple),log="xy")
legend("topleft",legend=c("sapwood","wood from mid","wood from base","shed sapwood","bark","shed bark","leaves","shed leaves", "seed", "other reproductive"),pch=c(16,10,7,1,15,0,17,2,8,4),bty="n")
legend("bottomright",legend=labels.spp(),col=col.spp(),pch=16, cex=.8,bty="n")

plot(P~N,data=subset(nutrient,tissue_simple=="reproductive"|tissue_simple=="seed"),col=col.spp(species),pch=16,log="xy",cex=0.75)
test <- subset(nutrient,tissue_simple=="reproductive"|tissue_simple=="seed")
text(test$P~test$N,labels=test$tissue_complete,cex=0.5,adj=-.2)
legend("bottomright",legend=labels.spp(),col=col.spp(),pch=16, cex=.8,bty="n")
dev.off()
```

#plotting live versus dead tissue
```{r}
pdf("figs2/Nutrients/live_vs_shed.pdf", height=11, width=8.5)
plot
par(mfrow=c(2,1), cex=1, omi=c(.1,.1,.05,.05), mai=c(.8,.8,.05,.05)) 
plot(P~N,data=subset(nutrient,tissue_complete=="leaves"),col=col.spp(species),pch=16,ylim=c(10,500),log="",xlim=c(.4,2.5))
points(P~N,data=subset(nutrient,tissue_complete=="senescent_leaves"),col=col.spp(species),pch=1,cex=1.5)
points(P~N,data=subset(nutrient,tissue_complete=="senescent_leaves"),col=col.spp(species),pch=1,cex=1)
text(1.05*par("usr")[1],0.95*par("usr")[4],"leaves versus shed leaves",adj=0)

plot(P~N,data=subset(nutrient,tissue_complete=="bark"),col=col.spp(species),pch=16,ylim=c(10,300),log="",xlim=c(.15,1.5))
points(P~N,data=subset(nutrient,tissue_complete=="shed_bark"),col=col.spp(species),pch=1,cex=1.5)
points(P~N,data=subset(nutrient,tissue_complete=="shed_bark"),col=col.spp(species),pch=1,cex=1)
text(1.05*par("usr")[1],0.95*par("usr")[4],"bark versus shed bark",adj=0)

plot(P~N,data=subset(nutrient,tissue_complete=="wood_from_tip"),col=col.spp(species),pch=16,ylim=c(1,300),log="",xlim=c(0,0.8),cex=1)
points(P~N,data=subset(nutrient,tissue_complete=="shed_sapwood"),col=col.spp(species),pch=1,cex=1.5)
points(P~N,data=subset(nutrient,tissue_complete=="shed_sapwood"),col=col.spp(species),pch=1,cex=1)
text(1.15*par("usr")[1],0.95*par("usr")[4],"sapwood versus shed sapwood",adj=0)

plot(P~N,data=subset(nutrient,tissue_complete=="wood_from_tip"),col=col.spp(species),pch=16,ylim=c(1,300),log="",xlim=c(0,0.8),cex=1)
points(P~N,data=subset(nutrient,tissue_complete=="wood_from_base"&age>3),col=col.spp(species),pch=1,cex=1.5)
points(P~N,data=subset(nutrient,tissue_complete=="wood_from_base"&age>3),col=col.spp(species),pch=1,cex=1)
text(1.05*par("usr")[1],0.95*par("usr")[4],"sapwood versus wood from base of plant for plants >3 years",adj=0)

plot(PUE~NUE,data=subset(nutrient_sum_spp_age),col=col.spp(species),pch=pch.tissue2(tissue),log="",xlim=c(-0.5,1),ylim=c(-0.5,1),xlab="NUE vs PUE by species*age")
rect(par("usr")[1],par("usr")[3],0,1.5,col="light gray",border=NA)
rect(par("usr")[1],par("usr")[3],1.5,0,col="light blue",border=NA)
rect(par("usr")[1],par("usr")[3],0,0,col="grey",border=NA)
points(PUE~NUE,data=subset(nutrient_sum_spp_age),col=col.spp(species),pch=pch.tissue2(tissue))
abline(0,1)
legend("topleft",legend=c("sapwood","bark","leaves"),pch=c(16,15,17),bty="n")
legend("bottomright",legend=labels.spp(),col=col.spp(),pch=16, cex=.8,bty="n")

plot(PUE~NUE,data=subset(nutrient_sum_spp),col=col.spp(species),pch=pch.tissue2(tissue),log="",xlim=c(-0.5,1),ylim=c(-0.5,1),xlab="NUE vs PUE by species")
rect(par("usr")[1],par("usr")[3],0,1.5,col="light gray",border=NA)
rect(par("usr")[1],par("usr")[3],1.5,0,col="light blue",border=NA)
rect(par("usr")[1],par("usr")[3],0,0,col="grey",border=NA)
points(PUE~NUE,data=subset(nutrient_sum_spp),col=col.spp(species),pch=pch.tissue2(tissue))
abline(0,1)
legend("topleft",legend=c("sapwood","bark","leaves"),pch=c(16,15,17),bty="n")
legend("bottomright",legend=labels.spp(),col=col.spp(),pch=16, cex=.8,bty="n")

dev.off()
```

#plotting NP data for each species, colored by age
```{r}
pdf("figs2/Nutrients/age_by_species_NP.pdf", height=7.5, width=11)
plot
par(mfrow=c(1,2), cex=1, omi=c(.1,.1,.05,.05), mai=c(.8,.8,.05,.05)) 
for (i in c("BAER","BOLE","COER","EPMI","GRBU","GRSP","HATE","HEPU","LEES","PELA","PEPU","PHPH","PILI","PUTU")) {
data <- subset(nutrient,species==i)
plot(P~N,data,col=col.age(age),pch=pch.tissue(tissue_simple),log="xy")
text(1,200,i)
data2 <- subset(data,tissue_simple=="reproductive"|tissue_simple=="seed")
text(P~N,data2,label=data2$tissue_complete,col="red",cex=0.7)
legend("bottomright",legend=labels.age(),col=col.age(),pch=16, cex=.8,bty="n")
legend("topleft",legend=c("sapwood","wood from mid","wood from base","shed sapwood","bark","shed bark","leaves","shed leaves", "seed", "other reproductive"),pch=c(16,10,7,1,15,0,17,2,8,4),bty="n")
}
dev.off()
```




###fits for nutrient concentration

tmp <- subset(stem_segments,tissue_complete==
                "wood_from_tip")
tmp <- subset(tmp,age>1.4)

mean_sapwood <- tmp %>% 
  dplyr::group_by(species) %>%
  dplyr::summarise_each(dplyr::funs(mean),P,N,mean_without_bark_diam_mm)

names(mean_sapwood) <- c("species","sapwood_P","sapwood_N","sapwood_diameter")

sapwood_fits <- select(stem_segments,species,age,tissue_complete,)

names(stem_segments) <- c("species","age","P","N","diameter","tissue_complete")
sapwood_fits <- subset(stem_segments,age>2 &tissue_complete!="wood_from_tip")

sapwood_fits <- merge(sapwood_fits,mean_sapwood, by="species")
sapwood_fits$radius <- sapwood_fits$diameter/2


# hh - hardwood conc as a fraction of sapwood_fits
# xx -sapwood depth
# ans = total conc predicted

data_tmp <- sapwood_fits
data_tmp <- split(data_tmp, data_tmp$species)

sapwood_output_P <- data.frame(matrix(ncol = 5, nrow = 0))
names(sapwood_output_P) <- c("species","logit.h","xx","logit.h t-value","xx t-value")


for(spp in c("BAER", "BOLE", "COER", "EPMI", "GRBU", "HATE", "HEPU", "LEES", "PELA", "PILI")) {
  #function to determine concentration for a single set of values
  concentration <- function(radius,logit.h,xx) {
    sapwood_conc <- mean(data_tmp[[spp]]$sapwood_P)
    hh <- exp(logit.h)/(1+exp(logit.h))
    #xx <- max(radius)*exp(logit.x)/(1+exp(logit.x))
    
    ans <- sapwood_conc*hh*(radius-xx)^2/radius^2 + sapwood_conc*(1-(radius-xx)^2/radius^2)
    ans[radius <= xx] <- sapwood_conc
    
    return(ans)
  }
  
  myfit <- nls(data_tmp[[spp]]$P ~ concentration(data_tmp[[spp]]$radius,logit.h,xx),
             data = data_tmp[[spp]],
             start = list(logit.h = -2,xx=0.5))
  summary(myfit)
  #confint(myfit)
  tmp_column <- data.frame(matrix(ncol = 5, nrow = 0))
  names(tmp_column) <- c("species","logit.h","xx","logit.h t-value","xx t-value")
  tmp_column[1,] <- c(spp,summary(myfit)$coefficients[1],summary(myfit)$coefficients[2],
                      summary(myfit)$coefficients[7],summary(myfit)$coefficients[8])
  sapwood_output_P <- rbind(sapwood_output_P,tmp_column)
}

for (i in c(2:5)) {
sapwood_output_P[,i] <- as.numeric(sapwood_output_P[,i])
}

sapwood_output_P$prop_decline <- (exp(sapwood_output_P$logit.h)/(1+(exp(sapwood_output_P$logit.h))))

write.csv(sapwood_output_P, file="output/Sapwood_P.csv",row.names=FALSE)




data_tmp <- subset(sapwood_fits,!is.na(sapwood_fits$N))

data_tmp <- split(data_tmp, data_tmp$species)



sapwood_output_N <- data.frame(matrix(ncol = 5, nrow = 0))
names(sapwood_output_N) <- c("species","logit.h","xx","logit.h t-value","xx t-value")

for(spp in c("BAER", "EPMI", "GRBU", "PELA","PEPU","PHPH")) {
  #function to determine concentration for a single set of values
  concentration <- function(radius,logit.h,xx) {
    sapwood_conc <- mean(data_tmp[[spp]]$sapwood_N)
    hh <- exp(logit.h)/(1+exp(logit.h))
    #xx <- max(radius)*exp(logit.x)/(1+exp(logit.x))
    
    ans <- sapwood_conc*hh*(radius-xx)^2/radius^2 + sapwood_conc*(1-(radius-xx)^2/radius^2)
    ans[radius <= xx] <- sapwood_conc
    
    return(ans)
  }
  
  myfit <- nls(data_tmp[[spp]]$N ~ concentration(data_tmp[[spp]]$radius,logit.h,xx),
               data = data_tmp[[spp]],
               start = list(logit.h = -.5,xx=0.5))
  summary(myfit)
  #confint(myfit)
  tmp_column <- data.frame(matrix(ncol = 5, nrow = 0))
  names(tmp_column) <- c("species","logit.h","xx","logit.h t-value","xx t-value")
  tmp_column[1,] <- c(spp,summary(myfit)$coefficients[1],summary(myfit)$coefficients[2],
                      summary(myfit)$coefficients[7],summary(myfit)$coefficients[8])
  sapwood_output_N <- rbind(sapwood_output_N,tmp_column)
}

for (i in c(2:5)) {
  sapwood_output_N[,i] <- as.numeric(sapwood_output_N[,i])
}

sapwood_output_N$prop_decline <- (exp(sapwood_output_N$logit.h)/(1+(exp(sapwood_output_N$logit.h))))
write.csv(sapwood_output_N, file="output/Sapwood_N.csv",row.names=FALSE)
