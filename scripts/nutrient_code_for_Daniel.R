#relevant data frames
#nutrient = all nutrient data
#stem_segments = data on stem segments, by tissue type
#stem_segments_select_N = as above, but removing suspect N conc values
#stem_segments_select_P = as above, but removing suspect P conc values
#sapwood_fits = takes data from stem_segments and calculates mean sapwood diameter and concentration for each species and includes those as additional columns



nutrient <- read.csv("~/R/Reproductive_Allocation_Kuringgai/data/nutrient_data.csv")
par(mfrow = c(4,2), cex = 1, omi=c(.1,.1,.6,.05), mai=c(1,1,.05,.05))

stem_segments <- nutrient %>%
  subset(tissue_complete %in% c("wood_from_base","wood_from_middle","wood_from_tip"))%>%
  dplyr::select(species,age,P,N,mean_without_bark_diam_mm,tissue_complete,use_for_N,use_for_P) 

names(stem_segments) <- c("species","age","P","N","stem_diameter","tissue","use_for_N","use_for_P")
stem_segments$stem_diameter <- as.numeric(stem_segments$stem_diameter)
stem_segments <- subset(stem_segments,stem_diameter>0)

#plots by species of N & P concentration by stem_diameter
pdf("output/stem segment by diameter.pdf", height=11, width=8)
plot

par(mfrow = c(4,2), cex = 1, omi=c(.1,.1,.6,.05), mai=c(1,1,.05,.05))

stem_segments_select <- subset(stem_segments,age>2|tissue=="wood_from_tip")
stem_segments_select_N <- subset(stem_segments_select,use_for_N==TRUE)
stem_segments_select_P <- subset(stem_segments_select,use_for_P==TRUE)


for (i in seq_along(all_species)) {
  
  data_sp_P <- filter(stem_segments_select_P, species == all_species[i])
  data_sp_N <- filter(stem_segments_select_N, species == all_species[i])
  temp2 <- subset(data_sp_P,stem_diameter>0)
  temp3 <- subset(data_sp_N,N>0)
  
  plot(P~stem_diameter,data=data_sp_P,log="",col=col.age(data_sp_P$age),pch=pch.tissue(data_sp_P$tissue),xlab="",ylab="",
       xlim=c(0,1.1*max(temp2$stem_diameter)),ylim=c(0,1.1*max(temp2$P)))
  mod <- sma(P~stem_diameter,data=data_sp_P,log="")
  mtext("stem diameter (mm)",1,line=1.7,cex=0.7)
  mtext("P concentration",2,line=2.3,cex=0.7)
  mtext(labels.spp.full(all_species[i]), 3, 0)
  plot(N~stem_diameter,data=data_sp_N,log="",col=col.age(data_sp_N$age),pch=pch.tissue(data_sp_N$tissue),xlab="",ylab="",
       xlim=c(0,1.1*max(temp2$stem_diameter)),ylim=c(0,1.1*max(temp3$N)))
  mtext("stem diameter (mm)",1,line=1.7,cex=0.7)
  mtext("N concentration",2,line=2.3,cex=0.7)
  
  if (i %in% c(1,5,9,13)) {
    legend("bottomleft",legend=c("sapwood","wood from mid","wood from base"),pch=c(16,10,7),bty="n",cex=0.8)
    legend("topright",legend=labels.age(),col=col.age(),pch=16, bty="n",cex=0.8)
  }
  
}

dev.off()



###fits for nutrient concentration based on code from Drew

tmp <- subset(stem_segments,tissue=="wood_from_tip")
tmp <- subset(tmp,age>1.4)

mean_sapwood <- tmp %>% 
  dplyr::group_by(species) %>%
  dplyr::summarise_each(dplyr::funs(mean),P,N,stem_diameter)

names(mean_sapwood) <- c("species","sapwood_P","sapwood_N","sapwood_diameter")

sapwood_fits <- select(stem_segments,species,age,tissue,)

sapwood_fits <- subset(stem_segments,age>2 &tissue!="wood_from_tip")

sapwood_fits <- merge(sapwood_fits,mean_sapwood, by="species")
sapwood_fits$stem_radius <- sapwood_fits$stem_diameter/2


# hh - hardwood conc as a fraction of sapwood_fits
# xx -sapwood depth
# ans = total conc predicted

data_tmp <- sapwood_fits
data_tmp <- split(data_tmp, data_tmp$species)

sapwood_output_P <- data.frame(matrix(ncol = 5, nrow = 0))
names(sapwood_output_P) <- c("species","logit.h","xx","logit.h t-value","xx t-value")


for(spp in c("BAER", "BOLE", "COER", "EPMI", "GRBU", "HATE", "HEPU", "LEES", "PELA", "PILI")) {
  #function to determine concentration for a single set of values
  concentration <- function(stem_radius,logit.h,xx) {
    sapwood_conc <- mean(data_tmp[[spp]]$sapwood_P)
    hh <- exp(logit.h)/(1+exp(logit.h))
    #xx <- max(stem_radius)*exp(logit.x)/(1+exp(logit.x))
    
    ans <- sapwood_conc*hh*(stem_radius-xx)^2/stem_radius^2 + sapwood_conc*(1-(stem_radius-xx)^2/stem_radius^2)
    ans[stem_radius <= xx] <- sapwood_conc
    
    return(ans)
  }
  
  myfit <- nls(data_tmp[[spp]]$P ~ concentration(data_tmp[[spp]]$stem_radius,logit.h,xx),
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

sapwood_output_P$prop_remaining <- (exp(sapwood_output_P$logit.h)/(1+(exp(sapwood_output_P$logit.h))))

write.csv(sapwood_output_P, file="output/Sapwood_P.csv",row.names=FALSE)



data_tmp <- subset(sapwood_fits,!is.na(sapwood_fits$N))
data_tmp <- split(data_tmp, data_tmp$species)


sapwood_output_N <- data.frame(matrix(ncol = 5, nrow = 0))
names(sapwood_output_N) <- c("species","logit.h","xx","logit.h t-value","xx t-value")

for(spp in c("BAER", "EPMI", "GRBU", "PELA","PEPU","PHPH")) {
  #function to determine concentration for a single set of values
  concentration <- function(stem_radius,logit.h,xx) {
    sapwood_conc <- mean(data_tmp[[spp]]$sapwood_N)
    hh <- exp(logit.h)/(1+exp(logit.h))
    #xx <- max(stem_radius)*exp(logit.x)/(1+exp(logit.x))
    
    ans <- sapwood_conc*hh*(stem_radius-xx)^2/stem_radius^2 + sapwood_conc*(1-(stem_radius-xx)^2/stem_radius^2)
    ans[stem_radius <= xx] <- sapwood_conc
    
    return(ans)
  }
  
  myfit <- nls(data_tmp[[spp]]$N ~ concentration(data_tmp[[spp]]$stem_radius,logit.h,xx),
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

sapwood_output_N$prop_remaining <- (exp(sapwood_output_N$logit.h)/(1+(exp(sapwood_output_N$logit.h))))
write.csv(sapwood_output_N, file="output/Sapwood_N.csv",row.names=FALSE)





data_tmp <- sapwood_fits


sapwood_output_P <- data.frame(matrix(ncol = 5, nrow = 0))
names(sapwood_output_P) <- c("species","logit.h","xx","logit.h t-value","xx t-value")


for(spp in c("BAER", "BOLE", "COER", "EPMI", "GRBU", "HATE", "HEPU", "LEES", "PELA", "PILI")) {
  #function to determine concentration for a single set of values
  concentration <- function(stem_radius,logit.h,xx) {
    sapwood_conc <- mean(data_tmp$sapwood_P)
    hh <- exp(logit.h)/(1+exp(logit.h))
    #xx <- max(stem_radius)*exp(logit.x)/(1+exp(logit.x))
    
    ans <- sapwood_conc*hh*(stem_radius-xx)^2/stem_radius^2 + sapwood_conc*(1-(stem_radius-xx)^2/stem_radius^2)
    ans[stem_radius <= xx] <- sapwood_conc
    
    return(ans)
  }
  
  myfit <- nls(data_tmp$P ~ concentration(data_tmp$stem_radius,logit.h,xx),
               data = data_tmp,
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

sapwood_output_P$prop_remaining <- (exp(sapwood_output_P$logit.h)/(1+(exp(sapwood_output_P$logit.h))))