ypoly <- function(data, n=6) {c(data[seq_len(n)],0,0)}


pdf("output/RA percent N.pdf", height=8, width=11)


par(mfrow=c(3,5), cex=1, omi=c(1,1.5,.1,0.1), mai=c(0.2,.45,.3,0.02))

data <- RA_Nutrient

data <- split(data, data$species)

for(spp in c("BOLE", "GRSP", "PILI", "HEPU", "EPMI", "GRBU", "LEES", "PUTU", "COER", "HATE", "PHPH", "BAER", "PEPU", "PELA")) {
  
  x <- c(1.4,2.4,5,7,9,32,32,1.4)
  if(spp %in% c("PILI")) x <- c(1.4,2.4,5,7,7,1.4)
  if(spp %in% c("PHPH"))  x <- c(2.4,5,7,9,32,32,2.4)
  if(spp %in% c("BOLE", "COER", "HEPU", "GRSP"))  x <- c(1.4,2.4,5,7,9,9,1.4)
  n <- length(x)-2
  y0 <- c(rep(1,n), 0, 0)
  
  plot(NA,log="x",ylim=c(0,1),xlim=c(1.06*1.4,0.94*32),ylab="",yaxt="n", xaxt="n",xaxs="i", yaxs="i")
  
  if (spp %in% c("HATE", "PHPH", "BAER", "PEPU", "PELA")) {
    axis(1, at= c(2,5, 10, 20), las=1)
  }
    
  if (spp %in% c("BOLE","GRBU","PHPH")) {
    axis(2, at= c(0, 0.2, 0.4, 0.6, 0.8, 1.0), las=1)
  }
    
  polygon2(c(1.4,32,32,1.4), c(1,1,0, 0), col="grey90")
  polygon2(x, y0, col="darkseagreen2")
  polygon2(x, ypoly(data[[spp]]$RA_N, n),col="coral3")
  
  extra.top.left.logx(spp)
  
  box()
}

mtext("Age (yr)",side=1,outer=TRUE,line=2, cex=2)
mtext("RA as %N",side=2,outer=TRUE,line=4, cex=2)

dev.off()

pdf("output/RA carbon.pdf", height=8, width=11)


par(mfrow=c(3,5), cex=1, omi=c(1,1.5,.1,0.1), mai=c(0.2,.45,.3,0.02))

data <- RA_Nutrient

data <- split(data, data$species)

for(spp in c("BOLE", "GRSP", "PILI", "HEPU", "EPMI", "GRBU", "LEES", "PUTU", "COER", "HATE", "PHPH", "BAER", "PEPU", "PELA")) {
  
  x <- c(1.4,2.4,5,7,9,32,32,1.4)
  if(spp %in% c("PILI")) x <- c(1.4,2.4,5,7,7,1.4)
  if(spp %in% c("PHPH"))  x <- c(2.4,5,7,9,32,32,2.4)
  if(spp %in% c("BOLE", "COER", "HEPU", "GRSP"))  x <- c(1.4,2.4,5,7,9,9,1.4)
  n <- length(x)-2
  y0 <- c(rep(1,n), 0, 0)
  
  plot(NA,log="x",ylim=c(0,1),xlim=c(1.06*1.4,0.94*32),ylab="",yaxt="n", xaxt="n",xaxs="i", yaxs="i")
  
  if (spp %in% c("HATE", "PHPH", "BAER", "PEPU", "PELA")) {
    axis(1, at= c(2,5, 10, 20), las=1)
  }
  
  if (spp %in% c("BOLE","GRBU","PHPH")) {
    axis(2, at= c(0, 0.2, 0.4, 0.6, 0.8, 1.0), las=1)
  }
  
  polygon2(c(1.4,32,32,1.4), c(1,1,0, 0), col="grey90")
  polygon2(x, y0, col="darkseagreen2")
  polygon2(x, ypoly(data[[spp]]$RA_max_1, n),col="coral3")
  
  extra.top.left.logx(spp)
  
  box()
}

mtext("Age (yr)",side=1,outer=TRUE,line=2, cex=2)
mtext("RA on dry mass basis",side=2,outer=TRUE,line=4, cex=2)

dev.off()

pdf("output/RA percent P.pdf", height=8, width=11)


par(mfrow=c(3,5), cex=1, omi=c(1,1.5,.1,0.1), mai=c(0.2,.45,.3,0.02))

data <- RA_Nutrient

data <- split(data, data$species)

for(spp in c("BOLE", "GRSP", "PILI", "HEPU", "EPMI", "GRBU", "LEES", "PUTU", "COER", "HATE", "PHPH", "BAER", "PEPU", "PELA")) {
  
  x <- c(1.4,2.4,5,7,9,32,32,1.4)
  if(spp %in% c("PILI")) x <- c(1.4,2.4,5,7,7,1.4)
  if(spp %in% c("PHPH"))  x <- c(2.4,5,7,9,32,32,2.4)
  if(spp %in% c("BOLE", "COER", "HEPU", "GRSP"))  x <- c(1.4,2.4,5,7,9,9,1.4)
  n <- length(x)-2
  y0 <- c(rep(1,n), 0, 0)
  
  plot(NA,log="x",ylim=c(0,1),xlim=c(1.06*1.4,0.94*32),ylab="",yaxt="n", xaxt="n",xaxs="i", yaxs="i")
  
  if (spp %in% c("HATE", "PHPH", "BAER", "PEPU", "PELA")) {
    axis(1, at= c(2,5, 10, 20), las=1)
  }
  
  if (spp %in% c("BOLE","GRBU","PHPH")) {
    axis(2, at= c(0, 0.2, 0.4, 0.6, 0.8, 1.0), las=1)
  }
  
  polygon2(c(1.4,32,32,1.4), c(1,1,0, 0), col="grey90")
  polygon2(x, y0, col="darkseagreen2")
  polygon2(x, ypoly(data[[spp]]$RA_P, n),col="coral3")
  
  extra.top.left.logx(spp)
  
  box()
}

mtext("Age (yr)",side=1,outer=TRUE,line=2, cex=2)
mtext("RA as %P",side=2,outer=TRUE,line=4, cex=2)

dev.off()



pdf("output/RA comparison.pdf", height=8, width=11)


par(mfrow=c(3,5), cex=1, omi=c(1,1.5,.1,0.1), mai=c(0.2,.45,.3,0.02))

data <- RA_Nutrient

data <- subset(RA_Nutrient,spp %in% c("BOLE","BAER","HATE","EPMI","PUTU"))

data <- split(data, data$species)

for(spp in c("BOLE", "EPMI", "PUTU", "HATE", "BAER")) {
  
  x <- c(1.4,2.4,5,7,9,32,32,1.4)
  if(spp %in% c("BOLE"))  x <- c(1.4,2.4,5,7,9,9,1.4)
  n <- length(x)-2
  y0 <- c(rep(1,n), 0, 0)
  
  plot(NA,log="x",ylim=c(0,1),xlim=c(1.06*1.4,0.94*32),ylab="",yaxt="n", xaxt="n",xaxs="i", yaxs="i")

  polygon2(c(1.4,32,32,1.4), c(1,1,0, 0), col="grey90")
  polygon2(x, y0, col="darkseagreen2")
  polygon2(x, ypoly(data[[spp]]$RA_max_1, n),col="coral3")
  
  extra.top.left.logx(spp)
  if (spp %in% c("BOLE")) {
    axis(2, at= c(0, 0.2, 0.4, 0.6, 0.8, 1.0), las=1)
  }
  
  box()
}

for(spp in c("BOLE", "EPMI", "PUTU", "HATE", "BAER")) {
  
  x <- c(1.4,2.4,5,7,9,32,32,1.4)
  if(spp %in% c("BOLE"))  x <- c(1.4,2.4,5,7,9,9,1.4)
  n <- length(x)-2
  y0 <- c(rep(1,n), 0, 0)
  
  plot(NA,log="x",ylim=c(0,1),xlim=c(1.06*1.4,0.94*32),ylab="",yaxt="n", xaxt="n",xaxs="i", yaxs="i")
  
  if (spp %in% c("BOLE")) {
    axis(2, at= c(0, 0.2, 0.4, 0.6, 0.8, 1.0), las=1)
  }
  
  polygon2(c(1.4,32,32,1.4), c(1,1,0, 0), col="grey90")
  polygon2(x, y0, col="darkseagreen2")
  polygon2(x, ypoly(data[[spp]]$RA_N, n),col="coral3")
  
  extra.top.left.logx(spp)
  
  box()
}

for(spp in c("BOLE", "EPMI", "PUTU", "HATE", "BAER")) {
  
  x <- c(1.4,2.4,5,7,9,32,32,1.4)
  if(spp %in% c("BOLE"))  x <- c(1.4,2.4,5,7,9,9,1.4)
  n <- length(x)-2
  y0 <- c(rep(1,n), 0, 0)
  
  plot(NA,log="x",ylim=c(0,1),xlim=c(1.06*1.4,0.94*32),ylab="",yaxt="n", xaxt="n",xaxs="i", yaxs="i")
  

  axis(1, at= c(2,5, 10, 20), las=1)
  if (spp %in% c("BOLE")) {
    axis(2, at= c(0, 0.2, 0.4, 0.6, 0.8, 1.0), las=1)
  }

  mtext("RA as %P     RA as %N    RA carbon",side=2,outer=TRUE,line=4, cex=2)
  
  polygon2(c(1.4,32,32,1.4), c(1,1,0, 0), col="grey90")
  polygon2(x, y0, col="darkseagreen2")
  polygon2(x, ypoly(data[[spp]]$RA_P, n),col="coral3")
  
  extra.top.left.logx(spp)
  
  box()
}
mtext("Age (yr)",side=1,outer=TRUE,line=2, cex=2)

dev.off()

pdf("output/stem segment by diameter.pdf", height=11, width=8)
plot

par(mfrow = c(4,2), cex = 1, omi=c(.1,.1,.6,.05), mai=c(1,1,.05,.05))

stem_segments_select <- subset(stem_segments,age>2|tissue_complete=="wood_from_tip")

for (i in seq_along(all_species)) {
  
  data_sp <- filter(stem_segments_select, species == all_species[i])
  temp2 <- subset(data_sp,diameter>0)
  temp3 <- subset(data_sp,N>0)
  
  plot(P~diameter,data=data_sp,log="",col=col.age(data_sp$age),pch=pch.tissue(data_sp$tissue_complete),xlab="",ylab="",
       xlim=c(0,1.1*max(temp2$diameter)),ylim=c(0,1.1*max(temp2$P)))
  mod <- sma(P~diameter,data=data_sp,log="")
  mtext("stem diameter (mm)",1,line=1.7,cex=0.7)
  mtext("P concentration",2,line=2.3,cex=0.7)
  mtext(labels.spp.full(all_species[i]), 3, 0)
  plot(N~diameter,data=data_sp,log="",col=col.age(data_sp$age),pch=pch.tissue(data_sp$tissue_complete),xlab="",ylab="",
       xlim=c(0,1.1*max(temp2$diameter)),ylim=c(0,1.1*max(temp3$N)))
  mtext("stem diameter (mm)",1,line=1.7,cex=0.7)
  mtext("N concentration",2,line=2.3,cex=0.7)
  
  if (i %in% c(1,5,9,13)) {
    legend("bottomleft",legend=c("sapwood","wood from mid","wood from base"),pch=c(16,10,7),bty="n",cex=0.8)
    legend("topright",legend=labels.age(),col=col.age(),pch=16, bty="n",cex=0.8)
  }
  
}

dev.off()


pdf("output/stem segment by diameter categorical.pdf", height=11, width=8)
plot

par(mfrow = c(4,2), cex = 1, omi=c(.1,.1,.6,.05), mai=c(1,1,.05,.05))

stem_segments_select <- subset(stem_segments,age>2|tissue_complete=="wood_from_tip")

for (i in seq_along(all_species)) {
  
  data_sp <- filter(stem_segments_select, species == all_species[i])
  temp2 <- subset(data_sp,diameter>0)
  temp3 <- subset(data_sp,N>0)
  
  plot(P~category,data=data_sp,log="",col=col.age(data_sp$age),pch=pch.tissue(data_sp$tissue_complete),xlab="",ylab="",
       xlim=c(0.5,3.5),ylim=c(0,1.1*max(temp2$P)),xaxt="n")
  axis(1, at= c(1,2,3), labels = c("wood from tip","wood from middle","wood from base"), las=1,cex.axis=0.5)
  #mod <- sma(P~diameter,data=data_sp,log="")
  mtext("stem type",1,line=1.7,cex=0.7)
  mtext("P concentration",2,line=2.3,cex=0.7)
  mtext(labels.spp.full(all_species[i]), 3, 0)
  plot(N~category,data=data_sp,log="",col=col.age(data_sp$age),pch=pch.tissue(data_sp$tissue_complete),xlab="",ylab="",
       xlim=c(0.5,3.5),ylim=c(0,1.1*max(temp3$N)),xaxt="n")
  axis(1, at= c(1,2,3), labels = c("wood from tip","wood from middle","wood from base"), las=1,cex.axis=0.5)
  mtext("stem type",1,line=1.7,cex=0.7)
  mtext("N concentration",2,line=2.3,cex=0.7)
  
  if (i %in% c(1,5,9,13)) {
    legend("bottomleft",legend=c("sapwood","wood from mid","wood from base"),pch=c(16,10,7),bty="n",cex=0.8)
    legend("topright",legend=labels.age(),col=col.age(),pch=16, bty="n",cex=0.8)
  }
  
}

dev.off()


pdf("output/repro_parts.pdf", height=11, width=8)
plot

col.parts <- function(x = NULL) {
  
  ret <- c(bud = "deep pink", petals = "red", flower_bract = "firebrick1", 
           flower_stigma = "firebrick3", finished_flower="orange", fruit_seed_immature = "gold1", calyx="light green", bract="grey", 
           green_parts="darkgreen", fruit_mature = "cyan2", seed_pod = "blue", seed = "purple", cone="brown")
  
  if (!is.null(x)) {
    ret <- ret[as.character(x)]
  }
  ret
}


SppReproMean <- 
  SummaryInvest %>%
  dplyr::group_by(species) %>%
  dplyr::summarise_at(c("weight","repro_P","repro_N"),sum)

SppReproMean$overall_P <- SppReproMean$repro_P / SppReproMean$weight
SppReproMean$overall_N <- SppReproMean$repro_N / SppReproMean$weight

par(mfrow=c(4,2), cex=1, omi=c(1,1.5,.1,0.1), mai=c(0.2,.45,.3,0.02))

data <- SummaryInvest

data <- split(data, data$species)
data2 <- split(SppReproMean, SppReproMean$species) 

for(spp in c("BOLE", "GRSP", "PILI", "HEPU", "EPMI", "GRBU", "LEES", "PUTU", "COER", "HATE", "PHPH", "BAER", "PEPU", "PELA")) {
  plot(N~P,data[[spp]],pch=16,col=col.parts(data[[spp]]$parts_to_match),cex=data[[spp]]$weight*6/max(data[[spp]]$weight),log="xy")
  extra.top.left.logxy(spp)
  points(overall_N~overall_P,data2[[spp]],pch=3,col="black",cex=3)
  points(overall_N~overall_P,data2[[spp]],pch=1,col="black",cex=1.5)
  if(spp %in% c("LEES", "PUTU", "PEPU", "PELA")) {
    mtext("P (ppm)",side=1,outer=FALSE,line=2, cex=0.8)
  }
  if(spp %in% c("BOLE", "PILI", "EPMI", "LEES", "COER", "PHPH", "PEPU")) {
    mtext("N (%)",side=2,outer=FALSE,line=2, cex=0.8)
  }
}


plot(0,0,cex=0.1,xaxt="n",yaxt="n")
legend("topleft",legend=c("bud", "petals", "flower_bract","flower_stigma", "finished_flower", "fruit_seed_immature","calyx","bract", 
                          "green_parts", "fruit_mature", "seed_pod","seed","cone"),col=col.parts(),pch=16, bty="n",cex=0.8)
dev.off()