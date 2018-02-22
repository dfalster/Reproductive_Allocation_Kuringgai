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