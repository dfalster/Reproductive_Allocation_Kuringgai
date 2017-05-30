
win.metafile("ms/RA/maxRA_LMA_correlation.wmf", height=4, width=4)
plot
par(mfcol=c(1,1), cex=1, omi=c(.1,.7,.1,.1), mai=c(.8,.3,.1,0.2)) 
i <- traits$RA_leaf_area_max
plot(i~traits$LMA,col="blue",pch=16,cex=1.5,xlab="",yaxt="n",log="x")
mtext("LMA",1,outer=FALSE,line=3)
mtext("maximum RA",2,outer=FALSE,line=3)
axis(2, at=c(.4,.6,.8,1,1.2,1.4), labels=c(.4,.6,.8,1,1.2,1.4),cex.axis=.8,las=1)
#mod <- lm(LMA~RA_leaf_area,traits)
#extra.bottom.left(paste("r2=",round(glance(mod)[1],2),"; p-value:",round(glance(mod)[5],4)))
dev.off()



###################

win.metafile("ms/RA/life_history_correlations.wmf", height=4, width=12)
plot
par(mfcol=c(1,3), cex=1, omi=c(.1,.7,.1,.1), mai=c(.8,.3,.1,0.2)) 
i <- traits$RA_leaf_area_max

plot(i~traits$lifespan,pch=16,cex=1.5,log="x",xlab="",yaxt="n")
mtext("lifespan (years)",1,outer=FALSE,line=2)
axis(2, at=c(.4,.6,.8,1,1.2,1.4), labels=c(0.4,0.6,0.8,1.0,1.2,1.4),cex.axis=.9,las=1)
# <- lm(log10(lifespan)~i,traits)
#extra.bottom.left.logx(paste("r2=",round(glance(mod)[1],2),"; p-value:",round(glance(mod)[5],4)))
mtext("maximum RA",2,outer=TRUE,line=1)

plot(i~traits$maturity,pch=16,cex=1.5,log="x",xlab="",yaxt="n")
mtext("age at maturity (years)",1,outer=FALSE,line=2)
axis(2, at=c(.4,.6,.8,1,1.2,1.4), labels=c(0.4,0.6,0.8,1.0,1.2,1.4),cex.axis=.9,las=1)
#mod <- lm(mature~log10(i),traits)
#extra.bottom.left.logx(paste("r2=",round(glance(mod)[1],2),"; p-value:",round(glance(mod)[5],4)))

plot(i~traits$maxH,pch=16,cex=1.5,log="x",xlab="",yaxt="n")
mtext("maximum height (mm)",1,outer=FALSE,line=2)
#mod <- lm(log10(maxH)~i,traits)
axis(2, at=c(.4,.6,.8,1,1.2,1.4), labels=c("","","","","",""),cex.axis=.8,las=1)
#extra.bottom.left.logx(paste("r2=",round(glance(mod)[1],2),"; p-value:",round(glance(mod)[5],4)))
dev.off()

##################


win.metafile("ms/RA/stacked_leaf_repro_lines.wmf", height=4, width=12)
par(mfrow=c(5,3), cex=1, omi=c(.1,0.1,.1,.1), mai=c(0.8,.8,.1,0.2)) 

data <- dplyr::select(SummarySppAge[["mean"]],species,age,repro_inv,growth_leaf,leaf_replacement,leaf_shed)


for(v in c("repro_inv","growth_leaf","leaf_replacement","leaf_shed")) {
  i <- is.na(data[[v]])
  data[[v]][i] <- 0
}

for(v in c("growth_leaf")) {
  i <- (data[[v]] < 0)
  data[[v]][i] <- 0
}


data <- split(data, data$species)


for(spp in names(data)) {
  
  
  data[[spp]] <- data[[spp]] %>% mutate(
    total_inv = repro_inv + growth_leaf + leaf_replacement,
    prop_repro = repro_inv / total_inv,
    prop_leaf_expand = (repro_inv + growth_leaf)/total_inv,
    prop_leaf_replacement = (leaf_replacement+repro_inv + growth_leaf)/ total_inv,
    stack1 = repro_inv,
    stack2 = repro_inv+growth_leaf,
    stack3= leaf_replacement+repro_inv + growth_leaf
  )

  plot(prop_repro~age,data[[spp]],pch=16,log="x",ylim=c(0.04,0.96),xlim=c(1.06*1.4,0.94*32),col="white")
  
  if(spp=="PILI") {
    polygon(x=c(1.4,7,7,1.4),y=c(1,1,0,0),col="darkseagreen4",density=NA)
    polygon(x=c(1.4,2.4,5,7,7,1.4),y=c(data[[spp]]$prop_leaf_expand[1],data[[spp]]$prop_leaf_expand[2],data[[spp]]$prop_leaf_expand[3],data[[spp]]$prop_leaf_expand[4],
                                       0,0),col="darkseagreen2",density=NA)
    polygon(x=c(1.4,2.4,5,7,7,1.4),y=c(data[[spp]]$repro_prop_surplus[1],data[[spp]]$repro_prop_surplus[2],data[[spp]]$repro_prop_surplus[3],data[[spp]]$repro_prop_surplus[4],
                                       0,0),col="coral3",density=NA)
    text(5,0.1,"reproductive")
    text(1.4,0.18,"additional leaves",adj=0)
    text(3,0.4,"replacement leaves",adj=0)
    text(2,0.7,"additional stems",adj=0)
  }
  
  if(spp=="PHPH") {
    polygon(x=c(2.4,32,32,2.4),y=c(1,1,0,0),col="darkseagreen4",density=NA)
    polygon(x=c(2.4,5,7,9,32,32,2.4),y=c(data[[spp]]$prop_leaf_expand[1],data[[spp]]$prop_leaf_expand[2],data[[spp]]$prop_leaf_expand[3],data[[spp]]$prop_leaf_expand[4],
                                         data[[spp]]$prop_leaf_expand[5],0,0),col="darkseagreen2",density=NA)
    polygon(x=c(2.4,5,7,9,32,32,2.4),y=c(data[[spp]]$repro_prop_surplus[1],data[[spp]]$repro_prop_surplus[2],data[[spp]]$repro_prop_surplus[3],data[[spp]]$repro_prop_surplus[4],
                                         data[[spp]]$repro_prop_surplus[5],0,0),col="coral3",density=NA)
    text(20,data[[spp]]$repro_prop_surplus[5]*.7,"reproductive")
    text(2.5,data[[spp]]$prop_leaf_expand[1]*.3,"additional leaves",adj=0)
    text(5,data[[spp]]$prop_leaf_replacement[4]*.6,"replacement leaves",adj=0)
    #text(15,0.9,"additional stems",adj=0)
  }
  
  
  if(spp=="BOLE"|spp=="COER"|spp=="HEPU"|spp=="GRSP") {
    polygon(x=c(1.4,9,9,1.4),y=c(1,1,0,0),col="darkseagreen4",density=NA) 
    polygon(x=c(1.4,2.4,5,7,9,9,1.4),y=c(data[[spp]]$prop_leaf_expand[1],data[[spp]]$prop_leaf_expand[2],data[[spp]]$prop_leaf_expand[3],data[[spp]]$prop_leaf_expand[4],
                                         data[[spp]]$prop_leaf_expand[5],0,0),col="darkseagreen2",density=NA)
    polygon(x=c(1.4,2.4,5,7,9,9,1.4),y=c(data[[spp]]$repro_prop_surplus[1],data[[spp]]$repro_prop_surplus[2],data[[spp]]$repro_prop_surplus[3],data[[spp]]$repro_prop_surplus[4],
                                         data[[spp]]$repro_prop_surplus[5],0,0),col="coral3",density=NA)
    text(8,data[[spp]]$repro_prop_surplus[5]*.4,"reproductive",adj=1)
    text(1.5,data[[spp]]$prop_leaf_expand[1]*.5,"additional leaves",adj=0)
    text(4,data[[spp]]$prop_leaf_replacement[3]*.6,"replacement leaves",adj=0)
    #text(6,0.9,"additional stems",adj=1)
  }
  
  if(spp=="BAER"|spp=="EPMI"|spp=="GRBU"|spp=="HATE"|spp=="PELA"|spp=="PUTU"|spp=="LEES"|spp=="PEPU") {
    polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="darkseagreen4",density=NA)
    polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$prop_leaf_expand[1],data[[spp]]$prop_leaf_expand[2],data[[spp]]$prop_leaf_expand[3],data[[spp]]$prop_leaf_expand[4],
                                             data[[spp]]$prop_leaf_expand[5],data[[spp]]$prop_leaf_expand[6],0,0),col="darkseagreen2",density=NA)
    polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$repro_prop_surplus[1],data[[spp]]$repro_prop_surplus[2],data[[spp]]$repro_prop_surplus[3],data[[spp]]$repro_prop_surplus[4],
                                             data[[spp]]$repro_prop_surplus[5],data[[spp]]$repro_prop_surplus[6],0,0),col="coral3",density=NA)
    text(20,data[[spp]]$repro_prop_surplus[6]*.4,"reproductive")
    text(1.5,data[[spp]]$prop_leaf_expand[1]*.3,"additional leaves",adj=0)
    text(5,data[[spp]]$prop_leaf_replacement[4]*.6,"replacement leaves",adj=0)
    #text(15,0.9,"additional stems",adj=0)
  }
  extra.top.left.logx(labels.spp.full(spp),3,1)
  
  if(spp=="BOLE"|spp=="COER"|spp=="HEPU"|spp=="GRSP") {
    polygon(x=c(1.4,2.4,7,9,9,1.4),y=c(data[[spp]]$stack3[1],data[[spp]]$stack3[2],data[[spp]]$stack3[4],
                                       data[[spp]]$stack3[5],0.1,0.1),col="darkseagreen4",density=NA)
    polygon(x=c(1.4,2.4,7,9,9,1.4),y=c(data[[spp]]$stack2[1],data[[spp]]$stack2[2],data[[spp]]$stack2[4],
                                       data[[spp]]$stack2[5],0.1,0.1),col="darkseagreen2",density=NA)
    polygon(x=c(1.4,2.4,7,9,9,1.4),y=c(data[[spp]]$stack1[1],data[[spp]]$stack1[2],data[[spp]]$stack1[4],
                                       data[[spp]]$stack1[5],0.1,0.1),col="coral3",density=NA)
    text(8,data[[spp]]$repro_prop_surplus[5]*.4,"reproductive",adj=1)
    text(1.5,data[[spp]]$prop_leaf_expand[1]*.5,"additional leaves",adj=0)
    text(4,data[[spp]]$prop_leaf_replacement[3]*.6,"replacement leaves",adj=0)
    #text(6,0.9,"additional stems",adj=1)
  }
}
dev.off()

##################


win.metafile("ms/RA/stacked_leaf_repro_lines_v2.wmf", height=12, width=16)
par(mfcol=c(4,4), cex=1, omi=c(.2,.2,.1,.1), mai=c(0.2,.4,.01,0.02)) 

data <- dplyr::select(SummarySppAge[["mean"]],species,age,repro_inv,growth_leaf,leaf_replacement,leaf_shed,all_leaf_and_repro_inv,growth_leaf_pos,repro_prop_surplus,leaf_shed_prop_surplus)

for(v in c("growth_leaf")) {
  i <- (data[[v]] < 0)
  data[[v]][i] <- 0
}


data <- split(data, data$species)

for(spp in c("BOLE","PILI","HEPU","COER","GRBU","GRSP","PHPH","PUTU","EPMI","LEES","PELA","PEPU","HATE","BAER")) {
  
  plot(repro_prop_surplus~age,data[[spp]],pch=16,log="x",ylim=c(0.04,0.96),xlim=c(1.06*1.4,0.94*32),col="white")
  
  if(spp=="PILI") {
    polygon(x=c(1.4,7,7,1.4),y=c(1,1,0,0),col="darkseagreen4",density=NA)
    polygon(x=c(1.4,2.4,5,7,7,1.4),y=c(data[[spp]]$leaf_shed_prop_surplus[1],data[[spp]]$leaf_shed_prop_surplus[2],data[[spp]]$leaf_shed_prop_surplus[3],data[[spp]]$leaf_shed_prop_surplus[4],
                                       0,0),col="darkseagreen2",density=NA)
    polygon(x=c(1.4,2.4,5,7,7,1.4),y=c(data[[spp]]$repro_prop_surplus[1],data[[spp]]$repro_prop_surplus[2],data[[spp]]$repro_prop_surplus[3],data[[spp]]$repro_prop_surplus[4],
                                       0,0),col="coral3",density=NA)
    points(x=c(1.4,2.4,5,7),y=c(data[[spp]]$leaf_shed_prop_surplus[1],data[[spp]]$leaf_shed_prop_surplus[2],data[[spp]]$leaf_shed_prop_surplus[3],data[[spp]]$leaf_shed_prop_surplus[4]),
          type="l",col="black")
    #text(5,0.1,"reproductive")
    #text(1.4,0.18,"additional leaves",adj=0)
    #text(3,0.4,"replacement leaves",adj=0)
    #text(2,0.7,"additional stems",adj=0)
  }
  
  if(spp=="PHPH") {
    polygon(x=c(2.4,32,32,2.4),y=c(1,1,0,0),col="darkseagreen4",density=NA)
    polygon(x=c(2.4,5,7,9,32,32,2.4),y=c(data[[spp]]$leaf_shed_prop_surplus[1],data[[spp]]$leaf_shed_prop_surplus[2],data[[spp]]$leaf_shed_prop_surplus[3],data[[spp]]$leaf_shed_prop_surplus[4],
                                         data[[spp]]$leaf_shed_prop_surplus[5],0,0),col="darkseagreen2",density=NA)
    polygon(x=c(2.4,5,7,9,32,32,2.4),y=c(data[[spp]]$repro_prop_surplus[1],data[[spp]]$repro_prop_surplus[2],data[[spp]]$repro_prop_surplus[3],data[[spp]]$repro_prop_surplus[4],
                                         data[[spp]]$repro_prop_surplus[5],0,0),col="coral3",density=NA)
    points(x=c(2.4,5,7,9,32),y=c(data[[spp]]$leaf_shed_prop_surplus[1],data[[spp]]$leaf_shed_prop_surplus[2],data[[spp]]$leaf_shed_prop_surplus[3],data[[spp]]$leaf_shed_prop_surplus[4],data[[spp]]$leaf_shed_prop_surplus[5]),
           type="l",col="black")
    #text(20,data[[spp]]$repro_prop_surplus[5]*.7,"reproductive")
    #text(2.5,data[[spp]]$leaf_shed_prop_surplus[1]*.3,"additional leaves",adj=0)
    #text(5,data[[spp]]$prop_leaf_replacement[4]*.6,"replacement leaves",adj=0)
    #text(15,0.9,"additional stems",adj=0)
  }
  
  
  if(spp=="BOLE"|spp=="COER"|spp=="HEPU"|spp=="GRSP") {
    polygon(x=c(1.4,9,9,1.4),y=c(1,1,0,0),col="darkseagreen4",density=NA) 
    polygon(x=c(1.4,2.4,5,7,9,9,1.4),y=c(data[[spp]]$leaf_shed_prop_surplus[1],data[[spp]]$leaf_shed_prop_surplus[2],data[[spp]]$leaf_shed_prop_surplus[3],data[[spp]]$leaf_shed_prop_surplus[4],
                                         data[[spp]]$leaf_shed_prop_surplus[5],0,0),col="darkseagreen2",density=NA)
    polygon(x=c(1.4,2.4,5,7,9,9,1.4),y=c(data[[spp]]$repro_prop_surplus[1],data[[spp]]$repro_prop_surplus[2],data[[spp]]$repro_prop_surplus[3],data[[spp]]$repro_prop_surplus[4],
                                         data[[spp]]$repro_prop_surplus[5],0,0),col="coral3",density=NA)
    points(x=c(1.4,2.4,5,7,9),y=c(data[[spp]]$leaf_shed_prop_surplus[1],data[[spp]]$leaf_shed_prop_surplus[2],data[[spp]]$leaf_shed_prop_surplus[3],data[[spp]]$leaf_shed_prop_surplus[4],data[[spp]]$leaf_shed_prop_surplus[5]),
           type="l",col="black")
    text(8,data[[spp]]$repro_prop_surplus[5]*.4,"reproductive",adj=1)
    text(1.5,data[[spp]]$leaf_shed_prop_surplus[1]*.5,"additional leaves",adj=0)
    text(4,data[[spp]]$prop_leaf_replacement[3]*.6,"replacement leaves",adj=0)
    #text(6,0.9,"additional stems",adj=1)
  }
  
  if(spp=="BAER"|spp=="EPMI"|spp=="GRBU"|spp=="HATE"|spp=="PELA"|spp=="PUTU"|spp=="LEES"|spp=="PEPU") {
    polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="darkseagreen4",density=NA)
    polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$leaf_shed_prop_surplus[1],data[[spp]]$leaf_shed_prop_surplus[2],data[[spp]]$leaf_shed_prop_surplus[3],data[[spp]]$leaf_shed_prop_surplus[4],
                                             data[[spp]]$leaf_shed_prop_surplus[5],data[[spp]]$leaf_shed_prop_surplus[6],0,0),col="darkseagreen2",density=NA)
    polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$repro_prop_surplus[1],data[[spp]]$repro_prop_surplus[2],data[[spp]]$repro_prop_surplus[3],data[[spp]]$repro_prop_surplus[4],
                                             data[[spp]]$repro_prop_surplus[5],data[[spp]]$repro_prop_surplus[6],0,0),col="coral3",density=NA)
    points(x=c(1.4,2.4,5,7,9,32),y=c(data[[spp]]$leaf_shed_prop_surplus[1],data[[spp]]$leaf_shed_prop_surplus[2],data[[spp]]$leaf_shed_prop_surplus[3],data[[spp]]$leaf_shed_prop_surplus[4],
                                     data[[spp]]$leaf_shed_prop_surplus[5],data[[spp]]$leaf_shed_prop_surplus[6]),
           type="l",col="black")
    #text(20,data[[spp]]$repro_prop_surplus[6]*.4,"reproductive")
    #text(1.5,data[[spp]]$leaf_shed_prop_surplus[1]*.3,"additional leaves",adj=0)
    #text(5,data[[spp]]$prop_leaf_replacement[4]*.6,"replacement leaves",adj=0)
    #text(15,0.9,"additional stems",adj=0)
  }
  extra.top.left.logx(labels.spp.full(spp),3,1)

}
dev.off()

###############

win.metafile("ms/RA/stacked_leaf_repro_lines_4_species.wmf", height=12, width=4)
par(mfrow=c(4,1), cex=1, omi=c(.2,.2,.1,.1), mai=c(0.2,.2,.01,0.02)) 

data <- dplyr::select(SummarySppAge[["mean"]],species,age,repro_inv,growth_leaf,leaf_replacement,leaf_shed,all_leaf_and_repro_inv,growth_leaf_pos,repro_prop_surplus,leaf_shed_prop_surplus)

for(v in c("growth_leaf")) {
  i <- (data[[v]] < 0)
  data[[v]][i] <- 0
}


data <- split(data, data$species)


for(spp in c("BOLE","GRBU","EPMI","BAER")) {
  
    plot(repro_prop_surplus~age,data[[spp]],pch=16,log="x",ylim=c(0.04,0.96),xlim=c(1.06*1.4,0.94*32),col="white")
  

  if(spp=="BOLE") {
    polygon(x=c(1.4,9,9,1.4),y=c(1,1,0,0),col="darkseagreen4",density=NA) 
    polygon(x=c(1.4,2.4,5,7,9,9,1.4),y=c(data[[spp]]$leaf_shed_prop_surplus[1],data[[spp]]$leaf_shed_prop_surplus[2],data[[spp]]$leaf_shed_prop_surplus[3],data[[spp]]$leaf_shed_prop_surplus[4],
                                         data[[spp]]$leaf_shed_prop_surplus[5],0,0),col="darkseagreen2",density=NA)
    polygon(x=c(1.4,2.4,5,7,9,9,1.4),y=c(data[[spp]]$repro_prop_surplus[1],data[[spp]]$repro_prop_surplus[2],data[[spp]]$repro_prop_surplus[3],data[[spp]]$repro_prop_surplus[4],
                                         data[[spp]]$repro_prop_surplus[5],0,0),col="coral3",density=NA)
    points(x=c(1.4,2.4,5,7,9),y=c(data[[spp]]$leaf_shed_prop_surplus[1],data[[spp]]$leaf_shed_prop_surplus[2],data[[spp]]$leaf_shed_prop_surplus[3],data[[spp]]$leaf_shed_prop_surplus[4],data[[spp]]$leaf_shed_prop_surplus[5]),
           type="l",col="black")
    text(8,data[[spp]]$repro_prop_surplus[5]*.4,"reproductive",adj=1)
    text(1.5,data[[spp]]$leaf_shed_prop_surplus[1]*.5,"additional leaves",adj=0)
    text(4,data[[spp]]$prop_leaf_replacement[3]*.6,"replacement leaves",adj=0)
    #text(6,0.9,"additional stems",adj=1)
  }
  
  if(spp=="BAER"|spp=="EPMI"|spp=="GRBU") {
    polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="darkseagreen4",density=NA)
    polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$leaf_shed_prop_surplus[1],data[[spp]]$leaf_shed_prop_surplus[2],data[[spp]]$leaf_shed_prop_surplus[3],data[[spp]]$leaf_shed_prop_surplus[4],
                                             data[[spp]]$leaf_shed_prop_surplus[5],data[[spp]]$leaf_shed_prop_surplus[6],0,0),col="darkseagreen2",density=NA)
    polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$repro_prop_surplus[1],data[[spp]]$repro_prop_surplus[2],data[[spp]]$repro_prop_surplus[3],data[[spp]]$repro_prop_surplus[4],
                                             data[[spp]]$repro_prop_surplus[5],data[[spp]]$repro_prop_surplus[6],0,0),col="coral3",density=NA)
    points(x=c(1.4,2.4,5,7,9,32),y=c(data[[spp]]$leaf_shed_prop_surplus[1],data[[spp]]$leaf_shed_prop_surplus[2],data[[spp]]$leaf_shed_prop_surplus[3],data[[spp]]$leaf_shed_prop_surplus[4],
                                     data[[spp]]$leaf_shed_prop_surplus[5],data[[spp]]$leaf_shed_prop_surplus[6]),
           type="l",col="black")
    #text(20,data[[spp]]$repro_prop_surplus[6]*.4,"reproductive")
    #text(1.5,data[[spp]]$leaf_shed_prop_surplus[1]*.3,"additional leaves",adj=0)
    #text(5,data[[spp]]$prop_leaf_replacement[4]*.6,"replacement leaves",adj=0)
    #text(15,0.9,"additional stems",adj=0)
  }
  extra.top.left.logx(labels.spp.full(spp),3,1)
  
}
dev.off()

###################################

# ----------------------------------------------------------

  groups <- list(
    Gradual = c("GRBU", "EPMI", "LEES"),
    Gradual = c("BOLE", "PILI", "GRSP"),
    Asymptotic = c("PHPH", "HEPU", "PEPU"),
    Increasing = c("BAER", "HATE", "PELA"))
  #  excluded <- c("COER", "PUTU")
  
  data1 <- split(SummaryInd, SummaryInd$species)
  data2 <- split(SummarySppAge[["mean"]], SummarySppAge[["mean"]]$species)
  
  win.metafile("ms/RA/RA_schedules.wmf", height=12, width=12)
  par(mfcol=c(3,4), cex=1, omi=c(.2,.2,.1,.1), mai=c(0.2,.2,.01,0.02))  
  for(j in seq_len(4)) {
    for(i in 1:3) {
      spp <- groups[[j]][i]
      y <- data1[[spp]][["RA_max_1"]]
      x <- data1[[spp]][["age"]]
      y2 <- data2[[spp]][["RA_max_1"]]
      x2 <- data2[[spp]][["age"]]
      if(j <= n ){
        plot(NA, log="x",xlim=c(1,33),ylim=c(0,1.2),yaxs="i", xaxs="i")
        abline(h=1, col="lightgrey", lty="dashed")
        if (j==2){
          polygon(c(15, 35, 35, 15), c(1.2, 1.2, 0, 0), col="lightgrey", border=NA)
        }
        
        points(x,y, col="black", pch=16, cex=1.2)
        points(x2,y2, col="red", lwd=2.5,type="l")
        if(i==1) {
          mtext(names(groups)[j], side=3, line =1)
        }
      } else {
        plot(NA, log="x",xlim=c(1,33),ylim=c(0,1.2), axes=FALSE)
      }
    }
  }

  mtext("Reproductive allocation", 2, outer=TRUE, line=2)
  mtext("Age (years)", 1, outer=TRUE, line=2)

dev.off()

###########################################

win.metafile("ms/RA/4_RA_calculation.wmf", height=4, width=16)
par(mfcol=c(1,4), cex=1, omi=c(.2,.2,.1,.1), mai=c(0.2,.4,.01,0.02)) 

data <- SummarySppAge[["mean"]]

for(v in c("growth_leaf")) {
  i <- (data[[v]] < 0)
  data[[v]][i] <- 0
}

data <- split(data, data$species)

for(spp in c("EPMI")) {
  plot(RA_seed~age,data[[spp]],pch=16,log="x",ylim=c(0.04,0.96),xlim=c(1.06*1.4,0.94*32),col="white")
  polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="black",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$RA_seed[1],data[[spp]]$RA_seed[2],data[[spp]]$RA_seed[3],data[[spp]]$RA_seed[4],
                                           data[[spp]]$RA_seed[5],data[[spp]]$RA_seed[6],0,0),col="coral3",density=NA) 
  
  plot(prop_repro~age,data[[spp]],pch=16,log="x",ylim=c(0.04,0.96),xlim=c(1.06*1.4,0.94*32),col="white")
  polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="black",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$prop_repro[1],data[[spp]]$prop_repro[2],data[[spp]]$prop_repro[3],data[[spp]]$prop_repro[4],data[[spp]]$prop_repro[5],data[[spp]]$prop_repro[6],0,0),col="coral3",density=NA)
  
  
  plot(repro_prop_surplus~age,data[[spp]],pch=16,log="x",ylim=c(0.04,0.96),xlim=c(1.06*1.4,0.94*32),col="white")
  polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="black",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$repro_prop_surplus[1],data[[spp]]$repro_prop_surplus[2],data[[spp]]$repro_prop_surplus[3],data[[spp]]$repro_prop_surplus[4],
                                           data[[spp]]$repro_prop_surplus[5],data[[spp]]$repro_prop_surplus[6],0,0),col="coral3",density=NA)
  
  plot(prop_repro~age,data[[spp]],pch=16,log="x",ylim=c(0.04,0.96),xlim=c(1.06*1.4,0.94*32),col="white")
  polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="black",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$RA_max_1[1],data[[spp]]$RA_max_1[2],data[[spp]]$RA_max_1[3],data[[spp]]$RA_max_1[4],
                                           data[[spp]]$RA_max_1[5],data[[spp]]$RA_max_1[6],0,0),col="coral3",density=NA)
    }
dev.off()

######################
win.metafile("ms/RA/4_RA_calculation_colors.wmf", height=4, width=16)
par(mfcol=c(1,4), cex=1, omi=c(.2,.2,.1,.1), mai=c(0.2,.4,.01,0.02)) 

data <- SummarySppAge[["mean"]]

for(v in c("growth_leaf")) {
  i <- (data[[v]] < 0)
  data[[v]][i] <- 0
}

data <- split(data, data$species)

for(spp in c("EPMI")) {
  
  plot(RA_seed~age,data[[spp]],pch=16,log="x",ylim=c(0.04,0.96),xlim=c(1.06*1.4,0.94*32),col="white")
  polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="saddlebrown",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$prop_leaf_replacement[1],data[[spp]]$prop_leaf_replacement[2],data[[spp]]$prop_leaf_replacement[3],data[[spp]]$prop_leaf_replacement[4],
                                           data[[spp]]$prop_leaf_replacement[5],data[[spp]]$prop_leaf_replacement[6],0,0),col="darkseagreen4",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$prop_leaf_expand[1],data[[spp]]$prop_leaf_expand[2],data[[spp]]$prop_leaf_expand[3],data[[spp]]$prop_leaf_expand[4],
                                           data[[spp]]$prop_leaf_expand[5],data[[spp]]$prop_leaf_expand[6],0,0),col="darkseagreen2",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$prop_repro[1],data[[spp]]$prop_repro[2],data[[spp]]$prop_repro[3],data[[spp]]$prop_repro[4],data[[spp]]$prop_repro[5],data[[spp]]$prop_repro[6],0,0),col="coral3",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$RA_seed[1],data[[spp]]$RA_seed[2],data[[spp]]$RA_seed[3],data[[spp]]$RA_seed[4],
                                           data[[spp]]$RA_seed[5],data[[spp]]$RA_seed[6],0,0),col="mistyrose",density=NA) 
  
  plot(prop_repro~age,data[[spp]],pch=16,log="x",ylim=c(0.04,0.96),xlim=c(1.06*1.4,0.94*32),col="white")
  polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="saddlebrown",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$prop_leaf_replacement[1],data[[spp]]$prop_leaf_replacement[2],data[[spp]]$prop_leaf_replacement[3],data[[spp]]$prop_leaf_replacement[4],
                                           data[[spp]]$prop_leaf_replacement[5],data[[spp]]$prop_leaf_replacement[6],0,0),col="darkseagreen4",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$prop_leaf_expand[1],data[[spp]]$prop_leaf_expand[2],data[[spp]]$prop_leaf_expand[3],data[[spp]]$prop_leaf_expand[4],
                                           data[[spp]]$prop_leaf_expand[5],data[[spp]]$prop_leaf_expand[6],0,0),col="darkseagreen2",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$prop_repro[1],data[[spp]]$prop_repro[2],data[[spp]]$prop_repro[3],data[[spp]]$prop_repro[4],data[[spp]]$prop_repro[5],data[[spp]]$prop_repro[6],0,0),col="coral3",density=NA)
  
  
  plot(repro_prop_surplus~age,data[[spp]],pch=16,log="x",ylim=c(0.04,0.96),xlim=c(1.06*1.4,0.94*32),col="white")
  polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="darkseagreen4",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$repro_and_leaf_growth_prop_surplus[1],data[[spp]]$repro_and_leaf_growth_prop_surplus[2],data[[spp]]$repro_and_leaf_growth_prop_surplus[3],
                                           data[[spp]]$repro_and_leaf_growth_prop_surplus[4],data[[spp]]$repro_and_leaf_growth_prop_surplus[5],data[[spp]]$repro_and_leaf_growth_prop_surplus[6],
                                          0,0),col="darkseagreen2",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$repro_prop_surplus[1],data[[spp]]$repro_prop_surplus[2],data[[spp]]$repro_prop_surplus[3],data[[spp]]$repro_prop_surplus[4],
                                           data[[spp]]$repro_prop_surplus[5],data[[spp]]$repro_prop_surplus[6],0,0),col="coral3",density=NA)
  
  plot(prop_repro~age,data[[spp]],pch=16,log="x",ylim=c(0.04,0.96),xlim=c(1.06*1.4,0.94*32),col="white")
  polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="darkseagreen2",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$RA_max_1[1],data[[spp]]$RA_max_1[2],data[[spp]]$RA_max_1[3],data[[spp]]$RA_max_1[4],
                                           data[[spp]]$RA_max_1[5],data[[spp]]$RA_max_1[6],0,0),col="coral3",density=NA)
}
dev.off()

######################


win.metafile("ms/RA/2_tone_RA_boxes.wmf", height=12, width=16)
par(mfcol=c(4,4), cex=1, omi=c(.2,.2,.1,.1), mai=c(0.2,.4,.01,0.02)) 

data <- dplyr::select(SummarySppAge[["mean"]],species,age,RA_max_1,repro_inv,growth_leaf,leaf_replacement,leaf_shed,all_leaf_and_repro_inv,growth_leaf_pos,repro_prop_surplus,leaf_shed_prop_surplus)

for(v in c("growth_leaf")) {
  i <- (data[[v]] < 0)
  data[[v]][i] <- 0
}


data <- split(data, data$species)

for(spp in c("BOLE","PILI","HEPU","COER","GRBU","GRSP","PHPH","PUTU","EPMI","LEES","PELA","PEPU","HATE","BAER")) {
  
  plot(RA_max_1~age,data[[spp]],pch=16,log="x",ylim=c(0.04,0.96),xlim=c(1.06*1.4,0.94*32),col="white")
  
  if(spp=="PILI") {
    polygon(x=c(1.4,7,7,1.4),y=c(1,1,0,0),col="darkseagreen2",density=NA)
    polygon(x=c(1.4,2.4,5,7,7,1.4),y=c(data[[spp]]$RA_max_1[1],data[[spp]]$RA_max_1[2],data[[spp]]$RA_max_1[3],data[[spp]]$RA_max_1[4],
                                       0,0),col="coral3",density=NA)
  }
  
  if(spp=="PHPH") {
    polygon(x=c(2.4,32,32,2.4),y=c(1,1,0,0),col="darkseagreen2",density=NA)
    polygon(x=c(2.4,5,7,9,32,32,2.4),y=c(data[[spp]]$RA_max_1[1],data[[spp]]$RA_max_1[2],data[[spp]]$RA_max_1[3],data[[spp]]$RA_max_1[4],
                                         data[[spp]]$RA_max_1[5],0,0),col="coral3",density=NA)
  }
  
  
  if(spp=="BOLE"|spp=="COER"|spp=="HEPU"|spp=="GRSP") {
    polygon(x=c(1.4,9,9,1.4),y=c(1,1,0,0),col="darkseagreen2",density=NA) 
    polygon(x=c(1.4,2.4,5,7,9,9,1.4),y=c(data[[spp]]$RA_max_1[1],data[[spp]]$RA_max_1[2],data[[spp]]$RA_max_1[3],data[[spp]]$RA_max_1[4],
                                         data[[spp]]$RA_max_1[5],0,0),col="coral3",density=NA)
  }
  
  if(spp=="BAER"|spp=="EPMI"|spp=="GRBU"|spp=="HATE"|spp=="PELA"|spp=="PUTU"|spp=="LEES"|spp=="PEPU") {
    polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="darkseagreen2",density=NA)
    polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$RA_max_1[1],data[[spp]]$RA_max_1[2],data[[spp]]$RA_max_1[3],data[[spp]]$RA_max_1[4],
                                             data[[spp]]$RA_max_1[5],data[[spp]]$RA_max_1[6],0,0),col="coral3",density=NA)
  }
  #extra.top.left.logx(labels.spp.full(spp),3,1)
  
}
dev.off()

############################

win.metafile("ms/RA/growth_vs_size.wmf", height=12, width=16)
par(mfcol=c(4,4), cex=1, omi=c(.2,.2,.1,.1), mai=c(0.2,.4,.01,0.02)) 

data <- SummaryInd

data <- subset(data,!(data$individual %in% c("COER_806","EPMI_907","GRBU_906","HATE_105","HATE_003","LEES_354",
                                          "LEES_352","LEES_355","LEES_353","LEES_351","PELA_161","PELA_162",
                                          "PUTU_108")))

data <- split(data, data$species)

for(spp in c("BOLE","PILI","HEPU","COER","GRBU","GRSP","PHPH","PUTU","EPMI","LEES","PELA","PEPU","HATE","BAER")) {

  y_max <-max(max(data[[spp]]$growth_leaf_pos),max(data[[spp]]$leaf_replacement),max(data[[spp]]$repro_inv))
  plot(leaf_replacement~height,data[[spp]],pch=16,log="xy",col="black",cex=1.5,ylim=c(1,(2*y_max)))  
  points(leaf_replacement~height,data[[spp]],pch=16,log="xy",col="darkseagreen4")
  points(growth_leaf_pos~height,data[[spp]],pch=16,log="xy",col="black",cex=1.5)
  points(growth_leaf_pos~height,data[[spp]],pch=16,log="xy",col="darkseagreen2")
  points(repro_inv~height,data[[spp]],pch=16,col="black",cex=1.5)
  points(repro_inv~height,data[[spp]],pch=16,col="coral3")
  
}  

dev.off()

###############
