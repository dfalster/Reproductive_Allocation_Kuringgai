traits <- SummarySpp[["mean"]]
traits$maxH <- SummarySpp[["max"]]$height

temp <- SummarySppAge[["mean"]]
temp <- temp %>%
  group_by(species) %>%
  summarise_each(funs(max), RA_leaf_area,RA_max_1,RA_vs_all_leaf)
names(temp) <- c("species","RA_leaf_area_max","RA_max_1_max","RA_vs_all_leaf_max")

traits <- merge(traits,temp,by="species")


win.metafile("ms/RA/maxRA_LMA_correlation.wmf", height=4, width=12)
plot
par(mfcol=c(1,3), cex=1, omi=c(.1,.7,.1,.1), mai=c(.8,.3,.1,0.2)) 

plot(RA_vs_all_leaf~LMA,traits,col="coral4",pch=16,cex=1.5,xlab="",yaxt="n",log="xy")
mtext("LMA",1,outer=FALSE,line=3)
mtext("maximum RA",2,outer=FALSE,line=3)
mtext("(prop inv in reproduction and all leaves)",2,outer=FALSE,line=2)
axis(2, at=c(0,0.1,0.2,.4,.6,.8,1,1.2,1.4), labels=c(0,0.1,0.2,0.4,.6,.8,1,1.2,1.4),cex.axis=.8,las=1)
mod <- sma(RA_vs_all_leaf~LMA,traits,log="xy")
summary(mod)


plot(RA_vs_all_leaf~wood_density,traits,col="coral4",pch=16,cex=1.5,xlab="",yaxt="n",log="xy")
mtext("wood density",1,outer=FALSE,line=3)
axis(2, at=c(.4,.6,.8,1,1.2,1.4), labels=c(.4,.6,.8,1,1.2,1.4),cex.axis=.8,las=1)
mod <- sma(RA_vs_all_leaf~wood_density,traits,log="xy")
summary(mod)

plot(RA_vs_all_leaf~seed_size,traits,col="coral4",pch=16,cex=1.5,xlab="",yaxt="n",log="xy")
mtext("seed size (mg)",1,outer=FALSE,line=3)
axis(2, at=c(.4,.6,.8,1,1.2,1.4), labels=c(.4,.6,.8,1,1.2,1.4),cex.axis=.8,las=1)
mod <- sma(RA_vs_all_leaf~seed_size,traits,log="xy")

dev.off()

###################

win.metafile("ms/RA/life_history_correlations.wmf", height=4, width=12)
plot
par(mfcol=c(1,3), cex=1, omi=c(.1,.7,.1,.1), mai=c(.8,.3,.1,0.2)) 
i <- traits$RA_vs_all_leaf

plot(i~traits$lifespan,pch=16,cex=1.5,log="x",xlab="",yaxt="n",col="coral4")
mtext("lifespan (years)",1,outer=FALSE,line=2)
axis(2, at=c(.4,.6,.8,1,1.2,1.4), labels=c(0.4,0.6,0.8,1.0,1.2,1.4),cex.axis=.9,las=1)
# <- lm(log10(lifespan)~i,traits)
#extra.bottom.left.logx(paste("r2=",round(glance(mod)[1],2),"; p-value:",round(glance(mod)[5],4)))
mtext("maximum RA",2,outer=TRUE,line=1)

plot(i~traits$maturity,pch=16,cex=1.5,log="x",xlab="",yaxt="n",col="coral4")
mtext("age at maturity (years)",1,outer=FALSE,line=2)
axis(2, at=c(.4,.6,.8,1,1.2,1.4), labels=c(0.4,0.6,0.8,1.0,1.2,1.4),cex.axis=.9,las=1)
#mod <- lm(mature~log10(i),traits)
#extra.bottom.left.logx(paste("r2=",round(glance(mod)[1],2),"; p-value:",round(glance(mod)[5],4)))

plot(i~traits$maxH,pch=16,cex=1.5,log="x",xlab="",yaxt="n",col="coral4")
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
par(mfrow=c(4,4), cex=1, omi=c(.2,.2,.1,.1), mai=c(0.2,.4,.01,0.02)) 

data <- dplyr::select(SummarySppAge[["mean"]],species,age,repro_inv,growth_leaf,leaf_replacement,leaf_shed,all_leaf_and_repro_inv,growth_leaf_pos,repro_prop_surplus,leaf_shed_prop_surplus)

for(v in c("growth_leaf")) {
  i <- (data[[v]] < 0)
  data[[v]][i] <- 0
}


data <- split(data, data$species)

for(spp in c("BOLE","PILI","HEPU","COER","GRBU","GRSP","GRSP","GRSP","EPMI","LEES","PHPH","PUTU","HATE","BAER","PELA","PEPU")) {
  
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
    #text(8,data[[spp]]$repro_prop_surplus[5]*.4,"reproductive",adj=1)
    #text(1.5,data[[spp]]$leaf_shed_prop_surplus[1]*.5,"additional leaves",adj=0)
    #text(4,data[[spp]]$prop_leaf_replacement[3]*.6,"replacement leaves",adj=0)
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
  plot(seed_prop_veg~age,data[[spp]],pch=16,log="x",ylim=c(0.04,0.96),xlim=c(1.06*1.4,0.94*32),col="white")
  polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="gray67",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$seed_prop_veg[1],data[[spp]]$seed_prop_veg[2],data[[spp]]$seed_prop_veg[3],data[[spp]]$seed_prop_veg[4],
                                           data[[spp]]$seed_prop_veg[5],data[[spp]]$seed_prop_veg[6],0,0),col="coral3",density=NA) 
  
  plot(prop_repro~age,data[[spp]],pch=16,log="x",ylim=c(0.04,0.96),xlim=c(1.06*1.4,0.94*32),col="white")
  polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="gray67",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$prop_repro[1],data[[spp]]$prop_repro[2],data[[spp]]$prop_repro[3],data[[spp]]$prop_repro[4],data[[spp]]$prop_repro[5],data[[spp]]$prop_repro[6],0,0),col="coral3",density=NA)
  
  
  plot(repro_prop_surplus~age,data[[spp]],pch=16,log="x",ylim=c(0.04,0.96),xlim=c(1.06*1.4,0.94*32),col="white")
  polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="gray67",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$repro_prop_surplus[1],data[[spp]]$repro_prop_surplus[2],data[[spp]]$repro_prop_surplus[3],data[[spp]]$repro_prop_surplus[4],
                                           data[[spp]]$repro_prop_surplus[5],data[[spp]]$repro_prop_surplus[6],0,0),col="coral3",density=NA)
  
  plot(prop_repro~age,data[[spp]],pch=16,log="x",ylim=c(0.04,0.96),xlim=c(1.06*1.4,0.94*32),col="white")
  polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="gray67",density=NA)
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
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$seed_prop_veg[1],data[[spp]]$seed_prop_veg[2],data[[spp]]$seed_prop_veg[3],data[[spp]]$seed_prop_veg[4],
                                           data[[spp]]$seed_prop_veg[5],data[[spp]]$seed_prop_veg[6],0,0),col="coral3",density=NA) 
  
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
par(mfrow=c(4,4), cex=1, omi=c(.2,.2,.1,.1), mai=c(0.2,.4,.01,0.02)) 

data <- dplyr::select(SummarySppAge[["mean"]],species,age,RA_max_1,repro_inv,growth_leaf,leaf_replacement,leaf_shed,all_leaf_and_repro_inv,growth_leaf_pos,repro_prop_surplus,leaf_shed_prop_surplus)

for(v in c("growth_leaf")) {
  i <- (data[[v]] < 0)
  data[[v]][i] <- 0
}


data <- split(data, data$species)

for(spp in c("BOLE","PILI","HEPU","COER","GRBU","GRSP","GRSP","GRSP","EPMI","LEES","PHPH","PUTU","HATE","BAER","PELA","PEPU")) {
  
  plot(RA_max_1~age,data[[spp]],pch=16,log="x",ylim=c(0.04,0.96),xlim=c(1.06*1.4,0.94*32),col="white",yaxt="n",xaxt="n")
  
  if(spp=="BOLE"|spp=="EPMI"|spp=="GRBU"|spp=="HATE") {
    axis(2, at=c(0,0.2,.4,.6,.8,1,1.2,1.4), labels=c(0,0.2,0.4,.6,.8,1,1.2,1.4),cex.axis=1.2,las=1)
  }

  if(spp=="BAER"|spp=="PEPU"|spp=="PELA"|spp=="HATE") {
    axis(1, at=c(2,5,10,20), labels=c(2,5,10,20),cex.axis=1.2,las=1)
  }
  
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
win.metafile("ms/RA/2_tone_RA_boxes.wmf", height=12, width=16)
par(mfcol=c(4,4), cex=1, omi=c(.2,.2,.1,.1), mai=c(0.2,.4,.01,0.02)) 

data <- dplyr::select(SummarySppAge[["mean"]],species,age,RA_max_1,repro_inv,growth_leaf,prop_leaf_loss,leaf_replacement,leaf_shed,all_leaf_and_repro_inv,growth_leaf_pos,repro_prop_surplus,leaf_shed_prop_surplus)

data <- split(data, data$species)

for(spp in c("BOLE","PILI","HEPU","COER","GRBU","GRSP","PHPH","PUTU","EPMI","LEES","PELA","PEPU","HATE","BAER")) {
  
  plot(RA_max_1~age,data[[spp]],pch=16,log="x",ylim=c(0.04,0.96),xlim=c(1.06*1.4,0.94*32),col="white")

  if(spp=="PILI") {
    polygon(x=c(1.4,7,7,1.4),y=c(1,1,0,0),col="darkseagreen2",density=NA)
    polygon(x=c(1.4,2.4,5,7,7,1.4),y=c(data[[spp]]$RA_max_1[1],data[[spp]]$RA_max_1[2],data[[spp]]$RA_max_1[3],data[[spp]]$RA_max_1[4],
                                       0,0),col="coral3",density=NA)
    points(x=c(1.4,2.4,5,7),y=c(data[[spp]]$prop_leaf_loss[1],data[[spp]]$prop_leaf_loss[2],data[[spp]]$prop_leaf_loss[3],
                                data[[spp]]$prop_leaf_loss[4]),type="l",col="black")
  }
  
  if(spp=="PHPH") {
    polygon(x=c(2.4,32,32,2.4),y=c(1,1,0,0),col="darkseagreen2",density=NA)
    polygon(x=c(2.4,5,7,9,32,32,2.4),y=c(data[[spp]]$RA_max_1[1],data[[spp]]$RA_max_1[2],data[[spp]]$RA_max_1[3],data[[spp]]$RA_max_1[4],
                                         data[[spp]]$RA_max_1[5],0,0),col="coral3",density=NA)
    points(x=c(2.4,5,7,9,32),y=c(data[[spp]]$prop_leaf_loss[1],data[[spp]]$prop_leaf_loss[2],data[[spp]]$prop_leaf_loss[3],
                                data[[spp]]$prop_leaf_loss[4],data[[spp]]$prop_leaf_loss[5]),type="l",col="black")
  }
  
  
  if(spp=="BOLE"|spp=="COER"|spp=="HEPU"|spp=="GRSP") {
    polygon(x=c(1.4,9,9,1.4),y=c(1,1,0,0),col="darkseagreen2",density=NA) 
    polygon(x=c(1.4,2.4,5,7,9,9,1.4),y=c(data[[spp]]$RA_max_1[1],data[[spp]]$RA_max_1[2],data[[spp]]$RA_max_1[3],data[[spp]]$RA_max_1[4],
                                         data[[spp]]$RA_max_1[5],0,0),col="coral3",density=NA)
    points(x=c(1.4,2.4,5,7,9),y=c(data[[spp]]$prop_leaf_loss[1],data[[spp]]$prop_leaf_loss[2],data[[spp]]$prop_leaf_loss[3],
                                 data[[spp]]$prop_leaf_loss[4],data[[spp]]$prop_leaf_loss[5]),type="l",col="black")
  }
  
  if(spp=="BAER"|spp=="EPMI"|spp=="GRBU"|spp=="HATE"|spp=="PELA"|spp=="PUTU"|spp=="LEES"|spp=="PEPU") {
    polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="darkseagreen2",density=NA)
    polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$RA_max_1[1],data[[spp]]$RA_max_1[2],data[[spp]]$RA_max_1[3],data[[spp]]$RA_max_1[4],
                                             data[[spp]]$RA_max_1[5],data[[spp]]$RA_max_1[6],0,0),col="coral3",density=NA)
    points(x=c(1.4,2.4,5,7,9,32),y=c(data[[spp]]$prop_leaf_loss[1],data[[spp]]$prop_leaf_loss[2],data[[spp]]$prop_leaf_loss[3],
                                 data[[spp]]$prop_leaf_loss[4],data[[spp]]$prop_leaf_loss[5],data[[spp]]$prop_leaf_loss[6]),type="l",col="black")
  }
  #extra.top.left.logx(labels.spp.full(spp),3,1)
  
}
dev.off()


#plot(prop_leaf_loss~age,SummarySppAge[["mean"]])
############################

win.metafile("ms/RA/growth_vs_size.wmf", height=12, width=16)
par(mfrow=c(4,3), cex=1, omi=c(.2,.2,.1,.1), mai=c(0.2,.4,.01,0.02)) 

data <- SummaryInd

data <- subset(data,!(data$individual %in% c("COER_806","EPMI_907","GRBU_906","HATE_105","HATE_003","LEES_354",
                                          "LEES_352","LEES_355","LEES_353","LEES_351","PELA_161","PELA_162",
                                          "PUTU_108")))

data <- split(data, data$species)

for(spp in c("BOLE","GRBU","EPMI","HATE")) {

  #y_max <-max(max(data[[spp]]$growth_leaf_pos),max(data[[spp]]$leaf_replacement),max(data[[spp]]$repro_inv)) ylim=c(1,(2*y_max))
  plot(diameter~age,data[[spp]],pch=16,log="xy",col="saddlebrown",cex=1.5)  
  plot(height~age,data[[spp]],pch=16,log="xy",col="saddlebrown",cex=1.5)
  plot(leaf_area~age,data[[spp]],pch=16,log="xy",col="darkseagreen4",cex=1.5)
}  

dev.off()
############################

win.metafile("ms/RA/size_vs_age.wmf", height=12, width=16)
par(mfcol=c(4,4), cex=1, omi=c(.2,.2,.1,.1), mai=c(0.2,.4,.01,0.02)) 

data <- SummaryInd

data <- subset(data,!(data$individual %in% c("COER_806","EPMI_907","GRBU_906","HATE_105","HATE_003","LEES_354",
                                             "LEES_352","LEES_355","LEES_353","LEES_351","PELA_161","PELA_162",
                                             "PUTU_108")))

data <- split(data, data$species)

for(spp in c("BOLE","PILI","HEPU","COER","GRBU","GRSP","PHPH","PUTU","EPMI","LEES","PELA","PEPU","HATE","BAER")) {
  
  y_max <-max(max(data[[spp]]$growth_leaf_pos),max(data[[spp]]$leaf_replacement),max(data[[spp]]$repro_inv))
  plot(leaf_replacement~diameter,data[[spp]],pch=16,log="xy",col="black",cex=1.5,ylim=c(1,(2*y_max)))  
  points(leaf_replacement~diameter,data[[spp]],pch=16,log="xy",col="darkseagreen4")
  points(growth_leaf_pos~diameter,data[[spp]],pch=16,log="xy",col="black",cex=1.5)
  points(growth_leaf_pos~diameter,data[[spp]],pch=16,log="xy",col="darkseagreen2")
  points(repro_inv~diameter,data[[spp]],pch=16,col="black",cex=1.5)
  points(repro_inv~diameter,data[[spp]],pch=16,col="coral3")
  
}  

dev.off()
###############
data <- SummaryInd

data <- subset(data,!(data$individual %in% c("COER_806","EPMI_907","GRBU_906","HATE_105","HATE_003","LEES_354",
                                             "LEES_352","LEES_355","LEES_353","LEES_351","PELA_161","PELA_162",
                                             "PUTU_108")))

# data$leaf_inv_log <- 0
# for (i in 1:length(data$individual)) {
#   if (data$growth_leaf[i] > 0) {
#     data$leaf_inv_log[i] <- log10(data$growth_leaf[i])
#   }else{
#     data$leaf_inv_log[i] <- -(log10(-data$growth_leaf[i]))  
#   }
# }

data$leaf_shed_log <- 0
for (i in 1:length(data$individual)) {
  if (data$leaf_shed[i] > 0) {
    data$leaf_shed_log[i] <- log10(data$leaf_shed[i])
  }else{
    data$leaf_shed_log[i] <- 0 
  }
}

data$leaf_expansion_log <- 0
for (i in 1:length(data$individual)) {
  if (data$growth_leaf[i] > 0) {
    data$leaf_expansion_log[i] <- log10(data$growth_leaf[i])
  }else{
    data$leaf_expansion_log[i] <- 0  
  }
}

data$leaf_replacement_log <- 0
for (i in 1:length(data$individual)) {
  if (data$leaf_replacement > 0) {
    data$leaf_replacement_log[i] <- log10(data$leaf_replacement[i])
  } else {
    data$leaf_replacement_log[i] <- -(log10(-data$leaf_replacement[i]))  
  }
}

data$repro_inv_log <- 0
for (i in 1:length(data$individual)) {
  if (data$repro_inv[i] > 0) {
    data$repro_inv_log[i] <- log10(data$repro_inv[i])
  }else{
    data$repro_inv_log[i] <- 0  
  }
}

win.metafile("ms/RA/Figure_investment.wmf", height=12, width=12)
plot
#pdf("figs2/RA/Figure_4a_investment.pdf", height=11, width=8)
par(mfrow=c(4,4), cex=1, omi=c(.3,.7,.1,.1), mai=c(0.5,.15,0.05,0.05))

data <- subset(data)
data <- split(data, data$species)

for(spp in c("BOLE","PILI","HEPU","COER","GRBU","GRSP","GRSP","GRSP","EPMI","LEES","PHPH","PUTU","HATE","BAER","PELA","PEPU")) {
  
  y_max <-max(max(data[[spp]]$leaf_expansion_log),max(data[[spp]]$repro_inv_log),max(data[[spp]]$leaf_shed_log))
  x_max <-max(data[[spp]]$total_weight_0) 
  x_min <-min(data[[spp]]$total_weight_0)
  plot(leaf_replacement_log~total_weight_0,data[[spp]], log="x",xlab="",ylab="",cex=0.7,ylim=c(-1,(1.1*y_max)),xlim=c((0.5*x_min),(2*x_max)),
                    col="darkseagreen4",cex.main=0.9, yaxt="n",pch=16)

  points(leaf_expansion_log~total_weight_0,data[[spp]],pch=16,cex=1,col="darkseagreen2")  
  points(repro_inv_log~total_weight_0,data[[spp]],pch=16,cex=1,col="coral3")
  points(leaf_expansion_log~total_weight_0,data[[spp]],pch=16,cex=0.5,col="darkseagreen2")  
  
  for (i in seq_along(data[[spp]]$individual)) {
    if (data[[spp]]$leaf_shed_log[i] > data[[spp]]$leaf_replacement_log[i]) {
      arrows(data[[spp]]$total_weight_0[i],data[[spp]]$leaf_replacement_log[i],data[[spp]]$total_weight_0[i],data[[spp]]$leaf_shed_log[i],
             length=0,col="darkseagreen4")
    } else {}
  }

  #points(leaf_shed_log~total_weight_0,data[[spp]],pch=1,cex=1,col="darkseagreen4")
  
  extra.top.left.logx(labels.spp.full(spp),3,1)

  abline(0,0)
  axis(2, at=c(-4,-2,0,1,2,3,4,5), labels=c("","","","","","","",""),cex.axis=.8,las=1,tck=-.03)
  
  # if(spp=="BOLE"|spp=="GRBU"|spp=="EPMI"|spp=="HATE"){
  #   axis(2, at=c(-4,-2,0,1,2,3,4,5), labels=c(-10000,-100,0,10,100,1000,10000,100000),cex.axis=.8,las=1)
  # } else {}
}

mtext("investment (mg)", 2, outer=TRUE,cex=1,line=2.5)
mtext("plant weight (mg)",1,outer=TRUE,cex=1,line=-.3)


dev.off()

###################
win.metafile("ms/RA/Figure_surplus_investment.wmf", height=12, width=12)
plot
#pdf("figs2/RA/Figure_4a_investment.pdf", height=11, width=8)
par(mfrow=c(4,4), cex=1, omi=c(.3,.7,.1,.1), mai=c(0.5,.15,0.05,0.05))

data <- subset(data)
data <- split(data, data$species)

for(spp in c("BOLE","PILI","HEPU","COER","GRBU","GRSP","GRSP","GRSP","EPMI","LEES","PHPH","PUTU","HATE","BAER","PELA","PEPU")) {
  
  y_max <-max(max(data[[spp]]$leaf_expansion_log),max(data[[spp]]$repro_inv_log),max(data[[spp]]$leaf_shed_log))
  x_max <-max(data[[spp]]$total_weight_0) 
  x_min <-min(data[[spp]]$total_weight_0)
  plot(leaf_expansion_log~total_weight_0,data[[spp]], log="x",xlab="",ylab="",cex=1,ylim=c(-1,(1.1*y_max)),xlim=c((0.5*x_min),(2*x_max)),
       col="darkseagreen2",cex.main=0.9, yaxt="n",pch=16)
  
  #points(leaf_replacement_log~total_weight_0,data[[spp]],pch=16,cex=0.7,col="darkseagreen4")  
  points(repro_inv_log~total_weight_0,data[[spp]],pch=16,cex=1,col="coral3")
  points(leaf_expansion_log~total_weight_0,data[[spp]],pch=16,cex=0.5,col="darkseagreen2")  
  
 # for (i in seq_along(data[[spp]]$individual)) {
  #  if (data[[spp]]$leaf_shed_log[i] > data[[spp]]$leaf_replacement_log[i]) {
 #     arrows(data[[spp]]$total_weight_0[i],data[[spp]]$leaf_replacement_log[i],data[[spp]]$total_weight_0[i],data[[spp]]$leaf_shed_log[i],
 #            length=0,col="darkseagreen4")
 #   } else {}
# }
  
  #points(leaf_shed_log~total_weight_0,data[[spp]],pch=1,cex=1,col="darkseagreen4")
  
  extra.top.left.logx(labels.spp.full(spp),3,1)
  
  abline(0,0)
  axis(2, at=c(-4,-2,0,1,2,3,4,5), labels=c("","","","","","","",""),cex.axis=.8,las=1,tck=-.03)
  
  # if(spp=="BOLE"|spp=="GRBU"|spp=="EPMI"|spp=="HATE"){
  #   axis(2, at=c(-4,-2,0,1,2,3,4,5), labels=c(-10000,-100,0,10,100,1000,10000,100000),cex.axis=.8,las=1)
  # } else {}
}

mtext("investment (mg)", 2, outer=TRUE,cex=1,line=2.5)
mtext("plant weight (mg)",1,outer=TRUE,cex=1,line=-.3)


dev.off()

###################

win.metafile("ms/RA/leaf_weight_by_stem_weight.wmf", height=12, width=12)
par(mfrow=c(4,4), cex=1, omi=c(.4,.4,.1,.1), mai=c(0.4,.4,.1,0.1)) 

data <- SummaryInd

data <- subset(data,!(data$individual %in% c("COER_806","EPMI_907","GRBU_906","HATE_105","HATE_003","LEES_354",
                                             "LEES_352","LEES_355","LEES_353","LEES_351","PELA_161","PELA_162",
                                             "PUTU_108")))

data <- split(data, data$species)

yvar <- "age"

for(spp in c("BOLE","PILI","HEPU","COER","GRBU","GRSP","XXXX","XXXX","EPMI","LEES","PHPH","PUTU","HATE","BAER","PELA","PEPU")) {

  if(spp == "XXXX") {
    plot(1,1, type="n", axes=FALSE, ann=FALSE)
  } else {
    y_max <-max(data[[spp]]$leaf_weight_0)
    y_min <-min(data[[spp]]$leaf_weight_0)
    x_max <-max(data[[spp]][[yvar]])
    x_min <-min(data[[spp]][[yvar]])
    plot(data[[spp]][["leaf_weight_0"]]~data[[spp]][[yvar]],pch=16,log="xy",col=col.age2(data[[spp]]$age),cex=1.5,ylim=c((0.5*y_min),(2*y_max)),xlim=c((0.5*x_min),(2*x_max)))
    extra.top.left.logxy(labels.spp.full(spp),3,1)
    }
}  

mtext("Age (yr)",outer=TRUE,1,line=0.25)
mtext("leaf weight (mg)",outer=TRUE,2,line=0.25)

dev.off()
**************
  
win.metafile("ms/RA/Figure_RA_schedules_new.wmf", height=12, width=12)
plot
par(mfrow=c(4,4), cex=1, omi=c(.7,.7,.1,.1), mai=c(0.15,.15,0.05,0.05)) 

data <- subset(SummarySppAge[["mean"]])
data <- split(data, data$species)

data2 <- subset(SummaryInd)
data2 <- split(data2, data2$species)

curves <- as.data.frame(c("determinate: gradual","determinate: gradual","indeterminate: asymptotic","indeterminate: asymptotic","determinate: bang-bang","determinate: bang-bang","indeterminate: gradual","determinate: bang-bang","determinate: gradual","determinate: gradual","indeterminate: declining","determinate: gradual","determinate: bang-bang","determinate: gradual"))

curves <- cbind(curves, SummarySpp[["mean"]]$species)
names(curves) <- c("RA_type","species")

data3 <- curves
data3 <- split(data3, data3$species)

for(spp in c("BOLE","PILI","HEPU","COER","GRBU","GRSP","GRSP","GRSP","EPMI","LEES","PHPH","PUTU","HATE","BAER","PELA","PEPU")) {
  plot(RA_vs_all_leaf~age,data2[[spp]],col=col.age2(data2[[spp]]$age),pch=16,xlab="",ylab="",log="x",xlim=c(1,40),ylim=c(-0.25,1.25),yaxt="n",xaxt="n")
  points(RA_vs_all_leaf~age,data[[spp]],pch=151,col=col.age2(data[[spp]]$age),cex=1.5)
  points(RA_leaf_area~age,data2[[spp]],pch=1,col=col.age2(data2[[spp]]$age),cex=1.25)
  if(spp=="BOLE"|spp=="COER") {
    rect(14,par("usr")[3],10^par("usr")[2],par("usr")[4],col="light grey",density=NA)
    rect(10^par("usr")[1],par("usr")[3],10^par("usr")[2],par("usr")[4],col="black",density=0)
  }
  if(spp=="GRSP"|spp=="HEPU") {
    rect(20,par("usr")[3],10^par("usr")[2],par("usr")[4],col="light grey",density=NA)
    rect(10^par("usr")[1],par("usr")[3],10^par("usr")[2],par("usr")[4],col="black",density=0)
  }
  if(spp=="PILI") {
    rect(10,par("usr")[3],10^par("usr")[2],par("usr")[4],col="light grey",density=NA)
    rect(10^par("usr")[1],par("usr")[3],10^par("usr")[2],par("usr")[4],col="black",density=0)
  }
  abline(0,0)
  abline(1,0,lty=3)
  axis(1, at=c(1,2,5,10,20,30), labels=c("","","","","",""),cex.axis=.8,las=1,tck=-.03) 
  axis(2, at=c(-0,0.5,1,1.5), labels=c("","","",""),cex.axis=.8,las=1,tck=-.03)
  
  if(spp=="PEPU"|spp=="PELA"|spp=="BAER"|spp=="HATE"){
    axis(1, at=c(1,2,5,10,20,30), labels=c(1,2,5,10,20,30),cex.axis=.8,las=1) 
  } else{}
  if(spp=="HATE"|spp=="BOLE"|spp=="EPMI"|spp=="GRBU"){
    axis(2, at=c(-0,0.5,1,1.5), labels=c(-0,0.5,1,1.5),cex.axis=.8,las=1)
  } else{}
  extra.top.left.logx(labels.spp.full(spp),3,1)
  extra.bottom.left.logx(data3[[spp]]$RA_type,1,0.8)
}

mtext("RA", 2, outer=TRUE,cex=1,line=2)
mtext("age",1, outer=TRUE,cex=1,line=1.8)

dev.off()

################################

win.metafile("ms/RA/repro_inv_vs_RA.wmf", height=6, width=12)
plot
par(mfcol=c(1,2), cex=1, omi=c(.5,.7,.1,.1), mai=c(.8,.8,.1,0.2)) 

plot(RA_vs_all_leaf~scaled_seed_count,SummarySpp[["mean"]],pch=16,cex=1.5,log="x",col="coral4",ylab="",xlab="seed count")
plot(RA_vs_all_leaf~scaled_repro_inv,SummarySpp[["mean"]],pch=16,cex=1.5,log="x",col="coral4",ylab="",xlab="reproductive investment")
#axis(2, at=c(.4,.6,.8,1,1.2,1.4), labels=c(0.4,0.6,0.8,1.0,1.2,1.4),cex.axis=.9,las=1)
# <- lm(log10(lifespan)~i,traits)
#extra.bottom.left.logx(paste("r2=",round(glance(mod)[1],2),"; p-value:",round(glance(mod)[5],4)))
mtext("maximum RA",2,outer=TRUE,line=1)
mtext("scaled to plant size",1,outer=TRUE,line=1)
dev.off()