######################
#####  FIGURE 2  #####
######################

extra.top.left.logx <- function(words, fontx, cexx) {
  text(10^((0.06 * (par("usr")[2] - par("usr")[1])) + par("usr")[1]), 
       ((0.95 * (par("usr")[4] - par("usr")[3])) + par("usr")[3]), words, adj = 0, cex = cexx,
       font = fontx)
}

win.metafile("ms/RA/Figure_2_4_RA_calculation_colors.wmf", height=4, width=16)
par(mfcol=c(1,4), cex=1, omi=c(.5,.6,.1,.1), mai=c(0.2,.25,.01,0.02)) 

data <- SummarySppAge[["mean"]]

for(v in c("growth_leaf")) {
  i <- (data[[v]] < 0)
  data[[v]][i] <- 0
}

data <- split(data, data$species)

for(spp in c("EPMI")) {
  
  plot(RA_seed~age,data[[spp]],pch=16,log="x",ylim=c(0.04,0.96),xlim=c(1.06*1.4,0.94*32),col="white",las=1,xaxs="i")
  polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="saddlebrown",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$replace_leaf_prop_no_accessory[1],data[[spp]]$replace_leaf_prop_no_accessory[2],
                                           data[[spp]]$replace_leaf_prop_no_accessory[3],data[[spp]]$replace_leaf_prop_no_accessory[4],
                                           data[[spp]]$replace_leaf_prop_no_accessory[5],data[[spp]]$replace_leaf_prop_no_accessory[6],0,0),col="darkseagreen4",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$growth_leaf_prop_no_accessory[1],data[[spp]]$growth_leaf_prop_no_accessory[2],
                                           data[[spp]]$growth_leaf_prop_no_accessory[3],data[[spp]]$growth_leaf_prop_no_accessory[4],
                                           data[[spp]]$growth_leaf_prop_no_accessory[5],data[[spp]]$growth_leaf_prop_no_accessory[6],0,0),
          col="darkseagreen2",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$seed_prop_all[1],data[[spp]]$seed_prop_all[2],
                                           data[[spp]]$seed_prop_all[3],data[[spp]]$seed_prop_all[4],
                                           data[[spp]]$seed_prop_all[5],data[[spp]]$seed_prop_all[6],0,0),col="coral1",density=NA) 
  extra.top.left.logx(words="a",fontx=2,cexx=1.3)
  
  plot(prop_repro~age,data[[spp]],pch=16,log="x",ylim=c(0.04,0.96),xlim=c(1.06*1.4,0.94*32),col="white",ylab="",yaxt="n",xaxs="i")
  polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="saddlebrown",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$prop_leaf_replacement[1],data[[spp]]$prop_leaf_replacement[2],data[[spp]]$prop_leaf_replacement[3],data[[spp]]$prop_leaf_replacement[4],
                                           data[[spp]]$prop_leaf_replacement[5],data[[spp]]$prop_leaf_replacement[6],0,0),col="darkseagreen4",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$prop_leaf_expand[1],data[[spp]]$prop_leaf_expand[2],data[[spp]]$prop_leaf_expand[3],data[[spp]]$prop_leaf_expand[4],
                                           data[[spp]]$prop_leaf_expand[5],data[[spp]]$prop_leaf_expand[6],0,0),col="darkseagreen2",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$prop_repro[1],data[[spp]]$prop_repro[2],data[[spp]]$prop_repro[3],data[[spp]]$prop_repro[4],data[[spp]]$prop_repro[5],data[[spp]]$prop_repro[6],0,0),col="coral3",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$seed_prop_all_acc[1],data[[spp]]$seed_prop_all_acc[2],
                                           data[[spp]]$seed_prop_all_acc[3],data[[spp]]$seed_prop_all_acc[4],
                                           data[[spp]]$seed_prop_all_acc[5],data[[spp]]$seed_prop_all_acc[6],0,0),col="coral1",density=NA) 
  extra.top.left.logx(words="b",fontx=2,cexx=1.3)
  
  plot(repro_prop_all_leaf~age,data[[spp]],pch=16,log="x",ylim=c(0.04,0.96),xlim=c(1.06*1.4,0.94*32),col="white",ylab="",yaxt="n",xaxs="i")
  polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="darkseagreen4",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$repro_and_leaf_growth_prop_surplus[1],data[[spp]]$repro_and_leaf_growth_prop_surplus[2],data[[spp]]$repro_and_leaf_growth_prop_surplus[3],
                                           data[[spp]]$repro_and_leaf_growth_prop_surplus[4],data[[spp]]$repro_and_leaf_growth_prop_surplus[5],data[[spp]]$repro_and_leaf_growth_prop_surplus[6],
                                           0,0),col="darkseagreen2",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$repro_prop_all_leaf[1],data[[spp]]$repro_prop_all_leaf[2],data[[spp]]$repro_prop_all_leaf[3],data[[spp]]$repro_prop_all_leaf[4],
                                           data[[spp]]$repro_prop_all_leaf[5],data[[spp]]$repro_prop_all_leaf[6],0,0),col="coral3",density=NA)
  
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$seed_prop_veg[1],data[[spp]]$seed_prop_veg[2],data[[spp]]$seed_prop_veg[3],data[[spp]]$seed_prop_veg[4],
                                           data[[spp]]$seed_prop_veg[5],data[[spp]]$seed_prop_veg[6],0,0),col="coral1",density=NA)
  extra.top.left.logx(words="c",fontx=2,cexx=1.3)
  
  plot(prop_repro~age,data[[spp]],pch=16,log="x",ylim=c(0.04,0.96),xlim=c(1.06*1.4,0.94*32),col="white",ylab="",yaxt="n",xaxs="i")
  polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="darkseagreen2",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$RA_max_1[1],data[[spp]]$RA_max_1[2],data[[spp]]$RA_max_1[3],data[[spp]]$RA_max_1[4],
                                           data[[spp]]$RA_max_1[5],data[[spp]]$RA_max_1[6],0,0),col="coral3",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$seed_prop_surplus[1],data[[spp]]$seed_prop_surplus[2],
                                           data[[spp]]$seed_prop_surplus[3],data[[spp]]$seed_prop_surplus[4],
                                           data[[spp]]$seed_prop_surplus[5],data[[spp]]$seed_prop_surplus[6],0,0),col="coral1",density=NA)
  extra.top.left.logx(words="d",fontx=2,cexx=1.3)
  
  mtext("age (yr)",side=1,outer=TRUE,line=1)
  mtext("RA (0-1)",side=2,outer=TRUE,line=1.75)
}
dev.off()

######################
#####  FIGURE 3  #####
######################
win.metafile("ms/RA/Figure_3_accessory_costs.wmf", height=4, width=8)

barplot_data <- SummarySpp[["mean"]]
barplot_data <- select(barplot_data,species,prop_embryo_endo_vs_all_repro,prop_pollen_attract_vs_all_repro,prop_pack_disp_vs_all_repro,
                       prop_prepollen_discarded_vs_all_repro,prop_postpollen_discarded_vs_all_repro)

barplot_data <- as.matrix(barplot_data[,2:6])
barplot_data <- t(barplot_data)
names(barplot_data) <-c("BAER", "BOLE", "COER", "EPMI", "GRBU", "GRSP", "HATE", "HEPU","LEES", "PELA", "PEPU", "PHPH", "PILI", "PUTU")
#barplot_data_matrix <- as.matrix(barplot_data[2:15])

par(mfcol=c(1,1), cex=1, omi=c(.3,.6,.01,.01), mai=c(.4,.15,.3,0.1)) 
barplot(barplot_data,beside=FALSE,width=0.8,cex.axis=1,cex.names=1,xlim=c(.5,17.2),
        col=c("black", "darkseagreen3","darkseagreen4","darkslategray2","darkslategray4"),
        ylab="proportion repro investment")
legend(13.5,1,legend=c("failed packaging &","dispersal inv","failed","pollen-attraction inv",
                       "successful","packaging &","dispersal inv","successful","pollen-attraction inv","seed investment"),
       pch=16, cex=0.8,bty="n",col=c("darkslategray4","white","darkslategray2","white","darkseagreen4","white","white","darkseagreen2","white","black"))
mtext(side=2,text="proportion reproductive investment (0-1)",line=2.5)
axis(1,labels=c("BAER", "BOLE", "COER", "EPMI", "GRBU", "GRSP", "HATE", "HEPU","LEES", "PELA", "PEPU", "PHPH", "PILI", "PUTU"),
     at=seq(0.5,13,(12.5/13)),tck=FALSE,line=-1.25,lwd=0,cex.axis=0.5)

dev.off()




######################
#####  FIGURE 4  #####
######################

win.metafile("ms/RA/Figure_4_RA_max1_boxes.wmf", height=12, width=16)
par(mfrow=c(4,4), cex=1, omi=c(.5,.6,.2,.1), mai=c(0.2,.2,.01,0.01)) 

data <- SummarySppAge[["mean"]]
data2 <- SummaryInd

for(v in c("growth_leaf")) {
  i <- (data[[v]] < 0)
  data[[v]][i] <- 0
}


data <- split(data, data$species)
data2 <- split(data2, data2$species)

for(spp in c("BOLE","PILI","HEPU","GRSP","GRBU","COER","XXXX","XXXX","EPMI","LEES","PHPH","PUTU","HATE","BAER","PELA","PEPU")) {
  
  if(spp == "XXXX") {
    plot(1,1, type="n", axes=FALSE, ann=FALSE)
  } else {
    plot(RA_max_1~age,data[[spp]],pch=16,log="x",ylim=c(-0.05,1.05),xlim=c(1.25,35),col="white",yaxt="n",xaxt="n",las=1,xaxs="i",yaxs="i")
    polygon(x=c(1.25,1.25,35,35),y=c(-0.05,1.05,1.05,-0.05),col="white")
    
    if(spp=="BOLE"|spp=="EPMI"|spp=="GRBU"|spp=="HATE") {
      axis(2, at=c(0,0.2,.4,.6,.8,1,1.2,1.4), labels=c(0,0.2,0.4,.6,.8,1,1.2,1.4),cex.axis=1,las=1)
    }
    
    if(spp=="BAER"|spp=="PEPU"|spp=="PELA"|spp=="HATE") {
      axis(1, at=c(2,5,10,20), labels=c(2,5,10,20),cex.axis=1,las=1)
    }
    
    if(spp=="PILI") {
      polygon(x=c(1.4,7,7,1.4),y=c(1,1,0,0),col="darkseagreen2",density=NA)
      polygon(x=c(1.4,2.4,5,7,7,1.4),y=c(data[[spp]]$RA_max_1[1],data[[spp]]$RA_max_1[2],data[[spp]]$RA_max_1[3],data[[spp]]$RA_max_1[4],
                                         0,0),col="coral3",density=NA)
    }
    
    if(spp=="PHPH") {
      polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="darkseagreen2",density=NA)
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
    points((RA_max_1)~(age),data2[[spp]],cex=1,lwd=1.5,pch=4)
    mtext(labels.spp.full(spp),font=3,outer=FALSE,side=3,adj=0)
    mtext(labels.RA.full(spp),adj=1,side=1,line=-1.75,cex=0.7)
    
  }
  #extra.top.left.logx(labels.spp.full(spp),3,1)
}
mtext("age (yr)",side=1,outer=TRUE,line=1)
mtext("RA (0-1)",side=2,outer=TRUE,line=1.75)

dev.off()

######################
#####  FIGURE 5  #####
######################
extra.top.left.logx <- function(words, fontx, cexx) {
  text(10^((0.06 * (par("usr")[2] - par("usr")[1])) + par("usr")[1]), 
       ((0.95 * (par("usr")[4] - par("usr")[3])) + par("usr")[3]), words, adj = 0, cex = cexx,
       font = fontx)
}

win.metafile("ms/RA/Figure_5_life_history_correlations.wmf", height=8, width=8)
plot
par(mfcol=c(2,2), cex=1, omi=c(.1,.7,.1,.1), mai=c(.8,.3,.1,0.2)) 


traits <- SummarySpp[["mean"]]
traits$maxH <- SummarySpp[["max"]]$height

temp <- SummarySppAge[["mean"]]
temp <- temp %>%
  group_by(species) %>%
  summarise_each(funs(max), RA_leaf_area,RA_max_1,RA_vs_all_leaf)
names(temp) <- c("species","RA_leaf_area_max","RA_max_1_max","RA_vs_all_leaf_max")

traits <- merge(traits,temp,by="species")
i <- traits$RA_max_1_max

plot(i~traits$lifespan,pch=16,cex=1.5,log="x",xlab="",yaxt="n",col="coral4",ylim=c(0,1))
mtext("lifespan (years)",1,outer=FALSE,line=2)
axis(2, at=c(0,.2,.4,.6,.8,1,1.2,1.4), labels=c(0,0.2,0.4,0.6,0.8,1.0,1.2,1.4),cex.axis=.9,las=1)
mod <- lm(log10(lifespan)~i,traits)
#extra.bottom.left.logx(paste("r2=",round(glance(mod)[1],2),"; p-value:",round(glance(mod)[5],4)),cexx=1,fontx=1)
extra.top.left.logx(words="a",fontx=2,cexx=1.3)

plot(i~traits$maturity,pch=16,cex=1.5,log="x",xlab="",yaxt="n",col="coral4",ylim=c(0,1))
mtext("age at maturity (years)",1,outer=FALSE,line=2)
axis(2, at=c(0,.2,.4,.6,.8,1,1.2,1.4), labels=c(0,0.2,0.4,0.6,0.8,1.0,1.2,1.4),cex.axis=.9,las=1)
mod <- lm(maturity~log10(i),traits)
#extra.bottom.left.logx(paste("r2=",round(glance(mod)[1],2),"; p-value:",round(glance(mod)[5],4)),cexx=1,fontx=1)
extra.top.left.logx(words="b",fontx=2,cexx=1.3)

plot(i~traits$maxH,pch=16,cex=1.5,log="x",xlab="",yaxt="n",col="coral4",ylim=c(0,1))
mtext("maximum height (mm)",1,outer=FALSE,line=2)
mod <- lm(log10(maxH)~i,traits)
axis(2, at=c(0,.2,.4,.6,.8,1,1.2,1.4), labels=c("","","","","","","",""),cex.axis=.9,las=1)
#extra.bottom.left.logx(paste("r2=",round(glance(mod)[1],2),"; p-value:",round(glance(mod)[5],4)),cexx=1,fontx=1)
extra.top.left.logx(words="c",fontx=2,cexx=1.3)

plot(RA_max_1 ~ prop_leaf_loss,subset(SummarySppAge[["mean"]],age>2),pch=16,cex=1.5,log="",xlab="",yaxt="n",col="coral4",ylim=c(0,1))
mtext("proportion leaf loss (0-1)",1,outer=FALSE,line=2)
axis(2, at=c(0,.2,.4,.6,.8,1,1.2,1.4), labels=c("","","","","","","",""),cex.axis=.9,las=1)
mod <- lm(RA_max_1~prop_leaf_loss,subset(SummarySppAge[["mean"]],age>2))
#extra.top.left(paste("r2=",round(glance(mod)[1],2)),cexx=1,fontx=1)
extra.top.left(words="d",fontx=2,cexx=1.3)
mtext("maximum RA, as fraction of surplus energy",2,outer=TRUE,line=1)

dev.off()

win.metafile("ms/RA/Figure_4b_life_history_correlation_matrix.wmf", height=8, width=8)
plot
par(mfcol=c(1,1), cex=1, omi=c(.1,.7,.1,.1), mai=c(.05,.05,.05,0.05)) 

temp2 <- select(traits,lifespan, maturity,maxH,RA_max_1_max)
names(temp2) <- c("lifespan","maturity","max height","maximum RA")
pairs(temp2,pch=16,upper.panel =NULL,log="xy")

dev.off()

######################
#####  FIGURE 6  #####
######################


data <- SummaryInd

data <- subset(data,!(data$individual %in% c("COER_806","EPMI_907","GRBU_906","HATE_105","HATE_003","LEES_354",
                                             "LEES_352","LEES_355","LEES_353","LEES_351","PELA_161","PELA_162",
                                             "PUTU_108")))
options(scipen=0)

#pdf("figs2/RA/Figure_6_investment.pdf", height=11, width=8)
win.metafile("ms/RA/Figure_6_investment.wmf", height=16, width=16)
par(mfrow=c(4,4), cex=1, omi=c(.5,.6,.1,.1), mai=c(.3,.6,0.3,0.02))

data <- subset(data)
data <- split(data, data$species)

for(spp in c("BOLE","PILI","HEPU","GRSP","GRBU","COER","XXXX","XXXX","EPMI","LEES","PHPH","PUTU","HATE","BAER","PELA","PEPU")) {
  
  
  if(spp == "XXXX") {
    plot(1,1, type="n", axes=FALSE, ann=FALSE)
  }else{
    
    data[[spp]]$leaf_shed_log <- log10(data[[spp]]$leaf_shed)
    data[[spp]]$leaf_expansion_log <- log10(data[[spp]]$growth_leaf)
    data[[spp]]$leaf_replacement_log <- log10(data[[spp]]$leaf_replacement)
    data[[spp]]$repro_inv_log <- log10(data[[spp]]$repro_inv)
    data[[spp]]$surplus_inv_log <- log10(data[[spp]]$surplus_inv)
    
    for (v in c("leaf_shed_log","leaf_expansion_log","leaf_replacement_log","repro_inv_log")) {
      i <- is.infinite(data[[spp]][[v]])
      data[[spp]][[v]][i] <- 0
      j <- is.na(data[[spp]][[v]])
      data[[spp]][[v]][j] <- 0
    }
    
    y_max <-max(max(data[[spp]]$leaf_expansion_log),max(data[[spp]]$repro_inv_log),max(data[[spp]]$leaf_shed_log))
    x_max <-max(data[[spp]]$total_weight_0) 
    x_min <-min(data[[spp]]$total_weight_0)
    y_min <-min(min(subset(data[[spp]]$leaf_expansion_log,data[[spp]]$leaf_expansion_log>0)),
                min(subset(data[[spp]]$repro_inv_log,data[[spp]]$repro_inv_log>0)),
                min(subset(data[[spp]]$leaf_shed_log,data[[spp]]$leaf_shed_log>0)))
    
    for (v in c("leaf_shed_log","leaf_expansion_log","leaf_replacement_log","repro_inv_log")) {
      for (i in 1:nrow(data[[spp]])) {
        if (data[[spp]][[v]][i] == 0) { 
          data[[spp]][[v]][i] <- y_min-1.5
        }
      }
    }
    
    plot(leaf_replacement_log~total_weight_0,data[[spp]],type="n",log="x",xlab="",ylab="",cex=0.7,ylim=c((y_min-2),(1.1*y_max)),xlim=c((0.5*x_min),(2*x_max)),
         col="darkseagreen4",cex.main=0.9, yaxt="n",pch=16,xaxs="i",yaxs="i")
    
    polygon(x=c(0.5*x_min,0.5*x_min,2*x_max,2*x_max),y=c(y_min-1,y_min-2,y_min-2,y_min-1),col="grey90")
    points(leaf_replacement_log~total_weight_0,data[[spp]],pch=16,cex=1,col="darkseagreen4") 
    #points(leaf_expansion_log~total_weight_0,data[[spp]],pch=16,cex=1,col="darkseagreen2")  
    #points(repro_inv_log~total_weight_0,data[[spp]],pch=16,cex=1,col="coral3")
    #points(leaf_expansion_log~total_weight_0,data[[spp]],pch=16,cex=0.5,col="darkseagreen2")
    points(surplus_inv_log~total_weight_0,data[[spp]],pch=16,cex=0.5,col="black")
    
    for (i in seq_along(data[[spp]]$individual)) {
      if (data[[spp]]$leaf_shed_log[i] > data[[spp]]$leaf_replacement_log[i]) {
        arrows(data[[spp]]$total_weight_0[i],data[[spp]]$leaf_replacement_log[i],data[[spp]]$total_weight_0[i],data[[spp]]$leaf_shed_log[i],
               length=0,col="darkseagreen4")
      } else {}
    }
    
    mtext(labels.spp.full(spp),font=3,outer=FALSE,side=3,adj=0)
    
    y_min_int <- floor(y_min)
    y_max_int <- ceiling(y_max)
    y_span <- y_max_int-y_min_int+1
    
    label_list <- as.data.frame(seq(y_min_int,y_max_int,by=1))
    names(label_list) <- c("base")
    label_list$nums <- 10^(label_list$base)
    label_list$to_use <- label_list$nums
    for (i in 1:nrow(label_list)) {
      if(label_list$to_use[i]>1000) {
        label_list$to_use[i] <- paste("10^",label_list$base[i],sep="")
      }
    }
    
    
    axis(side=2,at = label_list$base,label =label_list$to_use,las=1)
  }
  
  mtext("investment (mg)", 2, outer=TRUE,cex=1,line=1.5)
  mtext("plant weight (mg)",1,outer=TRUE,cex=1,line=1)
}

dev.off()


######################
#####  FIGURE 7  #####
######################

win.metafile("ms/RA/Figure_7_leaf_area_by_age.wmf", height=16, width=16)
par(mfrow=c(4,4), cex=1, omi=c(.5,.6,.2,.02), mai=c(0.2,.65,.01,0.01)) 

data <- SummaryInd

data <- split(data, data$species)


for(spp in c("BOLE","PILI","HEPU","GRSP","GRBU","COER","XXXX","XXXX","EPMI","LEES","PHPH","PUTU","HATE","BAER","PELA","PEPU")) {
  
  if(spp == "XXXX") {
    plot(1,1, type="n", axes=FALSE, ann=FALSE)
  } else {
    
    y_max <-max(data[[spp]]$leaf_area_0)
    y_min <-min(data[[spp]]$leaf_area_0)
    
    plot(leaf_area_0~age,data[[spp]],pch=16,log="xy",xlim=c(1,40),ylim=c(y_min*0.5,y_max*2),col=col.age2(age),xaxt="n",ylab="n",las=1,xaxs="i",yaxs="i")
    
    if(spp=="BAER"|spp=="PEPU"|spp=="PELA"|spp=="HATE") {
      axis(1, at=c(2,5,10,20), labels=c(2,5,10,20),cex.axis=1,las=1)
    }
    
    mtext(labels.spp.full(spp),font=3,outer=FALSE,side=3,adj=0)
  }
}
mtext("age (yr)",side=1,outer=TRUE,line=1)
mtext("leaf area (mm^2)",side=2,outer=TRUE,line=1.75)

dev.off()



######################
#####  SUPP 1  #####
######################

win.metafile("ms/RA/SupMat_01_leaf_area_vs_production.wmf", height=16, width=16)
par(mfrow=c(4,4), cex=1, omi=c(.5,.6,.02,.02), mai=c(0.2,.75,.4,0.01)) 
options(scipen=999)
data <- SummaryInd

data <- subset(data,!(data$individual %in% c("COER_806","EPMI_907","GRBU_906","HATE_105","HATE_003","LEES_354",
                                             "LEES_352","LEES_355","LEES_353","LEES_351","PELA_161","PELA_162",
                                             "PUTU_108")))


data <- split(data, data$species)


for(spp in c("BOLE","PILI","HEPU","COER","GRBU","GRSP","XXXX","XXXX","EPMI","LEES","PHPH","PUTU","HATE","BAER","PELA","PEPU")) {
  
  if(spp == "XXXX") {
    plot(1,1, type="n", axes=FALSE, ann=FALSE)
  } else {
    
    y_max <-max(data[[spp]]$all_leaf_and_repro_inv)
    y_min <-min(subset(data[[spp]]$all_leaf_and_repro_inv,data[[spp]]$all_leaf_and_repro_inv>0))
    x_max <-max(data[[spp]]$leaf_area_0)
    x_min <-min(subset(data[[spp]]$leaf_area_0,data[[spp]]$leaf_area_0>0))
    
    plot(all_leaf_and_repro_inv~leaf_area_0,data[[spp]],pch=16,log="xy",ylim=c(y_min*0.5,y_max*2),xlim=c(x_min*0.5,x_max*2),col=col.age2(age),ylab="",las=1,xaxs="i",yaxs="i")
    mtext(labels.spp.full(spp),font=3,outer=FALSE,side=3,adj=0,cex.axis=0.8)
    #points(repro_inv~leaf_area_0,data[[spp]])
  }
}
mtext("leaf area (mm^2)",side=1,outer=TRUE,line=1)
mtext("total production (mg)",side=2,outer=TRUE,line=1)

dev.off()

#########

win.metafile("ms/RA/SupMat_02_leaf_area_vs_tot_production.wmf", height=16, width=16)
par(mfrow=c(4,4), cex=1, omi=c(.5,.6,.02,.02), mai=c(0.2,.75,.4,0.01)) 
options(scipen=999)
data <- SummaryInd

data <- subset(data,!(data$individual %in% c("COER_806","EPMI_907","GRBU_906","HATE_105","HATE_003","LEES_354",
                                             "LEES_352","LEES_355","LEES_353","LEES_351","PELA_161","PELA_162",
                                             "PUTU_108")))


data <- split(data, data$species)


for(spp in c("BOLE","PILI","HEPU","COER","GRBU","GRSP","XXXX","XXXX","EPMI","LEES","PHPH","PUTU","HATE","BAER","PELA","PEPU")) {
  
  if(spp == "XXXX") {
    plot(1,1, type="n", axes=FALSE, ann=FALSE)
  } else {
    
    y_max <-max(data[[spp]]$total_inv)
    y_min <-min(subset(data[[spp]]$total_inv,data[[spp]]$total_inv>0))
    x_max <-max(data[[spp]]$leaf_area_0)
    x_min <-min(subset(data[[spp]]$leaf_area_0,data[[spp]]$leaf_area_0>0))
    
    plot(total_inv~leaf_area_0,data[[spp]],pch=16,log="xy",ylim=c(y_min*0.5,y_max*2),xlim=c(x_min*0.5,x_max*2),col=col.age2(age),ylab="",las=1,xaxs="i",yaxs="i")
    mtext(labels.spp.full(spp),font=3,outer=FALSE,side=3,adj=0,cex.axis=0.8)
    #points(repro_inv~leaf_area_0,data[[spp]])
  }
}
mtext("leaf area (mm^2)",side=1,outer=TRUE,line=1)
mtext("total production (mg)",side=2,outer=TRUE,line=1)

dev.off()
