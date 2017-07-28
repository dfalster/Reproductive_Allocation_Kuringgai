
figure_allocation_demo <- function(SummarySppAge) {

  par(mfcol=c(1,4), cex=1, omi=c(0.6,1,.1,2.6), mai=c(0.2,.45,.3,0.02))

  data <- SummarySppAge[["mean"]]

  for(v in c("growth_leaf")) {
    i <- (data[[v]] < 0)
    data[[v]][i] <- 0
  }

  data <- split(data, data$species)

  spp <- "EPMI"

  YLIM <- c(0,1)

  mylab <- function(...){
    text(1, 1.1, ..., cex=1.3, xpd=NA)
  }

  plot(RA_seed~age,data[[spp]],pch=16,log="x",ylim=YLIM,xlim=c(1.06*1.4,0.94*32),col="white",las=1,xaxs="i", yaxs="i")
  polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="saddlebrown",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$replace_leaf_prop_no_accessory[1:6],0,0),col="darkseagreen4",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$growth_leaf_prop_no_accessory[1:6],0,0),
          col="darkseagreen2",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$seed_prop_all[1:6],0,0),col="coral1",density=NA)
  mylab("(a)")

  plot(prop_repro~age,data[[spp]],pch=16,log="x",ylim=YLIM,xlim=c(1.06*1.4,0.94*32),col="white",ylab="",yaxt="n",xaxs="i", yaxs="i")
  polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="saddlebrown",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$prop_leaf_replacement[1:6],0,0),col="darkseagreen4",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$prop_leaf_expand[1:6],0,0),col="darkseagreen2",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$prop_repro[1:6],0,0),col="coral3",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$seed_prop_all_acc[1:6],0,0),col="coral1",density=NA)
  mylab("(b)")
  axis(2, at= c(0, 0.2, 0.4, 0.6, 0.8, 1.0), labels = FALSE)

  plot(repro_prop_all_leaf~age,data[[spp]],pch=16,log="x",ylim=YLIM,xlim=c(1.06*1.4,0.94*32),col="white",ylab="",yaxt="n",xaxs="i", yaxs="i")
  polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="darkseagreen4",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$repro_and_leaf_growth_prop_surplus[1:6],
                                           0,0),col="darkseagreen2",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$repro_prop_all_leaf[1:6],0,0),col="coral3",density=NA)

  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$seed_prop_veg[1:6],0,0),col="coral1",density=NA)
  mylab("(c)")
  axis(2, at= c(0, 0.2, 0.4, 0.6, 0.8, 1.0), labels = FALSE)

  plot(prop_repro~age,data[[spp]],pch=16,log="x",ylim=YLIM,xlim=c(1.06*1.4,0.94*32),col="white",ylab="",yaxt="n",xaxs="i", yaxs="i")
  polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="darkseagreen2",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$RA_max_1[1:6],0,0),col="coral3",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$seed_prop_surplus[1:6],0,0),col="coral1",density=NA)
  mylab("(d)")
  axis(2, at= c(0, 0.2, 0.4, 0.6, 0.8, 1.0), labels = FALSE)

  legend(40, 0.8, c("Stem growth", "Leaf replacement", "Leaf expansion", "Reproductive - accessories", "Reproductive - seed"),
    bty="n", pch=16,
    col=c("saddlebrown", "darkseagreen4", "darkseagreen2","coral3", "coral1"), xpd= NA)

  mtext("Age (yr)",side=1,outer=TRUE,line=1.5, cex=1.25)
  mtext("Reproductive allocation (0-1)",side=2,outer=TRUE,line=2, cex=1.25)

}

figure_allocation_all <- function(SummaryInd, SummarySppAge) {

  par(mfrow=c(3,5), cex=1, omi=c(.5,.5,.1,.1), mai=c(.3,.6,0.5,0.02))

  data <- SummarySppAge[["mean"]]
  data2 <- SummaryInd

  for(v in c("growth_leaf")) {
  i <- (data[[v]] < 0)
  data[[v]][i] <- 0
  }


  data <- split(data, data$species)
  data2 <- split(data2, data2$species)

  for(spp in species_order()) {

    plot(RA_max_1~age,data[[spp]],pch=16,log="x",ylim=c(-0.05,1.05),xlim=c(1.25,35),col="white",yaxt="n",xaxt="n",las=1,xaxs="i",yaxs="i")
    polygon(x=c(1.25,1.25,35,35),y=c(-0.05,1.05,1.05,-0.05),col="white")

    axis(2, at=c(0,0.2,.4,.6,.8,1), las=1)
    axis(1, at=c(2,5,10,20))

    if(spp %in% c("PILI")) {
      polygon(x=c(1.4,7,7,1.4),y=c(1,1,0,0),col="darkseagreen2",density=NA)
      polygon(x=c(1.4,2.4,5,7,7,1.4),y=c(data[[spp]]$RA_max_1[1],data[[spp]]$RA_max_1[2],data[[spp]]$RA_max_1[3],data[[spp]]$RA_max_1[4],
                                         0,0),col="coral3",density=NA)
    }

    if(spp %in% c("PHPH")) {
      polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="darkseagreen2",density=NA)
      polygon(x=c(2.4,5,7,9,32,32,2.4),y=c(data[[spp]]$RA_max_1[1],data[[spp]]$RA_max_1[2],data[[spp]]$RA_max_1[3],data[[spp]]$RA_max_1[4],
                                           data[[spp]]$RA_max_1[5],0,0),col="coral3",density=NA)
    }


    if(spp %in% c("BOLE", "COER", "HEPU", "GRSP")) {
      polygon(x=c(1.4,9,9,1.4),y=c(1,1,0,0),col="darkseagreen2",density=NA)
      polygon(x=c(1.4,2.4,5,7,9,9,1.4),y=c(data[[spp]]$RA_max_1[1],data[[spp]]$RA_max_1[2],data[[spp]]$RA_max_1[3],data[[spp]]$RA_max_1[4],
                                           data[[spp]]$RA_max_1[5],0,0),col="coral3",density=NA)
    }

    if(spp %in% c("BAER", "EPMI", "GRBU", "HATE", "PELA", "PUTU", "LEES", "PEPU")) {
      polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="darkseagreen2",density=NA)
      polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$RA_max_1[1],data[[spp]]$RA_max_1[2],data[[spp]]$RA_max_1[3],data[[spp]]$RA_max_1[4],
                                               data[[spp]]$RA_max_1[5],data[[spp]]$RA_max_1[6],0,0),col="coral3",density=NA)
    }
    points((RA_max_1)~(age),data2[[spp]],cex=1,lwd=1.5,pch=16)

    mtext(labels.spp.genus(spp),font=3,outer=FALSE,side=3,adj=0)
  }
  mtext("Age (yr)",side=1,outer=TRUE,line=1, cex=1.5)
  mtext("Reproductive allocation (0-1)",side=2,outer=TRUE,line=1, cex=1.5)

  legend(80, 0.8, c("Individual observation", "Leaf expansion", "Reproductive tissues"), bty="n", pch=16, col=c("black", "darkseagreen2","coral3"), xpd= NA)
}

figure_life_history <- function(SummarySpp, SummarySppAge){

  traits <- SummarySpp[["mean"]]
  traits$maxH <- SummarySpp[["max"]]$height

  temp <- SummarySppAge[["mean"]]
  temp <- temp %>%
    group_by(species) %>%
    summarise_each(funs(max), RA_leaf_area,RA_max_1,RA_vs_all_leaf)
  names(temp) <- c("species","RA_leaf_area_max","RA_max_1_max","RA_vs_all_leaf_max")

  traits <- merge(traits,temp,by="species")
  i <- traits$RA_max_1_max

  par(mfcol=c(1,1), cex=1, omi=c(.1,.7,.1,.1), mai=c(.05,.05,.05,0.05))

  temp2 <- select(traits,lifespan, maturity,maxH,RA_max_1_max)
  names(temp2) <- c("lifespan","maturity","max height","maximum RA")
  pairs(temp2,pch=16,upper.panel =NULL,log="xy",col="coral3",cex=1.2)
}

species_order <- function() {
  c("BOLE", "HEPU", "PILI", "COER", "EPMI", "GRSP", "LEES", "PHPH", "PUTU", "GRBU", "BAER", "HATE", "PEPU", "PELA")
}

figure_investment_weight <- function(SummaryInd) {

  data <- SummaryInd

  data <- subset(data,!(data$individual %in% c("COER_806","EPMI_907","GRBU_906","HATE_105","HATE_003","LEES_354",
                                               "LEES_352","LEES_355","LEES_353","LEES_351","PELA_161","PELA_162",
                                               "PUTU_108")))
  par(mfrow=c(3,5), cex=1, omi=c(.5,.5,.1,.1), mai=c(.3,.6,0.5,0.02))

  data <- split(data, data$species)

  cols <- c("darkseagreen4", "darkseagreen2", "coral3")
  vars <- c("leaf_replacement", "growth_leaf", "repro_inv")
  labs <- c("Leaf replacement", "Leaf expansion", "Reproductive tissues")

  for(spp in species_order()) {
    y_all <- unlist(data[[spp]][vars])
    y_max <-max(y_all)
    y_min <-min(y_all[y_all>0])

    x_max <-max(data[[spp]]$height)
    x_min <-min(data[[spp]]$height)

    plot(NA,type="n",log="xy",xlab="",ylab="", xaxs="i",yaxs="i", axes=FALSE,
      ylim=c((y_min/100),(10*y_max)),xlim=c((0.5*x_min),(2*x_max)))

    polygon(x=c(0.5*x_min,0.5*x_min,2*x_max,2*x_max),y=c(y_min/10,y_min/100,y_min/100,y_min/10),col="grey90")

    x <- data[[spp]][["height"]]

    for(i in 1:3){
      y <- data[[spp]][[vars[i]]]
      y[y<=0] <- y_min/30
      points(x,y,pch=16, col=cols[i])
    }

    mtext(labels.spp.genus(spp),font=3,outer=FALSE,side=3,adj=0)

    axis(1, at = c(18, 37, 75, 150, 300, 600, 1200, 2400, 4800))
    add_axis_log10(2, at = seq(floor(log10(y_min)),ceiling(log(y_max))))
    box()
    }
  legend(8000, 2000, labs, bty="n", pch=16, col=cols, xpd= NA)
  mtext("Investment (mg)", 2, outer=TRUE,cex=1.5,line=1)
  mtext("Plant height (mm)",1,outer=TRUE,cex=1.5,line=1)

}

figure_leaf_area <- function(Growth_all){

  par(mfrow=c(3, 5), cex=1, omi=c(.5,.5,.1,.1), mai=c(.3,.6,0.5,0.02))

  data <- Growth_all

  data <- split(data, data$species)

  for(spp in species_order()) {

    y_max <-max(data[[spp]]$leaf_weight)
    y_min <-min(data[[spp]]$leaf_weight)

    plot(leaf_weight~age,data[[spp]],pch=16,log="xy",
    xlim=c(0.05,40),ylim=c(y_min*0.5,y_max*2),col=col.age(age),axes=FALSE,ylab="n",xaxs="i",yaxs="i")

    fit.l <- fit_gam_loglog("leaf_weight", "age", data[[spp]])
    age.r <- seq_log_range(range(data[[spp]][["age"]], na.rm=TRUE), 50)
    points(age.r, y_hat(fit.l, age.r), type = "l")

    x <- c(0.125, 0.25, 0.5, 1, 2,5,10,20, 40)
    axis(1, at=x, labels= FALSE)
    axis(1, at=x[c(2,5,8)], labels= (spp %in% c("BAER", "PEPU", "PELA", "HATE")))

    add_axis_log10(2)

    box()
    mtext(labels.spp.genus(spp),font=3,outer=FALSE,side=3,adj=0)
  }

 legend(60, 2.5E4, "Sites Ages:", bty="n", xpd= NA)
 legend(80, 1E4, names(col.age()), bty="n", pch=16, col=col.age(), xpd= NA)

  mtext("Age (yr)",side=1,outer=TRUE,line=1, cex=1.5)
  mtext(expression(paste("Leaf area (", mm^2, " )")),side=2,outer=TRUE,line=0.8, cex=1.5)

}

figure_leaf_loss <- function(SummarySppAge){
  par(mfrow=c(1,1), cex=1, omi=c(.6,.6,.02,.02), mai=c(0.02,.02,.01,0.01))

  plot(RA_max_1 ~ prop_leaf_loss,subset(SummarySppAge[["mean"]],age>2),pch=16,col=col.age(age),xlim=c(0,1),ylim=c(0,1),xlab="",ylab="")
  mod <- lm(prop_leaf_loss~RA_max_1,subset(SummarySppAge[["mean"]],age>2))
  extra.top.left(paste("r2 =",round(glance(mod)[2],digits=2)))
  mtext("Proportion of leaves lost annually",side=1,outer=TRUE,line=2)
  mtext("Reproductive allocation (0-1)",side=2,outer=TRUE,line=2)
}

figure_leafarea_production <- function(SummaryInd){

  par(mfrow=c(4,4), cex=1, omi=c(.5,.6,.02,.02), mai=c(0.2,.75,.4,0.01))
  data <- SummaryInd

  data <- subset(data,!(data$individual %in% c("COER_806","EPMI_907","GRBU_906","HATE_105","HATE_003","LEES_354",
                                               "LEES_352","LEES_355","LEES_353","LEES_351","PELA_161","PELA_162",
                                               "PUTU_108")))

  data <- split(data, data$species)

  for(spp in species_order()) {

    y_max <-max(data[[spp]]$total_inv)
    y_min <-min(subset(data[[spp]]$total_inv,data[[spp]]$total_inv>0))
    x_max <-max(data[[spp]]$leaf_area_0)
    x_min <-min(subset(data[[spp]]$leaf_area_0,data[[spp]]$leaf_area_0>0))

    plot(total_inv~leaf_area_0,data[[spp]],pch=16,log="xy",
      ylim=c(y_min*0.5,y_max*2),xlim=c(x_min*0.5,x_max*2),
      col=col.age(age),ylab="",las=1,xaxs="i",yaxs="i", axes=FALSE)
    add_axis_log10(1)
    add_axis_log10(2)
    box()
    mtext(labels.spp.genus(spp),font=3,outer=FALSE,side=3,adj=0,cex.axis=0.8)
  }
  mtext(expression(paste("Leaf area (", mm^2, " )")),side=1,outer=TRUE,line=1)
  mtext("Total production (mg)",side=2,outer=TRUE,line=1)
}
