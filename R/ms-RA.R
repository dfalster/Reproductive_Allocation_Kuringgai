
figure_allocation_demo <- function(SummarySppAge) {

  par(mfcol=c(1,4), cex=1, omi=c(.5,.6,.1,.1), mai=c(0.2,.25,.01,0.02))

  data <- SummarySppAge[["mean"]]

  for(v in c("growth_leaf")) {
    i <- (data[[v]] < 0)
    data[[v]][i] <- 0
  }

  data <- split(data, data$species)

  spp <- "EPMI"

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
  extra.top.left.logx(words="a",font=2,cex=1.3)

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
  extra.top.left.logx(words="b",font=2,cex=1.3)

  plot(repro_prop_all_leaf~age,data[[spp]],pch=16,log="x",ylim=c(0.04,0.96),xlim=c(1.06*1.4,0.94*32),col="white",ylab="",yaxt="n",xaxs="i")
  polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="darkseagreen4",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$repro_and_leaf_growth_prop_surplus[1],data[[spp]]$repro_and_leaf_growth_prop_surplus[2],data[[spp]]$repro_and_leaf_growth_prop_surplus[3],
                                           data[[spp]]$repro_and_leaf_growth_prop_surplus[4],data[[spp]]$repro_and_leaf_growth_prop_surplus[5],data[[spp]]$repro_and_leaf_growth_prop_surplus[6],
                                           0,0),col="darkseagreen2",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$repro_prop_all_leaf[1],data[[spp]]$repro_prop_all_leaf[2],data[[spp]]$repro_prop_all_leaf[3],data[[spp]]$repro_prop_all_leaf[4],
                                           data[[spp]]$repro_prop_all_leaf[5],data[[spp]]$repro_prop_all_leaf[6],0,0),col="coral3",density=NA)

  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$seed_prop_veg[1],data[[spp]]$seed_prop_veg[2],data[[spp]]$seed_prop_veg[3],data[[spp]]$seed_prop_veg[4],
                                           data[[spp]]$seed_prop_veg[5],data[[spp]]$seed_prop_veg[6],0,0),col="coral1",density=NA)
  extra.top.left.logx(words="c",font=2,cex=1.3)

  plot(prop_repro~age,data[[spp]],pch=16,log="x",ylim=c(0.04,0.96),xlim=c(1.06*1.4,0.94*32),col="white",ylab="",yaxt="n",xaxs="i")
  polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="darkseagreen2",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$RA_max_1[1],data[[spp]]$RA_max_1[2],data[[spp]]$RA_max_1[3],data[[spp]]$RA_max_1[4],
                                           data[[spp]]$RA_max_1[5],data[[spp]]$RA_max_1[6],0,0),col="coral3",density=NA)
  polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$seed_prop_surplus[1],data[[spp]]$seed_prop_surplus[2],
                                           data[[spp]]$seed_prop_surplus[3],data[[spp]]$seed_prop_surplus[4],
                                           data[[spp]]$seed_prop_surplus[5],data[[spp]]$seed_prop_surplus[6],0,0),col="coral1",density=NA)
  extra.top.left.logx(words="d",font=2,cex=1.3)

  mtext("Age (yr)",side=1,outer=TRUE,line=1)
  mtext("RA (0-1)",side=2,outer=TRUE,line=1.75)

}

figure_allocation_all <- function(SummaryInd, SummarySppAge) {

  par(mfrow=c(4,4), cex=1, omi=c(.5,.6,.2,.1), mai=c(0.2,.2,.01,0.01))

  data <- SummarySppAge[["mean"]]
  data2 <- SummaryInd

  for(v in c("growth_leaf")) {
  i <- (data[[v]] < 0)
  data[[v]][i] <- 0
  }


  data <- split(data, data$species)
  data2 <- split(data2, data2$species)

  for(spp in species_order()) {

  if(spp == "XXXX") {
    plot(1,1, type="n", axes=FALSE, ann=FALSE)
  } else {
    plot(RA_max_1~age,data[[spp]],pch=16,log="x",ylim=c(-0.05,1.05),xlim=c(1.25,35),col="white",yaxt="n",xaxt="n",las=1,xaxs="i",yaxs="i")
    polygon(x=c(1.25,1.25,35,35),y=c(-0.05,1.05,1.05,-0.05),col="white")

    if(spp %in% c("BOLE", "EPMI", "GRBU", "HATE")) {
      axis(2, at=c(0,0.2,.4,.6,.8,1,1.2,1.4), labels=c(0,0.2,0.4,.6,.8,1,1.2,1.4),cex.axis=1,las=1)
    }

    if(spp %in% c("BAER", "PEPU", "PELA", "HATE")) {
      axis(1, at=c(2,5,10,20), labels=c(2,5,10,20),cex.axis=1,las=1)
    }

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
    points((RA_max_1)~(age),data2[[spp]],cex=1,lwd=1.5,pch=4)
    mtext(labels.spp.full(spp),font=3,outer=FALSE,side=3,adj=0)
    mtext(labels.RA.full(spp),adj=1,side=1,line=-1.75,cex=0.7)

  }
  #extra.top.left.logx(labels.spp.full(spp),3,1)
  }
  mtext("Age (yr)",side=1,outer=TRUE,line=1)
  mtext("RA (0-1)",side=2,outer=TRUE,line=1.75)

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
  c("BOLE","PILI","HEPU","GRSP","GRBU","COER","XXXX","XXXX","EPMI","LEES","PHPH","PUTU","HATE","BAER","PELA","PEPU")
}

figure_investment_weight <- function(SummaryInd) {


  data <- SummaryInd

  data <- subset(data,!(data$individual %in% c("COER_806","EPMI_907","GRBU_906","HATE_105","HATE_003","LEES_354",
                                               "LEES_352","LEES_355","LEES_353","LEES_351","PELA_161","PELA_162",
                                               "PUTU_108")))
  par(mfrow=c(4,4), cex=1, omi=c(.5,.6,.1,.1), mai=c(.3,.6,0.3,0.02))

  data <- split(data, data$species)

  for(spp in species_order()) {


    if(spp == "XXXX") {
      plot(1,1, type="n", axes=FALSE, ann=FALSE)
    } else {

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
      points(leaf_expansion_log~total_weight_0,data[[spp]],pch=16,cex=1,col="darkseagreen2")
      points(repro_inv_log~total_weight_0,data[[spp]],pch=16,cex=1,col="coral3")
      points(leaf_expansion_log~total_weight_0,data[[spp]],pch=16,cex=0.5,col="darkseagreen2")

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
}


figure_leaf_area <- function(SummaryInd){
  par(mfrow=c(4,4), cex=1, omi=c(.5,.6,.2,.02), mai=c(0.2,.65,.01,0.01))

  data <- SummaryInd

  data <- split(data, data$species)


  for(spp in species_order()) {

    if(spp == "XXXX") {
      plot(1,1, type="n", axes=FALSE, ann=FALSE)
    } else {

      y_max <-max(data[[spp]]$leaf_area_0)
      y_min <-min(data[[spp]]$leaf_area_0)

      plot(leaf_area_0~age,data[[spp]],pch=16,log="xy",xlim=c(1,40),ylim=c(y_min*0.5,y_max*2),col=col.age(age),xaxt="n",ylab="n",las=1,xaxs="i",yaxs="i")

      if(spp %in% c("BAER", "PEPU", "PELA", "HATE")) {
        axis(1, at=c(2,5,10,20), labels=c(2,5,10,20),cex.axis=1,las=1)
      }

      mtext(labels.spp.full(spp),font=3,outer=FALSE,side=3,adj=0)
    }
  }
  mtext("Age (yr)",side=1,outer=TRUE,line=1)
  mtext("Leaf area (mm^2)",side=2,outer=TRUE,line=1.75)

}

figure_leaf_loss <- function(SummarySppAge){
  par(mfrow=c(1,1), cex=1, omi=c(.6,.6,.02,.02), mai=c(0.02,.02,.01,0.01))

  plot(RA_max_1 ~ prop_leaf_loss,subset(SummarySppAge[["mean"]],age>2),pch=16,col=col.age(age),xlim=c(0,1),ylim=c(0,1),xlab="",ylab="")
  mod <- lm(prop_leaf_loss~RA_max_1,subset(SummarySppAge[["mean"]],age>2))
  extra.top.left(paste("r2 =",round(glance(mod)[2],digits=2)))
  mtext("proportion of leaves lost annually",side=1,outer=TRUE,line=2)
  mtext("RA (0-1)",side=2,outer=TRUE,line=2)
}

figure_leafarea_production <- function(SummaryInd){

  par(mfrow=c(4,4), cex=1, omi=c(.5,.6,.02,.02), mai=c(0.2,.75,.4,0.01))
  data <- SummaryInd

  data <- subset(data,!(data$individual %in% c("COER_806","EPMI_907","GRBU_906","HATE_105","HATE_003","LEES_354",
                                               "LEES_352","LEES_355","LEES_353","LEES_351","PELA_161","PELA_162",
                                               "PUTU_108")))


  data <- split(data, data$species)


  for(spp in species_order()) {

    if(spp == "XXXX") {
      plot(1,1, type="n", axes=FALSE, ann=FALSE)
    } else {

      y_max <-max(data[[spp]]$total_inv)
      y_min <-min(subset(data[[spp]]$total_inv,data[[spp]]$total_inv>0))
      x_max <-max(data[[spp]]$leaf_area_0)
      x_min <-min(subset(data[[spp]]$leaf_area_0,data[[spp]]$leaf_area_0>0))

      plot(total_inv~leaf_area_0,data[[spp]],pch=16,log="xy",ylim=c(y_min*0.5,y_max*2),xlim=c(x_min*0.5,x_max*2),col=col.age(age),ylab="",las=1,xaxs="i",yaxs="i")
      mtext(labels.spp.full(spp),font=3,outer=FALSE,side=3,adj=0,cex.axis=0.8)
      #points(repro_inv~leaf_area_0,data[[spp]])
    }
  }
  mtext("Leaf area (mm^2)",side=1,outer=TRUE,line=1)
  mtext("Total production (mg)",side=2,outer=TRUE,line=1)
}
