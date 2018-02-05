par_all_species_fig <- function() {
  par(mfrow=c(3,5), cex=1, omi=c(0.9,0.9,.1,.1), mai=c(.3,.3,0.2,0.02))
}

species_order <- function() {
  # this order based on sorting of values for Age at maturation in Table 1
  c("BOLE", "GRSP", "PILI", "HEPU", "EPMI", "GRBU", "LEES", "PUTU", "COER", "HATE", "PHPH", "BAER", "PEPU", "PELA")
}

glm_r2 <- function(model) {
  r2 <- sprintf("= %s", format_r2(R2_glm(model)))
  bquote(r^2 ~ .(r2))
}

gam_r2 <- function(model) {
  r2 <- sprintf("= %s", format_r2(summary(model)$ r.sq))
  bquote(r^2 ~ .(r2))
}

r2_inset <- function(r2, cex=0.75, font=1, pos=4, px = 0, py = 0.95, ...) {
   text(per_x(px), per_y(py), r2, cex=cex, font=font, pos=pos, ...)
}


species_labels <- function(spp) {
  mtext(labels.spp.full(spp),font=3,outer=FALSE,side=3,adj=0, line =0.25)
}

polygon2 <- function(...) polygon(..., density=NA)

ypoly <- function(data, n=6) {c(data[seq_len(n)],0,0)}

figure_allocation_demo_single <- function(SummarySppAge) {

  par(mfcol=c(1,4), cex=1, omi=c(0.6,1,.1,2.6), mai=c(0.2,.45,.3,0.02))

  data <- SummarySppAge[["mean"]]

  for(v in c("growth_leaf")) {
    i <- (data[[v]] < 0)
    data[[v]][i] <- 0
  }

  data <- split(data, data$species)

  spp <- "EPMI"

  x <- c(1.4,2.4,5,7,9,32,32,1.4)
  y0 <- c(rep(1,6), 0, 0)

  myplot <- function(lab, labels=FALSE){
    plot(NA,log="x",ylim=c(0,1),xlim=c(1.06*1.4,0.94*32),ylab="",yaxt="n",xaxs="i", yaxs="i")
    axis(2, at= c(0, 0.2, 0.4, 0.6, 0.8, 1.0), labels = labels, las=1)
    text(1, 1.1, lab, cex=1.3, xpd=NA)
    polygon2(c(1.4,32,32,1.4), c(1,1,0, 0), col="grey")
  }

  myplot("(a)", TRUE)
  polygon2(x, y0,col="saddlebrown")
  polygon2(x, ypoly(data[[spp]]$replace_leaf_prop_no_accessory),col="darkseagreen4")
  polygon2(x, ypoly(data[[spp]]$growth_leaf_prop_no_accessory),
          col="darkseagreen2")
  polygon2(x, ypoly(data[[spp]]$seed_prop_all),col="coral1")

  myplot("(b)")
  polygon2(x, y0,col="saddlebrown")
  polygon2(x, ypoly(data[[spp]]$prop_leaf_replacement),col="darkseagreen4")
  polygon2(x, ypoly(data[[spp]]$prop_leaf_expand),col="darkseagreen2")
  polygon2(x, ypoly(data[[spp]]$prop_repro),col="coral3")
  polygon2(x, ypoly(data[[spp]]$seed_prop_all_acc),col="coral1")

  myplot("(c)")
  polygon2(x, y0,col="darkseagreen4")
  polygon2(x, ypoly(data[[spp]]$repro_and_leaf_growth_prop_surplus),col="darkseagreen2")
  polygon2(x, ypoly(data[[spp]]$repro_prop_all_leaf),col="coral3")
  polygon2(x, ypoly(data[[spp]]$seed_prop_veg),col="coral1")

  myplot("(d)")
  polygon2(x, y0,col="darkseagreen2")
  polygon2(x, ypoly(data[[spp]]$RA_max_1),col="coral3")
  polygon2(x, ypoly(data[[spp]]$seed_prop_surplus),col="coral1")

  legend(40, 0.8, c("Stem growth", "Leaf replacement", "Leaf expansion", "Reproductive - accessories", "Reproductive - seed"),
    bty="n", pch=16, col=c("saddlebrown", "darkseagreen4", "darkseagreen2","coral3", "coral1"), xpd= NA)

  mtext("Age (yr)",side=1,outer=TRUE,line=1.5, cex=1.25)
  mtext("Fraction of mass allocated",side=2,outer=TRUE,line=2, cex=1.25)
}


figure_graphical_abstract <- function(SummarySppAge) {

  par(mfcol=c(1,2), cex=1, omi=c(0.6,0.75,.1,1.4), mai=c(0.2,.5,.1,0.02))

  data <- SummarySppAge[["mean"]]

  for(v in c("growth_leaf")) {
    i <- (data[[v]] < 0)
    data[[v]][i] <- 0
  }

  data <- split(data, data$species)

  spp <- "EPMI"

  x <- c(1.4,2.4,5,7,9,32,32,1.4)
  y0 <- c(rep(1,6), 0, 0)

  myplot <- function(labels=FALSE){
    plot(NA,log="x",ylim=c(0,1),xlim=c(1.06*1.4,0.94*32),ylab="",yaxt="n",xaxs="i", yaxs="i")
    axis(2, at= c(0, 0.2, 0.4, 0.6, 0.8, 1.0), labels = labels, las=1)
    polygon2(c(1.4,32,32,1.4), c(1,1,0, 0), col="grey")
  }

  myplot()
  polygon2(x, y0,col="darkseagreen4")
  polygon2(x, ypoly(data[[spp]]$repro_and_leaf_growth_prop_surplus),col="darkseagreen2")
  polygon2(x, ypoly(data[[spp]]$repro_prop_all_leaf),col="coral3")
  polygon2(x, ypoly(data[[spp]]$seed_prop_veg),col="coral1")
  axis(2, at= c(0, 0.2, 0.4, 0.6, 0.8, 1.0), labels = TRUE, las=1)

  myplot()
  polygon2(x, y0,col="darkseagreen2")
  polygon2(x, ypoly(data[[spp]]$RA_max_1),col="coral3")
  polygon2(x, ypoly(data[[spp]]$seed_prop_surplus),col="coral1")

  legend(25, 0.875, "Leaf:", bty="n", pch=NA, xpd=NA, xjust = 0)
  legend(35, 0.8, bty="n", xpd=NA, xjust = 0, pch =16,
         c("replacement", "expansion"), col=c("darkseagreen4", "darkseagreen2"))


  legend(25, 0.575, "Reproductive:", bty="n", pch=NA, xpd=NA)
  legend(35, 0.5, c("seed", "other"), bty="n", pch=16, col=c("coral1","coral3"), xpd= NA)

  mtext("Age (yr)",side=1,outer=TRUE,line=1.5, cex=1.25)
  mtext("Fraction of mass allocated",side=2,outer=TRUE,line=1.5, cex=1.25)
}


figure_allocation_demo_all <- function(SummarySppAge) {

  par(mfrow=c(14,4), cex=1, omi=c(1,1.5,.1,0.1), mai=c(0.2,.45,.3,0.02))

  data <- SummarySppAge[["mean"]]

  for(v in c("growth_leaf"))
    data[[v]][data[[v]] < 0] <- 0

  data <- split(data, data$species)

  for(spp in species_order()) {

    x <- c(1.4,2.4,5,7,9,32,32,1.4)
    if(spp %in% c("PILI")) x <- c(1.4,2.4,5,7,7,1.4)
    if(spp %in% c("PHPH"))  x <- c(2.4,5,7,9,32,32,2.4)
    if(spp %in% c("BOLE", "COER", "HEPU", "GRSP"))  x <- c(1.4,2.4,5,7,9,9,1.4)
    n <- length(x)-2
    y0 <- c(rep(1,n), 0, 0)

    myplot <- function(labelsx=FALSE, labelsy=FALSE){
      plot(NA,log="x",ylim=c(0,1),xlim=c(1.06*1.4,0.94*32),ylab="",yaxt="n", xaxt="n",xaxs="i", yaxs="i")
      axis(1, at= c(2,5, 10, 20), labels = labelsx, las=1)
      axis(2, at= c(0, 0.2, 0.4, 0.6, 0.8, 1.0), labels = labelsy, las=1)
      polygon2(c(1.4,32,32,1.4), c(1,1,0, 0), col="grey90")
      box()
    }

    i <- (spp %in% species_order()[14])
    myplot(i, TRUE)
    polygon2(x, y0, col="saddlebrown")
    polygon2(x, ypoly(data[[spp]]$replace_leaf_prop_no_accessory, n),col="darkseagreen4")
    polygon2(x, ypoly(data[[spp]]$growth_leaf_prop_no_accessory, n),
            col="darkseagreen2")
    polygon2(x, ypoly(data[[spp]]$seed_prop_all, n),col="coral1")
    mtext(labels.spp.full(spp), 2, line=4, font=3)

    myplot(i)
    polygon2(x, y0, col="saddlebrown")
    polygon2(x, ypoly(data[[spp]]$prop_leaf_replacement, n),col="darkseagreen4")
    polygon2(x, ypoly(data[[spp]]$prop_leaf_expand, n),col="darkseagreen2")
    polygon2(x, ypoly(data[[spp]]$prop_repro, n),col="coral3")
    polygon2(x, ypoly(data[[spp]]$seed_prop_all_acc, n),col="coral1")

    myplot(i)
    polygon2(x, y0, col="darkseagreen4")
    polygon2(x, ypoly(data[[spp]]$repro_and_leaf_growth_prop_surplus, n),col="darkseagreen2")
    polygon2(x, ypoly(data[[spp]]$repro_prop_all_leaf, n),col="coral3")
    polygon2(x, ypoly(data[[spp]]$seed_prop_veg, n),col="coral1")

    myplot(i)
    polygon2(x, y0, col="darkseagreen2")
    polygon2(x, ypoly(data[[spp]]$RA_max_1, n),col="coral3")
    polygon2(x, ypoly(data[[spp]]$seed_prop_surplus, n),col="coral1")
  }

  mtext("Age (yr)",side=1,outer=TRUE,line=2, cex=2)
  mtext("Fraction of mass allocated",side=2,outer=TRUE,line=4, cex=2)
}

# Fits a GLM with binomial to RA as function of age
# Also extract point at which RA = 0.5. Given the 
# equation for logistic curve R = 1/(1+exp(- (B_0 + B1 * x))), 
# we can solve for the point where R  = 0.5 as x0 = -B_0/ B_1

fit_allocation_model <- function(SummaryInd) {

  data <- split(SummaryInd, SummaryInd$species)

  fits <- list()

  for(spp in species_order()) {
    y <- data[[spp]][["RA_max_1"]]
    x <- data[[spp]][["age"]]
    fit <- glm (y ~ x, family = binomial)
    p50 <- coef(fit)[1]/-coef(fit)[2]
    fits[[spp]] <- list(fit = fit, x = x, y = y, spp = spp, p50 = p50)
  }
  fits
}


figure_allocation_all_a <- function(fits) {

  par_all_species_fig()


  for(spp in species_order()) {

    x <- c(1.4,2.4,5,7,9,32,32,1.4)
    if(spp %in% c("PILI")) x <- c(1.4,2.4,5,7,7,1.4)
    if(spp %in% c("PHPH"))  x <- c(2.4,5,7,9,32,32,2.4)
    if(spp %in% c("BOLE", "COER", "HEPU", "GRSP"))  x <- c(1.4,2.4,5,7,9,9,1.4)
    n <- length(x)-2

    plot(NA,log="x",ylim=c(-0.05,1.05),xlim=c(1.25,35),yaxt="n",xaxt="n",xaxs="i",yaxs="i")
    axis(2, at=c(0,0.2,.4,.6,.8,1), las=1, labels = (spp %in% species_order()[c(1,6,11)]))
    axis(1, at=c(2,5,10,20), labels = (spp %in% species_order()[10:14]))
    box()

    points(fits[[spp]][["x"]], fits[[spp]][["y"]], pch=16, col=col.age(fits[[spp]][["x"]]))
    points(fits[[spp]][["p50"]], 0.5, pch=16, col="black", cex=0.5)
    lines(rep(fits[[spp]][["p50"]],2), c(0,0.5), lty="dashed", col="black")


    x.pred <- seq_log_range(range(fits[[spp]][["x"]]), 100)
    lines(x.pred, predict(fits[[spp]][["fit"]], list(x = x.pred),type="response"), col = "black")
    r2_inset(glm_r2(fits[[spp]][["fit"]]))
    species_labels(spp)
  }
  mtext("Age (yr)",side=1,outer=TRUE,line=2, cex=1.5)
  mtext("Reproductive allocation",side=2,outer=TRUE,line=2, cex=1.5)

  legend(per_x(1.05), per_y(0.8), "Sites Ages:", bty="n", xpd= NA)
  legend(per_x(1.15), per_y(0.70), names(col.age()), bty="n", pch=16, col=col.age(), xpd= NA)
}

figure_allocation_all_h <- function(SummaryInd) {

  par_all_species_fig()

  data <- split(SummaryInd, SummaryInd$species)

  for(spp in species_order()) {

    y <- data[[spp]][["RA_max_1"]]
    x <- data[[spp]][["height"]]
    plot(NA, log="x",xlim= c(50, 4700), ylim=c(-0.05,1.05), yaxt="n",xaxt="n",xaxs="i",yaxs="i")
    points(y~x, col=col.age(data[[spp]][["age"]]), pch=16)

    fit <- glm (y ~ x, family = binomial)
    x.pred <- seq_log_range(range(x), 100)
    lines(x.pred, predict(fit, list(x = x.pred),type="response"), col = "black")

    r2_inset(glm_r2(fit))

    x <- c(0.125, 0.25, 0.5, 1, 2,5,10,20, 40)
    axis(1, at = c(15, 30, 60 , 125, 250, 500, 1000, 2000, 4000), labels = spp %in% species_order()[10:14])
    axis(2, label = spp %in% species_order()[c(1,6,11)], las=1)
    species_labels(spp)
  }

  legend(per_x(1.05), per_y(0.8), "Sites Ages:", bty="n", xpd= NA)
  legend(per_x(1.15), per_y(0.70), names(col.age()), bty="n", pch=16, col=col.age(), xpd= NA)

  mtext("Height (mm)",side=1,outer=TRUE,line=2, cex=1.5)
  mtext("Reproductive allocation",side=2,outer=TRUE,line=2, cex=1.5)

  # legend(40, 0.8, c("Individual observation", "Leaf expansion", "Reproductive tissues"), bty="n",
  #   pch=16, col=c("black", "darkseagreen2","coral3"), xpd= NA, cex=1.25)
}

figure_life_history <- function(SummarySpp, SummarySppAge, fits){

  traits <- SummarySpp[["mean"]]
  traits$maxH <- SummarySpp[["max"]]$height

  p50 <- sapply(fits, "[[", "p50")
  names(p50) <- names(fits)

  data <- traits %>%
    mutate(p50 = p50[species]) %>%
    filter(species != "PEPU")

  par(mfrow=c(1,2), cex=1, omi=c(0.9,0.9,0.2,0.2), mai=c(.3,.3,0.2,0.02))

  myplot <- function(x,y, data, log="x", ylab = NULL, xlab = NULL, py = 0.95){

    if(x == "maxH"){
      xlim <- c(250, 4000)
    }else {
      xlim <- c(0.005, 0.1)
    }

    if(y == "p50"){
      ylim <- c(1, 20)
    }else {
      ylim <- c(0, 1)
    }

    plot(data[[x]], data[[y]], log="xy", axes=FALSE, ann=FALSE, xlim=xlim, ylim = ylim, pch=16)
    axis(1, labels = !is.null(xlab))
    axis(2, labels = !is.null(ylab), las=1)
    box()
    if(!is.null(ylab))
      mtext(ylab, side=2, line=4, cex=1.25)
    if(!is.null(xlab))
      mtext(xlab, side=1, line=4, cex=1.25)

    fit <- glm (log(data[[y]])~log(data[[x]]), family = gaussian)
    alpha <- coef(fit)
    x.pred <- seq_log_range(range(data[[x]], na.rm=TRUE), 100)
    lines(x.pred, exp(alpha[1] + alpha[2]*log(x.pred)), col = "black")

    r2_inset(glm_r2(fit), cex=1, py = py)
  }

  myplot("LMA", "p50", data, ylab = "Age where RAAR = 0.5 (yr)", xlab = expression(paste("Leaf  mass per area (", mg~mm^-2, ")")))
  myplot("maxH", "p50", data, xlab = expression(paste("Maximum height (", mm, ")")))
}


figure_LMA_leafloss <- function(SummarySpp){

  par(mfrow=c(1,1), cex=1, omi=c(0.9,0.9,0.2,0.2), mai=c(.3,.3,0.2,0.02))

  myplot <- function(x,y, data, log="x", ylab = NULL, xlab = NULL, py = 0.95){

    ylim <- c(0.2, 1)
    xlim <- c(0.005, 0.1)

    plot(data[[x]], data[[y]], log="xy", axes=FALSE, ann=FALSE, xlim=xlim, ylim = ylim, pch=16)
    axis(1, labels = !is.null(xlab))
    axis(2, labels = !is.null(ylab), las=1)
    box()
    if(!is.null(ylab))
      mtext(ylab, side=2, line=4, cex=1.25)
    if(!is.null(xlab))
      mtext(xlab, side=1, line=4, cex=1.25)

    fit <- glm (log(data[[y]])~log(data[[x]]), family = gaussian)
    alpha <- coef(fit)
    x.pred <- seq_log_range(range(data[[x]], na.rm=TRUE), 100)
    lines(x.pred, exp(alpha[1] + alpha[2]*log(x.pred)), col = "black")

    r2_inset(glm_r2(fit), cex=1, py = py)
  }
  myplot("LMA", "prop_leaf_loss", SummarySpp[["mean"]], ylab = "Proportion of leaves lost", xlab = expression(paste("Leaf  mass per area (", mg~mm^-2, ")")))
}

figure_investment_weight <- function(SummaryInd) {

  data <- SummaryInd

  data <- subset(data,!(data$individual %in% c("COER_806","EPMI_907","GRBU_906","HATE_105","HATE_003","LEES_354",
                                               "LEES_352","LEES_355","LEES_353","LEES_351","PELA_161","PELA_162",
                                               "PUTU_108")))
  par_all_species_fig()

  data <- split(data, data$species)

  cols <- c("darkseagreen4", "darkseagreen2", "coral3")
  vars <- c("leaf_replacement", "growth_leaf", "repro_inv")
  labs <- c("Leaf replacement", "Leaf expansion", "Reproductive tissues")

  for(spp in species_order()) {
    y_all <- unlist(data[[spp]][vars])
    y_max <-0.95e6
    y_min <-0.01
    y_0 <- y_min*3.16

    x_max <- 4750
    x_min <- 20

    plot(NA,type="n",log="xy",xlab="",ylab="", xaxs="i",yaxs="i", axes=FALSE,
      ylim=c(y_min,y_max),xlim=c(x_min,x_max))
    polygon(x=c(x_min,x_min,x_max,x_max),y=c(y_min*10,y_min,y_min,y_min*10),col="grey90")

    x <- data[[spp]][["height"]]

    for(i in 1:3){
      y <- data[[spp]][[vars[i]]]
      y[y<=0] <- y_0
      points(x,y,pch=16, col=cols[i])
    }

    i <- data[[spp]]$leaf_shed > data[[spp]]$leaf_replacement

    arrows(data[[spp]]$height[i], data[[spp]]$leaf_replacement[i],
        data[[spp]]$height[i],data[[spp]]$leaf_shed[i],
               length=0,col="black")

    species_labels(spp)

    axis(1, at = c(15, 30, 60 , 125, 250, 500, 1000, 2000, 4000), labels = spp %in% species_order()[10:14])
    flag <- spp %in% species_order()[c(1,6,11)]
    add_axis_log10(2, at = -0:6, labels = flag)
    axis(2, at = y_0, labels = if(flag) c("0") else FALSE, las=1)

    box()
    }

  legend(per_x(1.05), per_y(0.8), bty="n", xpd= NA, cex=1.25,
    c(rev(labs), "Replacement deficit"), pch=c(16,16,16, NA), col= c(rev(cols), "black"), lty = c(NA,NA,NA, "solid"))
  mtext(expression(paste("Investment (", mg~yr^-1, ")")), 2, outer=TRUE,cex=1.5,line=2)

  mtext("Plant height (mm)",1,outer=TRUE,cex=1.5,line=2)
}


figure_leaf_weight <- function(Growth_all){

  par_all_species_fig()

  data <- Growth_all

  data <- split(data, data$species)

  for(spp in species_order()) {

    plot(leaf_weight~age,data[[spp]],pch=16,log="xy",
    xlim=c(0.05,40),ylim=c(0.8E0,1.5E6),col=col.age(age),axes=FALSE,ylab="n",xaxs="i",yaxs="i")

    fit <- fit_gam_loglog("leaf_weight", "age", data[[spp]])
    age.r <- seq_log_range(range(data[[spp]][["age"]], na.rm=TRUE), 50)
    points(age.r, y_hat(fit, age.r), type = "l")
    r2_inset(gam_r2(fit))

    x <- c(0.125, 0.25, 0.5, 1, 2,5,10,20, 40)
    axis(1, at=x[c(2,5,8)], labels= spp %in% species_order()[10:14])
    add_axis_log10(2, label = spp %in% species_order()[c(1,6,11)])

    box()
    species_labels(spp)
  }

  legend(per_x(1.05), per_y(0.8), "Sites Ages:", bty="n", xpd= NA)
  legend(per_x(1.15), per_y(0.70), names(col.age()), bty="n", pch=16, col=col.age(), xpd= NA)

  mtext("Age (yr)",side=1,outer=TRUE,line=2, cex=1.5)
  mtext(expression(paste("Leaf weight (", mg, " )")),side=2,outer=TRUE,line=2, cex=1.5)
}


figure_height <- function(Growth_all){

  par_all_species_fig()

  data <- Growth_all

  data <- split(data, data$species)

  for(spp in species_order()) {

    plot(height~age,data[[spp]],pch=16,log="xy",
    xlim=c(0.08,40),ylim=c(0.8E1, 5E3),col=col.age(age),axes=FALSE,ylab="n",xaxs="i",yaxs="i")

    fit <- fit_gam_loglog("height", "age", data[[spp]])
    age.r <- seq_log_range(range(data[[spp]][["age"]], na.rm=TRUE), 50)
    points(age.r, y_hat(fit, age.r), type = "l")

    x <- c(0.125, 0.25, 0.5, 1, 2,5,10,20, 40)
    axis(1, at=x[c(2,5,8)], labels= spp %in% species_order()[10:14])
    add_axis_log10(2, label = spp %in% species_order()[c(1,6,11)])
    box()
    r2_inset(gam_r2(fit))

    species_labels(spp)
  }

  legend(per_x(1.05), per_y(0.8), "Sites Ages:", bty="n", xpd= NA)
  legend(per_x(1.15), per_y(0.70), names(col.age()), bty="n", pch=16, col=col.age(), xpd= NA)

  mtext("Age (yr)",side=1,outer=TRUE,line=2, cex=1.5)
  mtext(expression(paste("Plant height (", mm, " )")),side=2,outer=TRUE,line=2, cex=1.5)
}

figure_leaf_loss <- function(SummarySppAge){
  par(mfrow=c(1,1), cex=1, omi=c(.6,.6,.02,.02), mai=c(0.02,.02,.01,0.01))

  data <- subset(SummarySppAge[["mean"]],age>2)

  plot(RA_max_1 ~ prop_leaf_loss, data,pch=16,col=col.age(age),xlim=c(0,1),ylim=c(0,1),xlab="",ylab="")
  mod <- lm(prop_leaf_loss~RA_max_1,data)
  extra.top.left(paste("r2 =",round(glance(mod)[2],digits=2)))

  mtext("Proportion of leaves lost annually",side=1,outer=TRUE,line=2)
  mtext("Reproductive allocation",side=2,outer=TRUE,line=2)
}

RA_examples_fns <- function(){

  RAS <- list()

  RAS[["Big bang"]] <- function(x, mat=0.5){
    y <- x*0 + 1
    y[x < mat] <- 0
    y
  }

  RAS[["Partial bang"]] <- function(x, mat=0.5, RAinit=0.5, RAmax=0.7, A50=0.05*mat){
    y <- RAinit + michaelis_menton(x-mat, A50=A50)*(RAmax - RAinit)
    y[x < mat] <- 0
    y
  }

  RAS[["Asymptotic"]] <- function(x, ...){
     RAS[["Partial bang"]](x, RAinit=0,...)
  }


  RAS[["Gradual - indeterminate"]] <- function(x, mat=0.5, A50=2*mat,  ...){
    RAS[["Partial bang"]](x, RAinit=0, mat=mat, A50=A50, ...)
  }

  RAS[["Gradual - determinate"]] <- function(x, mat=0.5, A50=0.5,  ...){
    RAS[["Partial bang"]](x, RAinit=0, RAmax=2, mat=mat, A50=A50, ...)
  }

  # based on formula for a parabola (x-h)^2 = 4p(y-k), with vertex (h,k)
  RAS[["Declining"]] <- function(x, mat=0.5, h=0.65, k=0.6, p=-0.1){
    y <- k + (x^2 - 2*h*x +h^2)/(4*p)
    y[x < mat] <- 0
    y
  }
  RAS
}

rectagular_hyperbolae <- function(X,  Amax, QY, theta, phi){
  (phi*X + Amax -  pow((phi*X + Amax)^2 - 4*theta*X*Amax*phi, 0.5))/ (2 * theta)
}

michaelis_menton <- function(x, A50, Amax=1){
  Amax * x / (x+A50)
}


plotRASexamples <- function(){

  RAS <- RA_examples_fns()

  par(oma = c(3,3,1,1), mar = c(2,3,2,2), mfrow=c(1,3))

  new_plot <- function(ylab=TRUE){
    plot.new()
    plot.window(xlim=c(0, 1), ylim=c(-0,1),  xaxt='n', yaxt='n', ann=FALSE)
    axis(2, at = c(0,1), labels =ylab, las = 1)
    axis(1, at = c(0.1,0.9), labels = c("young", "old"), las = 1)

    box()
  }

  col <- venetian_red
  x<-seq(0,1, by=0.001)

  for(i in 1:3) {

    new_plot()
    if(i ==1)
      y <- RAS[["Big bang"]](x, mat=0.5)
    if(i==2)
      y <- RAS[["Gradual - determinate"]](x, mat=0.5)
    if(i==3)
      y <- RAS[["Gradual - indeterminate"]](x, mat=0.5)
    points(x, y, type='l', lwd=2, col=col)
    text(-0.15, 1.2, sprintf("(%s)", letters[i]), cex=1.5, xpd=NA)
  }

  mtext("Plant age",1, 1, outer =  TRUE)
  mtext("Reproductive allocation", 2, 1, outer = TRUE,)

}
