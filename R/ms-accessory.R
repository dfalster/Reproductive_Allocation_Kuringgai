figure_accessory_bar <- function(SummarySpp) {

  data <- SummarySpp[["mean"]] %>%
          select(prop_prepollen_discarded_vs_all_repro,
                 prop_postpollen_discarded_vs_all_repro,
                 prop_pollen_attract_vs_all_repro,
                 prop_pack_disp_vs_all_repro,
                 prop_embryo_endo_vs_all_repro
                 )

  data <- t(as.matrix(data))
  colnames(data) <- SummarySpp[["mean"]]$species

  cols <- c("darkslategray4","darkslategray2", "black", "darkseagreen3","darkseagreen4")
  # sapply(cols, function(i) rgb(t(col2rgb(i))/255))
  labs <- c("Pollen attraction", "Packaging & dispersal", "Pollen attraction", "Packaging & dispersal", "Mature seeds")

  par(oma=c(0,0,0,10), mar=c(4,4,1,0))
  barplot(data, beside=FALSE,width=0.8,cex.axis=1,cex.names=0.7,las=1, col=cols,
          ylab="Proportion of total mass (0-1)", xlab = "Species", border=NA)
  legend(13.5,1,title = "Successfully matured:", legend=rev(labs)[1:3],col=rev(cols)[1:3], pch=16, bty="n", xpd=NA)
  legend(13.5,0.75,title = "Failed or discarded:", legend=rev(labs)[4:5],col=rev(cols)[4:5], pch=16, bty="n", xpd=NA)

}

figure_accessory_v_seed_size <- function(SummarySpp) {

  data <- SummarySpp[["mean"]]

  par(mfrow = c(3, 2), oma = c(2, 1, 1, 0), mar = c(5, 5, 2,2))

  XLIM <- c(0.008, 50)
  x.pred <- seq_log_range(range(data$embryo_endo_costs), 50)
  x.pred2 <- seq_log_range(range(data$seedset), 50)


  y.pred <- function(out, x) {
    predict(out, list(embryo_endo_costs = x),type="response")
  }
  
  y.pred2 <- function(out, x) {
    predict(out, list(prop_prepollen_success = x),type="response")
  }

  y.pred3 <- function(out, x) {
    predict(out, list(seedset = x),type="response")
  }

  legend_text <- function(out) {
    sprintf("R2 = %s, p %s", format_r2(R2_glm(out)), format_p2(coef(summary(out))[,4][2]))
  }

  px <- -0.25
  py  <- 1.15

  # panel - choosiness; lower seedset species invest lower proportion of success costs in pollen attraction tissues
  plot(prop_pollen_attract_vs_success ~ seedset, data, log = "x",
       col = venetian_red, pch = 16, xlim = c(1E-3, 1), ylim = c(0, 1),
       cex = 1, ylab = "", xlab = "", xaxt = "n", yaxt = "n")
  extra.top.left.logx("A", px = px, py = py, font=2)
  extra.top.left.logx("Division within successfully-matured units", px = px +0.05, py =py)
  add_axis_proprtion(2, las=1)
  add_axis_log10(1)

  mtext("Prop. to pollen-attraction", 2, line = 3)
  mtext("Ratio of seeds:ovules", 1, line = 3)
  
  out <- glm(prop_pollen_attract_vs_success ~ log10(seedset),
             family=gaussian(link="logit"), data =  data)
  lines(x.pred2, y.pred3(out, x.pred2), col = venetian_red)

  extra.bottom.left.logx(legend_text(out), px=0.70, py=0.05, cex=0.75, font=1)
  
  plot.new()

  # panel - choosiness; big seeded species have lower seedset
  plot(seedset ~ embryo_endo_costs, data, log = "xy",
       col = venetian_red, pch = 16,xlim = XLIM, ylim = c(1E-3, 1), yaxt = "n",
       cex = 1, ylab = "", xlab = "", xaxt = "n")
  extra.top.left.logxy("B", px = px, py = py, font=2)
  extra.top.left.logxy("Seedset", px = px +0.05, py =py)
  add_axis_log10(1)
  add_axis_log10(2)
  mtext("Ratio of seeds:ovules", 2, line = 3)
  
  out <- glm(log10(seedset) ~ log10(embryo_endo_costs), data = data)
  lines(x.pred, 10^y.pred(out, x.pred), col = venetian_red)
  extra.bottom.left.logxy(legend_text(out), px=0.70, py=0.05, cex=0.75, font=1)  

   # panel - success proportions; big seeded species invest more pollen-attraction energy into accessory tissues not associated with successful ovules
  plot(prop_prepollen_success ~ embryo_endo_costs, data, pch = 16,
    log = "x", col = venetian_red, ylim = c(0, 1),xlim = XLIM, xlab = "",
    ylab = "", xaxt = "n", yaxt = "n")
  extra.top.left.logx("C", px = px, py = py, font=2)
  extra.top.left.logx("Division of pollen-attraction resources", px = px+0.05, py = py)

  add_axis_proprtion(2, las=1)
  add_axis_log10(1)
  mtext("Prop. to successful units", 2, line = 3)

  out <- glm(prop_prepollen_success ~ log10(embryo_endo_costs),
             family=gaussian(link="logit"), data =  data)
  lines(x.pred, y.pred(out, x.pred), col = venetian_red)
  extra.bottom.left.logx(legend_text(out), px=0.70, py=0.05, cex=0.75, font=1)

  # panel - provisioning proportions; ; big seeded species invest more provisioning energy into accessory tissues associated with successful ovules
  plot(prop_postpollen_success ~ embryo_endo_costs, data, pch = 16,
    log = "x", col = venetian_red, ylim = c(0, 1.0),xlim = XLIM, xlab = "",
    ylab = "", xaxt = "n", yaxt = "n")
  extra.top.left.logx("D", px = px, py = py, font=2)
  extra.top.left.logx("Division of provisioning resources", px = px +0.05, py = py)
  add_axis_proprtion(2)
  add_axis_log10(1)
  mtext("Prop. to successful units", 2, line = 3)
  mtext("Seed size (mg)", 1, line = 3)

  out <- glm(prop_postpollen_success ~ log10(embryo_endo_costs),
             family=gaussian(link="logit"), data =  data)
  lines(x.pred, y.pred(out, x.pred), col = venetian_red)
  extra.bottom.left.logx(legend_text(out), px=0.70, py=0.05, cex=0.75, font=1)

  # panel - provisioning proportions; seed size vs. proportion of success investment that goes to provisioning
  plot(prop_provisioning_vs_success ~ embryo_endo_costs, data,
    pch = 16, log = "x", col = venetian_red, ylim = c(0, 1.0),xlim = XLIM,
    xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  extra.top.left.logx("E", px = px, py = py, font=2)
  extra.top.left.logx("Division within successfully-matured units", px = px +0.05, py = py)
  add_axis_log10(1)
  add_axis_proprtion(2)
  mtext("Prop. to seed provisioning", 2, line = 3)
  mtext("Seed size (mg)", 1, line = 3)

  out <- glm(prop_provisioning_vs_success ~ log10(embryo_endo_costs),
             family=gaussian(link="logit"), data =  data)
  lines(x.pred, y.pred(out, x.pred), col = venetian_red)
  extra.bottom.left.logx(legend_text(out), px=0.70, py=0.05, cex=0.75, font=1)
}
  

figure_scaling_costs_seed_size <- function(SummarySpp) {

  par(mfrow = c(1,1), oma = c(2, 2, 0,8), mar = c(3, 3, 1, 1))

  plot(NA,log = "xy", xlim = c(0.01, 100), ylim = c(0.01, 20000), xlab = "",
    ylab = "", xaxt = "n", yaxt = "n",axes=FALSE)
  add_axis_log10(1)
  add_axis_log10(2)
  abline(0, 1, lty="dashed")

  box()
  mtext("Seed size (mg)", 1, line = 3)
  mtext("Cost per seed (mg)", 2, line = 3)

  mod_repro_costs <- sma(repro_costs ~ embryo_endo_costs, SummarySpp[["mean"]], log = "xy",
    method = "SMA")
  plot(mod_repro_costs, col="black", pch =16, add=TRUE)
  mod_pollen_attract_costs <- sma(pollen_attract_costs ~ embryo_endo_costs, SummarySpp[["mean"]],
    log = "xy", method = "SMA", slope.test = mod_repro_costs$groupsummary$Slope_lowCI)
  plot(mod_pollen_attract_costs, col=cerulean, pch =16, add=TRUE)

  mod_provisioning_costs <- sma(provisioning_costs ~ embryo_endo_costs, SummarySpp[["mean"]],
    log = "xy", method = "SMA", slope.test = mod_repro_costs$groupsummary$Slope)
  plot(mod_provisioning_costs, col=venetian_red, pch =16, add=TRUE)

 legend(2e2, 1e4,legend=c("All", "Pollen attraction", "Provisioning"),
  col=c("black", cerulean, venetian_red), pch=16, bty="n", xpd=NA)
}


figure_proxy <- function(SummaryInd) {

  par(mfrow = c(1, 2), oma = c(2, 3, 2, 6), mar = c(3, 2, 1, 2))

  yvar <- "repro_inv"

  for(i in 1:2) {
    if(i==1) {
      xvar <- "embryo_endo_inv"
      xlab <- "Embryo and endosperm investment (mg)"
    } else {
      xvar <- "flower_inv"
      xlab <- "Flower weight x bud count (mg)"
    }

    data <- subset(SummaryInd, SummaryInd[[xvar]] > 0 & SummaryInd[[yvar]] > 0)

    fit <- sma(data[[yvar]] ~ data[[xvar]] * data[["species"]], method = "OLS", log = "xy")
    plot(fit, col = col.spp(fit$groupsummary$group), log = "xy", ylim = c(0.3, 3e+05),
      xlim = c(0.3, 3e+05), xlab = "", ylab = "", pch = 16, axes=FALSE)
    add_axis_log10(1)
    add_axis_log10(2)
    abline(0, 1, lty="dashed")

    mod <- sma(data[[yvar]] ~ data[[xvar]], log = "xy", method = "OLS")
    mtext(text = xlab, side = 1, line = 3)
    extra.top.left.logxy(LETTERS[i],px =-0.15, py = 1.15, cex=1.25)
    if(i==1)
      mtext(text = "Total  reproductive investment (mg)", side = 2, line = 3)

    if(i==2)
      legend(1e6,8e5,title = "Species:", legend=fit$groupsummary$group,col=col.spp(fit$groupsummary$group),
        pch=16, bty="n", xpd=NA)
  }
}
