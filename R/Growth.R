preprocessHarvest <- function(HarvestData_raw, IndividualsList) {

  keep <- filter(IndividualsList, use_for_allocation_calculations)$individual

  data <- filter(HarvestData_raw, individual %in% keep) %>%
          group_by(species, individual, site, age, start_end)

  segments <- list()
  segments[["1"]] <- c(as.character(1:8), "1.2", "1.3", "1.2.1", "1.2.1.1", "1.1.2", "1.1.2.1", "1.1.1.2")
  segments[["2"]] <- c(as.character(2:8), "1.1.2", "1.1.2.1", "1.1.1.2")
  segments[["3"]] <- c(as.character(3:8), "1.1.1.2")
  segments[["4"]] <- as.character(4:8)
  segments[["5"]] <- as.character(5:8)
  segments[["6"]] <- as.character(6:8)
  segments[["7"]] <- as.character(7:8)
  segments[["8"]] <- as.character(8)
  segments[["1.2"]] <- c("1.2", "1.2.1", "1.2.1.1")
  segments[["1.3"]] <- c("1.3")
  segments[["1.2.1"]] <- c("1.2.1", "1.2.1.1")
  segments[["1.2.1.1"]] <- c("1.2.1.1")
  segments[["1.1.2"]] <- c("1.1.2", "1.1.2.1")
  segments[["1.1.2.1"]] <- c("1.1.2.1")
  segments[["1.1.1.2"]] <- c("1.1.1.2")

  ## Get total mass of all material subtended by given node
  get.mass.at.node <- function(segment_name, data) {
    data %>%
      filter(segment %in% segments[[segment_name]]) %>%
      summarise(
          segment = segment_name,
          leaf_weight = sum(leaf_weight),
          stem_weight = sum(stem_weight),
          total_weight = sum(leaf_weight, stem_weight))
  }

  get.mass.at.node.by.segment <- function(data) {
    lapply(names(segments), get.mass.at.node, data=data)  %>% rbind_all()
  }

  # Get average diameter for base of segment. This is given by diameter readings for segment with names in list above
  get.diam.node.above <- function(level, data) {
    data %>%
      filter(segment == level) %>%
      group_by(individual) %>%
      summarise(
        segment = level,
        diameter = mean(c(diameter_1, diameter_2, diameter_3), na.rm = TRUE),
        stem_area = diameter^2 * pi/4,
        height = ifelse(segment==1, height, NaN),
        shed_weight = ifelse(segment=="shed",stem_weight, 0))
  }

  get.diam.node.above.by.segment <- function(data) {
    lapply(names(segments),  get.diam.node.above, data=data)  %>% rbind_all()
  }

  mass <-  do(data, get.mass.at.node.by.segment(.))
  diameters <- do(data, get.diam.node.above.by.segment(.))

  # Merge mass and diameter measurements
  merge(diameters, mass, by = c("species", "site", "individual", "age", "segment", "start_end")) %>%
   arrange(species, site, individual, age, segment, desc(start_end))
}


growth_calculations <- function(thisSpecies, HarvestData, IndividualsList) {

  individuals <- filter(IndividualsList, use_for_fit & alive)$individual
  HarvestData_basal <- filter(HarvestData,  individual %in% individuals & segment == 1) %>%
                      mutate(start_end=ifelse( site == "baby", "end", start_end),
                            age=ifelse( start_end == "start", age-1, age))
  HarvestData_basal_end  <- filter(HarvestData_basal, start_end=="end")

individuals_2 <- filter(IndividualsList, use_for_allocation_calculations & alive)$individual
HarvestData_basal_2 <- filter(HarvestData,  individual %in% individuals_2 & segment == 1) %>%
                       mutate(start_end=ifelse( site == "baby", "end", start_end),
                       age=ifelse( start_end == "start", age-1, age))
HarvestData_basal_end_2  <- filter(HarvestData_basal_2, start_end=="end")

  ### Calculate Regression coefficients based on common slope and intercept by the basal diameter at year 2013
  ## allow intercept to vary by individual
  getend <- function(thisindividual, df, v) {
    df[df$individual==thisindividual,v][1]
  }


  # Function for fitting a gam on log scaled values of X and Y
  # k: the dimension of the basis used to represent the smooth term.
  fit_gam_loglog <- function(Y, X, df, k=4){
    df <- subset(df, df[[Y]] != 0 & df[[X]]!=0)
    df$Y <- log(df[[Y]])
    df$X <- log(df[[X]])
    gam(Y ~ s(X, k=k), data=df)
  }

  y_hat <- function(fit, X, ...){
    exp(predict(fit, data.frame(X=log(X))))
  }

  predict_start_Y <- function(fit, Y2, X1, X2) {
    Y2*(y_hat(fit, X1)/y_hat(fit, X2))
  }

  # fit stem equation
  fit.s <- fit_gam_loglog("stem_weight", "diameter", HarvestData_basal_end)

  # fit leaf equation
  fit.l <- fit_gam_loglog("leaf_weight", "age", HarvestData_basal_end)


  ## Estimate weight at beginning and end  (year 2012 and 2013) using fitted regression
  suppressWarnings(
    out <- HarvestData_basal_2 %>%
    group_by(individual) %>%
    mutate(
        diameter.end = getend(individual, HarvestData_basal_end_2, "diameter"),
        age.end = getend(individual, HarvestData_basal_end_2, "age"),
        stem.end = getend(individual, HarvestData_basal_end_2, "stem_weight"),
        leaf.end = getend(individual, HarvestData_basal_end_2, "leaf_weight"),
        leaf_weight_est = predict_start_Y(fit.l, leaf.end, age, age.end),
        stem_weight_est = predict_start_Y(fit.s, stem.end, diameter, diameter.end),
        growth_stem = c(NA, diff(stem_weight_est)),
        growth_leaf = c(NA, diff(leaf_weight_est)),
        growth_inv = growth_stem - growth_leaf,
        growth_height = c(NA, diff(height)),
        growth_stem_diameter = c(NA, diff(diameter)),
        growth_stem_area = c(NA, diff(stem_area))
        )
  )

  add_line <- function(df, x, y) {
    if(nrow(df) > 1)
      points(df[[x]], df[[y]], col="red", type="l", lwd=0.5)
    data.frame()
  }

  path <- "output/growth_plots"
  dir.create(path, FALSE, TRUE)
  pdf(sprintf("%s/%s.pdf", path, thisSpecies), width=6, height =3)
  on.exit(dev.off())

   suppressWarnings({
  par(mfrow=c(1,2), cex=1, oma=c(1,1,2,1), mar = c(4,4,0,1))
  plot(stem_weight~diameter,data=HarvestData_basal_2, pch=16,log="xy",col=col.age(age))
  dia.r <- seq_log_range(c(0.1, 50), 50)
  points(dia.r, y_hat(fit.s, dia.r), type="l")
  out %>% group_by(individual) %>% do(add_line(., "diameter", "stem_weight_est"))
  text(stem_weight~diameter,data=HarvestData_basal_2,labels=gsub(paste0(thisSpecies, "_"), "", individual),cex=.2)

  plot(leaf_weight~age,data=HarvestData_basal_2, pch=16,log="xy",col=col.age(age))
  age.r <- seq_log_range(c(0.08, 35), 50)
  points(age.r, y_hat(fit.l,  age.r), type="l")
  out %>% group_by(individual) %>% do(add_line(., "age", "leaf_weight_est"))
  mtext(thisSpecies, line = 1, outer = TRUE)
  text(leaf_weight~age,data=HarvestData_basal_2,labels=gsub(paste0(thisSpecies, "_"), "", individual),cex=.2)
})

  out %>%
    filter(start_end == "end")%>%
    select(species, start_end, site, individual, age, height, diameter, stem_area, leaf_weight, stem_weight, total_weight, growth_inv, growth_stem, growth_leaf, growth_height, growth_stem_diameter, growth_stem_area)
}
