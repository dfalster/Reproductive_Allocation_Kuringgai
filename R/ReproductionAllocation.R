RA_Calculations <- function(thisSpecies, Species_Investment, HarvestData, Maps, IndividualsList) {

  individuals <- filter(IndividualsList, use_for_allocation_calculations & alive)$individual
  HarvestData_basal <- filter(HarvestData,  individual %in% individuals & segment == 1)
  HarvestData_basal_end  <- filter(HarvestData_basal, start_end=="end")

  ### Calculate Regression coefficients based on common slope and intercept by the basal diameter at year 2013
  ## allow intercept to vary by individual

  lm.fit <- function(var, data) {
    lm(log(data[[var]]) ~ log(data[["dia"]]))
  }

  fit_total <- lm.fit("total_weight", HarvestData_basal_end)
  fit_stem <- lm.fit("stem_weight", HarvestData_basal_end)

  HarvestData_basal_end <- HarvestData_basal_end %>%
    mutate(
      slope_total  = as.numeric(coef( fit_total)[2]),
      intercept_total = log(total_weight) - slope_total * log(dia),
      slope_stem  = as.numeric(coef( fit_stem)[2]),
      intercept_stem = log(stem_weight) - slope_stem * log(dia))


  predict_total_weight <- function(dia, thisindividual) {
    x <- HarvestData_basal_end %>% filter(individual==thisindividual)
    exp(x$intercept_total + x$slope_total * log(dia))
  }

  predict_stem_weight <- function(dia, thisindividual) {
    x <- HarvestData_basal_end %>% filter(individual==thisindividual)
    exp(x$intercept_stem + x$slope_stem * log(dia))
  }

  ## Estimate weight at beginning and end  (year 2012 and 2013) using fitted regression
  GrowthEst <- HarvestData_basal %>%
    group_by(individual) %>%
    mutate(total_weight_est =  predict_total_weight(dia, individual[1]),
           GrowthInv = c(NA, diff(total_weight_est)),
           stem_weight_est = predict_stem_weight(dia, individual[1]),
           growth_stem = c(NA, diff(stem_weight_est)),
           growth_leaf =  GrowthInv - growth_stem,
           growth_stem_diam = c(NA, diff(dia)),
           growth_stem_area = c(NA, diff(stem.area))
           ) %>%
    filter(start_end == "end") %>%
    select(species, site, individual, age, height, dia, stem.area, leaf_weight, stem_weight, total_weight, GrowthInv, growth_stem, growth_leaf, growth_stem_diam, growth_stem_area)

    # Use saved data to calculate total reproduction investment per individual plant
    ReproTotal <- Species_Investment$Investment %>%
     group_by(individual) %>%
     summarise(
      ReproInv = sum(Total)
      )

  # Merge two tables and calculate total investment and RAR
  InvestmentSummary <- merge(ReproTotal, GrowthEst, by = "individual", all.y = TRUE)
  # NA that appeared correspond to zero reproductive investment
  InvestmentSummary[is.na(InvestmentSummary)] <- 0

  InvestmentSummary %>%
    mutate(
      TotalInvestment = ReproInv + GrowthInv,
      RA = ReproInv/TotalInvestment)
}
