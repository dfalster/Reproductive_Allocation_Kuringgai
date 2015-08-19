RA_Calculations <- function(thisSpecies, Species_Investment, HarvestData, Maps, IndividualsList) {

  individuals <- filter(IndividualsList, use_for_allocation_calculations & alive)$individual
  HarvestData_basal <- filter(HarvestData,  individual %in% individuals & segment == 1)
  HarvestData_basal_end  <- filter(HarvestData_basal, start_end=="end")

  ### Calculate Regression coefficients based on common slope and intercept by the basal diameter at year 2013
  ## allow intercept to vary by individual

  lm.fit <- function(var, data) {
    lm(log(data[[var]]) ~ log(data[["diameter"]]))
  }

  fit_total <- lm.fit("total_weight", HarvestData_basal_end)
  fit_stem <- lm.fit("stem_weight", HarvestData_basal_end)

  HarvestData_basal_end <- HarvestData_basal_end %>%
    mutate(
      slope_total  = as.numeric(coef( fit_total)[2]),
      intercept_total = log(total_weight) - slope_total * log(diameter),
      slope_stem  = as.numeric(coef( fit_stem)[2]),
      intercept_stem = log(stem_weight) - slope_stem * log(diameter))


  predict_total_weight <- function(diameter, thisindividual) {
    x <- HarvestData_basal_end %>% filter(individual==thisindividual)
    exp(x$intercept_total + x$slope_total * log(diameter))
  }

  predict_stem_weight <- function(diameter, thisindividual) {
    x <- HarvestData_basal_end %>% filter(individual==thisindividual)
    exp(x$intercept_stem + x$slope_stem * log(diameter))
  }

  ## Estimate weight at beginning and end  (year 2012 and 2013) using fitted regression
  GrowthEst <- HarvestData_basal %>%
    group_by(individual) %>%
    mutate(total_weight_est =  predict_total_weight(diameter, individual[1]),
           growth_inv = c(NA, diff(total_weight_est)),
           stem_weight_est = predict_stem_weight(diameter, individual[1]),
           growth_stem = c(NA, diff(stem_weight_est)),
           growth_leaf =  growth_inv - growth_stem,
           growth_height = c(NA, diff(height)),
           growth_stem_diameter = c(NA, diff(diameter)),
           growth_stem_area = c(NA, diff(stem_area))
           ) %>%
    filter(start_end == "end") %>%
    select(species, site, individual, age, height, diameter, stem_area, leaf_weight, stem_weight, total_weight, growth_inv, growth_stem, growth_leaf, growth_height, growth_stem_diameter, growth_stem_area)

    # Use saved data to calculate total reproduction investment per individual plant
    ReproTotal <- Species_Investment$Investment %>%
     group_by(individual) %>%
     summarise(
      repro_inv = sum(Total)
      )

  # Merge two tables and calculate total investment and RAR
  InvestmentSummary <- merge(ReproTotal, GrowthEst, by = "individual", all.y = TRUE)
  # NA that appeared correspond to zero reproductive investment
  InvestmentSummary[is.na(InvestmentSummary)] <- 0

  InvestmentSummary %>%
    mutate(
      total_inv = repro_inv + growth_inv,
      RA = repro_inv/total_inv)
}
