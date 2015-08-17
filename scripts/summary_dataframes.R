
  SummaryInd <- SummaryInd %>% mutate(
    prop_maxH = height / maxH_spp,
    prop_max_weight = total_weight / max_total_weight,
    prop_max_repro = ReproInv / max_repro_inv,
    scaled_growth_stem_diam = growth_stem_diam / mean_stem_diam_spp,
    scaled_growth_shoot_diam = growth_shoot_diam / mean_shoot_diam_spp
    ) 
