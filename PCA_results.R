par(mfrow=c(1,1), cex=1, omi=c(.5,0.3,.1,0.2), mai=c(1.1,1,.6,0.2))
SummaryIndPCA <- SummaryInd %>%
  filter(lvs_end_total>0) %>%
  select(height,maxH_spp,WD_mean,LMA_mean,seedset,seed_size,growth_leaf,propagule_inv,prepollen_all_inv,RGR,leaf_area, seed_count,costs_per_seed,growth_shoot_area,growth_stem_area,lvs_end_total)

SummaryIndPCA <- SummaryInd %>%
  select(seedset,RA,growth_leaf,propagule_inv,prepollen_all_inv,RGR,leaf_area, seed_count,costs_per_seed,growth_shoot_area,growth_stem_area,ReproInv,lvs_end_total)

PCA_mod <- prcomp(SummaryIndPCA, scale=TRUE)
summary(PCA_mod)
biplot(PCA_mod)

View(SummaryIndPCA)


#This combination of variables shows that RGR is approximately opposite a collection of variables that indicate a plant is "bigger" - so smaller plants have higher RGR
#Orthoganal to these in WD vs. maxH, LMA_mean, seed_size, and shoot_growth - taller plants have bigger seeds, higher LMA, and greater shoot growth (both stem growth & leaf count), independent of their actual size
SummaryIndPCA <- SummaryInd %>%
  filter(lvs_end_total>0&prop_propagule!=0) %>%
  select(prop_maxH,height,maxH_spp,WD_mean,LMA_mean,seed_size,growth_leaf,RGR,RGR_leaf, leaf_area,growth_shoot_area,growth_stem_area,lvs_end_total,ReproInv,total_weight,RA,seedset,prop_propagule)


SummaryIndPCA <- SummaryInd %>%
  filter(lvs_end_total>0&prop_propagule!=0) %>%
  select(maxH_spp,WD_mean,LMA_mean,growth_leaf,RGR,total_weight,seedset,seed_count, RA, propagule_inv,prop_propagule)

SummaryIndPCA <- SummaryInd %>%
  filter(lvs_end_total>0&prop_propagule!=0) %>%
  select(height,maxH_spp,WD_mean,LMA_mean,seed_size,growth_leaf,RGR,leaf_area,growth_shoot_area,growth_stem_area,lvs_end_total,ReproInv,total_weight,seedset,seed_count, RA, propagule_inv,prop_propagule,prepollen_all_inv,prop_prepollen_all)



plot(WD_mean ~ LMA_mean, SummaryInd, pch=16, log="xy",col=col.spp[as.factor(species)])
mod <- lm(log(LMA_mean) ~log(WD_mean), SummaryInd)
words.bottom.left.logxy(mod)
