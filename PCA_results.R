
* This combination of variables shows that RGR is approximately opposite a collection of variables that indicate a plant is "bigger" - so smaller plants have higher RGR
* Orthoganal to these in WD vs. maxH, LMA_mean, seed_size, and shoot_growth - taller plants have bigger seeds, higher LMA, and greater shoot growth (both stem growth & leaf count), independent of their actual size
* fine tuning above; no accessory proportions, because then have to use subset of reproducing individuals - see below
* remove RGR leaf (similar to RGR)
* remove prop_max_weight and prop_max_repro (similar to prop_maxH)

par(mfrow=c(1,1), cex=1, omi=c(.5,0.3,.1,0.2), mai=c(1.1,1,.6,0.2))
SummaryIndPCA <- SummaryInd %>%
  filter(lvs_end_total>0) %>%
  select(prop_maxH,height,maxH_spp,WD_mean,LMA_mean,seed_size,growth_leaf,
         RGR,leaf_area,growth_shoot_area,growth_stem_area,lvs_end_total,total_weight,
         RA,seedset,repro_all_count,accessory_inv,propagule_inv)

SummaryIndPCA$leaf_area <- log((SummaryIndPCA$leaf_area)+.001)
SummaryIndPCA$total_weight <- log(SummaryIndPCA$total_weight)
SummaryIndPCA$height <- log(SummaryIndPCA$height)
SummaryIndPCA$maxH_spp <- log(SummaryIndPCA$maxH_spp)
SummaryIndPCA$seed_size <- log(SummaryIndPCA$seed_size)
SummaryIndPCA$accessory_inv <- log(SummaryIndPCA$accessory_inv+.001)
SummaryIndPCA$propagule_inv <- log(SummaryIndPCA$propagule_inv+.001)
SummaryIndPCA$growth_shoot_area <- log(SummaryIndPCA$growth_shoot_area)
SummaryIndPCA$growth_leaf <- log(SummaryIndPCA$growth_leaf+10000)
SummaryIndPCA$repro_all_count <- log(SummaryIndPCA$repro_all_count+1)
SummaryIndPCA$lvs_end_total <- log(SummaryIndPCA$lvs_end_total)
SummaryIndPCA$growth_stem_area <- log(SummaryIndPCA$growth_stem_area)

PCA_mod <- prcomp(SummaryIndPCA, scale=TRUE)
summary(PCA_mod)
biplot(PCA_mod,xlim=c(-.1,.15),ylim=c(-.1,.1),cex=.8)
plot(PCA_mod)
pairs(SummaryIndPCA,log="xy")

#just looking at reproducing individuals - changes patterns quite a bit, with variables like maxH_spp pulling toward all the "big size" arrows
SummaryIndPCA <- SummaryInd %>%
  filter(lvs_end_total>0&accessory_inv>0) %>%
  select(prop_maxH,maxH_spp,WD_mean,LMA_mean,seed_size,growth_leaf,
         RGR,leaf_area,growth_shoot_area,growth_stem_area,lvs_end_total,total_weight,
         RA,seedset,repro_all_count,accessory_inv,prop_accessory)

