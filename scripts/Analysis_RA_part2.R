traits <- as.data.frame(select(SummarySpp[["mean"]],species,RA_leaf_area,RA_max_1,RA_vs_all_leaf,embryo_endo_costs,LMA,wood_density,seed_count,lifespan,maturity,asymptotic_RA))
traits$maxH <- SummarySpp[["max"]]$height

temp <- subset(SummarySppAge[["mean"]], age >= asymptotic_RA)
temp <- temp %>%
  group_by(species) %>%
  summarise_each(funs(mean), RA_leaf_area,RA_max_1,RA_vs_all_leaf)
names(temp) <- c("species","RA_leaf_area_max","RA_max_1_max","RA_vs_all_leaf_max")

traits <- merge(traits,temp,by="species")
traits_4spp <- subset(traits,traits$species==c("HATE","EPMI","BOLE","GRBU"))


win.metafile("ms/RA/Figure_3_maxRA_correlates_v2.wmf", height=11, width=8)
plot
par(mfcol=c(3,2), cex=1, omi=c(.1,.7,.1,.1), mai=c(.8,.3,.1,0.2)) 
i <- traits$RA_leaf_area_max
#i <- traits$RA_max_1_max
#i <- traits$RA_vs_all_leaf_max
#for (i in c(traits$RA_leaf_area_max,traits$RA_max_1_max,traits$RA_vs_all_leaf_max)) {
  
  plot(i~traits$lifespan,pch=16,cex=1.5,log="x",xlab="",yaxt="n")
  mtext("lifespan (years)",1,outer=FALSE,line=2)
  axis(2, at=c(.4,.6,.8,1,1.2,1.4), labels=c(0.4,0.6,0.8,1.0,1.2,1.4),cex.axis=.9,las=1)
  # <- lm(log10(lifespan)~i,traits)
  #extra.bottom.left.logx(paste("r2=",round(glance(mod)[1],2),"; p-value:",round(glance(mod)[5],4)))
  mtext("maximum RA",2,outer=TRUE,line=1)
  
  plot(i~traits$maturity,pch=16,cex=1.5,log="x",xlab="",yaxt="n")
  mtext("age at maturity (years)",1,outer=FALSE,line=2)
  axis(2, at=c(.4,.6,.8,1,1.2,1.4), labels=c(0.4,0.6,0.8,1.0,1.2,1.4),cex.axis=.9,las=1)
  #mod <- lm(mature~log10(i),traits)
  #extra.bottom.left.logx(paste("r2=",round(glance(mod)[1],2),"; p-value:",round(glance(mod)[5],4)))
  
  plot(i~traits$embryo_endo_costs,pch=16,cex=1.5,log="x",xlab="",yaxt="n",xlim=c(.005,30),xaxt="n")
  mtext("embryo + endosperm weight (mg)",1,outer=FALSE,line=2)
  axis(2, at=c(.4,.6,.8,1,1.2,1.4), labels=c(0.4,0.6,0.8,1.0,1.2,1.4),cex.axis=.9,las=1)
  axis(1, at=c(.01,.1,1,10), labels=c(.01,.1,1,10),las=1)
  #mod <- lm(log10(embryo_endo_size)~i,traits)
  #extra.bottom.left.logx(paste("r2=",round(glance(mod)[1],2),"; p-value:",round(glance(mod)[5],4)))
  #legend("topleft",legend=labels.spp(),col=col.spp(),pch=16, cex=.6,bty="n")
  
  plot(i~traits$maxH,pch=16,cex=1.5,log="x",xlab="",yaxt="n")
  mtext("maximum height (mm)",1,outer=FALSE,line=2)
  #mod <- lm(log10(maxH)~i,traits)
  axis(2, at=c(.4,.6,.8,1,1.2,1.4), labels=c("","","","","",""),cex.axis=.8,las=1)
  #extra.bottom.left.logx(paste("r2=",round(glance(mod)[1],2),"; p-value:",round(glance(mod)[5],4)))
  
  plot(i~traits$LMA,pch=16,cex=1.5,xlab="",yaxt="n",log="x")
  mtext("LMA",1,outer=FALSE,line=2)
  axis(2, at=c(.4,.6,.8,1,1.2,1.4), labels=c("","","","","",""),cex.axis=.8,las=1)
  #mod <- lm(LMA~RA_leaf_area,traits)
  #extra.bottom.left(paste("r2=",round(glance(mod)[1],2),"; p-value:",round(glance(mod)[5],4)))
  
  plot(i~wood_density,traits,pch=16,cex=1.5,xlab="",yaxt="n")
  mtext("wood density",1,outer=FALSE,line=2)
  axis(2, at=c(.4,.6,.8,1,1.2,1.4), labels=c("","","","","",""),cex.axis=.8,las=1)
  #mod <- lm(wood_density~i,traits)
  #extra.top.left(paste("r2=",round(glance(mod)[1],2),"; p-value:",round(glance(mod)[5],4)))
  dev.off()
#}

write.csv(traits, file="figs2/RA/Table01_maxRA_correlates.csv",row.names=FALSE)

plot(RA_per_year ~ i,traits,col=col.spp(species),pch=16,cex=2,log="y")
mod <- lm(i~log10(RA_per_year),traits)
summary(mod)

plot(seed_count ~ i,traits,col=col.spp(species),pch=16,cex=2,log="y")

#new variables
``` {r}
SummaryInd$growth_leaf_fixed_max = SummaryInd$growth_leaf
SummaryInd$RA_fixed_max = SummaryInd$RA_leaf_area
for (i in 1:length(SummaryInd$individual)) {
  if (SummaryInd$RA_leaf_area[i] < 0) {
    SummaryInd$growth_leaf_fixed_max[i] = SummaryInd$repro_inv[i]*0.005
    SummaryInd$RA_fixed_max[i] = 1.1 
  }
  if (SummaryInd$RA_leaf_area[i] > 1) {
    SummaryInd$growth_leaf_fixed_max[i] = SummaryInd$repro_inv[i]*0.005
    SummaryInd$RA_fixed_max[i] = 1.1
  }
}

SummaryInd$leaf_inv_log <- 0
for (i in 1:length(SummaryInd$individual)) {
  if (SummaryInd$growth_leaf[i] > 0) {
    SummaryInd$leaf_inv_log[i] <- log10(SummaryInd$growth_leaf[i])
  }else{
    SummaryInd$leaf_inv_log[i] <- -(log10(-SummaryInd$growth_leaf[i]))  
  }
}

SummaryInd$repro_inv_log <- 0
for (i in 1:length(SummaryInd$individual)) {
  if (SummaryInd$repro_inv[i] > 0) {
    SummaryInd$repro_inv_log[i] <- log10(SummaryInd$repro_inv[i])
  }else{
    SummaryInd$repro_inv_log[i] <- 0  
  }
}

SummaryInd$leaf_repro_inv_log <- 0
for (i in 1:length(SummaryInd$individual)) {
  if (SummaryInd$leaf_repro_inv[i] > 0) {
    SummaryInd$leaf_repro_inv_log[i] <- log10(SummaryInd$leaf_repro_inv[i])
  }else{
    SummaryInd$leaf_repro_inv_log[i] <- -(log10(-SummaryInd$leaf_repro_inv[i]))  
  }
}

```
#Figure 2 - RA schedules for species, including indication of schedule-type
```{r}
win.metafile("ms/RA/Figure_2_RA_schedules.wmf", height=11, width=8)
plot
#pdf("figs2/RA/Figure_2_RA_schedules.pdf", height=11, width=8)
par(mfcol=c(5,3), cex=1, omi=c(.7,.7,.1,.1), mai=c(0.15,.15,0.05,0.05)) 

yvar <- "leaf_inv_log"
xvar <- "total_weight_0"

data <- subset(SummarySppAge[["mean"]])
data <- split(data, data$species)

#data2 <- subset(SummaryInd,RA_leaf_area>-2&RA_leaf_area<3)
data2 <- subset(SummaryInd)
data2 <- split(data2, data2$species)

curves <- as.data.frame(c("determinate: gradual","determinate: gradual","indeterminate: asymptotic","indeterminate: asymptotic","determinate: bang-bang","determinate: bang-bang","indeterminate: gradual","determinate: bang-bang","determinate: gradual","determinate: gradual","indeterminate: declining","determinate: gradual","determinate: bang-bang","determinate: gradual"))

curves <- cbind(curves, SummarySpp[["mean"]]$species)
names(curves) <- c("RA_type","species")

data3 <- curves
data3 <- split(data3, data3$species)

for(spp in names(data)) {
  plot(RA_fixed_max~age,data2[[spp]],col=col.age(data2[[spp]]$age),pch=16,xlab="",ylab="",log="x",xlim=c(1,40),ylim=c(-0.25,1.75),yaxt="n",xaxt="n")
  points(RA_leaf_area~age,data[[spp]],pch=151,col=col.age(data[[spp]]$age),cex=1.5)
  if(spp=="BOLE"|spp=="COER") {
    rect(14,par("usr")[3],10^par("usr")[2],par("usr")[4],col="light grey",density=NA)
    rect(10^par("usr")[1],par("usr")[3],10^par("usr")[2],par("usr")[4],col="black",density=0)
  }
  if(spp=="GRSP"|spp=="HEPU") {
    rect(20,par("usr")[3],10^par("usr")[2],par("usr")[4],col="light grey",density=NA)
    rect(10^par("usr")[1],par("usr")[3],10^par("usr")[2],par("usr")[4],col="black",density=0)
  }
  if(spp=="PILI") {
    rect(10,par("usr")[3],10^par("usr")[2],par("usr")[4],col="light grey",density=NA)
    rect(10^par("usr")[1],par("usr")[3],10^par("usr")[2],par("usr")[4],col="black",density=0)
  }
  abline(0,0)
  abline(1,0,lty=3)
  axis(1, at=c(1,2,5,10,20,30), labels=c("","","","","",""),cex.axis=.8,las=1,tck=-.03) 
  axis(2, at=c(-0,0.5,1,1.5), labels=c("","","",""),cex.axis=.8,las=1,tck=-.03)
  
  if(spp=="GRBU"|spp=="PELA"|spp=="PUTU"){
    axis(1, at=c(1,2,5,10,20,30), labels=c(1,2,5,10,20,30),cex.axis=.8,las=1) 
  } else{}
  if(spp=="BAER"|spp=="BOLE"|spp=="COER"|spp=="EPMI"|spp=="GRBU"){
    axis(2, at=c(-0,0.5,1,1.5), labels=c(-0,0.5,1,1.5),cex.axis=.8,las=1)
  } else{}
  extra.top.left.logx(labels.spp.full(spp),3,1)
  extra.bottom.left.logx(data3[[spp]]$RA_type,1,0.8)
}

mtext("RA", 2, outer=TRUE,cex=1,line=2)
mtext("age",1, outer=TRUE,cex=1,line=1.8)

dev.off()
```



#Figure 3, Table 1 - comparing max RA to various functional traits and life history traits
```{r}
RA_test <- as.data.frame(c("BAER","BOLE","COER","EPMI","GRBU","GRSP","HATE","HEPU","LEES","PELA","PEPU","PHPH","PILI","PUTU"))
names(RA_test) <- c("species")
RA_test$min_age <- c(8,6,6,8,8,6,8,6,8,8,6,8,4,8)
test <- SummaryInd
test <- dplyr::left_join(SummaryInd, RA_test, by = "species")
test <- subset(test,age>min_age)
test2 <- test %>% 
  group_by(species) %>% 
  summarise_each(funs(mean), maxH,RA_leaf_area)
names(test2) <- c("species","maxH","maxRA_leaf_area")
test2$mature <- c(7,1.4,2.4,2.4,5,2.4,7,1.4,2.4,9,7,2.4,1.4,2.4)
test2$lifespan <- c(40,9,12,40,30,20,40,20,40,40,40,30,7,30)

Table_01 <- as.data.frame(select(SummarySpp[["mean"]],species,RA_leaf_area,embryo_endo_costs,LMA,wood_density,growth_shoot_length,seed_count,scaled_seed_count))
Table_01 <- dplyr::right_join(Table_01,test2,by="species")
Table_01$lifetime_RA <- Table_01$lifespan*Table_01$RA_leaf_area
plot(Table_01$lifetime_RA~embryo_endo_costs,Table_01,col=col.spp(species),pch=16,log="x")

win.metafile("ms/RA/Figure_3_maxRA_correlates.wmf", height=11, width=8)
plot
par(mfcol=c(3,2), cex=1, omi=c(.1,.7,.1,.1), mai=c(.8,.3,.1,0.2)) 

plot(maxRA_leaf_area~lifespan,Table_01,col=col.spp(species),pch=16,cex=1.5,log="x",xlab="",yaxt="n")
mtext("lifespan (years)",1,outer=FALSE,line=2)
axis(2, at=c(.4,.6,.8,1,1.2,1.4), labels=c(0.4,0.6,0.8,1.0,1.2,1.4),cex.axis=.9,las=1)
mod <- lm(log10(lifespan)~maxRA_leaf_area,Table_01)
extra.bottom.left.logx(paste("r2=",round(glance(mod)[1],2),"; p-value:",round(glance(mod)[5],4)),1,1)
mtext("maximum RA",2,outer=TRUE,line=1)

plot(maxRA_leaf_area~mature,Table_01,col=col.spp(species),pch=16,cex=1.5,log="x",xlab="",yaxt="n")
mtext("age at maturity (years)",1,outer=FALSE,line=2)
axis(2, at=c(.4,.6,.8,1,1.2,1.4), labels=c(0.4,0.6,0.8,1.0,1.2,1.4),cex.axis=.9,las=1)
mod <- lm(mature~log10(maxRA_leaf_area),Table_01)
extra.bottom.left.logx(paste("r2=",round(glance(mod)[1],2),"; p-value:",round(glance(mod)[5],4)),1,1)

plot(maxRA_leaf_area~embryo_endo_costs,Table_01,col=col.spp(species),pch=16,cex=1.5,log="x",xlab="",yaxt="n",xlim=c(.005,30),xaxt="n")
mtext("embryo + endosperm weight (mg)",1,outer=FALSE,line=2)
axis(2, at=c(.4,.6,.8,1,1.2,1.4), labels=c(0.4,0.6,0.8,1.0,1.2,1.4),cex.axis=.9,las=1)
axis(1, at=c(.01,.1,1,10), labels=c(.01,.1,1,10),las=1)
mod <- lm(log10(embryo_endo_costs)~maxRA_leaf_area,Table_01)
extra.bottom.left.logx(paste("r2=",round(glance(mod)[1],2),"; p-value:",round(glance(mod)[5],4)),1,1)
legend("topleft",legend=labels.spp(),col=col.spp(),pch=16, cex=.6,bty="n")

plot(maxRA_leaf_area~maxH,Table_01,col=col.spp(species),pch=16,cex=1.5,log="x",xlab="",yaxt="n")
mtext("maximum height (mm)",1,outer=FALSE,line=2)
mod <- lm(log10(maxH)~maxRA_leaf_area,Table_01)
axis(2, at=c(.4,.6,.8,1,1.2,1.4), labels=c("","","","","",""),cex.axis=.8,las=1)
extra.bottom.left.logx(paste("r2=",round(glance(mod)[1],2),"; p-value:",round(glance(mod)[5],4)),1,1)

plot(maxRA_leaf_area~LMA,Table_01,col=col.spp(species),pch=16,cex=1.5,xlab="",yaxt="n")
mtext("LMA",1,outer=FALSE,line=2)
axis(2, at=c(.4,.6,.8,1,1.2,1.4), labels=c("","","","","",""),cex.axis=.8,las=1)
mod <- lm(LMA~RA_leaf_area,Table_01)
extra.bottom.left(paste("r2=",round(glance(mod)[1],2),"; p-value:",round(glance(mod)[5],4)))

plot(maxRA_leaf_area~wood_density,Table_01,col=col.spp(species),pch=16,cex=1.5,xlab="",yaxt="n")
mtext("wood density",1,outer=FALSE,line=2)
axis(2, at=c(.4,.6,.8,1,1.2,1.4), labels=c("","","","","",""),cex.axis=.8,las=1)
mod <- lm(wood_density~maxRA_leaf_area,Table_01)
extra.top.left(paste("r2=",round(glance(mod)[1],2),"; p-value:",round(glance(mod)[5],4)),1,1)
dev.off()

write.csv(Table_01, file="ms/RA/Table01_maxRA_correlates.csv",row.names=FALSE)
```


#Figure 4a - reproductive vs leaf investment across plant weights
#Shows clearly how there is a crossover in leaf vs reproductive investment for all species, but magnitude of effect is very different
```{r}
win.metafile("ms/RA/Figure_4a_investment.wmf", height=11, width=8)
plot
#pdf("figs2/RA/Figure_4a_investment.pdf", height=11, width=8)
par(mfcol=c(5,3), cex=1, omi=c(.3,.7,.1,.1), mai=c(0.5,.15,0.05,0.05))

yvar <- "leaf_inv_log"
xvar <- "total_weight_0"

data <- subset(SummaryInd)
data <- split(data, data$species)

data2 <- subset(SummaryInd,RA_leaf_area>-2&RA_leaf_area<3)
data2 <- split(data2, data2$species)

for(spp in names(data)) {
  
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar,log="x",xlab="",ylab="",cex=.8,ylim=c(-4,6),
                    col=col.age(data[[spp]]$age),cex.main=0.9, yaxt="n")
  
  points(repro_inv_log~total_weight_0,data[[spp]],pch=1,cex=1.2,col=col.age(data[[spp]]$age))
  points(leaf_repro_inv_log~total_weight_0,data[[spp]],pch=151,cex=1,col=col.age(data[[spp]]$age))
  extra.top.left.logx(labels.spp.full(spp),3,1)
  #arrows(data[[spp]]$total_weight_0,data[[spp]]$leaf_inv_log,data[[spp]]$total_weight_0,data[[spp]]$repro_inv_log,lwd=1,length=.08,
  #col=col.age(data[[spp]]$age))
  abline(0,0)
  axis(2, at=c(-4,-2,0,2,4), labels=c("","","","",""),cex.axis=.8,las=1,tck=-.03)
  
  if(spp=="BAER"|spp=="BOLE"|spp=="COER"|spp=="EPMI"|spp=="GRBU"){
    axis(2, at=c(-4,-2,0,2,4), labels=c(-10000,-100,0,100,10000),cex.axis=.8,las=1)
  } else {}
}

mtext("investment (mg)", 2, outer=TRUE,cex=1,line=2.5)
mtext("plant weight (mg)",1,outer=TRUE,cex=1,line=-.3)


dev.off()
```


data <- as.data.frame(select(SummarySppAge[["mean"]],species,age, repro_inv,growth_leaf,leaf_replacement,growth_stem,diameter))
for(v in c("repro_inv","growth_leaf","leaf_replacement","growth_stem")) {
  i <- is.na(data[[v]])
  data[[v]][i] <- 0
}

for(v in c("growth_leaf")) {
  i <- (data[[v]] < 0)
  data[[v]][i] <- 0
}

data$lifetime_repro_inv <- data$repro_inv
data$lifetime_growth_leaf <- data$growth_leaf

for (i in 1:length(data$diameter)) {
  if (data$age[i] == 2.4) {
    data$lifetime_repro_inv[i] <- data$lifetime_repro_inv[i]*2.6
    data$lifetime_growth_leaf[i] <- data$lifetime_growth_leaf[i]*2.6
  }
}

for (i in 1:length(data$diameter)) {
  if (data$age[i] == 5) {
    data$lifetime_repro_inv[i] <- data$lifetime_repro_inv[i]*2
    data$lifetime_growth_leaf[i] <- data$lifetime_growth_leaf[i]*2
  }
}

for (i in 1:length(data$diameter)) {
  if (data$age[i] == 7) {
    data$lifetime_repro_inv[i] <- data$lifetime_repro_inv[i]*2
    data$lifetime_growth_leaf[i] <- data$lifetime_growth_leaf[i]*2
  }
}


for (i in 1:length(data$diameter)) {
  if (data$age[i] == 9) {
    data$lifetime_repro_inv[i] <- data$lifetime_repro_inv[i]*5
    data$lifetime_growth_leaf[i] <- data$lifetime_growth_leaf[i]*5
  }
}


for (i in 1:length(data$diameter)) {
  if (data$age[i] == 32) {
    data$lifetime_repro_inv[i] <- data$lifetime_repro_inv[i]*18
    data$lifetime_growth_leaf[i] <- data$lifetime_growth_leaf[i]*18
  }
}

lifetime <- data %>%
  group_by(species) %>% 
  summarise(repro = sum(lifetime_repro_inv))

lifetime2 <- data %>%
  group_by(species) %>% 
  summarise(leaf_growth = sum(lifetime_growth_leaf))

lifetime <- merge(lifetime,lifetime2,by="species")

lifetime$RA <- lifetime$repro / (lifetime$repro + lifetime$leaf_growth)            
plot(RA~leaf_growth,lifetime,pch=16,col=col.spp(species),log="x")

#stacked lines
```{r}
#pdf("figs2/RA/Figure_1_stacked_lines.pdf", height=11.5, width=8)
#plot
par(mfrow=c(7,4), cex=1, omi=c(.1,0.1,.1,.1), mai=c(0.8,.8,.1,0.2)) 

data <- dplyr::select(SummarySppAge[["mean"]],species,age,repro_inv,growth_leaf,leaf_replacement,growth_stem,leaf_shed)


for(v in c("repro_inv","growth_leaf","leaf_replacement","growth_stem","leaf_shed")) {
  i <- is.na(data[[v]])
  data[[v]][i] <- 0
}

for(v in c("growth_leaf")) {
  i <- (data[[v]] < 0)
  data[[v]][i] <- 0
}


data <- split(data, data$species)


for(spp in names(data)) {
  
  
  data[[spp]] <- data[[spp]] %>% mutate(
    total_inv = repro_inv + growth_leaf + leaf_replacement + growth_stem,
    prop_repro = repro_inv / total_inv,
    prop_leaf_expand = (repro_inv + growth_leaf)/total_inv,
    prop_leaf_replacement = (leaf_replacement+repro_inv + growth_leaf)/ total_inv,
    prop_stem = total_inv/total_inv,
    stack1 = repro_inv,
    stack2 = repro_inv+growth_leaf,
    stack3= leaf_replacement+repro_inv + growth_leaf,
    stack4 = total_inv
  )
  
  plot(prop_repro~age,data[[spp]],pch=16,log="x",ylim=c(0.04,0.96),xlim=c(1.06*1.4,0.94*32),col="white")
  
  if(spp=="PILI") {
    polygon(x=c(1.4,7,7,1.4),y=c(1,1,0,0),col="burlywood4",density=NA)
    polygon(x=c(1.4,2.4,5,7,7,1.4),y=c(data[[spp]]$prop_leaf_replacement[1],data[[spp]]$prop_leaf_replacement[2],data[[spp]]$prop_leaf_replacement[3],data[[spp]]$prop_leaf_replacement[4],
                                       0,0),col="darkseagreen4",density=NA)
    polygon(x=c(1.4,2.4,5,7,7,1.4),y=c(data[[spp]]$prop_leaf_expand[1],data[[spp]]$prop_leaf_expand[2],data[[spp]]$prop_leaf_expand[3],data[[spp]]$prop_leaf_expand[4],
                                       0,0),col="darkseagreen2",density=NA)
    polygon(x=c(1.4,2.4,5,7,7,1.4),y=c(data[[spp]]$prop_repro[1],data[[spp]]$prop_repro[2],data[[spp]]$prop_repro[3],data[[spp]]$prop_repro[4],
                                       0,0),col="coral3",density=NA)
    text(5,0.1,"reproductive")
    text(1.4,0.18,"additional leaves",adj=0)
    text(3,0.4,"replacement leaves",adj=0)
    text(2,0.7,"additional stems",adj=0)
  }
  
  if(spp=="PHPH") {
    polygon(x=c(2.4,32,32,2.4),y=c(1,1,0,0),col="burlywood4",density=NA)
    polygon(x=c(2.4,5,7,9,32,32,2.4),y=c(data[[spp]]$prop_leaf_replacement[1],data[[spp]]$prop_leaf_replacement[2],data[[spp]]$prop_leaf_replacement[3],data[[spp]]$prop_leaf_replacement[4],
                                         data[[spp]]$prop_leaf_replacement[5],0,0),col="darkseagreen4",density=NA)
    polygon(x=c(2.4,5,7,9,32,32,2.4),y=c(data[[spp]]$prop_leaf_expand[1],data[[spp]]$prop_leaf_expand[2],data[[spp]]$prop_leaf_expand[3],data[[spp]]$prop_leaf_expand[4],
                                         data[[spp]]$prop_leaf_expand[5],0,0),col="darkseagreen2",density=NA)
    polygon(x=c(2.4,5,7,9,32,32,2.4),y=c(data[[spp]]$prop_repro[1],data[[spp]]$prop_repro[2],data[[spp]]$prop_repro[3],data[[spp]]$prop_repro[4],
                                         data[[spp]]$prop_repro[5],0,0),col="coral3",density=NA)
    text(20,data[[spp]]$prop_repro[5]*.7,"reproductive")
    text(2.5,data[[spp]]$prop_leaf_expand[1]*.3,"additional leaves",adj=0)
    text(5,data[[spp]]$prop_leaf_replacement[4]*.6,"replacement leaves",adj=0)
    text(15,0.9,"additional stems",adj=0)
  }
  
  if(spp=="BOLE"|spp=="COER"|spp=="HEPU"|spp=="GRSP") {
    polygon(x=c(1.4,9,9,1.4),y=c(1,1,0,0),col="burlywood4",density=NA) 
    polygon(x=c(1.4,2.4,5,7,9,9,1.4),y=c(data[[spp]]$prop_leaf_replacement[1],data[[spp]]$prop_leaf_replacement[2],data[[spp]]$prop_leaf_replacement[3],data[[spp]]$prop_leaf_replacement[4],
                                         data[[spp]]$prop_leaf_replacement[5],0,0),col="darkseagreen4",density=NA)
    polygon(x=c(1.4,2.4,5,7,9,9,1.4),y=c(data[[spp]]$prop_leaf_expand[1],data[[spp]]$prop_leaf_expand[2],data[[spp]]$prop_leaf_expand[3],data[[spp]]$prop_leaf_expand[4],
                                         data[[spp]]$prop_leaf_expand[5],0,0),col="darkseagreen2",density=NA)
    polygon(x=c(1.4,2.4,5,7,9,9,1.4),y=c(data[[spp]]$prop_repro[1],data[[spp]]$prop_repro[2],data[[spp]]$prop_repro[3],data[[spp]]$prop_repro[4],
                                         data[[spp]]$prop_repro[5],0,0),col="coral3",density=NA)
    text(8,data[[spp]]$prop_repro[5]*.4,"reproductive",adj=1)
    text(1.5,data[[spp]]$prop_leaf_expand[1]*.5,"additional leaves",adj=0)
    text(4,data[[spp]]$prop_leaf_replacement[3]*.6,"replacement leaves",adj=0)
    text(6,0.9,"additional stems",adj=1)
  }
  
  if(spp=="BAER"|spp=="EPMI"|spp=="GRBU"|spp=="HATE"|spp=="PELA"|spp=="PUTU"|spp=="LEES"|spp=="PEPU") {
    polygon(x=c(1.4,32,32,1.4),y=c(1,1,0,0),col="burlywood4",density=NA)
    polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$prop_leaf_replacement[1],data[[spp]]$prop_leaf_replacement[2],data[[spp]]$prop_leaf_replacement[3],data[[spp]]$prop_leaf_replacement[4],
                                             data[[spp]]$prop_leaf_replacement[5],data[[spp]]$prop_leaf_replacement[6],0,0),col="darkseagreen4",density=NA)
    polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$prop_leaf_expand[1],data[[spp]]$prop_leaf_expand[2],data[[spp]]$prop_leaf_expand[3],data[[spp]]$prop_leaf_expand[4],
                                             data[[spp]]$prop_leaf_expand[5],data[[spp]]$prop_leaf_expand[6],0,0),col="darkseagreen2",density=NA)
    polygon(x=c(1.4,2.4,5,7,9,32,32,1.4),y=c(data[[spp]]$prop_repro[1],data[[spp]]$prop_repro[2],data[[spp]]$prop_repro[3],data[[spp]]$prop_repro[4],
                                             data[[spp]]$prop_repro[5],data[[spp]]$prop_repro[6],0,0),col="coral3",density=NA)
    text(20,data[[spp]]$prop_repro[6]*.4,"reproductive")
    text(1.5,data[[spp]]$prop_leaf_expand[1]*.3,"additional leaves",adj=0)
    text(5,data[[spp]]$prop_leaf_replacement[4]*.6,"replacement leaves",adj=0)
    text(15,0.9,"additional stems",adj=0)
  }
  extra.top.left.logx(labels.spp.full(spp),3,1)
  
  
  plot(stack4~age,data[[spp]],col="white",log="x")
  if(spp=="BOLE"|spp=="COER"|spp=="HEPU"|spp=="GRSP") {
    polygon(x=c(1.4,2.4,7,9,9,1.4),y=c(data[[spp]]$stack4[1],data[[spp]]$stack4[2],data[[spp]]$stack4[4],
                                       data[[spp]]$stack4[5],0.1,0.1),col="burlywood4",density=NA) 
    polygon(x=c(1.4,2.4,7,9,9,1.4),y=c(data[[spp]]$stack3[1],data[[spp]]$stack3[2],data[[spp]]$stack3[4],
                                       data[[spp]]$stack3[5],0.1,0.1),col="darkseagreen4",density=NA)
    polygon(x=c(1.4,2.4,7,9,9,1.4),y=c(data[[spp]]$stack2[1],data[[spp]]$stack2[2],data[[spp]]$stack2[4],
                                       data[[spp]]$stack2[5],0.1,0.1),col="darkseagreen2",density=NA)
    polygon(x=c(1.4,2.4,7,9,9,1.4),y=c(data[[spp]]$stack1[1],data[[spp]]$stack1[2],data[[spp]]$stack1[4],
                                       data[[spp]]$stack1[5],0.1,0.1),col="coral3",density=NA)
    text(8,data[[spp]]$prop_repro[5]*.4,"reproductive",adj=1)
    text(1.5,data[[spp]]$prop_leaf_expand[1]*.5,"additional leaves",adj=0)
    text(4,data[[spp]]$prop_leaf_replacement[3]*.6,"replacement leaves",adj=0)
    text(6,0.9,"additional stems",adj=1)
  }
  
}

#dev.off()
```



#Figure 4b leaf area declines with age in some species, plateaus in others, and in a few continues to increase
```{r}
win.metafile("ms/RA/Figure_4b_leaf_area_vs_age.wmf", height=11, width=8)
plot
par(mfcol=c(5,3), cex=1, omi=c(.5,.4,.1,.1), mai=c(0.15,.5,0.05,0.05))
xvar <- "age"
yvar <- "leaf_area"

data <- subset(SummaryInd)
data <- split(data, data$species)
data2 <- subset(SummarySppAge[["mean"]])
data2 <- split(data2, data2$species)

for(spp in names(data)) {
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar,log="xy",xlab=xvar,ylab=yvar,cex=1,col=col.age(data[[spp]]$age),xlim=c(1,40),yaxt="n",xaxt="n")
  points(leaf_area~age,data2[[spp]],cex=1.5,col=col.age(data2[[spp]]$age),pch=151)
  extra.top.left.logxy(labels.spp.full(spp),3,1)
  if(spp=="BOLE"|spp=="COER") {
    rect(14,10^par("usr")[3],10^par("usr")[2],10^par("usr")[4],col="light grey",density=NA)
    rect(10^par("usr")[1],10^par("usr")[3],10^par("usr")[2],10^par("usr")[4],col="black",density=0)
  }
  if(spp=="GRSP"|spp=="HEPU") {
    rect(20,10^par("usr")[3],10^par("usr")[2],10^par("usr")[4],col="light grey",density=NA)
    rect(10^par("usr")[1],10^par("usr")[3],10^par("usr")[2],10^par("usr")[4],col="black",density=0)
  }
  if(spp=="PILI") {
    rect(10,10^par("usr")[3],10^par("usr")[2],10^par("usr")[4],col="light grey",density=NA)
    rect(10^par("usr")[1],10^par("usr")[3],10^par("usr")[2],10^par("usr")[4],col="black",density=0)
  }
  axis(1, at=c(1,2,5,10,20,30), labels=c("","","","","",""),cex.axis=.8,las=1,tck=-.03) 
  axis(2, at=c(5,10,50,100,500,1000,5000,10000,50000,100000,500000,1000000),labels=c("",10,"",100,"",1000,"","10000","","100000","","1000000"),
       cex.axis=0.8,las=2,hadj=.8,)
  if(spp=="GRBU"|spp=="PELA"|spp=="PUTU"){
    axis(1, at=c(1,2,5,10,20,30), labels=c(1,2,5,10,20,30),cex.axis=.8,las=1) 
  } else{}
}
mtext("leaf area (mm^2)",outer=TRUE,2,line=0.2)
mtext("age (years)",outer=TRUE,1,line=1.2)
dev.off()
```

#RV curves
```{r}
win.metafile("figs2/RA/Figure_EXTRA_RV_curves.wmf", height=11, width=8)
plot
par(mfcol=c(5,3), cex=1, omi=c(.3,.3,.1,.1), mai=c(0.4,.7,0.05,0.05))

yvar <- "repro_inv_log"
xvar <- "total_weight_0"

data <- subset(SummaryInd)
data <- split(data, data$species)

for(spp in names(data)) {
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar,log="x",xlab="",ylab="",cex=.8,yaxt="n",col=col.age(data[[spp]]$age),cex.main=0.9)
  extra.top.left.logx(labels.spp.full(spp),3)
  axis(2, at=c(0,1,2,3,4,5), labels=c(0,10,100,1000,10000,"100000"),cex.axis=0.9,las=1)
}
mtext("reproductive investment (mg)", 2, outer=TRUE,cex=1,line=0)
mtext("weight", 1, outer=TRUE,cex=1,line=0)
dev.off()
```

#predicting growth from leaf area versus plant weight
#using leaf area at the start of the year is a better predictor of total investment than is total plant weight
```{r}
par(mfrow=c(7,2), cex=1, omi=c(.7,.8,.1,.1), mai=c(.3,.4,.4,0.2)) 
xvar <- "leaf_area_0"
xvar2 <- "total_weight_0"
yvar <- "leaf_repro_inv"

data <- subset(SummaryInd)
data <- split(data, data$species)
lmfits <- lapply(data, function(x) lm((x[[yvar]]) ~  ((x[[xvar]]))))
results <- summarise_all_fits(lmfits)

results$r.squared <- round(results$r.squared,digits=3)
results$p.value <- round(results$p.value,digits=3)
results$a <- round(results$a,digits=2)
results$b <- round(results$b,digits=2)

data <- subset(SummaryInd)
data <- split(data, data$species)
lmfits <- lapply(data, function(x) lm((x[[yvar]]) ~  ((x[[xvar2]]))))
results2 <- summarise_all_fits(lmfits)

results2$r.squared <- round(results2$r.squared,digits=3)
results2$p.value <- round(results2$p.value,digits=3)
results2$a <- round(results2$a,digits=2)
results2$b <- round(results2$b,digits=2)

for(spp in names(data)) {
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar,log="xy",xlab=xvar,ylab=yvar,cex=1,col=col.age(data[[spp]]$age))
  plot_yvar_vs_xvar(data[[spp]], yvar, xvar2,log="xy",xlab=xvar2,ylab="",cex=1,main=spp,col=col.age(data[[spp]]$age),yaxt="n")
}
```

#Extra - leaf area is a better predictor of subsequent investment than is total plant weight. In particular, the age effect basically disappears
```{r}
#Leaf area is a better predictor of the subsequent years growth than is total weight. 
#result could be quite different if stem weight, even part of it were included
win.metafile("figs2/RA/Figure_EXTRA_leaf_area_better.wmf", height=11, width=8)
plot

par(mfrow=c(2,2), cex=1, omi=c(.1,.1,.1,.1), mai=c(.9,.9,.4,0.2)) 
compare <- select(results, species, r.squared)
compare2 <- select(results2, r.squared)
compare <- cbind(compare, compare2)
names(compare) <- c("species","leaf_area","total_weight")

plot(leaf_repro_inv~leaf_area_0,SummaryInd,col=col.age(age),pch=16,log="xy",xlab="leaf area at time=0",ylab="investment in reproduction and leaves")
abline(0,1)

plot(leaf_repro_inv~total_weight_0,SummaryInd,col=col.age(age),pch=16,log="xy",xlab="total plant weight at time=0",ylab="investment in reproduction and leaves")
abline(0,1)

plot(leaf_area~total_weight,compare,col=col.spp(species),pch=16,xlab="r2 of total weight vs. investment",ylab="r2 of leaf area vs. investment")
abline(0,1)

dev.off()
```

#Extra - shoot extenstion and increase in shoot leaf area also decline with age in most species
#the first plot is of increase in shoot leaf area on the leader - only for PEPU does the leader's leaf area continue to increase until the end. Some species, such as EPMI plateau and show limited decline, but most continue to decline steeply
```{r}
win.metafile("figs2/RA/Figure_EXTRA_shoot_growth.wmf", height=11, width=8)
plot
par(mfcol=c(5,3), cex=1, omi=c(.5,.4,.1,.1), mai=c(0.15,.5,0.05,0.05))

data <- subset(SummaryInd)
data <- split(data, data$species)

for(spp in names(data)) {
  plot(shoot_growth_leaf_area~age,data[[spp]],log="xy",pch=16,col=col.age(age),cex.main=0.9,xlim=c(1,40),xaxt="n",cex.axis=0.9)
  axis(1, at=c(1,2,5,10,20,30), labels=c("","","","","",""),cex.axis=.8,las=1,tck=-.03) 
  if(spp=="BOLE"|spp=="COER") {
    rect(14,10^par("usr")[3],10^par("usr")[2],10^par("usr")[4],col="light grey",density=NA)
    rect(10^par("usr")[1],10^par("usr")[3],10^par("usr")[2],10^par("usr")[4],col="black",density=0)
  }
  if(spp=="GRSP"|spp=="HEPU") {
    rect(20,10^par("usr")[3],10^par("usr")[2],10^par("usr")[4],col="light grey",density=NA)
    rect(10^par("usr")[1],10^par("usr")[3],10^par("usr")[2],10^par("usr")[4],col="black",density=0)
  }
  if(spp=="PILI") {
    rect(10,10^par("usr")[3],10^par("usr")[2],10^par("usr")[4],col="light grey",density=NA)
    rect(10^par("usr")[1],10^par("usr")[3],10^par("usr")[2],10^par("usr")[4],col="black",density=0)
  }
  if(spp=="GRBU"|spp=="PELA"|spp=="PUTU"){
    axis(1, at=c(1,2,5,10,20,30), labels=c(1,2,5,10,20,30),cex.axis=0.9,las=1) 
  } else{}
  extra.top.left.logxy(labels.spp.full(spp),3)
}

mtext("increase in leaf area on leader shoot (mm^2/year)",outer=TRUE,2,line=0.4)
mtext("age (years)",outer=TRUE,1,line=1)
dev.off()
```