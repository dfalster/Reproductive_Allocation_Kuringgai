check <- BAER_HarvestData
check <- BOLE_HarvestData
check <- COER_HarvestData
check <- EPMI_HarvestData
check <- GRBU_HarvestData
check <- GRSP_HarvestData
check <- HATE_HarvestData
check <- HEPU_HarvestData
check <- LEES_HarvestData
check <- PELA_HarvestData
check <- PEPU_HarvestData
check <- PHPH_HarvestData
check <- PILI_HarvestData
check <- PUTU_HarvestData

plot(dia~stem_weight,data=subset(check,segment==1), pch=16,log="xy",col=col.lots(individual))
text(dia~stem_weight,data=subset(check,segment==1),labels=individual,cex=.5,pos=3,offset=-.7)
mod <- lm(log(stem_weight)~log(dia),data=subset(check,segment==1))
words.bottom.right.logxy(mod)




plot(leaf_weight~stem_weight,data=subset(check), pch=16,log="xy",col="red")
text(leaf_weight~stem_weight,data=subset(check),labels=individual,cex=.5,pos=2,offset=-1)
mod <- lm(log(stem_weight)~log(leaf_weight),data=subset(check))
words.bottom.right.logxy(mod)

#age 10 break (e.g. slightly BAER, GRSP)
plot(leaf_weight~stem_weight,data=subset(check,age<10), pch=16,log="xy",col="red")
points(leaf_weight~stem_weight,data=subset(check,age>10), pch=16,log="xy",col="blue")
text(leaf_weight~stem_weight,data=subset(check,age<10),labels=individual,cex=.5,pos=2,offset=-1)
mod <- lm(log(stem_weight)~log(leaf_weight),data=subset(check,age<10))
words.bottom.right.logxy(mod)

#age 8 break (e.g. GRBU, COER, LEES)
plot(leaf_weight~stem_weight,data=subset(check,age<8), pch=16,log="xy",col="red")
points(leaf_weight~stem_weight,data=subset(check,age>8), pch=16,log="xy",col="blue")
text(leaf_weight~stem_weight,data=subset(check,age<8),labels=individual,cex=.5,pos=2,offset=-1)
mod <- lm(log(stem_weight)~log(leaf_weight),data=subset(check,age<8))
words.bottom.right.logxy(mod)

#age 6 break 
plot(leaf_weight~stem_weight,data=subset(check,age<6), pch=16,log="xy",col="red")
points(leaf_weight~stem_weight,data=subset(check,age>6), pch=16,log="xy",col="blue")
text(leaf_weight~stem_weight,data=subset(check,age<6),labels=individual,cex=.5,pos=2,offset=-1)
mod <- lm(log(stem_weight)~log(leaf_weight),data=subset(check,age<6))
words.bottom.right.logxy(mod)

#age 4 break (e.g. BOLE, PILI)
plot(leaf_weight~stem_weight,data=subset(check,age<4), pch=16,log="xy",col="red")
points(leaf_weight~stem_weight,data=subset(check,age>4), pch=16,log="xy",col="blue")
text(leaf_weight~stem_weight,data=subset(check,age<4),labels=individual,cex=.5,pos=2,offset=-1)
mod <- lm(log(stem_weight)~log(leaf_weight),data=subset(check,age<4))
words.bottom.right.logxy(mod)




plot(dia~stem_weight,data=subset(check,age==32), pch=16,log="xy",col=col.spp[as.factor(individual)])
text(dia~stem_weight,data=subset(check,segment==1&age==32),labels=individual,cex=.5,pos=3,offset=-.7)
mod <- lm(log(stem_weight)~log(dia),data=subset(check,age==32))
words.bottom.right.logxy(mod)

plot(dia~leaf_weight,data=subset(check,age==32), pch=16,log="xy",col=col.spp[as.factor(individual)])
text(dia~leaf_weight,data=subset(check,segment==1&age==32),labels=individual,cex=.5,pos=3,offset=-.7)

plot(leaf_weight~stem_weight,data=subset(check,age==32), pch=16,log="xy",col=col.spp[as.factor(individual)])
text(leaf_weight~stem_weight,data=subset(check,segment==1&age==32),labels=individual,cex=.5,pos=3,offset=-.7)
mod <- lm(log(stem_weight)~log(leaf_weight),data=subset(check,age==32))
words.bottom.right.logxy(mod)

plot(dia~stem_weight,data=subset(check,age==9), pch=16,log="xy",col=col.spp[as.factor(individual)])
text(dia~stem_weight,data=subset(check,age==9),labels=segment,cex=.5,pos=3,offset=-.7)
text(dia~stem_weight,data=subset(check,age==9),labels=individual,cex=.5,pos=3,offset=.5)
mod <- lm(log(stem_weight)~log(dia),data=subset(check,age==9))
words.bottom.right.logxy(mod)

plot(dia~leaf_weight,data=subset(check,age==9), pch=16,log="xy",col=col.spp[as.factor(individual)])
text(dia~leaf_weight,data=subset(check,segment==1&age==9),labels=individual,cex=.5,pos=3,offset=-.7)

plot(leaf_weight~stem_weight,data=subset(check,age==9), pch=16,log="xy",col=col.spp[as.factor(individual)])
text(leaf_weight~stem_weight,data=subset(check,segment==1&age==9),labels=individual,cex=.5,pos=3,offset=-.7)

plot(dia~stem_weight,data=subset(check,age==7), pch=16,log="xy",col=col.spp[as.factor(individual)])
text(dia~stem_weight,data=subset(check,age==7),labels=segment,cex=.5,pos=3,offset=-.7)
text(dia~stem_weight,data=subset(check,age==7&segment==1),labels=individual,cex=.5,pos=3,offset=.5)
mod <- lm(log(stem_weight)~log(dia),data=subset(check,age==7))
words.bottom.right.logxy(mod)

plot(dia~leaf_weight,data=subset(check,age==7), pch=16,log="xy",col=col.spp[as.factor(individual)])
text(dia~leaf_weight,data=subset(check,segment==1&age==7),labels=individual,cex=.5,pos=3,offset=-.7)

plot(leaf_weight~stem_weight,data=subset(check,age==7), pch=16,log="xy",col=col.spp[as.factor(individual)])
text(leaf_weight~stem_weight,data=subset(check,segment==1&age==7),labels=individual,cex=.5,pos=3,offset=-.7)

plot(dia~stem_weight,data=subset(check,age==5), pch=16,log="xy",col=col.spp[as.factor(individual)])
text(dia~stem_weight,data=subset(check,segment==1&age==5),labels=individual,cex=.5,pos=3,offset=-.7)
mod <- lm(log(stem_weight)~log(dia),data=subset(check,age==5))
words.bottom.right.logxy(mod)

plot(dia~leaf_weight,data=subset(check,age==5), pch=16,log="xy",col=col.spp[as.factor(individual)])
text(dia~leaf_weight,data=subset(check,segment==1&age==5),labels=individual,cex=.5,pos=3,offset=-.7)

plot(leaf_weight~stem_weight,data=subset(check,age==5), pch=16,log="xy",col=col.spp[as.factor(individual)])
text(leaf_weight~stem_weight,data=subset(check,segment==1&age==5),labels=individual,cex=.5,pos=3,offset=-.7)
mod <- lm(log(stem_weight)~log(leaf_weight),data=subset(check,age==5))
words.bottom.right.logxy(mod)

plot(dia~stem_weight,data=subset(check,age==2.4), pch=16,log="xy",col=col.spp[as.factor(individual)])
text(dia~stem_weight,data=subset(check,segment==1&age==2.4),labels=individual,cex=.5,pos=3,offset=-.7)
mod <- lm(log(stem_weight)~log(dia),data=subset(check,age==2.4))
words.bottom.right.logxy(mod)

plot(dia~leaf_weight,data=subset(check,age==2.4), pch=16,log="xy",col=col.spp[as.factor(individual)])
text(dia~leaf_weight,data=subset(check,segment==1&age==2.4),labels=individual,cex=.5,pos=3,offset=-.7)

plot(leaf_weight~stem_weight,data=subset(check,age==2.4), pch=16,log="xy",col=col.spp[as.factor(individual)])
text(leaf_weight~stem_weight,data=subset(check,segment==1&age==2.4),labels=individual,cex=.5,pos=3,offset=-.7)


plot(dia~stem_weight,data=subset(check,age<=1.4), pch=16,log="xy",col=col.lots(individual))
text(dia~stem_weight,data=subset(check,segment==1&age<=1.4),labels=individual,cex=.5,pos=3,offset=-.7)
mod <- lm(log(stem_weight)~log(dia),data=subset(check,age==1.4))
words.bottom.right.logxy(mod)

plot(dia~leaf_weight,data=subset(check,age<=1.4), pch=16,log="xy",col=col.lots(individual))
text(dia~leaf_weight,data=subset(check,segment==1&age<=1.4),labels=individual,cex=.5,pos=3,offset=-.7)

plot(leaf_weight~stem_weight,data=subset(check,age<=1.4), pch=16,log="xy",col=col.lots(individual))
text(leaf_weight~stem_weight,data=subset(check,segment==1&age<=1.4),labels=individual,cex=.5,pos=3,offset=-.7)


plot(dia~stem_weight,data=subset(check,segment==1), pch=16,log="xy",col=col.lots(individual))
text(dia~stem_weight,data=subset(check,segment==1),labels=individual,cex=.5,pos=3,offset=-.7)
mod <- lm(log(stem_weight)~log(dia),data=subset(check,segment==1))
words.bottom.right.logxy(mod)


plot(dia~stem_weight,data=subset(check), pch=16,log="xy")
text(dia~stem_weight,data=subset(check),labels=individual,cex=.5,pos=3,offset=-.7)
mod <- lm(log(stem_weight)~log(dia),data=subset(check))
words.bottom.right.logxy(mod)

plot(dia~leaf_weight,data=subset(check), pch=16,log="xy")
text(dia~leaf_weight,data=subset(check,segment==1&age<=1.4),labels=individual,cex=.5,pos=3,offset=-.7)

plot(leaf_weight~stem_weight,data=subset(check), pch=16,log="xy")
text(leaf_weight~stem_weight,data=subset(check,segment==1&age<=1.4),labels=individual,cex=.5,pos=3,offset=-.7)
mod <- lm(log(stem_weight)~log(leaf_weight),data=subset(check))
words.bottom.right.logxy(mod)

#for individual
plot(dia~stem_weight,data=subset(check), pch=16,log="xy",col="white")
points(dia~stem_weight,data=subset(check,age=9), pch=16,log="xy",col=col.spp[as.factor(individual)])
points(dia~stem_weight,data=subset(check,individual=="PUTU_401"), pch=16,log="xy",col="red")
text(dia~stem_weight,data=subset(check,individual=="PUTU_401"&segment==1),labels=individual,cex=.5,pos=2,offset=-1)
mod <- lm(log(stem_weight)~log(dia),data=subset(check,individual=="PUTU_401"))
words.bottom.right.logxy(mod)


plot(stem_weight~dia,data=subset(check,age==32),pch=16,log="xy",col="red")
text(stem_weight~dia,data=subset(check,age==32),labels=individual,cex=.5,pos=2,offset=-1)

plot(leaf_weight~dia,data=subset(check,age==32),pch=16,log="xy",col="red")
text(leaf_weight~dia,data=subset(check,age==32),labels=individual,cex=.5,pos=2,offset=-1)
mod <- lm(log(dia)~log(leaf_weight),data=subset(check,age==32))
words.bottom.right.logxy(mod)

plot(leaf_weight~stem_weight,data=subset(check,age==32), pch=16,log="xy",col="red")
text(leaf_weight~stem_weight,data=subset(check,age==32),labels=individual,cex=.5,pos=2,offset=-1)
mod <- lm(log(stem_weight)~log(leaf_weight),data=subset(check,age==32))
words.bottom.right.logxy(mod)

plot(stem_weight~dia,data=subset(check,age==9),pch=16,log="xy",col="red")
text(stem_weight~dia,data=subset(check,age==9),labels=individual,cex=.5,pos=2,offset=-1)

plot(leaf_weight~dia,data=subset(check,age==9),pch=16,log="xy",col="red")
text(leaf_weight~dia,data=subset(check,age==9),labels=individual,cex=.5,pos=2,offset=-1)
mod <- lm(log(dia)~log(leaf_weight),data=subset(check,age==9))
words.bottom.right.logxy(mod)

plot(leaf_weight~stem_weight,data=subset(check,age==9), pch=16,log="xy",col="red")
text(leaf_weight~stem_weight,data=subset(check,age==9),labels=individual,cex=.5,pos=2,offset=-1)
mod <- lm(log(stem_weight)~log(leaf_weight),data=subset(check,age==9))
words.bottom.right.logxy(mod)

  
plot(stem_weight~dia,data=subset(check,age==7),pch=16,log="xy",col="red")
text(stem_weight~dia,data=subset(check,age==7),labels=individual,cex=.5,pos=2,offset=-1)

plot(leaf_weight~dia,data=subset(check,age==7),pch=16,log="xy",col="red")
text(leaf_weight~dia,data=subset(check,age==7),labels=individual,cex=.5,pos=2,offset=-1)
mod <- lm(log(dia)~log(leaf_weight),data=subset(check,age==7))
words.bottom.right.logxy(mod)

plot(leaf_weight~stem_weight,data=subset(check,age==7), pch=16,log="xy",col="red")
text(leaf_weight~stem_weight,data=subset(check,age==7),labels=individual,cex=.5,pos=2,offset=-1)
mod <- lm(log(stem_weight)~log(leaf_weight),data=subset(check,age==7))
words.bottom.right.logxy(mod)



plot(stem_weight~dia,data=subset(check,age==5),pch=16,log="xy",col="red")
text(stem_weight~dia,data=subset(check,age==5),labels=individual,cex=.5,pos=2,offset=-1)

plot(leaf_weight~dia,data=subset(check,age==5),pch=16,log="xy",col="red")
text(leaf_weight~dia,data=subset(check,age==5),labels=individual,cex=.5,pos=2,offset=-1)
mod <- lm(log(dia)~log(leaf_weight),data=subset(check,age==5))
words.bottom.right.logxy(mod)

plot(leaf_weight~stem_weight,data=subset(check,age==5), pch=16,log="xy",col="red")
text(leaf_weight~stem_weight,data=subset(check,age==5),labels=individual,cex=.5,pos=2,offset=-1)
mod <- lm(log(stem_weight)~log(leaf_weight),data=subset(check,age==5))
words.bottom.right.logxy(mod)


plot(stem_weight~dia,data=subset(check,age==2.4),pch=16,log="xy",col="red")
text(stem_weight~dia,data=subset(check,age==2.4),labels=individual,cex=.5,pos=2,offset=-1)

plot(leaf_weight~dia,data=subset(check,age==2.4),pch=16,log="xy",col="red")
text(leaf_weight~dia,data=subset(check,age==2.4),labels=individual,cex=.5,pos=2,offset=-1)
mod <- lm(log(dia)~log(leaf_weight),data=subset(check,age==2.4))
words.bottom.right.logxy(mod)

plot(leaf_weight~stem_weight,data=subset(check,age==2.4), pch=16,log="xy",col="red")
text(leaf_weight~stem_weight,data=subset(check,age==2.4),labels=individual,cex=.5,pos=2,offset=-1)
mod <- lm(log(stem_weight)~log(leaf_weight),data=subset(check,age==2.4))
words.bottom.right.logxy(mod)


plot(stem_weight~dia,data=subset(check,age==1.4),pch=16,log="xy",col="red")
text(stem_weight~dia,data=subset(check,age==1.4),labels=individual,cex=.5,pos=2,offset=-1)

plot(leaf_weight~dia,data=subset(check,age==1.4),pch=16,log="xy",col="red")
text(leaf_weight~dia,data=subset(check,age==1.4),labels=individual,cex=.5,pos=2,offset=-1)
mod <- lm(log(dia)~log(leaf_weight),data=subset(check,age==1.4))
words.bottom.right.logxy(mod)

plot(leaf_weight~stem_weight,data=subset(check,age==1.4), pch=16,log="xy",col="red")
text(leaf_weight~stem_weight,data=subset(check,age==1.4),labels=individual,cex=.5,pos=2,offset=-1)
mod <- lm(log(stem_weight)~log(leaf_weight),data=subset(check,age==1.4))
words.bottom.right.logxy(mod)


plot(stem_weight~dia,data=subset(check,age==.1),pch=16,log="xy",col="red")
text(stem_weight~dia,data=subset(check,age==.1),labels=individual,cex=.5,pos=2,offset=-1)

plot(leaf_weight~dia,data=subset(check,age==.1),pch=16,log="xy",col="red")
text(leaf_weight~dia,data=subset(check,age==.1),labels=individual,cex=.5,pos=2,offset=-1)

plot(leaf_weight~stem_weight,data=subset(check,age==.1), pch=16,log="xy",col="red")
text(leaf_weight~stem_weight,data=subset(check,age==.1),labels=individual,cex=.5,pos=2,offset=-1)
mod <- lm(log(stem_weight)~log(leaf_weight),data=subset(check,age==.1))
words.bottom.right.logxy(mod)


BAER_check <- BAER_HarvestData
BOLE_check <- BOLE_HarvestData
COER_check <- COER_HarvestData
EPMI_check <- EPMI_HarvestData
GRBU_check <- GRBU_HarvestData
GRSP_check <- GRSP_HarvestData
HATE_check <- HATE_HarvestData
HEPU_check <- HEPU_HarvestData
LEES_check <- LEES_HarvestData
PELA_check <- PELA_HarvestData
PEPU_check <- PEPU_HarvestData
PHPH_check <- PHPH_HarvestData
PILI_check <- PILI_HarvestData
PUTU_check <- PUTU_HarvestData

check_all <- rbind(BAER_check,BOLE_check,COER_check,EPMI_check,GRBU_check,GRSP_check,HATE_check,HEPU_check,LEES_check,PELA_check,PEPU_check,PHPH_check,PILI_check,PUTU_check)

plot(leaf_weight~dia,data=check_all, pch=16,log="xy",col=col.spp(species))