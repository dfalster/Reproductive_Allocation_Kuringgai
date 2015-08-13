
#plotting and stats
#creating lists of information for plotting symbols, colors, labels
pch.spp <- c(0,1,2,15,16,17,18,0,1,2,15,16,17,18)
col.spp <- c("red", "darkolivegreen4", "blue", "purple", "grey", "light green","gold1","cyan2","black", "chocolate1","light blue","brown","deep pink","orchid1")
col.age <-c("orchid1","gold1","darkolivegreen3","cyan2","dodgerblue","purple")
pch.age <- c(1,1,16,16,16,16)
labels.spp <- c("Banksia ericifolia","Boronia ledifolia","Conospermum ericifolium","Epacris micraphylla", "Grevillea buxifolia","Grevillea speciosa","Hakea teretifolia","Hemigenia purpurea","Leucopogon esquamatus","Persoonia lanceolata", "Petrophile puchella", "Phyllota phyllicoides", "Pimelea linifolia", "Pultanaea tuberculata")
names(labels.spp) <- c("BAER","BOLE","COER","EPMI","GRBU","GRSP","HATE","HEPU","LEES","PELA","PEPU","PHPH","PILI","PUTU")
labels.age <- c("1.4","2.4","5","7","9","32")
names(labels.age) <- c("1.4","2.4","5","7","9","32")
labels.age2 <- c("2.4","5","7","9","32")
col.age2 <-c("gold1","darkolivegreen3","cyan2","dodgerblue","purple")
labels.age3 <- c("5","7","9","32")
col.age3 <-c("darkolivegreen3","cyan2","dodgerblue","purple")
labels.spp.noHATE <- c("Banksia ericifolia","Boronia ledifolia","Conospermum ericifolium","Epacris micraphylla", "Grevillea buxifolia","Grevillea speciosa","Hemigenia purpurea","Leucopogon esquamatus","Persoonia lanceolata", "Petrophile puchella", "Phyllota phyllicoides", "Pimelea linifolia", "Pultanaea tuberculata")
names(labels.spp.noHATE) <- c("BAER","BOLE","COER","EPMI","GRBU","GRSP","HEPU","LEES","PELA","PEPU","PHPH","PILI","PUTU")
col.spp.noHATE <- c("red", "darkolivegreen4", "blue", "purple", "grey", "light green","cyan2","black", "chocolate1","light blue","brown","deep pink","orchid1")
col.LMA <-c("orchid1","gold1","darkolivegreen3","cyan2","dodgerblue","purple","red")
labels.LMA <-c("age 1.4", "age 2.4", "age 5", "age 7", "age 9", "age 32", "HATE")
col.lots <- c("red", "darkolivegreen4", "blue", "purple", "grey", "light green","gold1","cyan2","black", "chocolate1","light blue","brown","deep pink","orchid1",
              "dodgerblue","darkcyan","brown2","bisque4","blue4","cadetblue","darkseagreen","darkslateblue","firebrick4","red", "darkolivegreen4", "blue", "purple", "grey", "light green","gold1","cyan2","black", "chocolate1","light blue","brown","deep pink","orchid1",
              "dodgerblue","darkcyan","brown2","bisque4","blue4","cadetblue","darkseagreen","darkslateblue","firebrick4","red", "darkolivegreen4", "blue", "purple", "grey", "light green","gold1","cyan2","black", "chocolate1","light blue","brown","deep pink","orchid1",
              "dodgerblue","darkcyan","brown2","bisque4","blue4","cadetblue","darkseagreen","darkslateblue","firebrick4")
col.mature <- c("cyan3","red")
labels.mature <- c("juvenile","mature")
names(labels.mature) <- c("juvenile","mature")

axis_labels <- read_csv("data/axis_labels.csv")

plot_yvar_vs_xvar <- function(data, yvar = "y", xvar = "x", ...) {
  Y <- data[[yvar]]
  X <- data[[xvar]]
  plot(Y ~ X, data=data,  cex.axis=.8, las=1, pch=16, ...)
}

plot_yvar2_vs_xvar2 <- function(data, yvar2 = "y", xvar2 = "x", ...) {
  Y <- data[[yvar2]]
  X <- data[[xvar2]]
  plot(Y ~ X, data=data,  cex.axis=.8, las=1, pch=16, ...)
}

plot_yvar3_vs_xvar3 <- function(data, yvar3 = "y", xvar3 = "x", ...) {
  Y <- data[[yvar3]]
  X <- data[[xvar3]]
  plot(Y ~ X, data=data,  cex.axis=.8, las=1, pch=16, ...)
}



summarise_fit <- function(x) {
  data.frame(
    select(glance(x), r.squared, p.value),
    n= length(resid(x)),
    a = coef(x)[1],
    b = coef(x)[2]
  )
}

summarise_all_fits <- function(x) {
  tmp <- lapply(x, summarise_fit) %>%
    bind_rows
  data.frame(species=names(x), tmp)
}

summarise_fit_2var <- function(x) {
  data.frame(
    select(glance(x), r.squared, p.value),
    n= length(resid(x)),
    intercept = coef(x)[1],
    coef_var1 = coef(x)[2],
    coef_var2 = coef(x)[3],
    coef_var1var2 = coef(x)[4],
    SS_var1 = anova(x)[1,2],
    SS_var2 = anova(x)[2,2],
    SS_var1var2 =anova(x)[3,2]
  )
}

summarise_all_fits_2var <- function(x) {
  tmp <- lapply(x, summarise_fit_2var) %>%
    bind_rows
  data.frame(species=names(x), tmp)
}

xy.error.bars <- function (x,y,xbar,ybar) {
  plot(x,y,pch=16, ylim=c(min(y-ybar),max(y+ybar)),xlim=c(min(x-xbar),max(x+xbar)))
  arrows(x,y-ybar,x,y+ybar,code=3,angle=90, length=0.1)
  arrows(x-xbar,y,x+xbar,y,code=3,angle=90, length=0.1)
}

se_function <- function(mean1,se1) {
  for(i in 1:14) {
    arrows(i,mean1[i]+se1[i],i,mean1[i]-se1[i],code=3,angle=90, length=0.1)
  }
}

words.top.right.logxy <- function (x) {
  output <- data.frame(select(glance(x), r.squared, p.value))
  output <- round(output,4)
  text(10^((0.7*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),10^((0.93*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),paste("r squared = ",output[1]),adj=0,cex=.9)
  text(10^((0.7*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),10^((0.88*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),paste("p-value = ",output[2]),adj=0,cex=.9)
  text(10^((0.99*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),10^((0.98*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),summary(mod)[1],cex=.7,adj=1)
}

words.top.right.logx <- function (x) {
  output <- data.frame(select(glance(x), r.squared, p.value))
  output <- round(output,4)
  text(10^((0.7*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),((0.93*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),paste("r squared = ",output[1]),adj=0,cex=.9)
  text(10^((0.7*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),((0.88*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),paste("p-value = ",output[2]),adj=0,cex=.9)
  text(10^((0.99*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),((0.98*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),summary(mod)[1],cex=.7,adj=1)
}

words.top.right.logy <- function (x) {
  output <- data.frame(select(glance(x), r.squared, p.value))
  output <- round(output,4)
  text(((0.7*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),10^((0.93*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),paste("r squared = ",output[1]),adj=0,cex=.9)
  text(((0.7*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),10^((0.88*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),paste("p-value = ",output[2]),adj=0,cex=.9)
  text(((0.99*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),10^((0.98*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),summary(mod)[1],cex=.7,adj=1)
}

words.top.right <- function (x) {
  output <- data.frame(select(glance(x), r.squared, p.value))
  output <- round(output,4)
  text(((0.7*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),((0.93*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),paste("r squared = ",output[1]),adj=0,cex=.9)
  text(((0.7*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),((0.88*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),paste("p-value = ",output[2]),adj=0,cex=.9)
  text(((0.99*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),((0.98*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),summary(mod)[1],cex=.7,adj=1)
}

words.bottom.right.logxy <- function (x) {
  output <- data.frame(select(glance(x), r.squared, p.value))
  output <- round(output,4)
  text(10^((0.7*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),10^((0.12*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),paste("r squared = ",output[1]),adj=0,cex=.9)
  text(10^((0.7*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),10^((0.07*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),paste("p-value = ",output[2]),adj=0,cex=.9)
  text(10^((0.99*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),10^((0.02*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),summary(mod)[1],cex=.7,adj=1)
}

words.bottom.right.logx <- function (x) {
  output <- data.frame(select(glance(x), r.squared, p.value))
  output <- round(output,4)
  text(10^((0.7*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),((0.12*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),paste("r squared = ",output[1]),adj=0,cex=.9)
  text(10^((0.7*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),((0.07*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),paste("p-value = ",output[2]),adj=0,cex=.9)
  text(10^((0.99*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),((0.02*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),summary(mod)[1],cex=.7,adj=1)
}

words.bottom.right <- function (x) {
  output <- data.frame(select(glance(x), r.squared, p.value))
  output <- round(output,4)
  text(((0.7*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),((0.12*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),paste("r squared = ",output[1]),adj=0,cex=.9)
  text(((0.7*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),((0.07*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),paste("p-value = ",output[2]),adj=0,cex=.9)
  text(((0.99*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),((0.02*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),summary(mod)[1],cex=.7,adj=1)
}

words.bottom.right.logy <- function (x) {
  output <- data.frame(select(glance(x), r.squared, p.value))
  output <- round(output,4)
  text(((0.7*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),10^((0.12*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),paste("r squared = ",output[1]),adj=0,cex=.9)
  text(((0.7*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),10^((0.07*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),paste("p-value = ",output[2]),adj=0,cex=.9)
  text(((0.99*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),10^((0.02*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),summary(mod)[1],cex=.7,adj=1)
}

words.bottom.left.logy <- function (x) {
  output <- data.frame(select(glance(x), r.squared, p.value))
  output <- round(output,4)
  text(((0.02*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),10^((0.12*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),paste("r squared = ",output[1]),adj=0,cex=.9)
  text(((0.02*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),10^((0.07*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),paste("p-value = ",output[2]),adj=0,cex=.9)
  text(((0.02*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),10^((0.02*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),summary(mod)[1],cex=.7,adj=0)
}

words.bottom.left <- function (x) {
  output <- data.frame(select(glance(x), r.squared, p.value))
  output <- round(output,4)
  text(((0.02*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),((0.12*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),paste("r squared = ",output[1]),adj=0,cex=.9)
  text(((0.02*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),((0.07*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),paste("p-value = ",output[2]),adj=0,cex=.9)
  text(((0.02*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),((0.02*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),summary(mod)[1],cex=.7,adj=0)
}

words.bottom.left.logx <- function (x) {
  output <- data.frame(select(glance(x), r.squared, p.value))
  output <- round(output,4)
  text(10^((0.02*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),((0.12*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),paste("r squared = ",output[1]),adj=0,cex=.9)
  text(10^((0.02*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),((0.07*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),paste("p-value = ",output[2]),adj=0,cex=.9)
  text(10^((0.02*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),((0.02*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),summary(mod)[1],cex=.7,adj=0)
}

words.bottom.left.logxy <- function (x) {
  output <- data.frame(select(glance(x), r.squared, p.value))
  output <- round(output,4)
  text(10^((0.02*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),10^((0.12*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),paste("r squared = ",output[1]),adj=0,cex=.9)
  text(10^((0.02*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),10^((0.07*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),paste("p-value = ",output[2]),adj=0,cex=.9)
  text(10^((0.02*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),10^((0.02*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),summary(mod)[1],cex=.7,adj=0)
}

words.top.left.logxy <- function (x) {
  output <- data.frame(select(glance(x), r.squared, p.value))
  output <- round(output,4)
  text(10^((0.02*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),10^((0.93*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),paste("r squared = ",output[1]),adj=0,cex=.9)
  text(10^((0.02*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),10^((0.88*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),paste("p-value = ",output[2]),adj=0,cex=.9)
  text(10^((0.02*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),10^((0.98*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),summary(mod)[1],cex=.7,adj=0)
}