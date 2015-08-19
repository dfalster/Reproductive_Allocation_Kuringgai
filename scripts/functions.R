
#plotting and stats
#creating lists of information for plotting symbols, colors, labels

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