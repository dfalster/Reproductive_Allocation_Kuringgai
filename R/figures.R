to.dev <- function(expr, dev, filename, ..., verbose=TRUE) {
  if ( verbose )
    cat(sprintf("Creating %s\n", filename))
  dev(filename, ...)
  on.exit(dev.off())
  eval.parent(substitute(expr))
}

to.pdf <- function(expr, filename, ...) {
  to.dev(expr, pdf, filename, ...)
}


to.png <- function(expr, filename, ...) {
  to.dev(expr, png, filename, ...)
}

base_plot_logged <- function(..., axes=TRUE, labels=c(TRUE, TRUE)) {

  plot(..., type="n", log="xy", xlab="",ylab="", ann=FALSE, axes=FALSE)
  if(axes) {
    axis.log10(1, labels=labels[1])
    axis.log10(2, labels=labels[2])
    box()
  }
}

base_plot <- function(..., axes=TRUE, labels=c(TRUE, TRUE)) {
  plot(..., type="n", xlab="",ylab="", ann=FALSE, axes=FALSE)
  if(axes) {
    axis(1, labels=labels[1])
    axis(2, labels=labels[2], las=1)
    box()
  }
}


axis.log10 <- function(side=1, horiz=FALSE, labels=TRUE, 
  baseAxis = TRUE, wholenumbers=TRUE, labelEnds=TRUE, las=1, at=NULL) {

  fg <- par("fg")

  if(is.null(at)){

    #get range on axis
    if(side ==1 | side ==3) {
      r <- par("usr")[1:2]   #upper and lower limits of x-axis
    } else {
      r <- par("usr")[3:4] #upper and lower limits of y-axis
    }

    #make pretty intervals
    at <- pretty(r)
    #drop ends if desirbale
    if(!labelEnds)
      at <- at[at > r[1] & at < r[2]]
  }

  #restrict to whole numbers if desirable
  if(wholenumbers)
    at<-at[is.wholenumber(at)]

  lab <- do.call(expression, lapply(at, function(i) bquote(10^.(i))))

  #convert at if
  if(baseAxis)
    at<-10^at

  #make labels
  if ( labels )
    axis(side, at=at, lab, col=if(horiz) fg else NA,
         col.ticks=fg, las=las)
  else
    axis(side, at=at, FALSE, col=if(horiz) fg else NA,
         col.ticks=fg, las=las)
}

is.wholenumber <-  function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}


##' Sequence in log space
##'
##' Unlike the billions of options for \code{seq}, only
##' \code{length.out} is supported here, and both \code{from} and
##' \code{to} must be provided.  For completeness, \code{seq_range}
##' generates a range in non-log space.
##' @title Sequence in log space
##' @param from Starting point
##' @param to Ending point
##' @param length.out Number of points to generate
##' @author Rich FitzJohn
##' @export
seq_log <- function(from, to, length.out) {
  exp(seq(log(from), log(to), length.out=length.out))
}

##' @export
##' @param r range (i.e., c(from, to)
##' @rdname seq_log
seq_log_range <- function(r, length.out) {
  seq_log(r[[1]], r[[2]], length.out)
}

##' @export
##' @rdname seq_log
seq_range <- function(r, length.out) {
  seq(r[[1]], r[[2]], length.out=length.out)
}

#define functions
se <- function(x) {
  sd(x)/sqrt(length(x))
}

con95 <- function(x){
  (sd(x)/sqrt(length(x)))*1.96
}


col.spp <- function(x=NULL){

  ret <- c(BAER="red", BOLE="darkolivegreen4", COER="blue", EPMI="purple", GRBU="grey",
    GRSP="light green", HATE="gold1", HEPU="cyan2",LEES="black", PELA="chocolate1",
    PEPU="light blue", PHPH= "brown", PILI="deep pink", PUTU="orchid1")

  if(!is.null(x)) {
    ret <- ret[as.character(x)]
  }
  ret
}

labels.spp <- function(x=NULL){
  names(col.spp(x))
}

labels.spp.full <- function(x=NULL){

  ret <- c("Banksia ericifolia","Boronia ledifolia","Conospermum ericifolium","Epacris microphylla", "Grevillea buxifolia","Grevillea speciosa","Hakea teretifolia",
           "Hemigenia purpurea","Leucopogon esquamatus","Persoonia lanceolata", "Petrophile pulchella", "Phyllota phylicoides", "Pimelea linifolia", "Pultenaea tuberculata")
  names(ret) <- c("BAER","BOLE","COER","EPMI","GRBU","GRSP","HATE","HEPU","LEES","PELA","PEPU","PHPH","PILI","PUTU")

  if(!is.null(x)) {
    ret <- ret[as.character(x)]
  }
  ret
}

labels.spp.genus <- function(x=NULL){
  
  ret <- c("B. ericifolia","B. ledifolia","C. ericifolium","E. microphylla", "G. buxifolia","G. speciosa","H. teretifolia","H. purpurea","L. esquamatus","P. lanceolata", 
           "P. pulchella", "P. phylicoides", "P. linifolia", "P. tuberculata")
  names(ret) <- c("BAER","BOLE","COER","EPMI","GRBU","GRSP","HATE","HEPU","LEES","PELA","PEPU","PHPH","PILI","PUTU")
  
  if(!is.null(x)) {
    ret <- ret[as.character(x)]
  }
  ret
}

col.age <- function(x=NULL){

  ret <- c("red","orchid1","gold1","darkolivegreen3","cyan2","dodgerblue3","purple")
  names(ret) <- c("0.1","1.4","2.4","5","7","9","32")

  if(!is.null(x)) {
    ret <- ret[as.character(x)]
  }
  ret
}

labels.age <- function(x=NULL){
  names(col.age(x))
}

col.mature <- function(x=NULL){

  ret <- c("cyan3","red")
  names(ret) <- c("FALSE","TRUE")

  if(!is.null(x)) {
    ret <- ret[as.character(x)]
  }
  ret
}

labels.mature <- function(x=NULL){
 names(col.mature(x))
}

col.lots <- function(x) {
  #returns up to 80 unique, nice colors, generated using http://tools.medialab.sciences-po.fr/iwanthue/
  # Starts repeating after 80
  n <- length(unique(x))
  cols<-rep(c("#75954F","#D455E9","#E34423","#4CAAE1","#451431","#5DE737","#DC9B94","#DC3788","#E0A732","#67D4C1","#5F75E2","#1A3125","#65E689","#A8313C","#8D6F96","#5F3819","#D8CFE4","#BDE640","#DAD799","#D981DD","#61AD34","#B8784B","#892870","#445662","#493670","#3CA374","#E56C7F","#5F978F","#BAE684","#DB732A","#7148A8","#867927","#918C68","#98A730","#DDA5D2","#456C9C","#2B5024","#E4D742","#D3CAB6","#946661","#9B66E3","#AA3BA2","#A98FE1","#9AD3E8","#5F8FE0","#DF3565","#D5AC81","#6AE4AE","#652326","#575640","#2D6659","#26294A","#DA66AB","#E24849","#4A58A3","#9F3A59","#71E764","#CF7A99","#3B7A24","#AA9FA9","#DD39C0","#604458","#C7C568","#98A6DA","#DDAB5F","#96341B","#AED9A8","#55DBE7","#57B15C","#B9E0D5","#638294","#D16F5E","#504E1A","#342724","#64916A","#975EA8","#9D641E","#59A2BB","#7A3660","#64C32A"),
            ceiling(n/80))
  cols[as.factor(x)]
}

format_p <- function(p, digits = 4) {
  x <- as.character(round(p, digits = digits))
  pc <- 10^-digits
  x[p < pc] <- paste("<", pc)
  x
}


print_xtable_for_species <- function(thisSpecies, data, file){
  print_xtable(filter(data, species==thisSpecies),file=file)
}

print_xtable <- function(data, file){
 print(xtable(data, include.rownames=FALSE), floating=FALSE, type="latex",
    file=file)
}


#plotting and stats
#creating lists of information for plotting symbols, colors, labels

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
  
  xr <- range(x[["model"]][2])
  data.frame(
    dplyr::select(glance(x), r.squared, p.value),
    n= length(resid(x)),
    a = coef(x)[1],
    b = coef(x)[2],
    x_low = xr[1],
    x_high = xr[2]
  )
}

summarise_all_fits <- function(x) {
  tmp <- lapply(x, summarise_fit) %>%
    bind_rows
  data.frame(species=names(x), tmp)
}

summarise_fit_2var <- function(x) {
  data.frame(
    dplyr::select(glance(x), r.squared, p.value),
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

y.error.bars <- function (x,y,ybar) {
  arrows(x,y-ybar,x,y+ybar,code=3,angle=90, length=0.1,col=col.spp())
  }

y.error.bars.2 <- function (x,y,ybar) {
  arrows(x,y-ybar,x,y+ybar,code=3,angle=90, length=0,col=col.spp())
}

y.error.bars.black <- function (x,y,ybar) {
  arrows(x,y-ybar,x,y+ybar,code=3,angle=90, length=0.1,col="black")
}

se_function <- function(mean1,se1) {
  for(i in 1:14) {
    arrows(i,mean1[i]+se1[i],i,mean1[i]-se1[i],code=3,angle=90, length=0.1)
  }
}

line_function_log_xy <- function(x_axis,mod,type) {
  intercept <- mod$coef[[1]][1,1]
  slope <- mod$coef[[1]][2,1]
  x_min <- min(x_axis)
  x_max <- max(x_axis)
  y_min <- (x_min^slope)*(10^intercept)
  y_max <- (x_max^slope)*(10^intercept)
  segments(x_min,y_min,x_max,y_max,lty=type)
}

line_function_log_xy_zero <- function(x_axis,mod,slope) {
  intercept <- mod$coef[[1]][1,1]
  slope <- slope
  x_min <- min(x_axis)
  x_max <- max(x_axis)
  y_min <- (x_min^slope)*(10^intercept)
  y_max <- (x_max^slope)*(10^intercept)
  segments(x_min,y_min,x_max,y_max,lty=2)
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

words.top.left.logy <- function (x) {
  output <- data.frame(select(glance(x), r.squared, p.value))
  output <- round(output,4)
  text(((0.02*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),10^((0.93*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),paste("r squared = ",output[1]),adj=0,cex=.9)
  text(((0.02*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),10^((0.88*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),paste("p-value = ",output[2]),adj=0,cex=.9)
  text(((0.02*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),10^((0.98*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),summary(mod)[1],cex=.7,adj=0)
}

extra.top.left.logxy <- function (words,fontx,cexx) {
  text(10^((0.02*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),10^((0.96*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),words,adj=0,font=fontx,cex=cexx)
}

extra.top.left.logx <- function (words,fontx) {
  text(10^((0.02*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),((0.96*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),words,adj=0,cex=.75,font=fontx)
}

extra.top.left <- function (words) {
  text(((0.02*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),((0.96*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),words,adj=0,cex=.75)
}

extra.bottom.left.logxy <- function (words,fontx,cexx) {
  text(10^((0.02*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),10^((0.04*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),words,adj=0,cex=cexx,font=fontx)
}

extra.bottom.left.logx <- function (words,fontx,cexx) {
  text(10^((0.012*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),((0.04*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),words,adj=0,cex=cexx,font=fontx)
}


extra.bottom.left <- function (words) {
  text(((0.02*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),((0.04*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),words,adj=0,cex=.75)
}

extra.bottom.right.logx <- function (words,fontx) {
  text(10^((0.98*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),((0.04*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),words,cex=.75,adj=1,,font=fontx)
}

extra.top.right.logx <- function (words,fontx) {
  text(10^((0.98*(par("usr")[2]-par("usr")[1]))+par("usr")[1]),((0.96*(par("usr")[4]-par("usr")[3]))+par("usr")[3]),words,cex=.75,adj=1,,font=fontx)
}

lines <- function(results) {
  results$linetype <- 1
  results$linewidth <- 2
  
  results[as.numeric(results[["pval"]]) > 0.05,"linetype"] <- 2
  results[as.numeric(results[["pval"]]) > 0.10,"linetype"] <- 3
  results[as.numeric(results[["pval"]]) > 0.05,"linewidth"] <- 1.5
  results[as.numeric(results[["pval"]]) > 0.10,"linewidth"] <- 1
  results
}

legend_with_r2 <- function(results,location) {
  for_legend <- dplyr::select(results,group,r2)
  for_legend$r2 <-round(as.numeric(for_legend$r2),digits=3)
  for_legend$text <- paste(for_legend$group,for_legend$r2,sep="  ")
  legend(location,legend=for_legend$text,col=col.spp(),pch=16, cex=.6,bty="n",xjust=1,title="spp     r2")
}

sma_sum <- function (x) {
  dplyr::select(x$groupsummary,r2,pval,Slope,Slope_lowCI,Slope_highCI,Int,Slope_test_p)
}

sma_abline <- function (x, ...) {
  abline(x$coef[[1]][1,1],x$coef[[1]][2,1])
}