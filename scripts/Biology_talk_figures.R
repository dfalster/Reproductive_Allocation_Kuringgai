remake::make("export")

SummaryInd <- readRDS("export/SummaryInd.rds")
SummarySpp <- readRDS("export/SummarySpp.rds")
SummarySppAge <- readRDS("export/SummarySppAge.rds")
HarvestData <- readRDS("export/HarvestData.rds")
InvestmentByPart <- readRDS("export/Investment_FD_all.rds")

source("R/figures.R")
source("R/RAS-functions.R")

figure_1 <- function(cex=1){
  par(mfrow=c(1,1), cex=cex, oma=c(4,4,1,3), mar=c(1,2,1,1))

  base_plot_logged(repro_inv~growth_leaf, SummaryInd, xlim=c(1,1000000),ylim=c(1,1000000))
  legend(2e6, 1000,legend=labels.spp(), col=col.spp(), pch=16, cex=.8,bty="n", xpd=NA, title ="Species")

  # Calculate leaf and repro divestment given different leaf and RA
  f <- function(r,x) {x*r / (1-r)}
  # RA =  R / (R+L) -->
  RA <- c(0.01, 0.1, 0.5, 0.75, 0.95)
  labs <- c(170, 19,2.1, 2, 2,2)
  X <- 10^c(-1,7)
  for(i in seq_along(RA)) {
    lines(X, f(RA[i], X))
    text(labs[i], f(RA[i],labs[i])*1.5,srt=45,paste("RA =", RA[i]),cex=1)
  }


  points(repro_inv~growth_leaf, SummaryInd, col=col.spp(species), pch=16)
  mtext("Reproductive investment (mg)", 2, outer=TRUE,cex=cex,line=2)
  mtext("Leaf investment (mg)", 1, outer=TRUE,cex=cex,line=2)
}

to.pdf(figure_1(), "figs2/RA_all.pdf", height=6, width=6)

# ----------------------------------------------------------
figure_2 <- function(n=4){
  groups <- list(
    Gradual = c("GRBU", "EPMI", "LEES"),
    Gradual = c("BOLE", "PILI", "GRSP"),
    Asymptotic = c("PHPH", "HEPU", "PEPU"),
    Increasing = c("BAER", "HATE", "PELA"))
  #  excluded <- c("COER", "PUTU")

  data1 <- split(SummaryInd, SummaryInd$species)

  par(mfcol=c(3,4), oma=c(4,4,4,4), mar=c(1,1,1,1))
  for(j in seq_len(4)) {
    for(i in 1:3) {
      spp <- groups[[j]][i]
      y <- data1[[spp]][["RA_leaf_area"]]
      x <- data1[[spp]][["age"]]
      if(j <= n ){
        base_plot(NA, log="x",xlim=c(1,33),ylim=c(0,1.2), labels = c(i==3, spp %in% groups[[1]]),
          yaxs="i", xaxs="i")
        abline(h=1, col="lightgrey", lty="dashed")
        if (j==2){
          polygon(c(15, 35, 35, 15), c(1.2, 1.2, 0, 0), col="lightgrey", border=NA)
        }

        points(x,y, col=col.age(data1[[spp]]$age), pch=16, cex=1.2)
        lines(lowess(y~x, f = 0.6), col='red', lwd=2)
        if(i==1) {
          mtext(names(groups)[j], side=3, line =1)
        }
      } else {
         base_plot(NA, log="x",xlim=c(1,33),ylim=c(0,1.2), axes=FALSE)
      }
    }
  }
  legend(50, 0.5,legend=labels.age(),col=col.age(),pch=16, bty="n", xpd=NA, title ="Age")
  mtext("Reproductive allocation", 2, outer=TRUE, line=2)
  mtext("Age (years)", 1, outer=TRUE, line=2)
}

for(i in 1:4){
  to.pdf(figure_2(i), paste0("figs2/RA_scheules-",i,".pdf"), height=6, width=8)
}

# ----------------------------------------------------------

data1 <- split(SummaryInd, SummaryInd$species)

figure_3 <- function(n=3){

  species <- c("BOLE", "GRBU", "EPMI", "HATE")
  yvars <- c("RA_leaf_area", "growth_leaf", "leaf_area")
  ylabs <- c("Reproductive allocation (0-1)", "Leaf investment (mg)", "Leaf area (cm2)")

  par(mfcol=c(4,3), oma=c(4,4,4,4), mar=c(1,4,2,1))
  for(j in seq_len(n)) {
    for(i in 1:4) {
      spp <- species[i]
      y <- data1[[spp]][[yvars[j] ]]
      x <- data1[[spp]][["age"]]
      if(j == 1) {
        base_plot(x,y, log="x", xlim=c(1,33), ylim = c(0, 1.2), labels = c(i==4, TRUE))
      } else {
       base_plot(x,y, log="x", xlim=c(1,33), labels = c(i==4, TRUE))
     }
      points(x,y, col=col.age(data1[[spp]]$age), pch=16, cex=1.2)
      lines(lowess(y~x, f = 0.6), col='red', lwd=2)
      if(i==1) {
        mtext(ylabs[j], side=3, line =4)
      }
      if(i==4 & j==1){
        mtext("Age (years)", 1, line=3)
      }
    }
  }
}

for(i in 1:3){
  to.pdf(figure_3(i), paste0("figs2/RA_leaf-",i,".pdf"), height=6, width=10)
}


figure_4 <- function(n=2){

  par(mfcol=c(1,2), oma=c(1,1,1,1), mar=c(3,5,1,1))

  H <- seq(0, 1.5, length.out = 200)
  RAS <- make_RAS_shapes()

  f <- function(x, L= 1, k= 10, x0=0.5) {L / (1+ exp(-k*(x-x0)))}
  Suplus <- f(H)
  RA <-   RAS[["Partial bang"]](H, mat=0.8, RAinit=0.1, RAmax=0.8, A50=0.1)
  R <- Suplus*RA
  plot(H, Suplus, type="n", lwd=2, ylim= c(0,1.2), axes=FALSE, ann=FALSE, yaxs="i", xaxs="i")
  axis(1, labels=FALSE)
  axis(2, labels=FALSE)
  mtext("Annual biomass production (kg/yr)",2, line=2)
  mtext("Plant height (m)",1, line=2)
  polygon(c(0, H, 1.5), c(0, Suplus, 0), col="red", border=NA))
  polygon(c(H, 1.5), c(R, 0), col="chocolate1", border=NA)

  if( n >1) {
    plot(H, RA, type="l", col="chocolate1", lwd=3, ylim= c(0,1), axes=FALSE, ann=FALSE, yaxs="i", xaxs="i")
    axis(1, labels=FALSE)
    axis(2, labels=TRUE, las=1)
    mtext("Reproductive allocation",2, line=3)
    mtext("Plant height",1, line=2)
  }
}

for(i in 1:2){
  to.pdf(figure_4(i), paste0("figs2/RA_intro-",i,".pdf"), height=4, width=12)
}



plotRASexamples <- function(){

  par(mfcol=c(1,5), oma=c(3,1,3,1), mar=c(3,5,1,1))

  new_plot <- function(){
    base_plot(NA, xlim=c(0, 1), ylim=c(-0,1), labels = c(FALSE, FALSE),
          yaxs="i", xaxs="i")
    axis(2, at = c(0,1), las = 1)
  }
 
  x <- seq(0,1, by=0.001)

  RAS <- make_RAS_shapes()

  types <- c("Big bang", "Gradual - determinate", "Asymptotic" ,"Gradual - indeterminate", "Declining")
  labs <- c("Big bang", "Gradual", "Asymptotic" ,"Increasing", "Declining")
  for(i in 1:5) {
    n <- types[[i]]
    new_plot()
    if(n == "Big bang")
      y <- RAS[[n]](x, mat=0.5)
    if(n == "Partial bang")
      y <- RAS[[n]](x, mat=0.5,  RAinit=0.6, A50=0.05,  RAmax=0.8)
    if(n == "Asymptotic")
      y <- RAS[[n]](x, mat=0.5, A50=0.025,  RAmax=0.8)
    if(n == "Gradual - indeterminate")
      y <- RAS[[n]](x, mat=0.3)
    if(n == "Gradual - determinate")
      y <- RAS[[n]](x,  mat=0.3, A50=0.5)
    if(n == "Declining")
      y <- RAS[[n]](x, mat=0.5)

    points(x, y, type='l', lwd=3, col="chocolate1")
    mtext(labs[[i]], 3, line=2)
    if(i==3)
      mtext("Plant size",1, 2) 
  }
}

to.pdf(plotRASexamples(), "figs2/RASexamples.pdf", height=4, width=12)
