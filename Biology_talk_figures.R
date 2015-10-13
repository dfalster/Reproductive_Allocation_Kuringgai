remake::make("export")

SummaryInd <- readRDS("export/SummaryInd.rds")
SummarySpp <- readRDS("export/SummarySpp.rds")
SummarySppAge <- readRDS("export/SummarySppAge.rds")
HarvestData <- readRDS("export/HarvestData.rds")
InvestmentByPart <- readRDS("export/Investment_FD_all.rds")

source("R/figures.R")

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
    determinate1 = c("BOLE", "PILI", "GRSP"),
    determinate2 = c("GRBU", "EPMI", "LEES"),
    asymptotic = c("PHPH", "HEPU", "PEPU"),
    indeterminate = c("BAER", "HATE", "PELA"))
  #  excluded <- c("COER", "PUTU") 

  data1 <- split(SummaryInd, SummaryInd$species)

  par(mfcol=c(3,4), oma=c(4,4,4,4), mar=c(1,1,1,1)) 
  for(j in seq_len(4)) {
    for(i in 1:3) {
      spp <- groups[[j]][i]
      y <- data1[[spp]][["RA_leaf_area"]]
      x <- data1[[spp]][["age"]]
      if(j <= n ){
        base_plot(NA, log="x",xlim=c(1,33),ylim=c(0,1.2), labels = c(i==3, spp %in% groups[[1]]))
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
  mtext("Reproductive allocation (0-1)", 2, outer=TRUE, line=2)
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

