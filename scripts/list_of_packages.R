install.packages("smatr")
install.packages("ggplot2")
install.packages("ggtern")
install.packages("tidyr")
install.packages("xtable")
#install.packages("igraph")
install.packages("rmarkdown")
install.packages("broom")
install.packages("yaml")
install.packages("mgcv")
install.packages("plyr")
install.packages("dplyr")
install.packages("devtools")
install.packages(c("R6", "yaml", "digest", "crayon", "optparse"))


devtools::install_github("richfitz/remake")
devtools::install_github("richfitz/storr")

packageurl <- "https://cran.r-project.org/src/contrib/Archive/igraph/igraph_0.7.1.tar.gz"
install.packages(packageurl, repos=NULL, type="source")

install_version("igraph", version = "0.7.1", repos = "http://cran.us.r-project.org")

library(devtools)



igraph_old <- "https://mran.revolutionanalytics.com/snapshot/2015-05-01/bin/windows/contrib/3.3/igraph_0.7.1.zip"
install.packages(igraph_old, type = "win.binary", repos = NULL)