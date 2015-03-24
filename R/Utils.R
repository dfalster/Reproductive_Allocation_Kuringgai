
read_csv <- function(filename) {
  read.csv(filename, header = TRUE, sep = ",", stringsAsFactors = FALSE)
}

tex_2_pdf <- function(texfile){
  filename <- tools::file_path_sans_ext(texfile)
  system(sprintf("pdflatex %s", texfile))
  aux.files <- paste0(filename, c(".log", ".aux", ".bbl", ".blg"))
  file.remove(aux.files[file.exists(aux.files)])
}

#define functions
se <- function(x) {
  sd(x)/sqrt(length(x))
}

con95 <- function(x){
  (sd(x)/sqrt(length(x)))*1.96
}

combine_data_frames <- function(..., d=list(...)) {
   ldply(d, function(x) x)
 }
