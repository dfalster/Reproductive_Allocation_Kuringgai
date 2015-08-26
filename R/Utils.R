
divide_zero <- function(x,y) {
  if(length(x) != length(y)) stop("bad")
  ret <- y*0
  i <- y!=0 
  ret[i] <- (x/y)[i]
  ret
}

read_csv <- function(filename) {
  read.csv(filename, header = TRUE, sep = ",", stringsAsFactors = FALSE)
}

tex_2_pdf <- function(texfile){
  filename <- tools::file_path_sans_ext(texfile)
  system(sprintf("pdflatex %s", texfile))
  aux.files <- paste0(filename, c(".log", ".aux", ".bbl", ".blg"))
  file.remove(aux.files[file.exists(aux.files)])
}

combine_data_frames <- function(..., d=list(...)) {
   ldply(d, function(x) x)
 }

combine_list_elements <- function(..., d=list(...), element) {
   ldply(d, function(x) x[[element]])
 }