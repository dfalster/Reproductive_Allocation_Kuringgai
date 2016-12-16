
# this function gives better behaviour of x/y when both x and y =0 and we are certain the answer should be zero
divide_zero <- function(x,y) {

  if(length(x) != length(y)) stop("bad")

  ret <- x/y

  # if x and y are zero
  i <- y==0 & x==0
  ret[i] <- 0

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