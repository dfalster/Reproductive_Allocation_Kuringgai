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
  exp(seq(log(from), log(to), length.out = length.out))
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
  seq(r[[1]], r[[2]], length.out = length.out)
}

# define functions
se <- function(x) {
  sd(x)/sqrt(length(x))
}

con95 <- function(x) {
  (sd(x)/sqrt(length(x))) * 1.96
}

clean_headers <- function(x) {
  x <- gsub("_", " ", x, fixed = TRUE)
  x <- lapply(x, firstup)
  x
}

# Captilise first letter of strings
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


# this function gives better behaviour of x/y when both x and y =0 and we are
# certain the answer should be zero
divide_zero <- function(x, y) {

  if (length(x) != length(y))
    stop("bad")

  ret <- x/y

  # if x and y are zero
  i <- y == 0 & x == 0
  ret[i] <- 0

  ret
}


read_csv <- function(filename) {
  read.csv(filename, header = TRUE, sep = ",", stringsAsFactors = FALSE)
}

combine_data_frames <- function(..., d = list(...)) {
  ldply(d, function(x) x)
}

combine_list_elements <- function(..., d = list(...), element) {
  ldply(d, function(x) x[[element]])
}

latex_build <- function(filename, bibliography = NULL, output = NULL, chdir = TRUE,
  interaction = "nonstopmode", max_attempts = 5L, clean = FALSE, engine = "pdflatex") {

  fileout <- sub(".tex$", ".pdf", filename)

  if (chdir) {
    owd <- setwd(dirname(filename))
    on.exit(setwd(owd))
    filename <- basename(filename)
  }

  res <- run_latex(filename, interaction, engine)
  if (engine == "xelatex") {
    res <- run_latex(filename, interaction, engine)
  }
  if (!is.null(bibliography)) {
    run_bibtex(filename)
    res <- run_latex(filename, interaction, engine)
  }

  Encoding(res) <- "UTF-8"

  pat <- c("Rerun to get cross-references right", # labels
           "Rerun to get citations correct",      # bibtex
           "Rerun to get outlines right")         # tikz

  isin <- function(p, x) {
    any(grepl(p, x))
  }

  for (i in seq_len(max_attempts)) {
    if (any(vapply(pat, isin, logical(1), res))) {
      res <- run_latex(filename, interaction, engine)
    } else {
      break
    }
  }

  if (clean) {
    latex_clean(filename)
  }

  if (!is.null(output)) {
    if (chdir) {
      setwd(owd)
    }
    file.copy(fileout, output, TRUE)
    file.remove(fileout)
  }

  invisible(NULL)
}

latex_clean <- function(filename) {
  filebase <- sub(".tex$", "", filename)
  exts <- c(".log", ".aux", ".bbl", ".blg", ".fls", ".out", ".snm", ".nav", ".tdo",
    ".toc")
  aux <- paste0(filebase, exts)
  file.remove(aux[file.exists(aux)])
}

run_latex <- function(filename, interaction = "nonstopmode", engine = "pdflatex") {
  args <- c(paste0("-interaction=", interaction), "-halt-on-error", filename)
  call_system(Sys_which(engine), args)
}

run_bibtex <- function(filename) {
  call_system(Sys_which("bibtex"), sub(".tex$", "", filename))
}

Sys_which <- function(x) {
  ret <- Sys.which(x)
  if (ret == "") {
    stop(sprintf("%s not found in $PATH", x))
  }
  ret
}


##' Function imported from callr package;  makes it easy to call a
##' system command from R and have it behave.
##'
##' This function uses \code{system2} to call a system command fairly
##' portably.  What it adds is a particular way of dealing with
##' errors.  \code{call_system} runs the command \code{command} with
##' arguments \code{args} (and with optionally set environment
##' variables \code{env}) and hides \emph{all} produced output to
##' stdout and stderr.  If the command fails (currently any nonzero
##' exit code is counted as a failure) then \code{call_system} will
##' throw an R error giving
##' \itemize{
##' \item the full string of the command run
##' \item the exit code of the command
##' \item any \code{errmsg} attribute that might have been returned
##' \item all output that the program produced to either stdout and
##' stderr
##' }
##'
##' This means that a successful invocation of a program produces no
##' output while the unsuccessful invocation throws an error and
##' prints all information to the screen (though this is delayed until
##' failure happens).
##'
##'
##' \code{call_system} also returns the contents of both stderr and
##' stdout \emph{invisibly} so that it can be inspected if needed.
##'
##' The function \code{run_system} does the same thing and will be
##' removed as soon as code that depends on it is out of use.
##'
##' @title Run a system command, stopping on error
##' @param command The system command to be invoked, as a character
##' string.  \code{\link{Sys.which}} is useful here.
##' @param args A character vector of arguments to \code{command}
##' @param env A character vector of name=value pairs to be set as
##' environment variables (see \code{\link{system2}}).
##' @param max_lines Maximum number of lines of program output to
##' print with the error message.  We may prune further to get the
##' error message under \code{getOption('warn.length')}, however.
##' @param p Fraction of the error message to show from the tail of
##' the output if truncating on error (default is 20\% lines are head,
##' 80\% is tail).
##' @param stdout,stderr Passed to \code{system2}.  Set one of these
##' to \code{FALSE} to avoid capturing output from that stream.  Setting
##' both to \code{FALSE} is not recommended.
##' @export
##' @author Rich FitzJohn
call_system <- function(command, args, env = character(), max_lines = 20, p = 0.8,
  stdout = TRUE, stderr = TRUE) {
  res <- suppressWarnings(system2(command, args, env = env, stdout = stdout,
    stderr = stderr))
  ok <- attr(res, "status")
  if (!is.null(ok) && ok != 0) {
    max_nc <- getOption("warning.length")

    cmd <- paste(c(env, shQuote(command), args), collapse = " ")
    msg <- sprintf("Running command:\n  %s\nhad status %d", cmd, ok)
    errmsg <- attr(cmd, "errmsg")
    if (!is.null(errmsg)) {
      msg <- c(msg, sprintf("%s\nerrmsg: %s", errmsg))
    }
    sep <- paste(rep("-", getOption("width")), collapse = "")

    ## Truncate message:
    if (length(res) > max_lines) {
      n <- ceiling(max_lines * p)
      res <- c(head(res, ceiling(max_lines - n)), sprintf("[[... %d lines dropped ...]]",
        length(res) - max_lines), tail(res, ceiling(n)))
    }

    ## compute the number of characters so far, including three new lines:
    nc <- (nchar(msg) + nchar(sep) * 2) + 3
    i <- max(1, which(cumsum(rev(nchar(res) + 1L)) < (max_nc - nc)))
    res <- res[(length(res) - i + 1L):length(res)]
    msg <- c(msg, "Program output:", sep, res, sep)
    stop(paste(msg, collapse = "\n"))
  }
  invisible(res)
}
