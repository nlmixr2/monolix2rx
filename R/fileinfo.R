#' File info initialization
#'
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.fileinfoIni <- function() {
  .monolix2rx$file <- character(0)
  .monolix2rx$header <- character(0)
  .monolix2rx$fileinfo <- list()
}
#' Parse fileinfo text
#'
#' @param text text to parse
#' @return parsed fileinfo
#' @noRd
#' @author Matthew L. Fidler
.fileinfo <- function(text) {
  .fileinfoIni()
  .Call(`_monolix2rx_trans_fileinfo`, text)
  .fileinfo <- list(file=.monolix2rx$file,
                    header=.monolix2rx$header)
  class(.fileinfo) <- "monolix2rxFileinfo"
  .fileinfoIni()
  .monolix2rx$fileinfo <- .fileinfo
}
#' Assign the file from the fileinfo section
#'
#' @param file file name
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.fileinfoFile <- function(file) {
  .monolix2rx$file <- file
}
#' Add a header column
#'
#' @param head header column to add
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.fileinfoHeader <- function(head) {
  .monolix2rx$header <- c(.monolix2rx$header, head)
}

#' @export
print.monolix2rxFileinfo <- function(x, ...) {
  if (length(x$file) == 1L) {
    cat("file = '", x$file, "'\n", sep="")
  }
  cat("delimiter = comma\n")
  cat("header = {", paste(x$header, collapse=", "), "}\n", sep="")
}
