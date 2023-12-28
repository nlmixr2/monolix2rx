.monolix2rx <- new.env(parent=emptyenv())

#' Translate a monolix file to rxode2
#'
#' @param mlxtran file name for mlxtran to translate to rxode2
#' @return rxode2 model
#' @export
#' @author Matthew L. Fidler
#' @useDynLib monolix2rx, .registration=TRUE
#' @importFrom Rcpp cppFunction
#' @importFrom dparser dparse
#' @importFrom stats setNames
#' @eval .monolix2rxBuildGram()
#' @examples
monolix2rx <- function(mlxtran) {
  .ret <- mlxtran(mlxtran, equation=TRUE)
  .model <- c("model({",
              .ret$MODEL$INDIVIDUAL$DEFINITION$rx,
              .ret$MODEL$LONGITUDINAL$EQUATION$rx,
              "})")
  .model <- str2lang(paste0(.model, collapse="\n"))
  .model
}
