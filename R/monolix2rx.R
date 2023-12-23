.monolix2rx <- new.env(parent=emptyenv())

#' Clear the monolix2rx environment
#'
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.clearMonolix2rx <- function() {
  .indDefIni()
}

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
monolixr2rx <- function(mlxtran) {

}
