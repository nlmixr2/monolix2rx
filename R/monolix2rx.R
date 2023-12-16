.monolix2rx <- new.env(parent=emptyenv())

#' Clear the monolix2rx environment
#'
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.clearMonolix2rx <- function() {
  .monolix2rx$varName  <- NA_character_
  .monolix2rx$dist     <- NA_character_
  .monolix2rx$isMean   <- NA
  .monolix2rx$varEst   <- NA_character_
  .monolix2rx$varVal   <- NA_real_
  .monolix2rx$varFixed <- NA
  .monolix2rx$sd       <- character(0)
  .monolix2rx$sdVal    <- numeric(0)
  .monolix2rx$var      <- character(0)
  .monolix2rx$varVal   <- numeric(0)
  .monolix2rx$min      <- -Inf
  .monolix2rx$max      <- Inf
  .monolix2rx$iov      <- character(0)
  .monolix2rx$cov      <- character(0)
}

#' Translate a monolix file to rxode2
#'
#' @param mlxtran file name for mlxtran to translate to rxode2
#' @return rxode2 model
#' @export
#' @author Matthew L. Fidler
#' @eval .monolix2rxBuildGram()
#' @examples
monolixr2rx <- function(mlxtran) {

}
