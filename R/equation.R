#' Parse equation block
#'
#' @param text monolix equation to text to rxode2 code
#' @return rode2 code (`$rx`) and odeType (`$odeType`)
#' @noRd
#' @author Matthew L. Fidler
.equation <- function(text) {
  .monolix2rx$equationLine <- character(0)
  .monolix2rx$odeType <- "nonStiff"
  .Call(`_monolix2rx_trans_equation`, text)
  .ret <- list(rx=.monolix2rx$equationLine,
               odeType=.monolix2rx$odeType)
  .ret
}
#' Add an equation line
#'
#' @param line add a line in the current model equation
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.equationLine <- function(line) {
  .monolix2rx$equationLine <- c(.monolix2rx$equationLine, line)
}
#' Set the ode type
#'
#' @param odeType ode type that is defined
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.equationOdeType <- function(odeType) {
  .monolix2rx$odeType <- odeType
}
