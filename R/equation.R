.equation <- function(text) {
  .monolix2rx$equationLine <- character(0)
  .monolix2rx$odeType <- "nonStiff"
  .Call(`_monolix2rx_trans_equation`, text)
  .ret <- list(rx=.monolix2rx$equationLine,
               odeType=.monolix2rx$odeType)
  .ret
}

.equationLine <- function(line) {
  .monolix2rx$equationLine <- c(.monolix2rx$equationLine, line)
}
