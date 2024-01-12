.covEq <- function(text) {
  .monolix2rx$equationLine <- character(0)
  .monolix2rx$odeType <- "nonStiff"
  .Call(`_monolix2rx_trans_equation`, text, "<MODEL> [COVARIATE] EQUATION:")
  .ret <- list(monolix=text,
               rx=c(.monolix2rx$equationLine,
                    .monolix2rx$pk$equation$endLines),
               odeType=.monolix2rx$odeType)
  class(.ret) <- "monolix2rxCovEq"
  .ret
}
