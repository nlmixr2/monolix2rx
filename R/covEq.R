#' Covariate equation interface
#'
#' @param text text for paring the covariate equation block
#' @return monolix2rxCovEq block
#' @noRd
#' @author Matthew L. Fidler
.covEq <- function(text) {
  .monolix2rx$equationLine <- character(0)
  .monolix2rx$odeType <- "nonStiff"
  .Call(`_monolix2rx_trans_equation`, text, "<MODEL> [COVARIATE] EQUATION:")
  .ret <- list(monolix=text,
               dplyr=gsub("<-", "=",
                          c(.monolix2rx$equationLine,
                            .monolix2rx$pk$equation$endLines), fixed=TRUE),
               odeType=.monolix2rx$odeType)
  class(.ret) <- "monolix2rxCovEq"
  .ret
}
