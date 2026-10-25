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
  # factorial() rounds its argument like Monolix
  .lines <- vapply(.monolix2rx$equationLine, function(l) {
    if (!grepl("\\bl?factorial[(]", l, perl=TRUE)) return(l)
    .e <- try(str2lang(l), silent=TRUE)
    if (inherits(.e, "try-error")) return(l)
    deparse1(.rxFactorial(.e))
  }, character(1), USE.NAMES=FALSE)
  .ret <- list(monolix=text,
               dplyr=gsub("<-", "=", .lines, fixed=TRUE),
               odeType=.monolix2rx$odeType)
  class(.ret) <- "monolix2rxCovEq"
  .ret
}
