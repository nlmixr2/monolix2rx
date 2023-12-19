#' Initialize the ini block for parsing
#'
#' @param full Is this a full parameter reset (if `TRUE` reset more)
#' @return nothing; called for side effects
#' @noRd
#' @author Matthew L. Fidler
.inputIni <- function(full=TRUE) {
  .monolix2rx$catLst <- character(0)
  .monolix2rx$cat <- character(0)
  if (full) {
    .monolix2rx$inpLst <- character(0)
  }
}

.input <- function(inp) {
  .inputIni(TRUE)
  .Call(`_monolix2rx_trans_individual`, inp)
}

.inputCatItem <- function(var) {
  .monolix2rx$catLst <- c(.monolix2rx$catLst, var)
}

.inputAdd <- function(var)  {
  .monolix2rx$inpLst <- c(.monolix2rx$inpLst, var)
}

.intputCat <- function(var) {
  ## .monolix2rx$catLst <- c(.monolix2rx$catLst, var)
}
