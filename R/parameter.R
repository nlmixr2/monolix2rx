#' Initialize the parameter variables for parsing
#'
#' @param full Is this a full parameter reset (if `TRUE` reset more)
#' @return nothing; called for side effects
#' @noRd
#' @author Matthew L. Fidler
.parameterIni <- function(full=TRUE) {
  .monolix2rx$varName  <- NA_character_
  .monolix2rx$varVal   <- NA_real_
  .monolix2rx$varMethod <- "unknown"
  if (full) {
    .monolix2rx$parameter <- data.frame(name=character(0), value=numeric(0), method=character(0))
  }
}
.parameterPush <- function() {
  if (!is.na(.monolix2rx$varName)) {
    .monolix2rx$parameter <- rbind(.monolix2rx$parameter,
                                   data.frame(name=.monolix2rx$varName,
                                              value=.monolix2rx$varVal,
                                              method=.monolix2rx$varMethod))
    .parameterIni(full=FALSE)
  }
}
#' Parse parameters
#'
#' @param text text to pasre
#' @return mlxtranParameter object
#' @noRd
#' @author Matthew L. Fidler
.parameter <- function(text) {
  .parameterIni(TRUE)
  .Call(`_monolix2rx_trans_parameter`, text)
  .parameterFinalize()
  .monolix2rx$parameter
}
#' Set parameter name for current estimate
#'
#' @param name name of the parameter
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.parameterName <- function(name) {
  .parameterPush()
  .monolix2rx$varName <- name
}
#' Finalize parameters
#'
#' @return  nothing; called for side effects
#' @noRd
#' @author Matthew L. Fidler
.parameterFinalize <- function() {
  .parameterPush()
  class(.monolix2rx$parameter) <- c("monolix2rxParameter", class(.monolix2rx$parameter))
}

#' Set the parameter value
#'
#' @param value value as a string
#' @return nothing called for side effects
#' @noRd
#' @author Matthew L. Fidler
.parameterValue <- function(value) {
  .monolix2rx$varVal <- as.numeric(value)
}
#' Set the parameter estimation method
#'
#' @param method the method for parameter estimation
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.parameterMethod <- function(method) {
  .monolix2rx$varMethod <- method
}
#' @export
print.monolix2rxParameter <- function(x, ...) {
  cat(paste(vapply(seq_along(.monolix2rx$parameter$name),
         function(i) {
           if (.monolix2rx$parameter$method[i] == "unknown") {
             paste0(.monolix2rx$parameter$name[i], " = ",
                    .monolix2rx$parameter$value[i])
           } else {
             paste0(.monolix2rx$parameter$name[i], " = {value=",
                    .monolix2rx$parameter$value[i], ", method=",
                    toupper(.monolix2rx$parameter$method[i]), "}")
           }
         }, character(1), USE.NAMES=TRUE),
        collapse="\n"), "\n", sep="")
}
