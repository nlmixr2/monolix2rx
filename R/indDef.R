#' Add a variable for monolix parsing
#'
#'
#' @param var Variable to add
#' @return nothing called for side effects
#' @noRd
#' @author Matthew L. Fidler
.addVar <- function(var) {
  .monolix2rx$var <- var
}

#' Add a distribution type for monolix parsing
#'
#' @param dist distribution type
#' @return nothing; called for side effects
#' @noRd
#' @author Matthew L. Fidler
.setDist <- function(dist) {
  .monoix2rx$dist <- tolower(dist)
}
#' Set population estimation variable
#'
#' @param var variable name
#' @param isMean is this the `mean` (`1L`) or `typical` (`0L`)
#' @return nothing; called for side effects
#' @author Matthew L. Fidler
#' @noRd
.setTypicalEst <- function(var, isMean) {
  .monolix2rx$isMean <- (isMean == 1L)
  .monolix2rx$varEst <- var
  .monolix2rx$varVal <- NA_real_
  .monolix2rx$varFixed <- FALSE
}
#' Set typical fixed variable
#'
#' @param num character vector of the number
#' @param isMean is this the `mean` (`1L`) or `typical` (`0L`)
#' @return nothing; called for side effects
#' @noRd
#' @author Matthew L. Fidler
.setTypicalFixed <- function(num, isMean) {
  .monolix2rx$isMean <- (isMean == 1L)
  .monolix2rx$varEst <- NA_character_
  .monolix2rx$varVal <- as.numeric(num)
  .monolix2rx$varFixed <- TRUE
}
