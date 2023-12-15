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

#'  Add a distribution type for monolix parsing
#'
#' @param dist distribution type
#' @return nothing; called for side effects
#' @noRd
#' @author Matthew L. Fidler
.setDist <- function(dist) {
  .monoix2rx$dist <- tolower(dist)
}
