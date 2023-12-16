#' Add a variable for monolix parsing
#'
#' @param var Variable to add
#' @return nothing called for side effects
#' @noRd
#' @author Matthew L. Fidler
.addVar <- function(var) {
  .monolix2rx$varName <- var
}

#' Add a distribution type for monolix parsing
#'
#' @param dist distribution type
#' @return nothing; called for side effects
#' @noRd
#' @author Matthew L. Fidler
.setDist <- function(dist) {
  .monoix2rx$dist <- tolower(dist)
  if (.monoix2rx$dist == "logitnormal") {
    # Set the defaults for min/max with logitNormal
    if (is.infinite(.monolix2rx$min) &&
          is.infinite(.monolix2rx$max)) {
      .monolix2rx$min <- 0
      .monolix2rx$max <- 0
    }
  }
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
#' Set the standard deviation
#'
#' @param var variable name or constant
#' @return
#' @noRd
#' @author Matthew L. Fidler
.setSd <- function(var) {
  if (length(.monolix2rx$var) != 0L) {
    stop("cannot specify 'var' and 'sd' in the same variable declaration (var: '",
         .monolix2rx$varName, "')", call.=FALSE)
  }
  .var <- suppressWarnings(as.numeric(var))
  if (is.na(.var)) {
    .monolix2rx$sd <- c(.monolix2rx$sd, var)
    .monolix2rx$sdVal <- c(.monolix2rx$sdVal, NA_real_)
  } else {
    .monolix2rx$sd <- c(.monolix2rx$sd, NA_character_)
    .monolix2rx$sdVal <- c(.monolix2rx$sdVal, .var)
  }
}

#' Set variability component
#'
#' @param var variable name or constant
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.setVar <- function(var) {
  if (length(.monolix2rx$sd) != 0L) {
    stop("cannot specify 'var' and 'sd' in the same variable declaration (var: '",
         .monolix2rx$varName, "')", call.=FALSE)
  }
  .var <- suppressWarnings(as.numeric(var))
  if (is.na(.var)) {
    .monolix2rx$var <- c(.monolix2rx$var, var)
    .monolix2rx$varVal <- c(.monolix2rx$varVal, NA_real_)
  } else {
    .monolix2rx$var <- c(.monolix2rx$var, NA_character_)
    .monolix2rx$varVal <- c(.monolix2rx$varVal, .var)
  }
}
#' Set the maximum value of the transformation
#'
#' @param var sting to change to a numeric for min/max
#' @return nothing called for side effects
#' @noRd
#' @author Matthew L. Fidler
.setMax <- function(var) {
  .var <- suppressWarnings(as.numeric(var))
  if (is.na(.var)) {
    stop("'max' must be a numeric value")
  }
  .monolix2rx$max <- .var
}
#' Set the maximum value of the transformation
#'
#' @param var sting to change to a numeric for min/max
#' @return nothing called for side effects
#' @noRd
#' @author Matthew L. Fidler
.setMin <- function(var) {
  .var <- suppressWarnings(as.numeric(var))
  if (is.na(.var)) {
    stop("'min' must be a numeric value")
  }
  .monolix2rx$min <- .var
}
#' This sets the IOV item from a monolix variable definition
#'
#' @param var iov definition to add
#' @return nothing called for side effects
#' @noRd
#' @author Matthew L. Fidler
.setIov <- function(var) {
  .v <- gsub(" +[*] +", "*", var)
  .monolix2rx$iov <- sort(unique(.monolix2rx$iov, .v))
}
