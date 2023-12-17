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
#' @return nothing, called for side effects
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
  .monolix2rx$iov <- c(.monolix2rx$iov, .v)
}
#' Add mu-referenced covariate to parameter definition
#'
#' @param var variable name
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.addCov <- function(var) {
  .monolix2rx$cov <- c(.monolix2rx$cov, var)
}
#' Pushes a coefficient list onto $coef (if any)
#'
#' @return nothing called for side effects
#' @noRd
#' @author Matthew L. Fidler
.pushCoefList <- function() {
  if (length(.monolix2rx$coefLst) != 0) {
    .monolix2rx$coef <- c(.monolix2rx$coef, .monolix2rx$coefLst)
    .monolix2rx$coefVal <- c(.monolix2rx$coefVal, .monolix2rx$coefLstVal)
    .monolix2rx$coefLst <- character(0)
    .monolix2rx$coefLstVal <- numeric(0)
  }
}
#' Add a single coefficient
#'
#' @param var variable name
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.addCoefSingle <- function(var) {
  .pushCoefList()
  .var <- suppressWarnings(as.numeric(var))
  if (is.na(.var)) {
    .monolix2rx$coef <- c(.monolix2rx$coef, list(NA_character_))
    .monolix2rx$coefVal <- c(.monolix2rx$coefVal, list(.var))

  } else {
    .monolix2rx$coef <- c(.monolix2rx$coef, list(var))
    .monolix2rx$coefVal <- c(.monolix2rx$coefVal, list(NA_real_))
  }
}
#'  Add a multiple item coefficient to the coefficent list
#'
#' @param var coeff variable name
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.addCoefMult <- function(var) {
  .var <- suppressWarnings(as.numeric(var))
  if (is.na(.var)) {
    .monolix2rx$coefLst <- c(.monolix2rx$coefLst, var)
    .monolix2rx$coefLstVal <- c(.monolix2rx$coefLstVal, NA_real_)
  } else {
    .monolix2rx$coefLst <- c(.monolix2rx$coefLst, NA_character_)
    .monolix2rx$coefLstVal <- c(.monolix2rx$coefLstVal, .var)
  }
}
#' Add correlation estimate
#'
#' @param var1 correlated variable 1
#' @param var2 correlated variable 2
#' @param estVal estimation variable
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.addCor <- function(var1, var2, estVal) {
  .monolix2rx$corDf <- rbind(.monolix2rx$corDf,
                             data.frame(level=.monolix2rx$corLevel,
                                        v1=var1, v2=var2, est=estVal))
}
#' Add correlation estimate
#'
#' @param var level of correlation
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.setCorLevel <- function(var) {
  .monolix2rx$corLevel <- var
}
