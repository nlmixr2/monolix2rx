.indDefIni <- function(full=TRUE) {
  .monolix2rx$varName  <- NA_character_
  .monolix2rx$dist     <- NA_character_
  .monolix2rx$isMean   <- NA
  .monolix2rx$varEst   <- NA_character_
  .monolix2rx$varVal   <- NA_real_
  .monolix2rx$sd       <- character(0)
  .monolix2rx$sdVal    <- numeric(0)
  .monolix2rx$var      <- character(0)
  .monolix2rx$varVal   <- numeric(0)
  .monolix2rx$min      <- -Inf
  .monolix2rx$max      <- Inf
  .monolix2rx$iov      <- character(0)
  .monolix2rx$cov      <- character(0)
  .monolix2rx$coef     <- NULL
  .monolix2rx$coefVal  <- NULL
  .monolix2rx$coefLst  <- character(0)
  .monolix2rx$coefLstVal <- numeric(0)
  .monolix2rx$corLevel <- "id"
  if (full) {
    .monolix2rx$rx <- character(0)
    .monolix2rx$defItems <- NULL
    .monolix2rx$corDf     <- data.frame(level=character(0), v1=character(0), v2=character(0), est=character(0))
    .monolix2rx$estDf <- data.frame(type=character(0), name=character(0), fixed=logical(0), level=character(0))
    .monolix2rx$defFixed <- numeric(0)
    .monolix2rx$indDef <- NULL
  }
}
#' Parses the mlxtran [individual] definition: text
#'
#'
#' @param text text from the individual defition
#' @return monolix2rxIndDef class
#' @noRd
#' @author Matthew L. Fidler
.indDef <- function(text) {
  .indDefIni()
  .Call(`_monolix2rx_trans_indDef`, text)
  .indDefFinalize()
  .monolix2rx$indDef
}

#' Finalized the mlxtran [individual] definition:
#'
#' @return nothing called for side effects
#' @noRd
#' @author Matthew L. Fidler
.indDefFinalize <- function() {
  .addIndDefItem()
  .indDef <- list(vars=.monolix2rx$defItems,
                  fixed=.monolix2rx$defFixed,
                  cor=.monolix2rx$corDf,
                  est=.monolix2rx$estDf,
                  rx=.monolix2rx$rx)
  .n <- c(.indDef$est$name,.indDef$cor$est)
  .n <- unique(.n[duplicated(.n)])
  if (length(.n) > 0) {
    stop("duplicated parameter estimates in [INDIVIDUAL] DEFINITION: '",
         paste(.n, collapse="', '"), "'",
         call.=FALSE)
  }
  class(.indDef) <- "monolix2rxIndDef"
  .indDefIni(TRUE)
  .monolix2rx$indDef <- .indDef
}
#' Add parsed item to the .monolix2rx$defItems
#'
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.addIndDefItem <- function() {
  if (!is.na(.monolix2rx$varName)) {
    if (length(.monolix2rx$varVal) == 0L) {
       stop("'", .monolix2rx$varName, "' needs a 'typical=' or 'mean=' declaration",
           call.=FALSE)
    }
    .ret <- list(distribution=.monolix2rx$dist)
    .rx <- paste0(.monolix2rx$varName, " <- ")
    .est <- .monolix2rx$varEst
    .fix <- FALSE
    if (.ret$distribution == "lognormal") {
      .rx <- paste0(.rx, "exp(")
    } else if (.ret$distribution == "normal") {
    } else if (.ret$distribution == "logitnormal") {
      .rx <- paste0(.rx, "expit(")
    } else if (.ret$distribution == "probitnormal") {
      .rx <- paste0(.rx, "probitInv(")
    }
    if (!is.na(.monolix2rx$varVal)) {
      .est <- paste0("rxTv_", .monolix2rx$varName)
      names(.monolix2rx$varVal) <- .est
      .monolix2rx$defFixed <- c(.monolix2rx$defFixed, .monolix2rx$varVal)
      .fix <- TRUE
    }
    .typical <- .est
    .monolix2rx$estDf <- rbind(.monolix2rx$estDf,
                               data.frame(type="typical", name=.est, fixed=.fix, level="pop"))
    .rx <- paste0(.rx, .est)
    if (.monolix2rx$isMean) {
      .ret$mean <- .typical
    } else {
      .ret$typical <- .typical
    }
    if (!is.null(.monolix2rx$coef)) {
      if (length(.monolix2rx$cov) != length(.monolix2rx$coef)) {
        if (length(.monolix2rx$cov) == 1L) {
          .monolix2rx$coef <- list(vapply(seq_along(.monolix2rx$coef), function(i) {
            .cf <- .monolix2rx$coef[[i]]
            if (length(.cf) != 1) {
              stop("number of covariates and coefficients need to match for '", .monolix2rx$varName, "'",
                   call.=FALSE)
            }
            .cf
          }, character(1), USE.NAMES=FALSE))
          .monolix2rx$coefVal <- list(vapply(seq_along(.monolix2rx$coefVal), function(i) {
            .cv <- .monolix2rx$coefVal[[i]]
            .cv
          }, numeric(1), USE.NAMES=FALSE))
        } else {
          stop("number of covariates and coefficients need to match for '", .monolix2rx$varName, "'",
               call.=FALSE)
        }
      }
      .coef <- lapply(seq_along(.monolix2rx$coef),
                      function(i) {
                        .c <- .monolix2rx$coef[[i]]
                        .cv <- .monolix2rx$coefVal[[i]]
                        .cov <- .monolix2rx$cov[i]
                        .w <- which(is.na(.c))
                        if (length(.w) == 0L) {
                          .monolix2rx$estDf <- rbind(.monolix2rx$estDf,
                                                     data.frame(type="cov", name=.c, fixed=FALSE, level="cov"))
                          return(.c)
                        }
                        .n <- paste0("rxCov_", .monolix2rx$varName, "_",
                                     .cov, "_", .w)
                        .fix <- .cv[.w]
                        names(.fix) <- .n
                        .monolix2rx$defFixed <- c(.monolix2rx$defFixed, .fix)
                        .c[.w] <- .n
                        .monolix2rx$estDf <- rbind(.monolix2rx$estDf,
                                                   data.frame(type="cov", name=.c,
                                                              fixed=vapply(.c,
                                                                           function(v) {
                                                                             any(names(.monolix2rx$defFixed) == v)
                                                                           },
                                                                           logical(1), USE.NAMES = FALSE),
                                                              level="cov"))
                        return(.c)
                      })
      .ret$cov <- .monolix2rx$cov
      .ret$coef <- .coef
      .rx <- paste0(.rx, " + ",
                    paste(vapply(seq_along(.ret$coef),
                                 function(i) {
                                   paste(paste0(.ret$coef[[i]], "*", .ret$cov[i]), collapse=" + ")
                                 }, character(1), USE.NAMES=FALSE),
                          collapse=" + "))
    }
    .vl <- "id"
    if (length(.monolix2rx$iov) > 0) {
      .ret$varlevel <- .monolix2rx$iov
      .vl <- .monolix2rx$iov
    }
    if (length(.monolix2rx$sd) > 0L) {
      if (!is.null(.ret$varlevel) &&
            length(.ret$varlevel) != length(.monolix2rx$sd)) {
        stop("length of 'varlevel=' needs to match length 'sd='",
             call.=FALSE)
      }
      .w <- which(is.na(.monolix2rx$sd))
      .sd <- .monolix2rx$sd
      if (length(.w) > 0L) {
        .estw <- paste0("rxVar_",.monolix2rx$varName, "_", .w)
        .fix <- .monolix2rx$sdVal[.w]
        names(.fix) <- .estw
        .sd[.w] <- .estw
        .monolix2rx$defFixed <- c(.monolix2rx$defFixed, .fix)
      }
      .ret$sd <- .sd
      .rx <- paste0(.rx, " + ", paste(.ret$sd, collapse=" + "))
      .monolix2rx$estDf <- rbind(.monolix2rx$estDf,
                                 data.frame(type="sd", name=.sd,
                                            fixed=vapply(.sd,
                                                         function(v) {
                                                           any(names(.monolix2rx$defFixed) == v)
                                                         }, logical(1), USE.NAMES = FALSE),
                                            level=.vl))
    } else if (length(.monolix2rx$var) > 0L) {
      if (!is.null(.ret$varlevel) &&
            length(.ret$varlevel) != length(.monolix2rx$var)) {
        stop("length of 'varlevel=' needs to match length 'var='",
             call.=FALSE)
      }
      .w <- which(is.na(.monolix2rx$var))
      .var <- .monolix2rx$var
      if (length(.w) > 0L) {
        .estw <- paste0("rxVar_",.monolix2rx$varName, "_", .w)
        .fix <- .monolix2rx$varVal[.w]
        names(.fix) <- .estw
        .var[.w] <- .estw
        .monolix2rx$defFixed <- c(.monolix2rx$defFixed, .fix)
      }
      .ret$var <- .var
      .rx <- paste0(.rx, " + ", paste(.ret$var, collapse=" + "))
      .monolix2rx$estDf <- rbind(.monolix2rx$estDf,
                                 data.frame(type="var", name=.var,
                                            fixed=vapply(.var,
                                                         function(v) {
                                                           any(names(.monolix2rx$defFixed) == v)
                                                         }, logical(1), USE.NAMES = FALSE),
                                            level=.vl))
    }
    if (any(.ret$distribution == c("lognormal", "probitnormal"))) {
      .rx <- paste0(.rx, ")")
    } else if (.ret$distribution == "logitnormal") {
      .ret$max <- .monolix2rx$max
      .ret$min <- .monolix2rx$min
      .rx <- paste0(.rx, ", ", .ret$min, ", ", .ret$max, ")")
    }
    .monolix2rx$rx <- c(.monolix2rx$rx, .rx)
    .ret <- list(.ret)
    names(.ret) <- .monolix2rx$varName
    .monolix2rx$defItems <- c(.monolix2rx$defItems, .ret)
    .indDefIni(FALSE)
  }
}
#' For a variable name and fixed constant
#'
#' @param varName variable name
#' @param fixed fixed number
#' @return this variable as defined in monolix
#' @noRd
#' @author Matthew L. Fidler
.varOrFixed <- function(varName, fixed) {
  vapply(seq_along(varName),
         function(i) {
           .n <- varName[i]
           .f <- fixed[.n]
           if (is.na(.f)) return(.n)
           paste(.f)
         },
         character(1), USE.NAMES=FALSE)
}

#' @export
print.monolix2rxIndDef <- function(x, ...) {
  ## .indDef <- list(vars=.monolix2rx$defItems,
  ##                 fixed=.monolix2rx$defFixed,
  ##                 cor=.monolix2rx$corDf)
  lapply(names(x$vars), function(n) {
    .cur <- x$vars[[n]]
    cat(n, " = {distribution=", .cur$distribution, sep="")
    if (!is.null(.cur$typical)) {
      cat(", typical=", .varOrFixed(.cur$typical, x$fixed), sep="")
    } else {
      cat(", mean=", .cur$mean, sep="")
    }
    if (!is.null(.cur$cov)) {
      cat(', covariate=')
      if (length(.cur$cov) == 1L) {
        cat(.cur$cov)
      } else {
        cat("{", paste(.cur$cov, collapse=", "), "}", sep="")
      }
    }
    if (!is.null(.cur$coef)) {
      cat(', coefficient=')
      if (length(.cur$coef) > 1L) cat("{")
      cat(paste(vapply(seq_along(.cur$coef),
             function(i) {
               .cv <- .varOrFixed(.cur$coef[[i]], x$fixed)
               if (length(.cv) == 1) return(.cv)
               paste0("{", paste(.cv, collapse=", "), "}")
             }, character(1), USE.NAMES=TRUE), collapse=", "))
      if (length(.cur$coef) > 1L) cat("}")
    }
    if (!is.null(.cur$varlevel)) {
      if (length(.cur$varlevel) == 1L) {
        cat(", varlevel=", .cur$varlevel, sep="")
      } else {
        cat(", varlevel={", paste(.cur$varlevel, collapse=", "), "}", sep="")
      }
    }
    if (!is.null(.cur$sd)) {
      if (length(.cur$sd) == 1L) {
        cat(", sd=", .varOrFixed(.cur$sd, x$fixed), sep="")
      } else {
        cat(", sd={", paste(.varOrFixed(.cur$sd, x$fixed), collapse=", "),"}", sep="")
      }
    } else if (!is.null(.cur$var)) {
      if (length(.cur$sd) == 1L) {
        cat(", var=", .varOrFixed(.cur$var, x$fixed), sep="")
      } else {
        cat(", var={", paste(.varOrFixed(.cur$var, x$fixed), collapse=", "),"}", sep="")
      }
    } else {
      cat(", no-variability")
    }
    if (!is.null(.cur$min)) {
      cat(", min=", .cur$min, sep="")
    }
    if (!is.null(.cur$max)) {
      cat(", max=", .cur$max, sep="")
    }
    cat("}\n")
  })
  .cor <- x$cor
  if (length(x$cor$level) > 0L) {
    .levels <- sort(unique(x$cor$level))
    lapply(.levels,
           function(lvl) {
             cat("correlation = {")
             if (length(.levels) == 1L &&
                   .levels == "id") {
             } else {
               cat("level=", lvl, ", ", sep="")
             }
             .c <- .cor[.cor$level == lvl, ]
             cat(paste(paste0("r(", .c$v1, ", ", .c$v2, ")=", .c$est), collapse=", "))
             cat("}\n")
           })
  }
  invisible(x)
}

#' @export
as.list.monolix2rxIndDef <- function(x, ...) {
  .x <- x
  class(.x) <- NULL
  .x
}

#' Add a variable for monolix parsing
#'
#' @param var Variable to add
#' @return nothing called for side effects
#' @noRd
#' @author Matthew L. Fidler
.addVar <- function(var) {
  .addIndDefItem()
  .monolix2rx$varName <- var
}

#' Add a distribution type for monolix parsing
#'
#' @param dist distribution type
#' @return nothing; called for side effects
#' @noRd
#' @author Matthew L. Fidler
.setDist <- function(dist) {
  .monolix2rx$dist <- tolower(dist)
  if (.monolix2rx$dist == "logitnormal") {
    # Set the defaults for min/max with logitNormal
    if (is.infinite(.monolix2rx$min) &&
          is.infinite(.monolix2rx$max)) {
      .monolix2rx$min <- 0
      .monolix2rx$max <- 1
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
  if (!is.na(.monolix2rx$isMean)) {
    stop("can only use 'typical=' or 'mean=' in '",.monolix2rx$varName,"' not both",
         call.=FALSE)
  }
  .monolix2rx$isMean <- (isMean == 1L)
  .monolix2rx$varEst <- var
  .monolix2rx$varVal <- NA_real_
}
#' Set typical fixed variable
#'
#' @param num character vector of the number
#' @param isMean is this the `mean` (`1L`) or `typical` (`0L`)
#' @return nothing; called for side effects
#' @noRd
#' @author Matthew L. Fidler
.setTypicalFixed <- function(num, isMean) {
  if (!is.na(.monolix2rx$isMean)) {
    stop("can only use 'typical=' or 'mean=' in '",.monolix2rx$varName,"' not both",
         call.=FALSE)
  }
  .monolix2rx$isMean <- (isMean == 1L)
  .monolix2rx$varEst <- NA_character_
  .monolix2rx$varVal <- as.numeric(num)
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
#' @param var sting to change to a numeric for max
#' @return nothing called for side effects
#' @noRd
#' @author Matthew L. Fidler
.setMax <- function(var) {
  .var <- as.numeric(var)
  .monolix2rx$max <- .var
}
#' Set the maximum value of the transformation
#'
#' @param var sting to change to a numeric for min
#' @return nothing called for side effects
#' @noRd
#' @author Matthew L. Fidler
.setMin <- function(var) {
  .var <- as.numeric(var)
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
    .monolix2rx$coef <- c(.monolix2rx$coef, list(.monolix2rx$coefLst))
    .monolix2rx$coefVal <- c(.monolix2rx$coefVal, list(.monolix2rx$coefLstVal))
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
  if (!is.na(.var)) {
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
  .v <- sort(c(var1, var2))
  .w <- which(.monolix2rx$corDf$level == .monolix2rx$corLevel &
                .monolix2rx$corDf$v1 == .v[1] &
                .monolix2rx$corDf$v2 == .v[2])
  if (length(.w) != 0) {
    stop("cannot define r(", .v[1], ", ", .v[2], ") for level=", .monolix2rx$corLevel,
         " more than once",
         call.=FALSE)
  }
  .monolix2rx$corDf <- rbind(.monolix2rx$corDf,
                             data.frame(level=.monolix2rx$corLevel,
                                        v1=.v[1], v2=.v[2], est=estVal))
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
