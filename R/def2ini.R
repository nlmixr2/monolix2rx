#' Logit to avoid import of logit from rxode2
#'
#' This way properties like Jacobian can be calculated without rxode2
#'
#' @param x value
#' @param low minimum value
#' @param high maximum value
#' @return logit transformed value
#' @noRd
#' @author Matthew L. Fidler
.logit <- function(x, low=0, high=1) {
  p <- (x-low)/(high-low)
  p <- ifelse(p >= 1 | p <= 0, NaN, p)
  -log(1/p - 1.0)
}
#' Expit in R so that Jacobian can be calculted without rxode2
#'
#' @param x value
#' @param low minimum value
#' @param high maximum value
#' @return expit transformed value
#' @noRd
#' @author Matthew L. Fidler
.expit <- function(x, low=0, high=1) {
  (high-low)/(1+exp(-x))+low
}

#' Get the parameter value based on the transformation
#'
#' This is for the typical values in monolix
#'
#' @param value The typical value from monolix (ie back-transformed)
#' @param distribution the distribution of the parameter
#' @param min the minimum value (numeric) for logit/expit transformations
#' @param max the maximum value (numeirc) for the logit/expit transformations
#' @return the mean value of the distribution (ie non-tranformed) or NA if unknown
#' @noRd
#' @author Matthew L. Fidler
.parsTransformValue <- function(value, distribution, min, max) {
  if (distribution == "lognormal") {
    return(log(value))
  } else if (distribution == "logitnormal") {
    if (is.null(min)) min <- 0
    if (is.null(max)) max <- 1
    return(.logit(value, min, max))
  } else if (distribution == "normal") {
    return(value)
  } else if (distribution == "probitnormal") {
    return(qnorm(value))
  }
  NA_real_
}
#' Get the parameter value from the parsed pars object
#'
#' @param pars parsed pars object
#' @param name name of the parameter being queried
#' @return value of the parameter or NA if not found
#' @noRd
#' @author Matthew L. Fidler
.parsGetValue <- function(pars, name) {
  .w <- which(pars$name == name)
  if (length(.w) == 1L) {
    return(pars[.w, "value"])
  }
  NA_real_
}
#' Determine if the parameter is fixed or not
#'
#' @param pars parsed <PARAMETERS> section
#' @param name character of the parameter name
#' @return boolean stating if this parameter value is fixed
#' @noRd
#' @author Matthew L. Fidler
.parsGetFixed <- function(pars, name) {
  .w <- which(pars$name == name)
  if (length(.w) == 1L) {
    return(pars[.w, "method"] == "FIXED")
  }
  FALSE
}
#' This sets up the diagonals in the omega list to match the defined variability
#'
#' @param cur The current variable definition
#' @param env environment where `omega` and `vl` are defined as a list
#'   and a empty character vector, respectively. `omegaDf` is also a
#'   data.frame for tracking estimates (which will be used later for reading
#'   correlations)
#' @param pars the parsed estimate block
#' @param name is the parameter name
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.def2iniSetupDiagSd <- function(cur, env, pars, name) {
  if (!is.null(cur$varlevel)) {
    .vl <- cur$varlevel
  } else {
    .vl <- "id"
  }
  env$vl <- unique(c(env$vl, .vl))
  if (!is.null(cur$sd)) {
    # don't need to check for the same length; checked in parsing
    lapply(seq_along(.vl),
           function(i) {
             .level <- .vl[i]
             .var <- cur$sd[i]
             env$omegaDf <- rbind(env$omegaDf,
                                  data.frame(level=.level, name=name,
                                             var=.var))
             if (.parsGetFixed(pars, .var)) {
               env$extraFixed <- c(env$extraFixed, .var)
             }
             env$omega[[.level]] <-
               c(env$omega[[.level]],
                 setNames(.parsGetValue(pars, .var), .var))
           })
  } else if (!is.null(cur$var)) {
    # same as above but setup sqrt to give sqrt matrix
    lapply(seq_along(.vl),
           function(i) {
             .level <- .vl[i]
             .var <- cur$var[i]
             env$omegaDf <- rbind(env$omegaDf,
                                  data.frame(level=.level, name=name,
                                             var=.var))
             if (.parsGetFixed(pars, .var)) {
               env$extraFixed <- c(env$extraFixed, .var)
             }
             env$omega[[.level]] <-
               c(env$omega[[.level]],
                 setNames(sqrt(.parsGetValue(pars, .var)), .var))
           })
  }
}
#' Get the covariance matrix based on the parameter definitions
#'
#' @param env env environment where `omega` and `vl` are defined as a list
#'   and a empty character vector, respectively. `omegaDf` is also a
#'   data.frame for tracking estimates (which will be used later for reading
#'   correlations)
#' @param def parsed individual definition
#' @param pars parsed <PARAMETER> block
#' @param level the level of varaibility being processed
#' @return covariance for level
#' @noRd
#' @author Matthew L. Fidler
.def2iniGetCov <- function(env, def, pars, level) {
  .w <- which(names(env$omega) == level)
  if (length(env$omega[[.w]]) == 1) {
    .r <- .sd <- diag(1)
    .sd[1, 1] <- env$omega[[.w]]
    if (is.na(.sd[1, 1])) {
      warning("cannot find estimate of ", names(env$omega), " assuming ",
              .monolix2rx$iniSd,
              call. = FALSE)
      .sd[1, 1] <- .monolix2rx$iniSd
    }
  } else {
    .omega <- env$omega[[.w]]
    .w2 <- which(is.na(.omega))
    if (length(.w2) > 0) {
      warning("cannot find estimate of ", paste(names(.omega)[.w2], collapse=", "),
              " assuming ", .monolix2rx$iniSd,
              call. = FALSE)
      .omega[.w2] <- .monolix2rx$iniSd
    }
    .sd <- diag(.omega)
    .r <- diag(length(env$omega[[.w]]))
  }
  .n <- names(env$omega[[.w]])
  dimnames(.r) <- list(.n, .n)
  env$r <- .r
  .cor <- def$cor
  .cor <- .cor[.cor$level == level, ]
  lapply(seq_along(.cor$v1), function(i) {
    .cur <- .cor[i, ]
    .v1 <- .cur$v1
    .v2 <- .cur$v2
    .v1 <- env$omegaDf[env$omegaDf$level == level & env$omegaDf$name == .v1, "var"]
    .v2 <- env$omegaDf[env$omegaDf$level == level & env$omegaDf$name == .v2, "var"]
    .val <- .parsGetValue(pars, .cur$est)
    if (.parsGetFixed(pars, .cur$est)) {
      env$extraFixed <- c(env$extraFixed, .v1, .v2)
    }
    if (is.na(.val)) {
      warning("cannot find correlation estimate between ", .v1, " and ", .v2," assuming ",
              .monolix2rx$iniCor, call. = FALSE)
      .val <- .monolix2rx$iniCor
    }
    env$r[.v1, .v2] <- .val
    env$r[.v2, .v1] <- .val
  })
  dimnames(.sd) <- list(.n, .n)
  .omega <- .sd %*% env$r %*% .sd
  dimnames(.omega) <- list(.n, .n)
  .omega
}

#' Based on a correctly setup environment with env$df = ini df fix an
#' omega or omega block
#'
#' @param env environment with env$df == ini block
#' @param v variable name to fix
#' @return nothing, called for side effects on env
#' @noRd
#' @author Matthew L. Fidler
.def2iniFixOmegaVar <- function(env, v) {
  .w <- which(env$df$name == v)
  env$df$fix[.w] <- TRUE
  .neta <- env$df$neta1[.w]
  .etas <- .neta
  .fixedEtas <- NULL
  while (length(.etas) > 0) {
    .neta <- .etas[1]
    .w <- which(env$df$neta1 == .neta | env$df$neta2 == .neta)
    env$df$fix[.w] <- TRUE
    .etas <- unique(c(.etas, env$df$neta1[.w], env$df$neta2[.w]))
    .fixedEtas <- c(.neta, .fixedEtas)
    .etas <- .etas[!(.etas %in% .fixedEtas)]
  }
}

#' Fix the omega blocks associated with fixed parameters in monolix model
#'
#' @param omega initial omega list
#' @param fixVars fixed omega variables from monolix
#' @return lotri object for converting to an expression
#' @noRd
#' @author Matthew L. Fidler
.def2iniFixOmega <- function(omega, fixVars=character(0)) {
  if (length(fixVars) == 0L) return(omega)
  .env <- new.env(parent=emptyenv())
  .env$df <- as.data.frame(omega)
  lapply(fixVars,
         function(v) {
           .def2iniFixOmegaVar(.env, v)
         })
  lotri::as.lotri(.env$df)
}
#' Rename occasion based on the nesting level
#'
#'
#' @param vl occasion levels; in monolix they are defined like id*occ
#'   and id*occ*occ instead of by a variable;
#' @return This changes id*occ -> occ and id*occ->occ2 etc
#' @noRd
#' @author Matthew L. Fidler
.def2iniRenameOcc <- function(vl) {
  vapply(vl,
         function(v) {
           .s <- strsplit(v, "[*]")[[1]]
           if (length(.s) == 1) return(v)
           return(paste0("occ", ifelse(length(.s) == 1L, "", length(.s))))
         }, character(1), USE.NAMES = FALSE)
}

#' Get the ini block based on a mlxtran parsed sections
#'
#' @param def `[INDIVIDUAL] DEFINITION:` section (parsed)
#' @param pars `<PARAMETER>` section (parsed)
#' @param longDef `[LONGITUDIAL] DEFINITION:` section (parsed)
#' @return ini block
#' @noRd
#' @author Matthew L. Fidler
.def2ini <- function(def, pars, longDef) {
  if (!requireNamespace("lotri", quietly = TRUE)) {
    stop("reqires 'lotri'")
  }
  .env <- new.env(parent=emptyenv())
  .env$pars <- pars
  lapply(names(longDef$fixed),
         function(v) {
           .env$pars <- rbind(.env$pars,
                              data.frame(name=v, value=longDef$fixed[v], method="FIXED"))
         })
  lapply(names(def$fixed),
         function(v) {
           .env$pars <- rbind(.env$pars,
                              data.frame(name=v, value=def$fixed[v], method="FIXED"))
         })
  pars <- .env$pars
  .var <- def$var
  .n <- names(.var)
  .env$err <- character(0)
  lapply(seq_along(longDef$endpoint), function(i) {
    .env$err <- c(.env$err, longDef$endpoint[[i]]$err$typical)
  })

  .env$omega <- list()
  .env$vl <- character(0)
  .env$omegaDf <- data.frame(level=character(0), name=character(0), var=character(0))
  .env$extraFixed <- character(0)

  .coef <- lapply(.n, function(n) {
    .cur <- .var[[n]]
    if (!is.null(.cur$coef)) {
      .v <- lapply(seq_along(.cur$coef), function(i) {
        .coef <- .cur$coef[[i]]
        lapply(.coef, function(var) {
          .val <- .parsGetValue(pars, var)
          ## .val <- .parsTransformValue(.val, .cur$distribution,
          ##                             min=.cur$min, max=.cur$max)
          ## if (is.na(.val) && .inNaVal) {
          ##   stop("transformed value of initial estimate for ", .var,
          ##        " is not in correct range",
          ##        call.=FALSE)
          ## }
          if (.parsGetFixed(pars, var)) {
            bquote(.(str2lang(var)) <- fixed(.(.val)))
          } else {
            bquote(.(str2lang(var)) <- .(.val))
          }
        })
      })
      .v <- do.call(`c`, .v)
      .v
    } else {
      NULL
    }
  })
  .coef <- do.call(`c`, .coef)
  .in <- which(vapply(seq_along(.coef),
                      function(i) {
                        as.character(.coef[[i]][[2]]) %in% .monolix2rx$ignoredCoef
                      }, logical(1), USE.NAMES = FALSE))
  if (length(.in) > 0L) {
    .coef <- .coef[-.in]
  }
  .pop <- c(list(quote(`{`)),
            lapply(.n, function(n) {
              .cur <- .var[[n]]
              if (!is.null(.cur$typical)) {
                .var <- .cur$typical
                .val <- .parsGetValue(pars, .var)
                .inNaVal <- !is.na(.val)
                .val <- .parsTransformValue(.val, .cur$distribution,
                                            min=.cur$min, max=.cur$max)
                if (is.na(.val) && .inNaVal) {
                  stop("transformed value of initial estimate for ", .var,
                       " is not in correct range",
                       call.=FALSE)
                }
              } else if (!is.null(.cur$mean)) {
                .var <- .cur$mean
                .val <- .parsGetValue(pars, .var)
              }
              if (is.na(.val)) {
                warning("cannot find estimate of ", .var,
                        " assuming ", .monolix2rx$iniTheta,
                        call. = FALSE)
                .val <- .monolix2rx$iniTheta
              }
              .def2iniSetupDiagSd(.cur, .env, pars, n)
              if (.parsGetFixed(pars, .var)) {
                bquote(.(str2lang(.var)) <- fixed(.(.val)))
              } else {
                bquote(.(str2lang(.var)) <- .(.val))
              }
            }),
            .coef,
            lapply(.env$err,
                   function(e) {
                     if (.parsGetFixed(pars, e)) {
                       bquote(.(str2lang(e)) <- fixed(.(.parsGetValue(pars, e))))
                     } else {
                       bquote(.(str2lang(e)) <- .(.parsGetValue(pars, e)))
                     }
                   }))
  if (length(.env$omega) != 0L) {
    .omega <- setNames(lapply(.env$vl,
                              function(level) {
                                .def2iniGetCov(.env, def, pars, level)
                              }),
                       .def2iniRenameOcc(.env$vl))
    class(.omega) <- "lotriFix"
    .omega <- .def2iniFixOmega(.omega, .env$extraFixed)
    .omega <- as.expression(.omega)
    .omega <- .omega[[2]]
    .ini <- c(.pop,
              lapply(seq_along(.omega)[-1], function(x) {.omega[[x]]}))
  } else {
    .ini <- .pop
  }
  as.call(c(list(quote(`ini`)), as.call(.ini)))
}
