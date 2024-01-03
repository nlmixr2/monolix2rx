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
    return(rxode2::logit(value, min, max))
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
  .w <- which(pars$name == "name")
  if (length(.w) == 1L) {
    return(tolower(pars[.w, "method"]) == "fixed")
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
             env$omega[[.level]] <-
               c(env$omega[[.level]],
                 setNames(.parsGetValue(pars, .var), .var))
           })
  } else if (!is.null(cur$var)) {
    # same as above but setup sqrt to give sqrt matrix
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
  .sd <- diag(env$omega[[level]])
  .r <- diag(length(env$omega[[level]]))
  .n <- names(env$omega[[level]])
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
      env$extraFixed <- c(env$extraFixed, .cur$est)
    }
    env$r[.v1, .v2] <- .val
    env$r[.v2, .v1] <- .val
  })
  dimnames(.sd) <- list(.n, .n)
  .omega <- .sd %*% .r %*% .sd
  dimnames(.omega) <- list(.n, .n)
  .omega
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
           .w <- which(.env$df$name == v)
           .env$df$fix[.w] <- TRUE
           .neta <- .env$df$neta1[.w]
           .etas <- .neta
           .fixedEtas <- NULL
           while (length(.etas) > 0) {
             .neta <- .etas[1]
             .w <- which(.env$df$neta1 == .neta | .env$df$neta2 == .neta)
             .ini$fix[.w] <- TRUE
             .etas <- unique(c(.etas, .env$df$neta1[.w], .env$df$neta2[.w]))
             .fixedEtas <- c(.neta, .fixedEtas)
             .etas <- .etas[!(.etas %in% .fixedEtas)]
           }
         })
  lotri::as.lotri(.env$df)
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
  .pop <- c(list(quote(`{`)),
            lapply(.n, function(n) {
              .cur <- .var[[n]]
              if (!is.null(.cur$typical)) {
                .var <- .cur$typical
                .val <- .parsTransformValue(.parsGetValue(pars, .var), .cur$distribution,
                                            min=.cur$min, max=.cur$max)
              } else if (!is.null(.cur$mean)) {
                .var <- .cur$mean
                .val <- .parsGetValue(pars, .var)
              }
              .def2iniSetupDiagSd(.cur, .env, pars, n)
              if (.parsGetFixed(pars, .var)) {
                bquote(.(str2lang(.var)) <- fixed(.(.val)))
              } else {
                bquote(.(str2lang(.var)) <- .(.val))
              }
            }),
            lapply(.env$err,
                   function(e) {
                     if (.parsGetFixed(pars, e)) {
                       bquote(.(str2lang(e)) <- fixed(.(.parsGetValue(pars, e))))
                     } else {
                       bquote(.(str2lang(e)) <- .(.parsGetValue(pars, e)))
                     }
                   }))
  .omega <- setNames(lapply(.env$vl,
                            function(level) {
                              .def2iniGetCov(.env, def, pars, level)
                            }), .env$vl)
  class(.omega) <- "lotriFix"
  .omega <- .def2iniFixOmega(.omega)
  .omega <- as.expression(.omega)
  .omega <- .omega[[2]]
  .ini <- c(.pop,
            lapply(seq_along(.omega)[-1], function(x) {.omega[[x]]}))
  as.call(c(list(quote(`ini`)), as.call(.ini)))
}
