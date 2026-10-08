#' Translate Monolix structural model mixtures
#'
#' `wsmm(f1, p1, f2, p2, ...)` (within subject) is the weighted
#' prediction `p1*f1 + p2*f2 + ...`.  `bsmm(f1, p1, f2, p2, ...)`
#' (between subject) puts each subject in one group, which is rxode2's
#' `mix(f1, p1, f2, ...)`; its probabilities must be `ini()` parameters,
#' so each one has to be a population parameter: a literal or an
#' individual parameter without variability or covariates, whose typical
#' value becomes the probability.
#'
#' @param model parsed `model({})` call
#' @param mlxtran parsed mlxtran object
#' @return list with `model`, and `prob`: a data frame of the `mix()`
#'   probabilities (`name`, `var` the individual parameter or NA, `value`
#'   for a literal)
#' @noRd
#' @author Matthew L. Fidler
.mixtureRewrite <- function(model, mlxtran) {
  .env <- new.env(parent=emptyenv())
  .env$prob <- NULL
  .env$vars <- mlxtran$MODEL$INDIVIDUAL$DEFINITION$vars
  .ret <- .mixtureWalk(model, .env)
  .prob <- .env$prob
  if (!is.null(.prob)) {
    .vars <- .env$vars
    if (!any(vapply(.vars, function(v) !is.null(v$sd), logical(1)))) {
      stop("bsmm() (rxode2 mix()) needs a parameter with between-subject variability",
           call.=FALSE)
    }
    if (utils::packageVersion("rxode2") < "5.1.8") {
      stop("bsmm() (mix()) needs rxode2 >= 5.1.8", call.=FALSE)
    }
    .ret <- .mixtureProbLines(.ret, .prob)
  }
  list(model=.ret, prob=.prob)
}

#' Walk the model, rewriting bsmm() and wsmm() calls
#'
#' @param x expression
#' @param env environment with `prob` and `vars`
#' @return rewritten expression
#' @noRd
#' @author Matthew L. Fidler
.mixtureWalk <- function(x, env) {
  if (!is.call(x)) return(x)
  .args <- lapply(as.list(x)[-1], .mixtureWalk, env=env)
  .fun <- x[[1]]
  if (identical(.fun, quote(wsmm))) {
    .k <- seq(1, length(.args), by=2)
    .terms <- lapply(.k, function(k) bquote((.(.args[[k + 1]])) * (.(.args[[k]]))))
    return(Reduce(function(a, b) bquote(.(a) + .(b)), .terms))
  }
  if (identical(.fun, quote(bsmm))) {
    .k <- seq(1, length(.args), by=2)
    .n <- length(.k)
    if (.n < 2L || length(.args) %% 2L != 0L) {
      stop("bsmm() needs at least two (model, probability) pairs", call.=FALSE)
    }
    .mixtureLastProb(.args[.k + 1])
    .prob <- .mixtureProb(.args[.k[-.n] + 1], env$vars)
    if (is.null(env$prob)) {
      env$prob <- .prob
    } else if (!identical(env$prob, .prob)) {
      stop("all bsmm() calls must use the same probabilities (one rxode2 mixture)",
           call.=FALSE)
    }
    .mix <- c(list(quote(mix)), .args[.k[1]])
    for (.i in seq_len(.n - 1L)) {
      .mix <- c(.mix, list(str2lang(.prob$name[.i]), .args[[.k[.i + 1]]]))
    }
    return(as.call(.mix))
  }
  as.call(c(list(.fun), .args))
}

#' Warn when the last bsmm() probability is not 1 minus the others
#'
#' rxode2's mix() takes the last group's probability as the remainder.
#'
#' @param p list of all the probability expressions
#' @return nothing, called for the warning
#' @noRd
#' @author Matthew L. Fidler
.mixtureLastProb <- function(p) {
  .n <- length(p)
  .last <- p[[.n]]
  .rest <- p[-.n]
  if (all(vapply(p, is.numeric, logical(1)))) {
    if (abs(.last - (1 - sum(unlist(.rest)))) < 1e-8) return(invisible())
  } else {
    .expect <- Reduce(function(a, b) bquote(.(a) - .(b)), .rest, 1)
    if (identical(gsub(" ", "", deparse1(.last)), gsub(" ", "", deparse1(.expect)))) {
      return(invisible())
    }
  }
  warning("the last bsmm() probability '", deparse1(.last), "' is taken as 1 minus the others",
          call.=FALSE)
}

#' The ini() probabilities of a bsmm() call
#'
#' @param p list of probability expressions (all but the last group)
#' @param vars individual parameter definitions
#' @return data frame with name, var and value
#' @noRd
#' @author Matthew L. Fidler
.mixtureProb <- function(p, vars) {
  .ret <- lapply(seq_along(p), function(i) {
    .p <- p[[i]]
    if (is.numeric(.p)) {
      return(data.frame(name=paste0("rxBsmmP", i), var=NA_character_, value=.p))
    }
    .v <- if (is.name(.p)) vars[[as.character(.p)]] else NULL
    .typ <- if (is.null(.v)) NULL else c(.v$typical, .v$mean)[1]
    if (is.null(.typ) || !is.null(.v$sd) || !is.null(.v$cov)) {
      stop("bsmm() probability '", deparse1(.p), "' must be a number or an individual ",
           "parameter without variability or covariates (rxode2 mix() needs ",
           "population probabilities)", call.=FALSE)
    }
    data.frame(name=.typ, var=as.character(.p), value=NA_real_)
  })
  do.call(rbind, .ret)
}

#' Make the probability parameters plain in the model
#'
#' The typical value is the probability itself, so `p1 <- expit(p1_pop)`
#' becomes `p1 <- p1_pop`.
#'
#' @param model model call
#' @param prob probability data frame
#' @return model call
#' @noRd
#' @author Matthew L. Fidler
.mixtureProbLines <- function(model, prob) {
  .body <- as.list(model[[2]])
  .vars <- prob$var[!is.na(prob$var)]
  for (.i in seq_along(.body)[-1]) {
    .l <- .body[[.i]]
    if (is.call(.l) && (identical(.l[[1]], quote(`<-`)) || identical(.l[[1]], quote(`=`))) &&
          is.name(.l[[2]]) && as.character(.l[[2]]) %in% .vars) {
      .w <- which(prob$var == as.character(.l[[2]]))
      .body[[.i]] <- bquote(.(.l[[2]]) <- .(str2lang(prob$name[.w])))
    }
  }
  model[[2]] <- as.call(.body)
  model
}

#' Put the bsmm() probabilities in the ini block on their natural scale
#'
#' @param ini ini call from `.def2ini()`
#' @param prob probability data frame from `.mixtureRewrite()`
#' @param pars parsed `<PARAMETER>` section
#' @return ini call
#' @noRd
#' @author Matthew L. Fidler
.mixtureIni <- function(ini, prob, pars) {
  if (is.null(prob)) return(ini)
  .body <- as.list(ini[[2]])
  for (.i in seq_len(nrow(prob))) {
    .n <- prob$name[.i]
    if (is.na(prob$var[.i])) {
      .new <- bquote(.(str2lang(.n)) <- fixed(.(prob$value[.i])))
    } else {
      .v <- .parsGetValue(pars, .n)
      if (is.na(.v)) stop("bsmm() probability '", .n, "' is not in <PARAMETER>", call.=FALSE)
      .new <- if (.parsGetFixed(pars, .n)) {
        bquote(.(str2lang(.n)) <- fixed(.(.v)))
      } else {
        # unbounded: nlmixr2 estimates mix() probabilities on the mlogit scale
        bquote(.(str2lang(.n)) <- .(.v))
      }
    }
    .w <- which(vapply(.body, function(l) {
      is.call(l) && identical(l[[1]], quote(`<-`)) && identical(l[[2]], str2lang(.n))
    }, logical(1)))
    if (length(.w) == 1L) {
      .body[[.w]] <- .new
    } else {
      .body <- c(.body, list(.new))
    }
  }
  ini[[2]] <- as.call(.body)
  ini
}
