#' Parse the population definition section
#'
#' @param text text for parsing
#' @return a monolix2rxPopDef object
#' @noRd
#' @author Matthew L. Fidler
.popDef <- function(text) {
  .ret <- .indDef(text)
  class(.ret) <- "monolix2rxPopDef"
  .ret
}

#' @export
as.character.monolix2rxPopDef <- as.character.monolix2rxIndDef

#' @export
print.monolix2rxPopDef <- print.monolix2rxIndDef

# @export
print.monolix2rxPopDef <- print.monolix2rxIndDef

#' @export
as.list.monolix2rxPopDef <- as.list.monolix2rxIndDef

#' MAP priors as rxode2 `ini()` priors
#'
#' Monolix's prior on a typical value has the distribution of the
#' parameter and its `sd` on the Gaussian scale, the scale of the imported
#' theta (`log(ka_pop)` for a log-normal one); babelmixr2 writes the
#' inverse.  A covariate coefficient is untransformed, so only a normal
#' prior fits it.  Omega and residual error priors are not normal priors
#' on the imported values, so they (and a prior whose distribution or
#' bounds differ from its parameter's) are skipped with a warning.
#'
#' @param popDef parsed `[POPULATION] DEFINITION:` (or NULL)
#' @param pars parsed `<PARAMETER>`
#' @param indDef parsed `[INDIVIDUAL] DEFINITION:`
#' @param theta names of the `ini()` parameters
#' @return list of `prior(name) ~ dnorm(mean, sd)` calls for the
#'   `method=MAP` parameters
#' @noRd
#' @author Matthew L. Fidler
.popPrior <- function(popDef, pars, indDef=NULL, theta=NULL) {
  if (is.null(popDef) || is.null(pars)) return(list())
  .vars <- popDef$vars
  .fixed <- popDef$fixed
  .ind <- if (is.null(indDef)) list() else indDef$vars
  .typ <- vapply(.ind, function(v) if (is.null(v$typical)) NA_character_ else v$typical, character(1))
  .coef <- unique(unlist(lapply(.ind, function(v) unlist(v$coef))))
  .skip <- function(n, why) {
    warning("the MAP prior on '", n, "' is not imported: ", why, call.=FALSE)
    NULL
  }
  .ret <- lapply(names(.vars), function(.n) {
    .w <- which(pars$name == .n)
    if (length(.w) != 1L || toupper(pars$method[.w]) != "MAP") return(NULL)
    .v <- .vars[[.n]]
    .tv <- .fixed[.v$typical]
    .sd <- .fixed[.v$sd]
    if (length(.tv) != 1L || length(.sd) != 1L || is.na(.tv) || is.na(.sd)) {
      return(.skip(.n, "it is not a typical value and sd"))
    }
    if (!is.null(theta) && !(.n %in% theta)) return(.skip(.n, "it is not an ini() parameter"))
    .tv <- setNames(.tv, NULL)
    .lo <- if (is.null(.v$min)) 0 else .v$min
    .hi <- if (is.null(.v$max)) 1 else .v$max
    .i <- match(.n, .typ)
    if (!is.na(.i)) {
      # a typical value: the prior must have its parameter's scale
      .p <- .ind[[.i]]
      .plo <- if (is.null(.p$min)) 0 else .p$min
      .phi <- if (is.null(.p$max)) 1 else .p$max
      if (!identical(.p$distribution, .v$distribution) ||
            (.v$distribution == "logitnormal" && (.lo != .plo || .hi != .phi))) {
        return(.skip(.n, "its distribution differs from the parameter's"))
      }
    } else if (.n %in% .coef) {
      if (.v$distribution != "normal") {
        return(.skip(.n, "a covariate coefficient takes a normal prior"))
      }
    } else {
      return(.skip(.n, "it is not a typical value or covariate coefficient"))
    }
    # log() is in base R; rxode2 evaluates priors there, so the logit and
    # probit means are written as numbers
    .mean <- switch(.v$distribution,
                    lognormal=call("log", .tv),
                    normal=.tv,
                    logitnormal=signif(.logit(.tv, .lo, .hi), 15),
                    probitnormal=signif(stats::qnorm(.tv), 15),
                    NULL)
    if (is.null(.mean)) return(.skip(.n, paste0("a '", .v$distribution, "' prior")))
    bquote(prior(.(as.name(.n))) ~ dnorm(.(.mean), .(setNames(.sd, NULL))))
  })
  .ret[!vapply(.ret, is.null, logical(1))]
}
