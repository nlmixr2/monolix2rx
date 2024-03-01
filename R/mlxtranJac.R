#' Get the Jacobian diagonal for transformation of covariances
#'
#' @param indDef individual definition list
#' @param pars parameters
#' @return return the jacobian transformation
#' @noRd
#' @author Matthew L. Fidler
.mlxtranJacobianDiag <- function(indDef, pars) {
  .env <- new.env(parent=emptyenv())
  .env$jac <- NULL
  lapply(names(indDef$vars),
         function(v) {
           .cur <- indDef$vars[[v]]
           if (!is.null(.cur$typical)) {
             .var <- .cur$typical
             .x <- .parsTransformValue(.parsGetValue(pars, .var), .cur$distribution,
                                         min=.cur$min, max=.cur$max)
           } else if (!is.null(.cur$mean)) {
             .var <- .cur$mean
             .x <- .parsGetValue(pars, .var)
           }
           # jacobian for lognormal
           #
           # D(S("exp(x)"),"x") = exp(x)
           # If the parameter comes from the rxode2 ui, then this would be:
           # exp(x); from monolix it would be exp(log(x)) = x

           # jacobian for expit()
           # exp(-x)*(hi - low)/(1 + exp(-x))^2

           # probit for probitNormal
           # 0.5*sqrt(2)*exp((-1/2)*x^2)*(hi - low)/sqrt(pi)
           #
           # This can be used to transform Monolix's covariance matrix to

           # nlmixr's style of covariance matrix
           if (.cur$distribution == "lognormal") {
             .env$jac <- c(.env$jac, setNames(exp(.x), .var))
           } else if (.cur$distribution == "logitnormal") {
             .hi <- 1
             .low <- 0
             if (!is.null(.cur$min)) .low <- .cur$min
             if (!is.null(.cur$max)) .hi <- .cur$max
             .env$jac <- c(.env$jac, setNames(exp(-.x)*(.hi - .low)/(1 + exp(-.x))^2, .var))
           } else if (.cur$distribution == "normal") {
             .env$jac <- c(.env$jac, setNames(1, .var))
           } else if (.cur$distribution == "probitnormal") {
             .env$jac <- c(.env$jac, setNames(0.5*sqrt(2)*exp((-1/2)*.x^2)*1.0/sqrt(pi), .var))
           }
         })
  # This only calculates the "interesting" values; the rest should be 1
  .env$jac
}
