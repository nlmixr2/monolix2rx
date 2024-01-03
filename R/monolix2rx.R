.monolix2rx <- new.env(parent=emptyenv())

#' Translate a monolix file to rxode2
#'
#' @param mlxtran file name for mlxtran to translate to rxode2
#' @return rxode2 model
#' @export
#' @author Matthew L. Fidler
#' @useDynLib monolix2rx, .registration=TRUE
#' @importFrom Rcpp cppFunction
#' @importFrom dparser dparse
#' @importFrom stats setNames
#' @eval .monolix2rxBuildGram()
#' @examples
monolix2rx <- function(mlxtran, envir=parent.frame()){
  if (!requireNamespace("rxode2", quietly=FALSE) ||
        !requireNamespace("lotri", quietly=FALSE)) {
    stop("'monolix2rx' requires 'rxode2' and 'lotri'",
         call.=FALSE)
  }
  .ret <- mlxtran(mlxtran, equation=TRUE)
  .equation <- .ret$MODEL$LONGITUDINAL$EQUATION$rx # includes PK: macro
  .model <- c("model({",
              .ret$MODEL$INDIVIDUAL$DEFINITION$rx,
              .equation,
              vapply(seq_along(.ret$MODEL$LONGITUDINAL$DEFINITION$endpoint),
                     function(i) {
                       .handleSingleEndpoint(.ret$MODEL$LONGITUDINAL$DEFINITION$endpoint[[i]])
                     }, character(1), USE.NAMES = FALSE),
              "})")
  .model <- str2lang(paste0(.model, collapse="\n"))
  .ini <- .def2ini(v$MODEL$INDIVIDUAL$DEFINITION,
                   v$PARAMETER$PARAMETER,
                   v$MODEL$LONGITUDINAL$DEFINITION)
  .ret <- function() {}
  body(.ret) <- as.call(c(list(quote(`{`)), .ini, .model))
  .ret <- eval(.ret, envir=envir)
  .ui <- .ret()
  .ui
}
#' Handle a single endpoint and convert to rxode2
#'
#' @param endpoint The endpoint to convert to syntax
#' @return rxode2 syntax for the monolix endpoint
#' @noRd
#' @author Matthew L. Fidler
.handleSingleEndpoint <- function(endpoint) {
  # $MODEL$LONGITUDINAL$DEFINITION$endpoint[[i]]
  if (endpoint$dist == "event") {
    stop("'event' endpoint not supported in translation yet",
         call.=FALSE)
  } else if (endpoint$dist == "categorical") {
    stop("'categorical' endpoint not supported in translation yet",
         call.=FALSE)
  } else if (endpoint$dist == "count") {
    stop("'count' endpoint not supported in translation yet",
         call.=FALSE)
  } else if (endpoint$dist == "lognormal") {
    .add <- "lnorm"
  } else if (endpoint$dist == "normal") {
    .add <- "add"
  } else if (endpoint$dist == "logitnormal") {
    .add <- "logitNorm"
  } else if (endpoint$dist == "probitnormal") {
    .add <- "probitNorm"
  }
  if (endpoint$err$errName == "constant") {
    return(paste0(endpoint$pred, " ~ ",
                  .add,
                  "(",
                  endpoint$err$typical[1],
                  ") | ",
                  endpoint$var))
  } else if (endpoint$err$errName == "proportional") {
    return(paste0(endpoint$pred, " ~ ",
                  ifelse(.add == "add", "", paste0(.add, "(NA) + ")),
                  "prop(",
                  endpoint$err$typical[1],
                  ") | ",
                  endpoint$var))
  }
  if (endpoint$err$errName %in% c("combined1", "combined1c")) {
    .combined <- " + combined1()"
  } else if (endpoint$err$errName %in% c("combined2", "combined2c")) {
    .combined <- " + combined2()"
  }
  if (endpoint$err$errName %in% c("combined1", "combined2")) {
    .prop <- paste0(" + prop(", endpoint$err$typical[2], ")")
  } else if (endpoint$err$errName %in% c("combined1c", "combined2c")) {
    .prop <- paste0(" + pow(", endpoint$err$typical[2], ", ",
                    endpoint$err$typical[3], ")")
  }
  return(paste0(endpoint$pred, " ~ ",
                .add, "(", endpoint$err$typical[1],
                ")", .prop, .combined, " | ",
                endpoint$var))
}
