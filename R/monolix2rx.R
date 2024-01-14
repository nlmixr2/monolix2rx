.monolix2rx <- new.env(parent=emptyenv())

#' Translate a monolix file to rxode2
#'
#' @param mlxtran file name for mlxtran to translate to rxode2
#' @param update is a boolean that represents if the final parameter
#'   estimates should be used for the translation (when present)
#' @param thetaMatType This lists the preferred source for `thetaMat`
#'   covariance matrix.  By default it is `sa` for simulated
#'   annealing, though you could use `lin` for linearized covariance
#'   calculation. If only one is present, then use whatever is present
#' @param sd Default standard deviation for between subject
#'   variability/inter-occasion variability that are missing.
#' @param cor Default correlation for missing correlations estimate
#' @param theta default population estimate
#' @param envir represents the environment used for evaluating the
#'   corresponding rxode2 function
#' @return rxode2 model
#' @export
#' @author Matthew L. Fidler
#' @useDynLib monolix2rx, .registration=TRUE
#' @importFrom Rcpp cppFunction
#' @importFrom dparser dparse
#' @importFrom stats setNames qnorm
#' @importFrom utils read.csv
#' @eval .monolix2rxBuildGram()
#' @examples
monolix2rx <- function(mlxtran, update=TRUE, thetaMatType=c("sa", "lin"),
                       sd=1.0, cor=1e-5, theta=0.5,
                       envir=parent.frame()){
  if (!requireNamespace("rxode2", quietly=FALSE) ||
        !requireNamespace("lotri", quietly=FALSE)) {
    stop("'monolix2rx' requires 'rxode2' and 'lotri'",
         call.=FALSE)
  }
  checkmate::assertNumeric(sd, lower=0, finite=TRUE, any.missing = FALSE, len=1)
  checkmate::assertNumeric(cor, lower= -1, upper=1, finite=TRUE, any.missing = FALSE, len=1)
  checkmate::assertNumeric(theta, finite=TRUE, any.missing = FALSE, len=1)
  .monolix2rx$iniSd <- sd
  .monolix2rx$iniCor <- cor
  .monolix2rx$iniTheta <- theta
  thetaMatType <- match.arg(thetaMatType)
  .mlxtran <- mlxtran(mlxtran, equation=TRUE, update=update)
  if (is.null(.mlxtran$MODEL$LONGITUDINAL$EQUATION) &&
        !is.null(.mlxtran$MODEL$LONGITUDINAL$PK)) {
    .equation <- .equation("", .mlxtran$MODEL$LONGITUDINAL$PK)$rx
  } else if (!is.null(.mlxtran$MODEL$LONGITUDINAL$EQUATION)) {
    .equation <- .mlxtran$MODEL$LONGITUDINAL$EQUATION$rx # includes PK: macro
  } else {
    .equation <- character(0)
  }
  .model <- c("model({",
              .mlxtran$MODEL$INDIVIDUAL$DEFINITION$rx,
              .equation,
              vapply(seq_along(.mlxtran$MODEL$LONGITUDINAL$DEFINITION$endpoint),
                     function(i) {
                       .handleSingleEndpoint(.mlxtran$MODEL$LONGITUDINAL$DEFINITION$endpoint[[i]])
                     }, character(1), USE.NAMES = FALSE),
              "})")
  if (all(.model == c("model({", "})"))) {
    stop("there are not equations to translate in this mlxtran file",
         call.=FALSE)
  }
  .model <- str2lang(paste0(.model, collapse="\n"))
  .ini <- .def2ini(.mlxtran$MODEL$INDIVIDUAL$DEFINITION,
                   .mlxtran$PARAMETER$PARAMETER,
                   .mlxtran$MODEL$LONGITUDINAL$DEFINITION)
  .ret <- function() {}
  if (gsub(" +", "", deparse1(.ini)) == "ini({})") {
    body(.ret) <- as.call(c(list(quote(`{`)), .model))
  } else {
    body(.ret) <- as.call(c(list(quote(`{`)), .ini, .model))
  }
  .ret <- eval(.ret, envir=envir)
  ini <- rxode2::ini
  model <- rxode2::model
  lotri <- lotri::lotri
  .ui <- try(.ret(), silent=TRUE)
  if (inherits(.ui, "try-error")) {
    print(.ret)
    .ret()
  }
  .ui <- rxode2::rxUiDecompress(.ui)
  .dfObs <- attr(.mlxtran, "dfObs")
  if (.dfObs > 0L) assign("dfObs", as.double(.dfObs), envir=.ui$meta)
  .dfSub <- attr(.mlxtran, "dfSub")
  if (.dfSub > 0L) assign("dfSub", as.double(.dfSub), envir=.ui$meta)
  if (thetaMatType == "sa") {
    .thetaMatType <- c("covSaUntransformed", "covLinUntransformed")
  } else {
    .thetaMatType <- c("covLinUntransformed", "covSaUntransformed")
  }
  for (.tt in .thetaMatType) {
    if (inherits(attr(.mlxtran, .tt), "matrix")) {
      .thetaMat <- names(.ui$theta)
      .thetaMat <- attr(.mlxtran, .tt)[.thetaMat, .thetaMat]
      .thetaMatType <- .tt
      break
    }
  }
  if (length(.thetaMatType) == 1L) {
    assign("thetaMat", .thetaMat, envir=.ui$meta)
  }
  if (!is.null(attr(.mlxtran, "desc")) &&
        attr(.mlxtran, "desc") != "") {
    assign("description", attr(.mlxtran, "desc"), envir=.ui$meta)
  }
  rxode2::rxUiCompress(.ui)
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
                  ifelse(endpoint$dist == "logitnormal",
                         paste0(", ", endpoint$min, ", ", endpoint$max),
                         ""),
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
