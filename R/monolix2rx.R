.monolix2rx <- new.env(parent=emptyenv())
.monolix2rx$lixoftConnectors <- NA
#' Output the information
#'
#' @param text character vector of the text to echo
#' @param ... other information sent to cli::cli_alert_info
#' @param .envir environment where alert is processed
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.minfo <- function (text, ..., .envir = parent.frame()) {
  cli::cli_alert_info(gettext(text), ..., .envir = .envir)
}

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
#' @param ci confidence interval for validation, by default 0.95
#' @param sigdig number of significant digits for validation, by default 3
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
#' @import ggplot2
#' @eval .monolix2rxBuildGram()
#' @examples
#' # First load in the model; in this case the theo model
#' # This is modified from the Monolix demos by saving the model
#' # File as a text file (hence you can access without model library)
#' # setup.
#' #
#' # This example is also included in the monolix2rx package, so
#' # you refer to the location with `system.file()`:
#'
#' pkgTheo <- system.file("theo", package="monolix2rx")
#'
#' rx <- monolix2rx(file.path(pkgTheo, "theophylline_project.mlxtran"))
#'
#' pkgCov <- system.file("cov", package="monolix2rx")
#'
#' rx <- monolix2rx(file.path(pkgCov, "warfarin_covariate3_project.mlxtran"))
#'
#' rx
monolix2rx <- function(mlxtran, update=TRUE, thetaMatType=c("sa", "lin"),
                       sd=1.0, cor=1e-5, theta=0.5, ci=0.95, sigdig=3,
                       envir=parent.frame()) {
  if (!requireNamespace("rxode2", quietly=FALSE) ||
        !requireNamespace("lotri", quietly=FALSE)) {
    stop("'monolix2rx' requires 'rxode2' and 'lotri'",
         call.=FALSE)
  }
  on.exit({
    .Call(`_monolix2rx_r_parseFree`)
  })
  checkmate::assertNumeric(sd, lower=0, finite=TRUE, any.missing = FALSE, len=1)
  checkmate::assertNumeric(cor, lower= -1, upper=1, finite=TRUE, any.missing = FALSE, len=1)
  checkmate::assertNumeric(theta, finite=TRUE, any.missing = FALSE, len=1)
  checkmate::assertNumeric(ci, lower=0, upper=1, finite=TRUE, any.missing=FALSE, len=1)
  checkmate::assertIntegerish(sigdig, lower=1, any.missing=FALSE, len=1)
  .monolix2rx$ignoredCoef <- character(0)
  .monolix2rx$iniSd <- sd
  .monolix2rx$iniCor <- cor
  .monolix2rx$iniTheta <- theta
  .monolix2rx$iniCi <- ci
  .monolix2rx$iniSigdig <- sigdig
  thetaMatType <- match.arg(thetaMatType)
  if (length(mlxtran) == 1L && is.character(mlxtran) &&
        grepl("[.]txt$", mlxtran, ignore.case = TRUE)) {
    .mlxtran <- mlxTxt(mlxtran)
  } else {
    .mlxtran <- mlxtran(mlxtran, equation=TRUE, update=update)
  }
  .admd <- NULL
  .cmt <- NULL
  if (!is.null(.mlxtran$MODEL$LONGITUDINAL$LONGITUDINAL$file)) {
    withr::with_dir(.monolixGetPwd(.mlxtran), {
      if (!file.exists(.mlxtran$MODEL$LONGITUDINAL$LONGITUDINAL$file)) {
        stop("the model file '", .mlxtran$MODEL$LONGITUDINAL$LONGITUDINAL$file, "' does not exist\nyou may need to setup the model library to complete translation",
             call.=FALSE)
      }
    })
  }
  if (is.null(.mlxtran$MODEL$LONGITUDINAL$EQUATION) &&
        !is.null(.mlxtran$MODEL$LONGITUDINAL$PK)) {
    .e <- .equation("", .mlxtran$MODEL$LONGITUDINAL$PK)
    .equation <- .e$rx
    .cmt <- .e$cmtPrefix
    .admd <- .e$admd
  } else if (!is.null(.mlxtran$MODEL$LONGITUDINAL$EQUATION)) {
    .equation <- .mlxtran$MODEL$LONGITUDINAL$EQUATION$rx # includes PK: macro
    .cmt <- .mlxtran$MODEL$LONGITUDINAL$EQUATION$cmtPrefix
    .admd <- .mlxtran$MODEL$LONGITUDINAL$EQUATION$admd
  } else {
    .equation <- character(0)
  }
  if (length(.cmt) == 1L && .cmt == "cmt()") .cmt <- NULL
  .model <- c("model({",
              .cmt,
              mlxtranTransformGetRxCode(.mlxtran),
              .mlxtran$MODEL$INDIVIDUAL$DEFINITION$rx,
              .equation,
              vapply(seq_along(.mlxtran$MODEL$LONGITUDINAL$DEFINITION$endpoint),
                     function(i) {
                       .handleSingleEndpoint(.mlxtran$MODEL$LONGITUDINAL$DEFINITION$endpoint[[i]])
                     }, character(1), USE.NAMES = FALSE),
              "})")
  if (length(.model) == 2L && all(.model == c("model({", "})"))) {
    stop("there are not equations to translate in this mlxtran file",
         call.=FALSE)
  }
  .model0 <- try(str2lang(paste0(.model, collapse="\n")), silent=TRUE)
  if (inherits(.model0, "try-error")) {
    message("Bad Model:\n")
    message(paste(.model, collapse="\n"))
    stop("model translation did not parse into a rxode2/nlmixr2 model", call.=FALSE)
  }
  .model <- .mlxtranChangeVal(.model0, .mlxtran)
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
      .iniDf <- .ui$iniDf
      .thetaMatNames <- .iniDf[is.na(.iniDf$neta1) & !.iniDf$fix,"name"]
      .thetaMat <- attr(.mlxtran, .tt)
      .sharedNames <- intersect(.thetaMatNames, dimnames(.thetaMat)[[1]])
      if (length(.sharedNames) != length(.thetaMatNames)) {
        .diff <- setdiff(.thetaMatNames, .sharedNames)
        warning(paste("The following parameters are missing from the thetaMat covariance matrix:.",
                      paste(.diff, collapse=", ")),
                      call.=FALSE)
      }
      .d <- diag(.thetaMat)
      .w <- which(is.nan(.d) | is.na(.d))
      if (length(.w) > 0L) {
        warning(paste("The following parameters are missing from the thetaMat covariance matrix because they were NaN/NA:",
                      paste(dimnames(.thetaMat)[[1]][.w], collapse=", ")),
                call.=FALSE)
        .thetaMat <- .thetaMat[-.w, -.w]
      }
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
  assign("admd", .admd, envir=.ui)
  assign("mlxtran", .mlxtran, envir=.ui)
  .monolixData <- try(monolixDataImport(.ui))
  if (inherits(.monolixData, "try-error")) .monolixData <- NULL
  if (!is.null(.monolixData)) {
    .minfo(paste0("imported monolix and translated to rxode2 compatible data (",
                  crayon::bold$blue("$monolixData"), ")"))
    .ui$monolixData <- .monolixData
    .ui$sticky <- "monolixData"
  }
  .etaData <- try(monolixEtaImport(.ui))
  if (inherits(.etaData, "try-error")) .etaData <- NULL
  if (!is.null(.etaData)) {
    .minfo(paste0("imported monolix ETAS (_SAEM) imported to rxode2 compatible data (",
                  crayon::bold$blue("$etaData"), ")"))
    .ui$etaData <- .etaData
  }
  .predIpredData <- try(monolixPredIpred(.ui))
  if (inherits(.predIpredData, "try-error")) .predIpredData <- NULL
  if (!is.null(.predIpredData)) {
    .minfo(paste0("imported monolix pred/ipred data to compare (",
                  crayon::blue$bold("$predIpredData"), ")"))
    .ui$predIpredData <- .predIpredData
  }
  if (!is.null(.ui$monolixData) && (.dfObs <= 0L || .dfSub <= 0L)) {
    .trans <- rxode2::etTrans(.ui$monolixData, .ui)
    .lst <- attr(class(.trans), ".rxode2.lst")
    if (.dfObs <= 0L) {
      assign("dfObs", as.double(.lst$nobs), envir=.ui$meta)
    }
    .dfSub <- attr(.mlxtran, "dfSub")
    if (.dfSub <= 0L) {
      assign("dfSub", as.double(.lst$nid), envir=.ui$meta)
    }
  }
  try(.validateModel(.ui, ci=ci, sigdig=sigdig))
  .ui <- rxode2::rxUiCompress(.ui)
  class(.ui) <- c("monolix2rx", class(.ui))
  .ui
}
