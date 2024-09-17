#' Get the number of doses for steady state for monolix2rx
#'
#' @param x monolix2rx object
#' @return Number of doses for monolix2rx object
#' @export
#' @keywords internal
#' @author Matthew L. Fidler
.getNbdoses <- function(x) {
  if (inherits(x, "monolix2rx")) {
    x <- x$mlxtran
  }
  if (inherits(x, "mlxtran")) {
    x <- x$DATAFILE$CONTENT$CONTENT
  }
  if (inherits(x, "monolix2rxContent")) {
    return(x$nbdoses)
  }
  7L
}
#' Get if the model object is stiff
#'
#' @param x monolix2rx object
#' @return boolean indicating if the object is a stiff system (as indicated by monolix)
#' @export
#' @keywords internal
#' @author Matthew L. Fidler
.getStiff <- function(x) {
  if (inherits(x, "monolix2rx")) {
    x <- x$mlxtran
  }
  if (inherits(x, "mlxtran")) {
    x <- x$MODEL$LONGITUDINAL$EQUATION
  }
  if (inherits(x, "monolix2rxEquation")) {
    return(x$odeType != "nonStiff")
  }
  FALSE
}
#' Get the atol/rtol values for the monolix imported object
#'
#' @param x monolix2rx object
#' @return tolerance
#' @noRd
#' @author Matthew L. Fidler
.getRtolAtol <- function(x) {
  ifelse(.getStiff(x), 1e-9, 1e-6)
}

#' Get ode solving method
#'
#' @param x monolix2rx
#' @return ode solving method
#' @noRd
#' @author Matthew L. Fidler
.getMethod <- function(x) {
  ifelse(.getStiff(x), "liblsoda", "dop853")
}

.subsetMonolix <- function(ui, data, iwres=NULL) {
  if (is.null(ui$predDf)) {

  } else if (length(ui$predDf$cond) == 1L) {
    # single endpoint
    .ret <- data[, c("id", "time", "ipredSim", iwres)]
    if (!is.null(iwres)) {
      names(.ret) <- c("id", "time", "ipred", iwres)
    } else {
      names(.ret) <- c("id", "time", "pred")
    }
    .ret$cmt <- ui$predDf$var[1]
    .ret
  } else {
    # multiple endpoint
    .ret <- do.call("rbind",
                    lapply(seq_along(ui$predDf$cond),
                           function(i) {
                             #id time    pred   ipred       iwres   cmt
                             .ret <- data[which(data$CMT == ui$predDf$cmt[i]),
                                          c("id", "time", "ipredSim", iwres)]
                             if (!is.null(iwres)) {
                               names(.ret) <- c("id", "time", "ipred", iwres)
                             } else {
                               names(.ret) <- c("id", "time", "pred")
                             }
                             .ret$cmt <- ui$predDf$var[i]
                             .ret
                           }))
    return(.ret)
  }
}
#' Validate the imported model
#'
#' @param ui rxode2 ui that is used to validate the model
#' @return ui with updated validation information
#' @noRd
#' @author Matthew L. Fidler
.validateModel <- function(ui, ci=0.95, sigdig=3) {
  # default for Monolix: nbSSDoses=7
  .ui <- rxode2::rxUiDecompress(ui)
  .nss <- .getNbdoses(.ui)
  .tol <- .getRtolAtol(.ui)
  .method <- .getMethod(.ui)
  .pop <- .parameterThetaEta(.ui, pop=TRUE)
  .ind <- .parameterThetaEta(.ui, pop=FALSE)
  .model <- ui$monolixModelIwres
  .data <- ui$monolixData
  if (any(names(.data) == "cens")) {
    .minfo("filtering out censored observations for validation")
    .data <- .data[.data$cens == 0, names(.data) != "cens"]
    .minfo("done")
  }
  if (!is.null(.data) &&
      length(.data) > 0 &&
      length(.data[, 1]) > 0 &&
      length(.ind) > 0 &&
      length(.ind[, 1]) > 0) {
    .minfo("solving ipred problem")
    .ipredSolve <- try(rxode2::rxSolve(.model, .ind, .data, returnType = "data.frame",
                                       covsInterpolation="locf",
                                       #addlKeepsCov=TRUE, addlDropSs=TRUE, ssAtDoseTime=TRUE,
                                       #safeZero=TRUE, ss2cancelAllPending=TRUE,
                                       maxSS=.nss + 1,
                                       minSS=.nss,
                                       atol=.tol, rtol=.tol,
                                       ssAtol=100, ssRtol=100, omega=NULL,
                                       addDosing = FALSE))

    .ipredSolve <- .subsetMonolix(.ui, .ipredSolve, c("iwres", "ires"))
    .minfo("done")
    .minfo("solving pred problem")
    .predSolve <- try(rxode2::rxSolve(.model, .pop, .data, returnType = "data.frame",
                                      covsInterpolation="locf",
                                      #addlKeepsCov=TRUE, addlDropSs=TRUE, ssAtDoseTime=TRUE,
                                      #safeZero=TRUE, ss2cancelAllPending=TRUE,
                                      maxSS=.nss + 1,
                                      minSS=.nss,
                                      atol=.tol, rtol=.tol,
                                      ssAtol=100, ssRtol=100, omega=NULL,
                                      addDosing = FALSE))
    .predSolve <- .subsetMonolix(.ui, .predSolve)
    .nPredSolve <- length(.predSolve[, 1])
    .nIpredSolve <- length(.ipredSolve[, 1])
    if (.nPredSolve == .nIpredSolve &&
          all(.ipredSolve$id == .predSolve$id) &&
          all(.ipredSolve$time == .predSolve$time) &&
          all(.ipredSolve$cmt == .predSolve$cmt)) {
      .both <- .ipredSolve
      .both$pred <-.predSolve$pred
      .nBoth <- length(.both[, 1])
    } else {
      .minfo("ipred and pred rxode2 solves do not match for id, time and cmt")
      return(invisible())
    }
    .monolix <- ui$predIpredData
    names(.monolix) <- vapply(names(.monolix),
                              function(n) {
                                if (n == "pred") return("monolixPred")
                                if (n %in% c("ipred", "iwres")) {
                                  return(paste0("monolixI", substr(n, 2, nchar(n))))
                                }
                                n
                              }, character(1), USE.NAMES = FALSE)
    .nMonolix <- length(.monolix[, 1])

    .monolix$rxMonolixRowN <- seq_along(.monolix[,1])
    .both <- merge(.monolix, .both)
    .monolixNot <- .monolix[!(.monolix$rxMonolixRowN %in% .both$rxMonolixRowN), ,drop=FALSE]
    if (length(.monolixNot$rxMonolixRowN) != 0L) {
      .minfo(paste0("monolix and rxode2 solves have different number of rows (",
                    crayon::blue$bold("$monolixNotMatched"),")"))
      .monolixNot <- .monolixNot[,names(.monolixNot) != "rxMonolixRowN"]
      assign("monolixNotMatched", .monolixNot, .ui)
    }
    .both <- .both[, names(.both) != "rxMonolixRowN"]
    .ci0 <- .ci <- ci
    .sigdig <- sigdig
    .ci <- (1 - .ci) / 2
    .q <- c(0, .ci, 0.5, 1 - .ci, 1)
    .qi <- stats::quantile(with(.both, 100*abs((ipred-monolixIpred)/monolixIpred)), .q, na.rm=TRUE)
    .qai <- stats::quantile(with(.both, abs(ipred-monolixIpred)), .q, na.rm=TRUE)
    .qp <- stats::quantile(with(.both, 100*abs((pred-monolixPred)/monolixIpred)), .q, na.rm=TRUE)
    .qap <- stats::quantile(with(.both, abs(pred-monolixPred)), .q, na.rm=TRUE)

    .qw <- stats::quantile(with(.both, 100*abs((iwres-monolixIwres)/monolixIwres)), .q, na.rm=TRUE)
    .qaw <- stats::quantile(with(.both, abs(iwres-monolixIwres)), .q, na.rm=TRUE)

    .ui$ipredAtol <- .qai[3]
    .ui$ipredRtol <- .qi[3]/100
    .ui$ipredCompare <- .both[,grepl("(id|time|monolixIpred|ipred|cmt)", names(.both), ignore.case=TRUE)]

    .ui$predAtol <- .qap[3]
    .ui$predRtol <- .qp[3]/100
    .ui$predCompare <- .both[,grepl("^(id|time|monolixPred|pred|cmt)$", names(.both), ignore.case=TRUE)]

    .ui$iwresAtol <- .qaw[3]
    .ui$iwresRtol <- .qw[3]/100
    .ui$iwresCompare <- .both[,grepl("(id|time|monolixIwres|iwres|cmt)", names(.both), ignore.case=TRUE)]

    .msg <- c(paste0("ipred relative difference compared to Monolix ipred: ", round(.qi[3], 2),
                     "%; ", .ci0 * 100,"% percentile: (",
                     round(.qi[2], 2), "%,", round(.qi[4], 2), "%); rtol=",
                     signif(.qi[3] / 100, digits=.sigdig)),
              paste0("ipred absolute difference compared to Monolix ipred: ", .ci0 * 100,"% percentile: (",
                     signif(.qai[2], .sigdig), ", ", signif(.qai[4], .sigdig), "); atol=",
                     signif(.qai[3], .sigdig)),
              paste0("pred relative difference compared to Monolix pred: ", round(.qp[3], 2),
                     "%; ", .ci0 * 100,"% percentile: (",
                     round(.qp[2], 2), "%,", round(.qp[4], 2), "%); rtol=",
                     signif(.qp[3] / 100, digits=.sigdig)),
              paste0("pred absolute difference compared to Monolix pred: ", .ci0 * 100,
                     "% percentile: (",
                     signif(.qap[2], .sigdig), ", ",
                     signif(.qap[4], .sigdig), "); atol=",
                     signif(.qap[3], .sigdig)),
              paste0("iwres relative difference compared to Monolix iwres: ", round(.qp[3], 2),
                     "%; ", .ci0 * 100,"% percentile: (",
                     round(.qw[2], 2), "%,", round(.qw[4], 2), "%); rtol=",
                     signif(.qw[3] / 100, digits=.sigdig)),
              paste0("iwres absolute difference compared to Monolix pred: ", .ci0 * 100,
                     "% percentile: (",
                     signif(.qaw[2], .sigdig), ", ",
                     signif(.qaw[4], .sigdig), "); atol=",
                     signif(.qaw[3], .sigdig)))
    assign("validation", .msg, .ui$meta)
    assign("predIpredData", .both, .ui)
    .minfo("done")
  } else  {
    .minfo("cannot find individual parameter estimates")
  }
  .ui
}
