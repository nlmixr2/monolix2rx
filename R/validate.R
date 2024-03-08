#' Get the number of doses for steady state for monolix2rx
#'
#' @param x monolix2rx object
#' @return Number of doses for monolix2rx object
#' @noRd
#' @author Matthew L. Fidler
.getNbdoses <- function(x) {
  if (inherits(x, "monolix2rx")){
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
#' @noRd
#' @author Matthew L. Fidler
.getStiff <- function(x) {
  if (inherits(x, "monolix2rx")){
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

  } else {
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
#' @param ui
#' @return ui with updated validation information
#' @noRd
#' @author Matthew L. Fidler
.validateModel <- function(ui) {
  # default for Monolix: nbSSDoses=7
  .nss <- .getNbdoses(ui)
  .tol <- .getRtolAtol(ui)
  .method <- .getMethod(ui)
  .pop <- .parameterThetaEta(ui, pop=TRUE)
  .ind <- .parameterThetaEta(ui, pop=FALSE)
  .model <- ui$monolixModelIwres
  .data <- ui$monolixData
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
  .ipredSolve <- .subsetMonolix(ui, .ipredSolve, "iwres")
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
  .predSolve <- .subsetMonolix(ui, .predSolve)

  .both <- merge(.predSolve, .ipredSolve)
  .minfo("done")
}
