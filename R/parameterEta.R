#' This takes the theta and etas to get a data frame for rxode2 solving & validation
#'
#' @param ui rxode2 ui imported from monolix
#' @param pop boolean that indicates if this parameter is for a
#'   population solve
#' @return the theta/eta data frame used for validation
#' @noRd
#' @author Matthew L. Fidler
.parameterThetaEta <- function(ui, pop=FALSE) {
  .etaData <- ui$etaData
  .theta <- ui$theta
  if (pop) {
    .n <- names(ui$etaData)
    .n <- .n[.n != "id"]
    return(c(.theta, setNames(rep(0.0, length(.n)), .n)))
  }
  .nid <- length(.etaData$id)
  .id <- .etaData[,"id", drop=FALSE]
  .rest <- .etaData[, -which(names(.etaData) == "id"), drop = FALSE]
  .thetaDf <- lapply(names(.theta),
                     function(t) {
                       rep(.theta[t], .nid)
                     })
  names(.thetaDf) <- names(.theta)
  .thetaDf <- as.data.frame(.thetaDf)
  cbind(.id, .thetaDf, .rest)
}
