#' This imports the pred/ipred taking into account that monolix may
#' split them in multiple files
#'
#' This assumes that the working directory is already set so that the
#' export path will pick up the correct files
#'
#' @param endpoint endpoint to read (or `NULL` for single endpoint)
#' @param mlxtran parsed mlxtran object
#' @param est `"mode"` or `"SAEM"` (see `.monolixEstSuffix()`)
#' @return A single ipred/pred dataset read in.  With endpoint
#'   defined, add the endpoint as a `cmt` column
#' @noRd
#' @author Matthew L. Fidler
.monolixPredIpredFile <- function(endpoint, mlxtran, na.strings, est="mode") {
  if (!is.null(endpoint)) {
    .est <- file.path(.mlxtranExportPath(mlxtran),
                      paste0("predictions_", endpoint, ".txt"))
  } else {
    .est <- file.path(.mlxtranExportPath(mlxtran),
                      "predictions.txt")
  }
  .try <- try(file.exists(.est), silent=TRUE)
  if (inherits(.try, "try-error")) return(NULL)
  if (length(.try) != 1L) return(NULL)
  if (!.try) return(NULL)
  .ret <- read.csv(.est, row.names=NULL, na.strings = na.strings)
  .suf <- .monolixEstSuffix(names(.ret), c("^indivPred_", "^indWRes_"), est)
  # first subset to the right information
  .ret <- .ret[, vapply(names(.ret),
                        function(n) {
                          if (n == "id") return(TRUE)
                          if (n == "time") return(TRUE)
                          if (n == "popPred") return(TRUE)
                          if (n %in% paste0(c("indivPred_", "indWRes_"), .suf)) return(TRUE)
                          FALSE
                        }, logical(1),
                        USE.NAMES=FALSE)]
  names(.ret) <- vapply(names(.ret),
                        function(n) {
                          if (n == "id") return("id")
                          if (n == "time") return("time")
                          if (n == "popPred") return("pred")
                          if (n == paste0("indivPred_", .suf)) return("ipred")
                          if (n == paste0("indWRes_", .suf)) return("iwres")
                          "..."
                        }, character(1), USE.NAMES=FALSE)
  if (!is.null(endpoint)) {
    # Check -- could be different
    .ret$cmt <- endpoint
  }
  attr(.ret, "est") <- .suf
  .ret
}
#' Load ipred/pred from a monolix run
#'
#' @param mlxtran mlxtran file where data input is specified
#' @param est `"mode"` or `"SAEM"`; every file is read with the same one,
#'   SAEM when a file has no modes
#' @inheritParams utils::read.table
#' @return the predictions with the attribute `est`
#' @author Matthew L Fidler
#' @noRd
monolixPredIpred <- function(mlxtran, na.strings=c("NA", "."), est=c("mode", "SAEM")) {
  est <- match.arg(est)
  mlxtran <- .monolixGetMlxtran(mlxtran)
  withr::with_dir(.monolixGetPwd(mlxtran), {
    .all <- monolixEndpoints(mlxtran)
    # discrete endpoints have no predictions
    .disc <- vapply(mlxtran$MODEL$LONGITUDINAL$DEFINITION$endpoint,
                    function(e) e$dist %in% c("categorical", "count", "event"),
                    logical(1))
    .end <- .all[!.disc]
    .read <- function(est) {
      if (length(.end) == 1) {
        .ret <- .monolixPredIpredFile(NULL, mlxtran=mlxtran, na.strings=na.strings, est=est)
        if (is.null(.ret)) .ret <- .monolixPredIpredFile(.end, mlxtran, na.strings=na.strings, est=est)
        if (!is.null(.ret) && length(.all) > 1L) .ret$cmt <- .end
        return(list(.ret))
      }
      lapply(.end, function(e) .monolixPredIpredFile(e, mlxtran, na.strings=na.strings, est=est))
    }
    .l <- .read(est)
    .suf <- vapply(.l, function(d) if (is.null(d)) est else attr(d, "est"), character(1))
    if (any(.suf != est)) {
      est <- "SAEM"
      .l <- .read(est)
    }
    .ret <- do.call("rbind", .l)
    if (!is.null(.ret)) attr(.ret, "est") <- est
    .ret
  })
}
