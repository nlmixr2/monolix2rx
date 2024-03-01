#' This imports the pred/ipred taking into account that monolix may
#' split them in multiple files
#'
#' This assumes that the working directory is already set so that the
#' export path will pick up the correct files
#'
#' @param endpoint endpoint to read (or `NULL` for single endpoint)
#' @param mlxtran parsed mlxtran object
#' @return A single ipred/pred dataset read in.  With endpoint
#'   defined, add the endpoint as a `cmt` column
#' @noRd
#' @author Matthew L. Fidler
.monolixPredIpredFile <- function(endpoint, mlxtran, na.strings) {
  if (!is.null(endpoint)) {
    .est <- file.path(mlxtran$MONOLIX$SETTINGS$GLOBAL$exportpath,
                      paste0("predictions_", endpoint, ".txt"))
  } else {
    .est <- file.path(mlxtran$MONOLIX$SETTINGS$GLOBAL$exportpath,
                      "predictions.txt")
  }
  .try <- try(file.exists(.est), silent=TRUE)
  if (inherits(.try, "try-error")) return(NULL)
  if (length(.try) != 1L) return(NULL)
  if (!.try) return(NULL)
  .ret <- read.csv(.est, row.names=NULL, na.strings = na.strings)
  # first subset to the right information
  .ret <- .ret[, vapply(names(.ret),
                        function(n) {
                          if (n == "id") return(TRUE)
                          if (n == "time") return(TRUE)
                          if (n == "popPred") return(TRUE)
                          if (grepl("_SAEM$", n)) return(TRUE)
                          FALSE
                        }, logical(1),
                        USE.NAMES=FALSE)]
  names(.ret) <- vapply(names(.ret),
                        function(n) {
                          if (n == "id") return("id")
                          if (n == "time") return("time")
                          if (n == "popPred") return("pred")
                          if (n == "indivPred_SAEM") return("ipred")
                          if (n == "indWRes_SAEM") return("iwres")
                          "..."
                        }, character(1), USE.NAMES=FALSE)
  if (!is.null(endpoint)) {
    # Check -- could be different
    .ret$cmt <- endpoint
  }
  .ret
}
#' Load ipred/pred from a monolix run
#'
#' @param mlxtran mlxtran file where data input is specified
#' @inheritParams read.table
#' @author Matthew L Fidler
#' @export
monolixPredIpred <- function(mlxtran, na.strings=c("NA", ".")) {
  if (inherits(mlxtran, "rxUi")) mlxtran <- mlxtran$mlxtran
  if (is.null(mlxtran)) return(NULL)
  withr::with_dir(attr(mlxtran, "dirn"), {
    .end <- monolixEndpoints(mlxtran)
    if (length(.end) == 1) {
      .ret <- .monolixPredIpredFile(NULL, mlxtran=mlxtran, na.strings=na.strings)
    } else {
      .ret <- do.call("rbind",
                      lapply(.end,
                             function(e) {
                               .monolixPredIpredFile(e, mlxtran, na.strings=na.strings)
                             }))
    }
    .ret
  })
}
