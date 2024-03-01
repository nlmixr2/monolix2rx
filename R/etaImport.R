#' Load data from a mlxtran defined dataset
#'
#' @param mlxtran mlxtran file where data input is specified
#' @inheritParams read.table
#' @noRd
.monolixImportEtas <- function(mlxtran, na.strings=c("NA", ".")) {
  if (inherits(mlxtran, "monolix2rx")) mlxtran <- mlxtran$mlxtran
  withr::with_dir(attr(m$mlxtran, "dirn"), {
    .est <- file.path(mlxtran$MONOLIX$SETTINGS$GLOBAL$exportpath,
                      "IndividualParameters",
                      "estimatedRandomEffects.txt")
    .try <- try(file.exists(.est), silent=TRUE)
    if (inherits(.try, "try-error")) return(NULL)
    if (!.try) return(NULL)
    .ret <- read.csv(.est, row.names=NULL, na.strings = na.strings)
    .ret <- .ret[, vapply(names(.ret),
                          function(n) {
                            if (n == "id") return(TRUE)
                            grepl("_SAEM$", n)
                          }, logical(1), USE.NAMES=FALSE)]
    names(.ret) <- vapply(names(.ret),
                          function(n) {
                            if (n == "id") return("id")
                            sub("^eta_(.*)_SAEM$", "omega_\\1", n)
                          }, character(1), USE.NAMES = FALSE)
    .ret
  })
}
