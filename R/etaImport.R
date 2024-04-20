#' Load data from a mlxtran defined dataset
#'
#' @param mlxtran mlxtran file where data input is specified
#' @inheritParams utils::read.table
#' @author Matthew L Fidler
#' @export
monolixEtaImport <- function(mlxtran, na.strings=c("NA", ".")) {
  mlxtran <- .monolixGetMlxtran(mlxtran)
  if (is.null(mlxtran)) return(NULL)
  if (!inherits(mlxtran, "monolix2rxMlxtran")) return(NULL)
  withr::with_dir(.monolixGetPwd(mlxtran), {
    .est <- file.path(mlxtran$MONOLIX$SETTINGS$GLOBAL$exportpath,
                      "IndividualParameters",
                      "estimatedRandomEffects.txt")
    .try <- try(file.exists(.est), silent=TRUE)
    if (inherits(.try, "try-error")) return(NULL)
    if (length(.try) != 1L) return(NULL)
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
