#' Load data from a mlxtran defined dataset
#'
#' @param mlxtran mlxtran file where data input is specified
#' @inheritParams utils::read.table
#' @author Matthew L Fidler
#' @noRd
monolixEtaImport <- function(mlxtran, na.strings=c("NA", ".")) {
  mlxtran <- .monolixGetMlxtran(mlxtran)
  if (is.null(mlxtran)) return(NULL)
  if (!inherits(mlxtran, "monolix2rxMlxtran")) return(NULL)
  withr::with_dir(.monolixGetPwd(mlxtran), {
    .est <- file.path(.mlxtranExportPath(mlxtran),
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
    .sd <- .etaImportSd(mlxtran)
    names(.ret) <- vapply(names(.ret),
                          function(n) {
                            if (n == "id") return("id")
                            .p <- sub("^eta_(.*)_SAEM$", "\\1", n)
                            if (.p %in% names(.sd)) return(.sd[[.p]])
                            paste0("omega_", .p)
                          }, character(1), USE.NAMES = FALSE)
    .ret
  })
}

#' Eta name of each individual parameter
#'
#' Monolix names the random effect columns after the parameter
#' (eta_Cl), while the rxode2 eta is the parameter's sd= or var=
#' name, which need not be omega_Cl.
#'
#' @param mlxtran monolix2rxMlxtran object
#' @return named character vector, parameter -> eta name
#' @noRd
#' @author Matthew L. Fidler
.etaImportSd <- function(mlxtran) {
  .def <- try(as.list(mlxtran$MODEL$INDIVIDUAL$DEFINITION)$vars, silent=TRUE)
  if (inherits(.def, "try-error") || !is.list(.def)) return(character(0))
  .ret <- vapply(.def, function(v) {
    .i <- if (is.null(v$varlevel)) 1L else match("id", v$varlevel)
    .s <- if (!is.null(v$sd)) v$sd else v$var
    if (is.na(.i) || !is.character(.s) || length(.s) < .i) return(NA_character_)
    .s[.i]
  }, character(1))
  .ret[!is.na(.ret)]
}
