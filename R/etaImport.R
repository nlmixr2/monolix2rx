#' Load data from a mlxtran defined dataset
#'
#' @param mlxtran mlxtran file where data input is specified
#' @param est Monolix estimate to import: `"mode"` (the conditional
#'   modes, falling back to `"SAEM"` when the run has none) or `"SAEM"`
#' @param idText keep the ids as Monolix wrote them (text) instead of
#'   `.monolixIdType()`
#' @inheritParams utils::read.table
#' @author Matthew L Fidler
#' @noRd
monolixEtaImport <- function(mlxtran, na.strings=c("NA", "."), est=c("mode", "SAEM"),
                             idText=FALSE) {
  est <- match.arg(est)
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
    .ret <- read.csv(.est, row.names=NULL, na.strings = na.strings,
                     colClasses = c(id = "character"))
    if (!idText) .ret$id <- .monolixIdType(.ret$id)
    .suf <- .monolixEstSuffix(names(.ret), "^eta_.*_", est)
    .ret <- .ret[, vapply(names(.ret),
                          function(n) {
                            if (n == "id") return(TRUE)
                            grepl(paste0("_", .suf, "$"), n)
                          }, logical(1), USE.NAMES=FALSE), drop=FALSE]
    .sd <- .etaImportSd(mlxtran)
    names(.ret) <- vapply(names(.ret),
                          function(n) {
                            if (n == "id") return("id")
                            .p <- sub(paste0("^eta_(.*)_", .suf, "$"), "\\1", n)
                            if (.p %in% names(.sd)) return(.sd[[.p]])
                            paste0("omega_", .p)
                          }, character(1), USE.NAMES = FALSE)
    # occasions (an occasion column or EVID=4) repeat the random effects
    # of a model without IOV
    if (anyDuplicated(.ret$id) && !anyDuplicated(unique(.ret)$id)) {
      .ret <- .ret[!duplicated(.ret$id), , drop=FALSE]
      rownames(.ret) <- NULL
    }
    attr(.ret, "est") <- .suf
    .ret
  })
}

#' Suffix of the Monolix estimate to read
#'
#' The conditional modes (`_mode`) are what Monolix's `indivPred_mode`
#' is computed from; `_SAEM` averages the parameters and the random
#' effects separately, so the two do not reproduce each other.
#'
#' @param names column names of the Monolix table
#' @param prefix regular expressions before the suffix; all must be present
#' @param est requested estimate
#' @return `"mode"` when requested and present, otherwise `"SAEM"`
#' @noRd
#' @author Matthew L. Fidler
.monolixEstSuffix <- function(names, prefix, est="mode") {
  if (est == "mode" &&
        all(vapply(prefix, function(p) any(grepl(paste0(p, "mode$"), names)), logical(1)))) {
    return("mode")
  }
  "SAEM"
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
