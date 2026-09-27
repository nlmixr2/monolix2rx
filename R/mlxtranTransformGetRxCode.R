#' Transform Mlxtran Covariate Definitions to `rxode2` Code
#'
#' This function takes an mlxtran object and extracts the covariate
#' transformation definitions, converting them into `rxode2`-compatible
#' code.
#'
#' @param mlxtran A list representing the `mlxtran` object, which
#'   contains model definitions including covariates and their
#'   transformations. This comes from the `mlxtran()` function
#'
#' @return A character vector of `rxode2`-compatible code for the
#'   covariate transformations, or `NULL` if no covariate
#'   transformations are defined.
#'
#' @noRd
#'
#' @author Matthew L. Fidler
#'
#' @examples
#'
#' m <- mlxtran(file.path(system.file("cov", package="monolix2rx"), "warfarin_covariate3_project.mlxtran"))
#' mlxtranTransformGetRxCode(m)
#'
mlxtranTransformGetRxCode <- function(mlxtran) {
  .cov <- mlxtran$MODEL$COVARIATE$COVARIATE
  if (is.null(.cov)) return(NULL)
  .cov <- mlxtran$MODEL$COVARIATE$DEFINITION
  if (is.null(.cov)) return(NULL)
  .transform <- .cov$transform
  if (length(.transform) == 0) return(NULL)
  paste(
    vapply(names(.transform),
         function(n) {
           .t <- .transform[[n]]
           .v <- .t$transform
           if (length(.v) != 1L || !nzchar(.v)) {
             stop("covariate transformation '", n, "' does not specify 'transform='",
                  call.=FALSE)
           }
           if (length(.t$catLabel) == 0L) {
             stop("covariate transformation '", n, "' does not specify 'categories='",
                  call.=FALSE)
           }
           # one non-numeric value means the data column is character
           .q <- anyNA(suppressWarnings(as.numeric(unlist(.t$catValue))))
           .cw <- vapply(seq_along(.t$catLabel),
                         function(i) {
                           .val <- .t$catValue[[i]]
                           if (.q) .val <- .mlxtranTransformLabel(.val)
                           .or <- paste(paste0(.v, " == ", .val),
                                        collapse=" || ")
                           paste0("if (", .or, ") {\n  ", n, " <- ",
                                  .mlxtranTransformLabel(.t$catLabel[i]), "\n}")
                         }, character(1), USE.NAMES=FALSE)
           .cw <- paste(.cw, collapse=" else ")
           # without a reference, unmatched values fall back to the first category
           .ref <- .t$reference
           if (length(.ref) != 1L || !nzchar(.ref)) .ref <- .t$catLabel[1]
           paste0(.cw, " else {\n  ", n, " <- ", .mlxtranTransformLabel(.ref), "\n}")
         }, character(1),
         USE.NAMES=FALSE),
    collapse="\n")
}
#' Quote a category label as an rxode2 string
#'
#' @param x category label(s)
#' @return quoted label
#' @noRd
#' @author Matthew L. Fidler
.mlxtranTransformLabel <- function(x) {
  ifelse(grepl("'", x, fixed=TRUE), paste0('"', x, '"'), paste0("'", x, "'"))
}
