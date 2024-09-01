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
#' @author Matthew L. Fidler
mlxtranTransformGetRxCode <- function(mlxtran) {
  .cov <- mlxtran$MODEL$COVARIATE$COVARIATE
  if (is.null(.cov)) return(NULL)
  .cov <- mlxtran$MODEL$COVARIATE$DEFINITION
  if (is.null(.cov)) return(NULL)
  .transform <- .cov$transform
  vapply(names(.transform),
         function(n) {
           .t <- .transform[[n]]
           .v <- .t$transform
           .cw <- vapply(seq_along(.t$catLabel),
                         function(i) {
                           .tmp <- suppressWarnings(as.numeric(.t$catValue[[i]]))
                           .q <- ""
                           if (any(is.na(.tmp))) {
                             .q <- "'"
                           }
                           .or <- paste(paste0(.v, " == ", .q, .t$catValue[[i]], .q),
                                        collapse=" || ")
                           .or <- paste0("if (", .or, ") {\n")
                           .or <- paste0(.or, "  ", n, " <- '", .t$catLabel[i], "'\n")
                           .or <- paste0(.or, "}")
                           .or
                         }, character(1), USE.NAMES = TRUE)
           .cw <- paste(.cw, collapse=" else ")
           if (checkmate::testCharacter(.t$reference, min.chars = 1)) {
             .cw <- paste0(.cw," else {\n  ", n, "<- '", .t$reference, "'\n}")
           } else {
             .cw <- c(.cw, paste0("else {\n  ", n, "<- 1\n}"))
           }
           .cw
         }, character(1),
         USE.NAMES=FALSE)
}
