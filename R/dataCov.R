#' Get the string of the mutate statement for input dataset based on
#' mlxtran
#'
#' @param mlxtran input mlxtran file
#' @return mlxtran string that can be applied to a model (by evaluating it)
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' covD <- system.file("cov", package="monolix2rx")
#' m <- mlxtran(file.path(covD, "phenobarbital_project.mlxtran"))
#' message(mlxtranGetMutate(m))
mlxtranGetMutate <- function(mlxtran) {
  .cov <- mlxtran$MODEL$COVARIATE$COVARIATE
  .mutate <- character(0)
  # Convert all covariates to factors
  if (!is.null(.cov)) {
    .cat <- .cov$cat
    .mutate <- paste0("\tdplyr::mutate(", paste(vapply(names(.cat), function(x) {
      paste0(x, "=factor(as.character(", x, "), labels=", deparse1(.cat[[x]]$cat), ")")
    }, character(1), USE.NAMES=TRUE), collapse=", "), ")")
  }
  .cov <- mlxtran$MODEL$COVARIATE$DEFINITION
  if (!is.null(.cov)) {
    .transform <- .cov$transform
    .mutate <- c(.mutate,
                 vapply(names(.transform),
                        function(n) {
                          .t <- .transform[[n]]
                          .v <- .t$transform
                          .cw <- vapply(seq_along(.t$catLabel),
                                        function(i) {
                                          paste0(.v, " %in% c(",
                                                 paste(paste0("'", .t$catValue[[i]], "'"),
                                                       collapse=", "),
                                                 ") ~ '", .t$catLabel[i], "'")
                                        }, character(1), USE.NAMES = TRUE)
                          if (checkmate::testCharacter(.t$reference, min.chars = 1)) {
                            .cw <- c(.cw, paste0("TRUE ~ '", .t$reference, "'"))
                          } else {
                            .cw <- c(.cw, paste0("TRUE ~ NA_character_"))
                          }
                          paste0("dplyr::mutate(", n, "= dplyr::case_when(",
                                paste(.cw, collapse=",\n\t\t"),
                                 "))")
                        }, character(1),
                        USE.NAMES=FALSE))
  }
  .cov <- mlxtran$MODEL$COVARIATE$EQUATION
  if (!is.null(.cov)) {
    .mutate <- c(.mutate,
                 paste0("dplyr::mutate(",
                        paste(.cov$dplyr, collapse=",\n\t\t"),
                       ")"))
  }
  if (length(.mutate) == 0) return(NULL)
  # Add equations
  paste(.mutate, collapse=" |> \n\t")
}
