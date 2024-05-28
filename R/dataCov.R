mlxtranGetMutate <- function(mlxtran) {
  .cov <- mlxtran$MODEL$COVARIATE$COVARIATE
  .mutate <- character(0)
  # Convert all covariates to factors
  if (!is.null(.cov)) {
    .cat <- .cov$cat
    .mutate <- paste0("dplyr::mutate(", paste(vapply(names(.cat), function(x) {
      paste0(x, "=factor(", x, ", labels=", deparse1(.cat[[x]]$cat), ")")
    }, character(1), USE.NAMES=TRUE), collapse=", "), ")")
  }
  # Add equations

}
