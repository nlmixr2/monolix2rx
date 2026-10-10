#' Get the string of the mutate statement for input dataset based on
#' mlxtran
#'
#' @param mlxtran input mlxtran file
#' @return mlxtran string that can be applied to a model (by evaluating it)
#' @noRd
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
  if (length(.cov$cat) > 0L) {
    .cat <- .cov$cat
    .mutate <- paste0("\tdplyr::mutate(", paste(vapply(names(.cat), function(x) {
      paste0(x, "=factor(as.character(", x, "), labels=", deparse1(.cat[[x]]$cat), ")")
    }, character(1), USE.NAMES=TRUE), collapse=", "), ")")
  }
  .cov <- mlxtran$MODEL$COVARIATE$EQUATION
  if (!is.null(.cov)) {
    # min()/max() of a Monolix covariate are per row
    .dplyr <- vapply(.cov$dplyr, function(l) {
      .e <- try(str2lang(sub("=", "<-", l, fixed=TRUE)), silent=TRUE)
      if (inherits(.e, "try-error")) return(l)
      sub(" <- ", " = ", deparse1(.mutatePminmax(.e)), fixed=TRUE)
    }, character(1), USE.NAMES=FALSE)
    .mutate <- c(.mutate,
                 paste0("dplyr::mutate(",
                        paste(.dplyr, collapse=",\n\t\t"),
                       ")"))
  }
  if (length(.mutate) == 0) return(NULL)
  # Add equations
  paste(.mutate, collapse=" |> \n\t")
}

#' Use pmin()/pmax() for min()/max() in a dplyr mutate
#'
#' @param x expression
#' @return expression with min/max calls changed
#' @noRd
#' @author Matthew L. Fidler
.mutatePminmax <- function(x) {
  if (!is.call(x)) return(x)
  if (identical(x[[1]], quote(min))) x[[1]] <- quote(pmin)
  if (identical(x[[1]], quote(max))) x[[1]] <- quote(pmax)
  as.call(c(x[[1]], lapply(as.list(x)[-1], .mutatePminmax)))
}
