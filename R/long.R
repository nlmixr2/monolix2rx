#' Longitudinal parsing
#'
#' @param text parsing text
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.longitudinal <- function(text) {
  .ret <- .ind(text, "[LONGITUDINAL]")
  class(.ret) <- "monolix2rxLongitudinal"
  .ret
}

#' @export
print.monolix2rxLongitudinal <- function(x, ...) {
  print.monolix2rxInd(x, ...)
}

#' @export
as.list.monolix2rxLongitudinal <- function(x, ...) {
  .x <- x
  class(.x) <- NULL
  .x
}
