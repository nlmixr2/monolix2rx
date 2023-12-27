#' mlxtran logical operator
#'
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.mlxtranOpIni <- function() {
  .monolix2rx$mlxtranOp <- list()
}
#' Handle logical operator in mlxtran
#'
#' @param id character of the option identifier
#' @param val string value (yes or no) of logical operator
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.mlxtranLogicalOp <- function(id, val) {
  .monolix2rx$mlxtranOp <- c(.monolix2rx$mlxtranOp, setNames(list(val == "yes"), id))
}
#' Handle the numeric values
#'
#' @param id  character of the option identifer
#' @param val string value of the number
#' @noRd
#' @author Matthew L. Fidler
.mlxtranNumOp <- function(id, val) {
  .monolix2rx$mlxtranOp <- c(.monolix2rx$mlxtranOp, setNames(list(as.numeric(val)), id))
}
#' Handle character options
#'
#' @param id mlxtran operator
#' @param val character value
#' @noRd
#' @author Matthew L. Fidler
.mlxtranCharOp <- function(id, val) {
  .monolix2rx$mlxtranOp <- c(.monolix2rx$mlxtranOp, setNames(list(val), id))
}
#' mlxtran options function
#'
#' @param value text to parse
#' @param errText text for the parsing error messages
#' @return monolix2rxOp options list
#' @noRd
#' @author Matthew L. Fidler
.mlxtranOp <- function(value, errText="errTxt") {
  .mlxtranOpIni()
  .Call(`_monolix2rx_trans_mlxtran_op`, value, errText)
  .ret <- .monolix2rx$mlxtranOp
  class(.ret) <- "monolix2rxOp"
  .ret
}

#' @export
print.monolix2rxOp <- function(x, ...) {
  lapply(names(x), function(n) {
    .v <- x[[n]]
    if (is.logical(.v)) {
      cat(n, " = ", ifelse(.v, "yes", "no"), "\n", sep="")
    } else if (is.character(.v)) {
      cat(n, " = '", .v, "'\n", sep="")
    } else {
      cat(n, " = ", .v, "\n", sep="")
    }
  })
  invisible(x)
}

#' @export
as.list.monolix2rxOp <- function(x, ...) {
  .x <- x
  class(.x) <- NULL
  return(.x)
}
