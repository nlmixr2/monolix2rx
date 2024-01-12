#' mlxtran logical operator
#'
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.mlxtranOpIni <- function(full=TRUE) {
  if (full) {
    .monolix2rx$mlxtranOp <- list()
  } else {
    if (length(.monolix2rx$mlxtranVal) > 0) {
      .monolix2rx$mlxtranOp[[.monolix2rx$mlxtranName]] <-
        list(val=.monolix2rx$mlxtranVal,
             valQ=.monolix2rx$mlxtranValQ)
    }
    .monolix2rx$mlxtranName <- NA_character_
    .monolix2rx$mlxtranVal <- character(0)
    .monolix2rx$mlxtranValQ <- logical(0)
  }
}

#' Handle logical operator in mlxtran
#'
#' @param id character of the option identifier
#' @param val string value (yes or no) of logical operator
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.mlxtranLogicalOp <- function(id, val) {
  .mlxtranOpIni(FALSE)
  .monolix2rx$mlxtranOp <- c(.monolix2rx$mlxtranOp, setNames(list(val == "yes"), id))
}
#' Handle the numeric values
#'
#' @param id  character of the option identifer
#' @param val string value of the number
#' @noRd
#' @author Matthew L. Fidler
.mlxtranNumOp <- function(id, val) {
  .mlxtranOpIni(FALSE)
  .monolix2rx$mlxtranOp <- c(.monolix2rx$mlxtranOp, setNames(list(as.numeric(val)), id))
}
#' Handle character options
#'
#' @param id mlxtran operator
#' @param val character value
#' @noRd
#' @author Matthew L. Fidler
.mlxtranCharOp <- function(id, val) {
  .mlxtranOpIni(FALSE)
  .monolix2rx$mlxtranOp <- c(.monolix2rx$mlxtranOp, setNames(list(val), id))
}
#' Add a list variable name
#'
#' @param var variable name for list
#' @return  nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.mlxtranListOp <- function(var) {
  .mlxtranOpIni(FALSE)
  .monolix2rx$mlxtranName <- var
}
#' Add value to mlxtran option list
#'
#' @param var variable value to add
#' @param q is this variable quoted (integes)
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.mlxtranListVal <- function(var, q) {
  .monolix2rx$mlxtranVal <- c(.monolix2rx$mlxtranVal, var)
  .monolix2rx$mlxtranValQ <- c(.monolix2rx$mlxtranValQ, as.logical(q))
}
#' mlxtran options function
#'
#' @param value text to parse
#' @param errText text for the parsing error messages
#' @return monolix2rxOp options list
#' @noRd
#' @author Matthew L. Fidler
.mlxtranOp <- function(value, errText="errTxt") {
  .mlxtranOpIni(TRUE)
  .Call(`_monolix2rx_trans_mlxtran_op`, value, errText)
  .mlxtranOpIni(FALSE)
  .ret <- .monolix2rx$mlxtranOp
  class(.ret) <- "monolix2rxOp"
  .ret
}

#' @export
as.character.monolix2rxOp <- function(x, ...) {
  vapply(names(x), function(n) {
    .v <- x[[n]]
    if (is.logical(.v)) {
      paste0(n, " = ", ifelse(.v, "yes", "no"))
    } else if (is.character(.v)) {
      paste0(n, " = '", .v, "'")
    } else if (is.list(.v)) {
      .asCharacterSingleOrList(n, .v$val, .v$valQ, comma="", eq=" = ")
    } else {
      paste0(n, " = ", .v)
    }
  }, character(1), USE.NAMES = FALSE)
}

#' @export
print.monolix2rxOp <- function(x, ...) {
  cat(paste(as.character.monolix2rxOp(x, ...), collapse="\n"), "\n", sep="")
  invisible(x)
}

#' @export
as.list.monolix2rxOp <- function(x, ...) {
  .x <- x
  class(.x) <- NULL
  return(.x)
}
