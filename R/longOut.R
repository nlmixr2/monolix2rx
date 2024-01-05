#' longitudinal output ini
#'
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.longIni <- function() {
  .monolix2rx$output <- character(0)
  .monolix2rx$table <- character(0)
}
#' longitudinal output parsing
#'
#' @param text longitudinal out text
#' @return  monolix2rxLongOut object with $output and $table entries
#' @noRd
#' @author Matthew L. Fidler
.longOut <- function(text) {
  .longIni()
  .Call(`_monolix2rx_trans_longoutput`, text)
  .ret <- list(output=.monolix2rx$output,
               table=.monolix2rx$table)
  class(.ret) <- "monolix2rxLongOut"
  .ret
}
#' Add longitudinal output item
#'
#' @param item item to add to the output list
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.longOutItem <- function(item) {
  .monolix2rx$output <- c(.monolix2rx$output, item)
}
#' Add table item to longitudinal item
#'
#' @param item longitudinal table item
#' @return  nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.longTableItem <- function(item) {
  .monolix2rx$table <- c(.monolix2rx$table, item)
}

#' @export
as.list.monolix2rxLongOut <- function(x, ...) {
  .x <- x
  class(.x) <- NULL
  .x
}

#' @export
as.character.monolix2rxLongOut <- function(x, ...) {
  .ret <- character(0)
  if (length(x$output) == 1L) {
    .ret <- c(.ret, paste0("output = ", x$output))
  }
  if (length(x$output) > 1L) {
    .ret <- c(.ret, paste0("output = {", paste(x$output, collapse=", "), "}"))
  }
  if (length(x$table) > 1L) {
    .ret <- c(.ret, paste0("table = {", paste(x$table, collapse=", "), "}"))
  }
  if (length(x$table) == 1L) {
    .ret <- c(.ret, paste0("table = ", x$table))
  }
  .ret
}

#' @export
print.monolix2rxLongOut <- function(x, ...) {
  cat(paste(as.character.monolix2rxLongOut(x, ...), collapse="\n"), "\n", sep="")
  return(invisible(x))
}
