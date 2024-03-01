#' Initialize task object
#'
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.taskIni <- function() {
  .monolix2rx$funs <- list()
  .monolix2rx$funName <- NA_character_
  .monolix2rx$funArgs <- list()
  .monolix2rx$funArgCur <- character(0)
}
#' Parse task text
#'
#' @param text character string of task to be parsed
#' @return monolix2rxTask object
#' @noRd
#' @author Matthew L. Fidler
.task <- function(text) {
  .taskIni()
  .Call(`_monolix2rx_trans_mlxtrantask`, text)
  .taskFun(NA_character_)
  .ret <- .monolix2rx$funs
  class(.ret) <- "monolix2rxTask"
  .ret
}
#' Parse the function name
#'
#' @param funName parse the function names
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.taskFun <- function(funName) {
  if (!is.na(.monolix2rx$funName)) {
    .ret <- list(.monolix2rx$funArgs)
    names(.ret) <- .monolix2rx$funName
    .monolix2rx$funs <- c(.monolix2rx$funs, .ret)
  }
  .monolix2rx$funName <- funName
}
#' Handle task argument task list
#'
#' @param argName argument name
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.taskArgList <- function(argName) {
  .monolix2rx$funArgCur <- argName
  .monolix2rx$funArgs[[.monolix2rx$funArgCur]] <- list()
}
#' Add task argument character
#'
#' @param argName Add argument name
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.taskArgChar <- function(argName) {
  .monolix2rx$funArgCur <- argName
  .monolix2rx$funArgs[[.monolix2rx$funArgCur]] <- character(0)
}
#' Add task argument value
#'
#' @param argVal add argument value
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.taskVal <- function(argVal) {
  if (is.list(.monolix2rx$funArgs[[.monolix2rx$funArgCur]])) {
    .monolix2rx$funArgs[[.monolix2rx$funArgCur]] <- c(.monolix2rx$funArgs[[.monolix2rx$funArgCur]],
                                                      list(argVal))
  } else {
    .monolix2rx$funArgs[[.monolix2rx$funArgCur]] <- c(.monolix2rx$funArgs[[.monolix2rx$funArgCur]],
                                                      argVal)
  }
}

#' @export
as.list.monolix2rxTask <- function(x, ...) {
  .x <- x
  class(.x) <- NULL
  .x
}
#' @export
as.character.monolix2rxTask <- function(x, ...) {
  .n <- names(x)
  vapply(.n, function(n) {
    .ret <- paste0(n, "(", sep="")
    .cur <- x[[n]]
    .n2 <- names(.cur)
    .ret <- paste0(.ret, paste(vapply(.n2, function(n2) {
      .c <- .cur[[n2]]
      if (is.list(.c)) {
        return(paste0(n2, " = {", paste(unlist(.c), collapse=", "), "}"))
      }
      paste0(n2, " = ", .c)
    }, character(1), USE.NAMES = FALSE), collapse=", "))
    paste0(.ret, ")")
  }, character(1), USE.NAMES=FALSE)
}
#' @export
print.monolix2rxTask <- function(x, ...) {
  cat(paste(as.character.monolix2rxTask(x, ...), collapse="\n"), "\n", sep="")
  invisible(x)
}
