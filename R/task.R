.taskIni <- function() {
  .monolix2rx$funs <- list()
  .monolix2rx$funName <- NA_character_
  .monolix2rx$funArgs <- list()
  .monolix2rx$funArgCur <- character(0)
}

.task <- function(text) {
  .taskIni()
  .Call(`_monolix2rx_trans_mlxtrantask`, text)
  .taskFun(NA_character_)
  .ret <- .monolix2rx$funs
  class(.ret) <- "monolix2rxTask"
  .ret
}

.taskFun <- function(funName) {
  if (!is.na(.monolix2rx$funName)) {
    .ret <- list(.monolix2rx$funArgs)
    names(.ret) <- .monolix2rx$funName
    .monolix2rx$funs <- c(.monolix2rx$funs, .ret)
  }
  .monolix2rx$funName <- funName
}

.taskArgList <- function(argName) {
  .monolix2rx$funArgCur <- argName
  .monolix2rx$funArgs[[.monolix2rx$funArgCur]] <- list()
}

.taskArgChar <- function(argName) {
  .monolix2rx$funArgCur <- argName
  .monolix2rx$funArgs[[.monolix2rx$funArgCur]] <- character(0)
}

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
