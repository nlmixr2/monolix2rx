.mlxEnv <- new.env(parent=emptyenv())
#' Initialize the parsing environment
#'
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.mlxtranIni <- function() {
  .mlxEnv$section <- NA_character_
  .mlxEnv$subsection <- NA_character_
  .mlxEnv$subsubsection <- NA_character_
  .mlxEnv$lst <- list(mlxtran="")
  .mlxEnv$isDesc <- FALSE
  .mlxEnv$desc <- ""
}

#' This parses a single line from something like readLines
#'
#' @param l line to pars
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.mlxtranParseItem <- function(l) {
  l <- stringi::stri_trans_general(l, "latin-ascii")
  l <- trimws(l)
  .begin <- 1
  .end <- .nc <- nchar(l)
  if (.nc >= 12) {
    if (substr(l, 1, 12) == "DESCRIPTION:") {
      .mlxEnv$isDesc <- TRUE
      if (.nc >= 14) {
        .mlxEnv$desc <- .mlxtranPasteLine(.mlxEnv$desc, trimws(substr(l, 13, .nc)))
      }
      return(invisible())
    }
  }
  .f <- substr(l, .begin, .begin)
  .e <- substr(l, .end, .end)
  if (.f == "<" && .e == ">") {
    .mlxtranSection(substr(l, .begin + 1, .end - 1))
    return(invisible())
  }
  if (.f == "[" && .e == "]") {
    .mlxtranSubsection(substr(l, .begin + 1, .end - 1))
    return(invisible())
  }
  if (.e == ":") {
    .sec <- substr(l, .begin, .end - 1)
    if (toupper(.sec) == .sec) {
      .mlxtranSubsubsection(.sec)
      return(invisible())
    }
  }
  .mlxtranLine(l)
  return(invisible())
}
.mlxtranFinalize <- function(.ret, equation=FALSE, update=FALSE) {
  if (!is.null(.ret$DATA_FORMATTING)) {
    if (!is.null(.ret$DATA_FORMATTING$FILEINFO$FILEINFO)) {
      .ret$DATA_FORMATTING$FILEINFO$FILEINFO <- .fileinfo(.ret$DATA_FORMATTING$FILEINFO$FILEINFO)
    }
    if (!is.null(.ret$DATA_FORMATTING$CONTENT$CONTENT)) {
      .ret$DATA_FORMATTING$CONTENT$CONTENT <- .content(.ret$DATA_FORMATTING$CONTENT$CONTENT)
    }
  }
  if (!is.null(.ret$PARAMETER)) {
    .ret$PARAMETER$PARAMETER <- .parameter(.ret$PARAMETER$PARAMETER)
  }
  if (!is.null(.ret$FIT)) {
    .ret$FIT$FIT <- .fit(.ret$FIT$FIT)
  }
  if (!is.null(.ret$DATAFILE)) {
    if (!is.null(.ret$DATAFILE$FILEINFO)) {
      .ret$DATAFILE$FILEINFO$FILEINFO <- .fileinfo(.ret$DATAFILE$FILEINFO$FILEINFO)
    }
    if (!is.null(.ret$DATAFILE$CONTENT)) {
      .ret$DATAFILE$CONTENT$CONTENT <- .content(.ret$DATAFILE$CONTENT$CONTENT)
    }
    if (!is.null(.ret$DATAFILE$SETTINGS)) {
      .ret$DATAFILE$SETTINGS$SETTINGS <- .dataSettings(.ret$DATAFILE$SETTINGS$SETTINGS)
    }
  }
  if (!is.null(.ret$MODEL)) {
    if (!is.null(.ret$MODEL$COVARIATE)) {
      if (!is.null(.ret$MODEL$COVARIATE$COVARIATE)) {
        .ret$MODEL$COVARIATE$COVARIATE <- .ind(.ret$MODEL$COVARIATE$COVARIATE)
      }
      if (!is.null(.ret$MODEL$COVARIATE$DEFINITION)) {
        .ret$MODEL$COVARIATE$DEFINITION <- .longDef(.ret$MODEL$COVARIATE$DEFINITION,
                                                    "<MODEL> [COVARIATE] DEFINITION:")
      }
      if (!is.null(.ret$MODEL$COVARIATE$EQUATION)) {
        .ret$MODEL$COVARIATE$EQUATION <- .covEq(.ret$MODEL$COVARIATE$EQUATION)
      }
    }
    if (!is.null(.ret$MODEL$INDIVIDUAL)) {
      .ret$MODEL$INDIVIDUAL$INDIVIDUAL <- .ind(.ret$MODEL$INDIVIDUAL$INDIVIDUAL)
      if (!is.null(.ret$MODEL$INDIVIDUAL$DEFINITION)) {
        .ret$MODEL$INDIVIDUAL$DEFINITION <- .indDef(.ret$MODEL$INDIVIDUAL$DEFINITION)
      }
    }
    if (!is.null(.ret$MODEL$LONGITUDINAL)) {
      .ret$MODEL$LONGITUDINAL$LONGITUDINAL <- .longitudinal(.ret$MODEL$LONGITUDINAL$LONGITUDINAL)
      if (!is.null(.ret$MODEL$LONGITUDINAL$DEFINITION)) {
        .ret$MODEL$LONGITUDINAL$DEFINITION <- .longDef(.ret$MODEL$LONGITUDINAL$DEFINITION)
      }
      if (!is.null(.ret$MODEL$LONGITUDINAL$PK)) {
        .ret$MODEL$LONGITUDINAL$PK <- .pk(.ret$MODEL$LONGITUDINAL$PK)
      }
      if (equation && !is.null(.ret$MODEL$LONGITUDINAL$EQUATION)) {
        .ret$MODEL$LONGITUDINAL$EQUATION <- .equation(.ret$MODEL$LONGITUDINAL$EQUATION,
                                                      .ret$MODEL$LONGITUDINAL$PK)
      }
      if (!is.null(.ret$MODEL$LONGITUDINAL$OUTPUT)) {
        .ret$MODEL$LONGITUDINAL$OUTPUT <- .longOut(.ret$MODEL$LONGITUDINAL$OUTPUT)
      }
    }
    if (!is.null(.ret$MODEL$POPULATION)) {
      if (!is.null(.ret$MODEL$POPULATION$DEFINITION)) {
        .ret$MODEL$POPULATION$DEFINITION <- .popDef(.ret$MODEL$POPULATION$DEFINITION)
      }
    }
  }
  if (!is.null(.ret$MONOLIX)) {
    if (!is.null(.ret$MONOLIX$SETTINGS)) {
      if (!is.null(.ret$MONOLIX$SETTINGS$GLOBAL)) {
        .ret$MONOLIX$SETTINGS$GLOBAL <- .mlxtranOp(.ret$MONOLIX$SETTINGS$GLOBAL, "<MONOLIX> [SETTINGS] GLOBAL:")
      }
      if (!is.null(.ret$MONOLIX$SETTINGS$POPULATION)) {
        .ret$MONOLIX$SETTINGS$POPULATION <- .mlxtranOp(.ret$MONOLIX$SETTINGS$POPULATION, "<MONOLIX> [SETTINGS] POPULATION:")
      }
      if (!is.null(.ret$MONOLIX$SETTINGS$LL)) {
        .ret$MONOLIX$SETTINGS$LL <- .mlxtranOp(.ret$MONOLIX$SETTINGS$LL, "<MONOLIX> [SETTINGS] LL:")
      }
      if (!is.null(.ret$MONOLIX$SETTINGS$INDIVIDUAL)) {
        .ret$MONOLIX$SETTINGS$INDIVIDUAL <- .mlxtranOp(.ret$MONOLIX$SETTINGS$INDIVIDUAL, "<MONOLIX> [SETTINGS] INDIVIDUAL:")
      }
    }
    if (!is.null(.ret$MONOLIX$TASKS$TASKS)) {
      .ret$MONOLIX$TASKS$TASKS <- .task(.ret$MONOLIX$TASKS$TASKS)
    }
  }

  if (update && !is.null(.ret$PARAMETER)) {
    .ret <- .parameterUpdate(.ret)
  }
  .ret <- .mlxtranCov(.ret)
  attr(.ret, "desc") <- .mlxEnv$desc
  class(.ret) <- "monolix2rxMlxtran"
  .ret
}
#' This applies mlxtran to a set of lines
#'
#' @param lines a character vector representing a set of lines to parse
#' @param equation when TRUE, try to parse equation too
#' @param update when TRUE, try to update the initial estimates to the final estimates
#' @return a mlxtran object
#' @noRd
#' @author Matthew L. Fidler
.mlxtran <- function(lines, equation=FALSE,
                     update=FALSE) {
  .mlxtranIni()
  lapply(lines, .mlxtranParseItem)
  # Add file entries
  if (!is.null(.mlxEnv$lst$MODEL$LONGITUDINAL)) {
    .long <- .longitudinal(.mlxEnv$lst$MODEL$LONGITUDINAL$LONGITUDINAL)
    .file <- .long$file
    mlxTxt(.long$file,  retFile=TRUE)
  }
  .mlxtranFinalize(.mlxEnv$lst, equation=equation, update=update)
}
#' Paste together lines, ignoring empty ones and adding \n between substantial lines
#'
#' @param prior prior line
#' @param new new line
#' @return prior \n new (or prior / new depending on the simplicity of the lines)
#' @noRd
#' @author Matthew L. Fidler
.mlxtranPasteLine <- function(prior, new) {
  if (prior == "") return(new)
  if (new == "") return(prior)
  paste0(prior, "\n", new)
}
#' This handles a non-section line for a mlxtran file
#'
#' @param line line to put in the right place in the lst
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.mlxtranLine <- function(line) {
  if (.mlxEnv$isDesc) {
    .mlxEnv$desc <- .mlxtranPasteLine(.mlxEnv$desc, line)
    return(invisible())
  }
  .s <- .mlxEnv$section
  .ss <- .mlxEnv$subsection
  .sss <- .mlxEnv$subsubsection
  if (is.na(.s)) {
    .mlxEnv$lst$mlxtran <- paste0(.mlxEnv$lst$mlxtran, "\n", line)
    return(invisible())
  }
  if (is.null(.mlxEnv$lst[[.s]])) {
    .mlxEnv$lst[[.s]] <- list()
    .mlxEnv$lst[[.s]][[.s]] <- ""
  }
  if (is.na(.ss)) {
    .mlxEnv$lst[[.s]][[.s]] <- .mlxtranPasteLine(.mlxEnv$lst[[.s]][[.s]], line)
    return(invisible())
  }
  if (is.null(.mlxEnv$lst[[.s]][[.ss]])) {
    .mlxEnv$lst[[.s]][[.ss]] <- list()
    .mlxEnv$lst[[.s]][[.ss]][[.ss]] <- ""
  }
  if (is.na(.sss)) {
    .mlxEnv$lst[[.s]][[.ss]][[.ss]] <- .mlxtranPasteLine(.mlxEnv$lst[[.s]][[.ss]][[.ss]], line)
    return(invisible())
  }
  if (is.null(.mlxEnv$lst[[.s]][[.ss]][[.sss]])) {
    .mlxEnv$lst[[.s]][[.ss]][[.sss]] <- ""
  }
  .mlxEnv$lst[[.s]][[.ss]][[.sss]] <- .mlxtranPasteLine(.mlxEnv$lst[[.s]][[.ss]][[.sss]], line)
  return(invisible())
}
#' This handles the section text when it encounters it
#'
#' @param sec section text
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.mlxtranSection <- function(sec) {
  .mlxEnv$section <- sec
  .mlxEnv$subsection <- NA_character_
  .mlxEnv$subsubsection <- NA_character_
  .mlxEnv$isDesc <- FALSE
}
#' This handles the subsection text when it encounters it
#'
#' @param sec subsection text
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.mlxtranSubsection <- function(sec) {
  .mlxEnv$subsection <- sec
  .mlxEnv$subsubsection <- NA_character_
  .mlxEnv$isDesc <- FALSE
}
#' This handles the sub-subsection text
#'
#' @param sec sub-subsection text
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.mlxtranSubsubsection <- function(sec) {
  if (sec == "DESCRIPTION") {
    .mlxEnv$isDesc <- TRUE
    return(invisible())
  }
  .mlxEnv$subsubsection <- sec
  .mlxEnv$isDesc <- FALSE
}
#' Read and parse mlxtran lines
#'
#' @param file mlxtran file to process
#' @param equation parse the equation block to rxode2 (some models cannot be translated)
#' @param update when true, try to update the parameter block to the final parameter estimates
#' @return mlxtran object
#' @export
#' @author Matthew L. Fidler
#' @examples
mlxtran <- function(file, equation=FALSE, update=FALSE) {
  checkmate::assertLogical(equation, any.missing=FALSE, len=1)
  checkmate::assertLogical(update, any.missing=FALSE, len=1)
  if (inherits(file, "monolix2rxMlxtran")) {
    if (equation && !is.null(file$MODEL$LONGITUDINAL$EQUATION)) {
      file$MODEL$LONGITUDINAL$EQUATION <- .equation(file$MODEL$LONGITUDINAL$EQUATION,
                                                    file$MODEL$LONGITUDINAL$PK)
    }
    if (update && !is.null(file$PARAMETER)) {
      file <- .parameterUpdate(file)
      file <- .mlxtranCov(file)
    }
    return(file)
  }
  if (length(file) > 1L) {
    .lines <- file
    .dirn <- getwd()
  } else {
    checkmate::assertFileExists(file)
    .lines <- suppressWarnings(readLines(file))
    .dirn <- dirname(file)
  }
  withr::with_dir(.dirn,
                  .mlxtran(.lines, equation=equation))
}

#' @export
as.character.monolix2rxMlxtran <- function(x, ...) {
  .env <- new.env(parent=emptyenv())
  .env$catText <- FALSE
  .desc <- attr(x, "desc")
  .ret <- character(0)
  if (.desc != "") {
    .ret <- c(.ret, "DESCRIPTION:")
    .ret <- c(.ret, .desc, "")
  }
  .env$ret <- .ret
  lapply(names(x), function(ns) {
    if (ns != "mlxtran") {
      .env$ret <- c(.env$ret,
                    ifelse(.env$catText, "", character(0)),
                    paste0("<", ns, ">"))
      .env$catText <- FALSE
    }
    .sec <- x[[ns]]
    lapply(names(.sec), function(nss) {
      if (ns != nss) {
        .env$ret <- c(.env$ret,
                      ifelse(.env$catText, "", character(0)),
                      paste0("[", nss, "]"))
        .env$catText <- FALSE
      }
      .subsec <- .sec[[nss]]
      .cls <- class(.subsec)
      if (length(.cls) == 1L) {
        if (.cls == "character") {
          if (.subsec == "") return(invisible())
          .env$catText <- TRUE
          .ret <- c(.ret,
                    .subsec)
          return(invisible())
        } else if (.cls == "list") {
          lapply(names(.subsec), function(nsss){
            .subsubsec <- .subsec[[nsss]]
            if (nss != nsss) {
              .env$ret <- c(.env$ret,
                            ifelse(.env$catText, "", character(0)),
                            paste0(nsss, ":"))
              .env$catText <- FALSE
            }
            .cls <- class(.subsubsec)
            if (length(.cls) == 1L) {
              if (.cls == "character") {
                if (.subsubsec == "") return(invisible())
                .env$catText <- TRUE
                .env$ret <- c(.env$ret,
                              ifelse(.env$catText, "", character(0)),
                              .subsubsec)
                return(invisible())
              }
            }
            .env$catText <- TRUE
            .env$ret <- c(.env$ret,
                          paste0('; parsed: $', ns, "$", nss,"$", nsss),
                          as.character(.subsubsec))
          })
          return(invisible())
        }
      }
      .env$catText <- TRUE
      .env$ret <- c(.env$ret,
                    paste0('; parsed: $', ns, "$", nss),
                    as.character(.subsec))
    })
  })
  .up <- .unparsedMlxtran(x, ...)
  if (length(.up) > 0) .up <- c("", "; unparsed sections:", paste0(";  $", .up))
  c(.env$ret[!is.na(.env$ret)], .up)
}

.unparsedMlxtran <- function(x, ...) {
  .env <- new.env(parent=emptyenv())
  .desc <- attr(x, "desc")
  .ret <- character(0)
  .env$ret <- .ret
  lapply(names(x), function(ns) {
    .sec <- x[[ns]]
    lapply(names(.sec), function(nss) {
      .subsec <- .sec[[nss]]
      .cls <- class(.subsec)
      if (length(.cls) == 1L) {
        if (.cls == "character") {
          if (.subsec == "") return(invisible())
          .env$ret <- c(.env$ret,
                        paste0(ns, "$", nss))
          return(invisible())
        } else if (inherits(.subsec, "data.frame")) {
        } else if (.cls == "list") {
          lapply(names(.subsec), function(nsss){
            .subsubsec <- .subsec[[nsss]]
            .cls <- class(.subsubsec)
            if (length(.cls) == 1L) {
              if (.cls == "character") {
                if (.subsubsec == "") return(invisible())
                .env$ret <- c(.env$ret,
                              paste0(ns, "$", nss,"$", nsss))
                return(invisible())
              }
            }
          })
          return(invisible())
        }
      }
      if (length(.cls) == 1L) {
        .env$ret <- c(.env$ret,
                      paste0(ns, "$", nss))

      }
    })
  })
  .env$ret
}


#' @export
print.monolix2rxMlxtran <- function(x, ...) {
  cat(paste(as.character.monolix2rxMlxtran(x, ...), collapse="\n"), "\n", sep="")
  invisible(x)
}

#' @export
as.list.monolix2rxMlxtran <- function(x, ...) {
  .n <- names(x)
  .x <- setNames(lapply(.n,
               function(n) {
                 .y <- x[[n]]
                 if (is.list(.y)) {
                   .n2 <- names(.y)
                   .y <- setNames(
                     lapply(.n2,
                            function(n2) {
                              .z <- .y[[n2]]
                              if (inherits(.z, "data.frame")) {
                                return(as.data.frame(.z))
                              } else {
                                .z <- as.list(.z)
                                .n3 <- names(.z)
                                .z <- setNames(
                                  lapply(.n3,
                                         function(n3) {
                                           .w <- .z[[n3]]
                                           if (inherits(.w, "data.frame")) {
                                             return(as.data.frame(.w))
                                           }
                                           return(as.list(.w))
                                         }),
                                  .n3
                                )
                                return(.z)
                              }
                            }), .n2)
                 }
                 .y
               }), .n)
  class(.x) <- NULL
  .x
}
