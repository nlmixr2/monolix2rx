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
  .begin <- 1
  .end <- .nc <- nchar(l)

  .f <- substr(l, .begin, .begin)
  while (.f %in% c(" ", "\t")) {
    .begin <- .begin + 1
    if (.begin > .nc) {
      .mlxtranLine(l)
      return(invisible())
    }
    .f <- substr(l, .begin, .begin)
  }
  .e <- substr(l, .end, .end)
  while (.e %in% c(" ", "\t")) {
    .end <- .end - 1
    if (.end < 1) {
      .mlxtranLine(l)
      return(invisible())
    }
    .e <- substr(l, .begin, .begin)
  }
  if (.f == "<" && .e == ">") {
    .mlxtranSection(substr(l, .begin + 1, .end - 1))
    return(invisible())
  }
  if (.f == "[" && .e == "]") {
    .mlxtranSubsection(substr(l, .begin + 1, .end - 1))
    return(invisible())
  }
  if (.e == ":") {
    .mlxtranSubsubsection(substr(l, .begin, .end - 1))
    return(invisible())
  }
  .mlxtranLine(l)
  return(invisible())
}
#' This applies mlxtran to a set of lines
#'
#' @param lines a character vector representing a set of lines to parse
#' @param equation when TRUE, try to parse equation too
#' @return a mlxtran object
#' @noRd
#' @author Matthew L. Fidler
.mlxtran <- function(lines, equation=FALSE) {
  .mlxtranIni()
  lapply(lines, .mlxtranParseItem)
  # Add file entries
  if (!is.null(.mlxEnv$lst$MODEL$LONGITUDINAL)) {
    .long <- .longitudinal(.mlxEnv$lst$MODEL$LONGITUDINAL$LONGITUDINAL)
    .file <- .long$file
    if (file.exists(.file)) {
      .m2 <- c("<MODEL>",
               suppressWarnings(readLines(.file)))
      lapply(.m2, .mlxtranParseItem)
    }
  }
  .ret <- .mlxEnv$lst
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
  }
  if (!is.null(.ret$MODEL)) {
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
        .ret$MODEL$LONGITUDINAL$EQUATION <- .equation(.ret$MODEL$LONGITUDINAL$EQUATION)
      }
      if (!is.null(.ret$MODEL$LONGITUDINAL$OUTPUT)) {
        .ret$MODEL$LONGITUDINAL$OUTPUT <- .longOut(.ret$MODEL$LONGITUDINAL$OUTPUT)
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
    }
    if (!is.null(.ret$MONOLIX$TASKS$TASKS)) {
      .ret$MONOLIX$TASKS$TASKS <- .task(.ret$MONOLIX$TASKS$TASKS)
    }
  }
  attr(.ret, "desc") <- .mlxEnv$desc
  class(.ret) <- "monolix2rxMlxtran"
  .ret
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
#' @return mlxtran object
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
mlxtran <- function(file, equation=FALSE) {
  checkmate::assertLogical(equation, any.missing=FALSE, len=1)
  if (inherits(file, "monolix2rxMlxtran")) {
    if (equation && !is.null(file$MODEL$LONGITUDINAL$EQUATION)) {
      file$MODEL$LONGITUDINAL$EQUATION <- .equation(file$MODEL$LONGITUDINAL$EQUATION)
    }
    return(file)
  }
  if (length(file) > 1L) {
    .lines <- file
    .dirn <- getwd()
  } else {
    checkmate::checkFileExists(file)
    .lines <- suppressWarnings(readLines(file))
    .dirn <- dirname(file)
  }
  withr::with_dir(.dirn,
                  .mlxtran(.lines, equation=equation))
}

#' @export
print.monolix2rxMlxtran <- function(x, ...) {
  .env <- new.env(parent=emptyenv())
  .env$catText <- FALSE
  .desc <- attr(x, "desc")
  if (.desc != "") {
    cat("DESCRIPTION:\n")
    cat(.desc, "\n", sep="")
    cat("\n")
  }
  lapply(names(x), function(ns) {
    if (ns != "mlxtran") {
      cat(ifelse(.env$catText, "\n", ""), "<", ns, ">\n", sep="")
      .env$catText <- FALSE
    }
    .sec <- x[[ns]]
    lapply(names(.sec), function(nss) {
      if (ns != nss) {
        cat(ifelse(.env$catText, "\n", ""), "[", nss, "]\n", sep="")
        .env$catText <- FALSE
      }
      .subsec <- .sec[[nss]]
      .cls <- class(.subsec)
      if (length(.cls) == 1L) {
        if (.cls == "character") {
          if (.subsec == "") return(invisible())
          .env$catText <- TRUE
          cat(.subsec, "\n", sep="")
          return(invisible())
        } else if (.cls == "list") {
          lapply(names(.subsec), function(nsss){
            .subsubsec <- .subsec[[nsss]]
            if (nss != nsss) {
              cat(ifelse(.env$catText, "\n", ""), nsss, ":\n", sep="")
              .env$catText <- FALSE
            }
            .cls <- class(.subsubsec)
            if (length(.cls) == 1L) {
              if (.cls == "character") {
                if (.subsubsec == "") return(invisible())
                .env$catText <- TRUE
                cat(.subsubsec, "\n", sep="")
                return(invisible())
              }
            }
            .env$catText <- TRUE
            cat('; parsed: $', ns, "$", nss,"$", nsss, "\n", sep="")
            print(.subsubsec)
          })
          return(invisible())
        }
      }
      .env$catText <- TRUE
      cat('; parsed: $', ns, "$", nss, "\n", sep="")
      print(.subsec)
    })
  })
  cat("\n")
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
