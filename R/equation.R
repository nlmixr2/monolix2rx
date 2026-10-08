#' Parse equation block
#'
#' @param text monolix equation to text to rxode2 code
#' @param pk is the parsed pk
#' @return rode2 code (`$rx`) and odeType (`$odeType`)
#' @noRd
#' @author Matthew L. Fidler
.equation <- function(text, pk=NULL) {
  if (inherits(text, "monolix2rxEquation")) return(text)
  .monolix2rx$stateDepot <- character(0)
  .monolix2rx$stateExtra <- character(0)
  .monolix2rx$equationLine <- character(0)
  .monolix2rx$state <- character(0)
  .monolix2rx$equationLhs <- character(0)
  .monolix2rx$equationRhs <- character(0)
  .monolix2rx$odeType <- "nonStiff"
  .monolix2rx$pk <- NULL
  .start <- .end <- character(0)
  if (is.null(pk)) pk <- .pk("")
  if (is.character(pk)) pk <- .pk(pk)
  if (length(pk$preEq) > 0) {
    .start <- pk$preEq
  }
  if (length(pk$postEq) > 0) {
    .end <- pk$postEq
  }
  # Apparently pk macros can also be in the EQUATION: block
  .pkIni(TRUE)
  # lines before a pkmodel()/macro must stay before its ODEs
  .monolix2rx$pkLong <- TRUE
  on.exit(.monolix2rx$pkLong <- FALSE)
  if (text!="") {
    .Call(`_monolix2rx_trans_equation`, text, "[LONGITUDINAL] EQUATION:")
  }
  .monolix2rx$pkLong <- FALSE
  if (any(grepl("\\bdelay[(]", c(.monolix2rx$preEq, .monolix2rx$equationLine))) &&
        utils::packageVersion("rxode2") < "5.1.7") {
    stop("delay() needs rxode2 >= 5.1.7", call.=FALSE)
  }
  .eqPre <- .monolix2rx$preEq
  .pkPushStatement()
  .validatePkModel(.monolix2rx$pkPars, .monolix2rx$pkCe)
  .pk2 <- list(Cc=.monolix2rx$pkCc,
               Ce=.monolix2rx$pkCe,
               pkmodel=.monolix2rx$pkPars,
               compartment=.monolix2rx$pkCmt,
               peripheral=.monolix2rx$pkPerip,
               effect=.monolix2rx$pkEffect,
               transfer=.monolix2rx$pkTransfer,
               depot=.monolix2rx$pkDepot,
               oral=.monolix2rx$pkOral,
               iv=.monolix2rx$pkIv,
               empty=.monolix2rx$pkEmpty,
               reset=.monolix2rx$pkReset,
               elimination=.monolix2rx$pkElimination,
               admd=.monolix2rx$admd)
  class(.pk2) <- "monolix2rxPk"
  .lhs <- c(pk$Cc, pk$Ce, .monolix2rx$equationLhs, .pk2$Cc, .pk2$Ce)
  .rhs <- .monolix2rx$equationRhs
  .lhs <- .lhs[!is.na(.lhs)]
  .monolix2rx$curLhs <- .lhs
  .monolix2rx$pk <- .pk2rx(pk)
  .admd <- .monolix2rx$pk$admd
  .monolix2rx$pk$admd <- NULL
  .cmt0 <- .monolix2rx$pk$cmt
  .lhs <- c(.lhs, .monolix2rx$pkLhs)
  .monolix2rx$curLhs <- .lhs
  .pk3 <- .pk2rx(.pk2)
  .admd <- rbind(.admd, .pk3$admd)
  .pk3$admd <- NULL
  .cmt3 <- .monolix2rx$pk$cmt
  .env <- new.env(parent=emptyenv())
  .env$extra <- character(0)
  .cmtNum <- vapply(seq_len(max(length(.cmt0), length(.cmt3))),
                    function(i) {
                      if (is.character(.cmt0[[i]]) && is.character(.cmt3[[i]])) {
                        if (.cmt0[[i]] == .cmt3[[i]]) {
                          return(.cmt0[[i]])
                        } else {
                          return(.cmt3[[i]])
                        }
                      }
                      if (is.character(.cmt0[[i]])) return(.cmt0[[i]])
                      if (is.character(.cmt3[[i]])) return(.cmt3[[i]])
                      NA_character_
                    }, character(1), USE.NAMES = FALSE)
  .cmtNum <- c(.cmtNum, .env$extra)
  .lhs <- unique(c(.lhs, .monolix2rx$pkLhs))
  .monolix2rx$extraPred <- character(0)
  .monolix2rx$equationLhs <- character(0)
  lapply(.monolix2rx$endpointPred,
         function(var) {
           if (!(var %in% .lhs) && (var %in% .rhs)) {
             .monolix2rx$equationLhs <- c(.monolix2rx$equationLhs, var)
             .monolix2rx$extraPred <- c(.monolix2rx$extraPred, paste0(var, " <- ", var))
           }
         })
  .lhs <- c(.lhs, .monolix2rx$equationLhs)
  .w <- which(.monolix2rx$endpointPred %in% .lhs)
  if (length(.w) == 1L && length(.monolix2rx$endpointPred) > 1L) {
    .monolix2rx$extraPred <- c(.monolix2rx$extraPred,
                               paste0(.monolix2rx$endpointPred[-.w],
                                      " <- ",
                                      .monolix2rx$endpointPred[.w]))
  }
  .monolix2rx$equationLine <- c(.monolix2rx$equationLine)
  # depot(target=) into a compartment whose ODE a macro wrote
  .tgt <- unique(unlist(lapply(list(.monolix2rx$pk$equation, .pk3$equation), function(e) {
    lapply(e[c("lhsDepot", "dur", "f", "tlag")], names)
  })))
  .tgt <- setdiff(.tgt, .monolix2rx$state)
  # a compartment with no other flows still needs an ODE to receive the depot
  for (.s in intersect(.tgt, c(pk$compartment$amount, .pk2$compartment$amount))) {
    if (length(.whichDdt(c(.monolix2rx$pk$pk, .pk3$pk), .s)) == 0L) {
      .monolix2rx$pk$pk <- c(.monolix2rx$pk$pk, paste0("d/dt(", .s, ") <- 0"))
    }
  }
  for (.p in list(.monolix2rx$pk, .pk3)) {
    .monolix2rx$pk$pk <- .updateDdtEq(.tgt, .monolix2rx$pk$pk, .p)
    .pk3$pk <- .updateDdtEq(.tgt, .pk3$pk, .p)
  }
  .monolix2rx$equationLine <- .updateDdtEq(.monolix2rx$state, .monolix2rx$equationLine, .monolix2rx$pk)
  .monolix2rx$equationLine <- .updateDdtEq(.monolix2rx$state, .monolix2rx$equationLine, .pk3)
  .w <- which(grepl("^ *[<][-] *$", .monolix2rx$equationLine))
  if (length(.w) > 0L) {
    .monolix2rx$equationLine <- .monolix2rx$equationLine[-.w]
  }
  .eqPre <- .updateDdtEq(.monolix2rx$state, .eqPre, .monolix2rx$pk)
  .eqPre <- .updateDdtEq(.monolix2rx$state, .eqPre, .pk3)
  .eqPre <- .eqPre[!grepl("^ *[<][-] *$", .eqPre)]
  .cmtOther <- vapply(c(.monolix2rx$stateExtra, .monolix2rx$state),
                      function(x) {
                        if (x %in% .cmtNum) return(NA_character_)
                        x
                      }, character(1), USE.NAMES = FALSE)
  .cmtPre <- vapply(.monolix2rx$stateDepot,
                    function(x) {
                      if (x %in% .cmtNum) return(NA_character_)
                      x
                    }, character(1), USE.NAMES = FALSE)
  .cmtNum <- c(.cmtPre, .cmtNum, .cmtOther)
  .cmtNum <- .cmtNum[!is.na(.cmtNum)]
  .rx <- .pkMacroRx(c(.start,
                      .monolix2rx$pk$pk,
                      .end,
                      .eqPre,
                      .pk3$pk,
                      .monolix2rx$equationLine,
                      .monolix2rx$extraPred,
                      .monolix2rx$pk$equation$endLines))
  .ret <- list(monolix=text,
               rx=.rx,
               lhs=.monolix2rx$equationLhs,
               odeType=.monolix2rx$odeType,
               admd=.admd,
               cmtPrefix=paste0("cmt(", .cmtNum, ")"))
  class(.ret) <- "monolix2rxEquation"
  .ret
}

#' Translate the Monolix expressions in PK macro lines
#'
#' PK macro arguments (like `depot(p=)`) are copied as written; lines
#' with Monolix-only syntax are translated and the others kept as is.
#'
#' @param lines rxode2 lines from the PK macros
#' @return translated lines
#' @noRd
#' @author Matthew L. Fidler
.pkMacroRx <- function(lines) {
  if (length(lines) == 0L) return(lines)
  if (any(grepl("\\binftDose\\b", lines, perl=TRUE))) {
    stop("'inftDose' Monolix declaration not supported in translation", call.=FALSE)
  }
  .fun <- c(invlogit="expit", norminv="qnorm", normcdf="pnorm", gammaln="lgamma",
            factln="lfactorial")
  .re <- paste0("~=|\\bt\\b|\\bamtDose\\b|\\btDose\\b|\\b(",
                paste(names(.fun), collapse="|"), ")[(]")
  vapply(lines, function(l) {
    .i <- regexpr(" <- ", l, fixed=TRUE)
    if (.i < 0) return(l)
    .rhs <- substring(l, .i + 4)
    if (grepl(.re, .rhs, perl=TRUE)) {
      .rhs <- gsub("~=", "!=", .rhs, fixed=TRUE)
      .rhs <- gsub("\\bt\\b", "time", .rhs, perl=TRUE)
      .rhs <- gsub("\\bamtDose\\b", "dose()", .rhs, perl=TRUE)
      .rhs <- gsub("\\btDose\\b", "tlast", .rhs, perl=TRUE)
      for (.n in names(.fun)) {
        .rhs <- gsub(paste0("\\b", .n, "[(]"), paste0(.fun[[.n]], "("), .rhs, perl=TRUE)
      }
    }
    # a chained power the walker did not write; otherwise keep the text
    if (grepl("\\^.*\\^", .rhs)) {
      .e <- try(str2lang(.rhs), silent=TRUE)
      if (!inherits(.e, "try-error")) {
        .p <- .rxPowParen(.e)
        if (!identical(.p, .e)) .rhs <- deparse1(.p)
      }
    }
    paste0(substr(l, 1, .i - 1), " <- ", .rhs)
  }, character(1), USE.NAMES=FALSE)
}

#' Parenthesize chained powers, which rxode2 does not parse
#'
#' @param e expression
#' @return expression with `a^b^c` as `a^(b^c)`
#' @noRd
#' @author Matthew L. Fidler
.rxPowParen <- function(e) {
  if (!is.call(e)) return(e)
  e <- as.call(lapply(as.list(e), .rxPowParen))
  if (identical(e[[1]], quote(`^`)) && is.call(e[[3]])) {
    .x <- e[[3]]
    # a^b^c and a^-b^c (a signed power)
    if (identical(.x[[1]], quote(`^`)) ||
          (length(.x) == 2L && (identical(.x[[1]], quote(`-`)) || identical(.x[[1]], quote(`+`))) &&
             is.call(.x[[2]]) && identical(.x[[2]][[1]], quote(`^`)))) {
      e[[3]] <- call("(", .x)
    }
  }
  e
}

#' Add an equation line
#'
#' @param line add a line in the current model equation
#' @param ddt being defined
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.equationLine <- function(line, ddt) {
  # depot/dur/f/tlag lines are added afterwards by .updateDdtEq()
  .monolix2rx$equationLine <- c(.monolix2rx$equationLine, line)
}
#' Add to the lhs variables of the equation object
#'
#' @param v value of the equation object
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.equationLhs <- function(v) {
  .monolix2rx$equationLhs <- c(.monolix2rx$equationLhs, v)
}
#' Add state information to the equation object
#'
#' @param v state to add
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.equationState <- function(v) {
  .monolix2rx$state <- c(.monolix2rx$state, v)
}
#' Add to the rhs variables of the equation object
#'
#'
#' @param v value of the equation object
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.equationRhs <- function(v) {
  .monolix2rx$equationRhs <- c(.monolix2rx$equationRhs, v)
}
#' Set the ode type
#'
#' @param odeType ode type that is defined
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.equationOdeType <- function(odeType) {
  .monolix2rx$odeType <- odeType
}
#' @export
as.character.monolix2rxEquation <- function(x, ...) {
  strsplit(x$monolix, "\n")[[1]]
}
#' @export
print.monolix2rxEquation <- function(x, ...) {
  cat(paste(as.character.monolix2rxEquation(x, ...), collapse="\n"), "\n", sep="")
  invisible(x)
}

#' @export
as.list.monolix2rxEquation <- function(x, ...) {
  .x <- x
  class(.x) <- NULL
  .x
}

#' @export
as.character.monolix2rxCovEq <- as.character.monolix2rxEquation

#' @export
print.monolix2rxCovEq <- print.monolix2rxEquation

#' @export
as.list.monolix2rxCovEq <- as.list.monolix2rxEquation

#' Get equation block from a Monolix model txt file
#'
#' @param file string representing the model text file.  Can be
#'   lib:fileName.txt if library setup/available
#'
#' @param retFile boolean that tells `mlxTxt()` to return the file
#'   name instead of error if the file does not exist
#'
#' @param dirn directory the model text file.  default (`NULL`) `file`
#'   (working directory); only needed when the model file
#'   lives in a different directory than the R session.
#'
#' @return parsed equation or file name
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' # First load in the model; in this case the theo model
#' # This is modified from the Monolix demos by saving the model
#' # File as a text file (hence you can access without model library)
#' # setup.
#' #
#' # This example is also included in the monolix2rx package, so
#' # you refer to the location with `system.file()`:
#'
#' pkgTheo <- system.file("theo", package="monolix2rx")
#'
#' mod <- mlxTxt(file.path(pkgTheo, "oral1_1cpt_kaVCl.txt"))
#'
#' mod
mlxTxt <- function(file, retFile=FALSE, dirn=NULL) {
  on.exit({
    .Call(`_monolix2rx_r_parseFree`)
  })
  dirn <- .monolixDirn(dirn)
  if (!retFile) .mlxtranIni()
  .exit <- FALSE
  .monolixLib <- NULL
  if (length(file) > 1L) {
    .lines <- file
    .dirn <- if (is.null(dirn)) .monolixDirn(getwd()) else dirn
  } else {
    if (monolix2rxlixoftConnectors()) {
      if (checkmate::testCharacter(file, min.chars = 5, len=1)) {
        .pre <- substr(file, 1, 4)
      } else {
        .pre <- ""
      }
      if (.pre == "lib:") {
        if (is.na(.monolix2rx$lixoftConnectors)) {
          x <- try(monolix2rxInitializeLixoftConnectors(software = "monolix", force=TRUE), silent=TRUE)
          if (inherits(x, "try-error")) {
            warning("lixoftConnectors cannot be initialized",
                    call.=FALSE)
            .monolix2rx$lixoftConnectors <- FALSE
          } else {
            .monolix2rx$lixoftConnectors <- TRUE
          }
        }
        if (.monolix2rx$lixoftConnectors) {
          .ret <- try(monolix2rxGetLibraryModelContent(file), silent=TRUE)
          if (!inherits(.ret, "try-error")) {
            .monolixLib <- strsplit(as.character(.ret), "\n")[[1]]
          }
        }
      }
    }
    if (!is.null(.monolixLib)) {
      .lines <- .monolixLib
      # a model read from the model library has no directory of its own,
      # so keep the project directory when one was given
      .dirn <- dirn
    } else {
      .f <- .monolixInDirn(.mlxtranLib(file), dirn)
      if (checkmate::testFileExists(.f, "r")) {
        .lines <- suppressWarnings(readLines(.f))
        .dirn <- .monolixDirn(dirname(.f))
      } else {
        .exit <- TRUE
      }
    }
  }
  if (!.exit) {
    .m2 <- c("<MODEL>",
             .lines)
    lapply(.m2, .mlxtranParseItem)
    .mlxEnv$parsedFile <- TRUE
    if (retFile) return(file)
    .ret <- .mlxtranFinalize(.mlxEnv$lst, equation=TRUE, update=FALSE)
    attr(.ret, "dirn") <- .dirn
    return(.ret)
  }
  if (retFile) return(file)
  stop("could not find the model file '", file,
       "'\nif it is in another directory, give that directory with 'dirn='",
       call.=FALSE)
}
