#' Parse equation block
#'
#' @param text monolix equation to text to rxode2 code
#' @param pk is the parsed pk
#' @return rode2 code (`$rx`) and odeType (`$odeType`)
#' @noRd
#' @author Matthew L. Fidler
.equation <- function(text, pk=NULL) {
  if (inherits(text, "monolix2rxEquation")) return(text)
  .monolix2rx$equationLine <- character(0)
  .monolix2rx$state <- character(0)
  .monolix2rx$equationLhs <- character(0)
  .monolix2rx$equationRhs <- character(0)
  .monolix2rx$odeType <- "nonStiff"
  if (is.null(pk)) pk <- .pk("")
  if (is.character(pk)) pk <- .pk(pk)
  # Apparently pk macros can also be in the EQUATION: block
  .pkIni(TRUE)
  if (text!="") {
    .Call(`_monolix2rx_trans_equation`, text, "[LONGITUDINAL] EQUATION:")
  }
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
  .lhs <- c(.lhs, .monolix2rx$pkLhs)
  .monolix2rx$curLhs <- .lhs
  .pk3 <- .pk2rx(.pk2)
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
  .ret <- list(monolix=text,
               rx=c(.monolix2rx$equationLine,
                    .monolix2rx$pk$pk,
                    .pk3$pk,
                    .monolix2rx$extraPred,
                    .monolix2rx$pk$equation$endLines),
               lhs=.monolix2rx$equationLhs,
               odeType=.monolix2rx$odeType)
  class(.ret) <- "monolix2rxEquation"
  .ret
}


#' Add an equation line
#'
#' @param line add a line in the current model equation
#' @param ddt being defined
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.equationLine <- function(line, ddt) {
  if (ddt != "") {
    if (!is.null(.monolix2rx$pk$equation$lhsDepot[[ddt]])) {
      .monolix2rx$equationLine <- c(.monolix2rx$equationLine,
                                    paste0(.monolix2rx$pk$rhsDepot[[ddt]], " <- ", .monolix2rx$pk$lhsDepot[[ddt]]),
                                    paste0(line, .monolix2rx$pk$rhsExtra[[ddt]]))
      if (!is.null(.monolix2rx$pk$equation$fDepot[[ddt]])) {
        .monolix2rx$equationLine <- c(.monolix2rx$equationLine,
                                      .monolix2rx$pk$equation$fDepot[[ddt]])
      }
      if (!is.null(.monolix2rx$pk$equation$tlagDepot[[ddt]])) {
        .monolix2rx$equationLine <- c(.monolix2rx$equationLine,
                                      .monolix2rx$pk$equation$tlagDepot[[ddt]])
      }
    } else {
      .monolix2rx$equationLine <- c(.monolix2rx$equationLine, line)
    }
    if (!is.null(.monolix2rx$pk$equation$dur[[ddt]])) {
      .monolix2rx$equationLine <- c(.monolix2rx$equationLine,
                                    .monolix2rx$pk$equation$dur[[ddt]])
    }
    if (!is.null(.monolix2rx$pk$equation$f[[ddt]])) {
      .monolix2rx$equationLine <- c(.monolix2rx$equationLine,
                                    .monolix2rx$pk$equation$f[[ddt]])
    }
    if (!is.null(.monolix2rx$pk$equation$tlag[[ddt]])) {
      .monolix2rx$equationLine <- c(.monolix2rx$equationLine,
                                    .monolix2rx$pk$equation$tlag[[ddt]])
    }
  } else {
    .monolix2rx$equationLine <- c(.monolix2rx$equationLine, line)
  }
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
#' @examples
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
#' @return parsed equation or file name
#' @export
#' @author Matthew L. Fidler
#' @examples
mlxTxt <- function(file, retFile=FALSE) {
  on.exit({
    .Call(`_monolix2rx_r_parseFree`)
  })
  if (!retFile) .mlxtranIni()
  .exit <- FALSE
  if (length(file) > 1L) {
    .lines <- file
    .dirn <- getwd()
  } else {
    .f <- .mlxtranLib(file)
    if (checkmate::testFileExists(.f, "r")) {
      .lines <- suppressWarnings(readLines(.f))
      .dirn <- dirname(.f)
    } else {
      .exit <- TRUE
    }
  }
  if (!.exit) {
    .m2 <- c("<MODEL>",
             .lines)
    lapply(.m2, .mlxtranParseItem)
    if (retFile) return(file)
    .ret <- .mlxtranFinalize(.mlxEnv$lst, equation=TRUE, update=FALSE)
    attr(.ret, "dirn") <- .dirn
    return(.ret)
  }
  if (retFile) return(file)
  stop("could not find the model file", call.=FALSE)
}
