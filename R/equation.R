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
  .monolix2rx$odeType <- "nonStiff"
  if (is.null(pk)) pk <- .pk("")
  if (is.character(pk)) pk <- .pk(pk)
  .monolix2rx$pk <- .pk2rx(pk)
  # Apparently pk macros can also be in the EQUATION: block
  .pkIni(TRUE)
  .Call(`_monolix2rx_trans_equation`, text, "[LONGITUDINAL] EQUATION:")
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
  .pk3 <- .pk2rx(.pk2)
  .ret <- list(monolix=text,
               rx=c(.monolix2rx$pk$pk,
                    .pk3$pk,
                    .monolix2rx$equationLine,
                    .monolix2rx$pk$equation$endLines),
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
  if (!retFile) .mlxtranIni()
  .f <- .mlxtranLib(file)
  if (checkmate::testFileExists(.f, "r")) {
    .m2 <- c("<MODEL>",
             suppressWarnings(readLines(.f)))
    lapply(.m2, .mlxtranParseItem)
    if (retFile) return(file)
    return(.mlxtranFinalize(.mlxEnv$lst, equation=TRUE, update=FALSE))
  }
  if (retFile) return(file)
  stop("could not find the file for getting model text")
}
