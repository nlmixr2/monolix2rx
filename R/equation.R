#' Parse equation block
#'
#' @param text monolix equation to text to rxode2 code
#' @param pk is the parsed pk
#' @return rode2 code (`$rx`) and odeType (`$odeType`)
#' @noRd
#' @author Matthew L. Fidler
.equation <- function(text, pk=NULL) {
  .monolix2rx$equationLine <- character(0)
  .monolix2rx$odeType <- "nonStiff"
  if (is.null(pk)) pk <- .pk("")
  if (is.character(pk)) pk <- .pk(pk)
  .monolix2rx$pk <- .pk2rx(pk)
  .Call(`_monolix2rx_trans_equation`, text)
  .ret <- list(monolix=text,
               rx=c(.monolix2rx$pk$pk,
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
print.monolix2rxEquation <- function(x, ...) {
  cat(x$monolix, "\n", sep="")
}

#' @export
as.list.monolix2rxEquation <- function(x, ...) {
  .x <- x
  class(.x) <- NULL
  .x
}
