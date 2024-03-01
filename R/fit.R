#' Initialize the fit information
#'
#'
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.fitIni <- function() {
  .monolix2rx$fitDat <- character(0)
  .monolix2rx$fitQuote <- logical(0)
  .monolix2rx$modelDat <- character(0)
  .monolix2rx$modelQuote <- logical(0)
}
#' Parse the <FIT> object
#'
#' @param text FIT text
#' @return monolix2rxFit object
#' @noRd
#' @author Matthew L. Fidler
.fit <- function(text) {
  .fitIni()
  .Call(`_monolix2rx_trans_fit`, text)
  if (length(.monolix2rx$fitDat) != length(.monolix2rx$modelDat)) {
    stop("number of model and data endpoints in <FIT> need to be the same",
         call.=FALSE)
  }
  .ret <- data.frame(data=.monolix2rx$fitDat, dataQuote=.monolix2rx$fitQuote,
                     model=.monolix2rx$modelDat, modelQuote=.monolix2rx$modelQuote)
  class(.ret) <- c("monolix2rxFit", class(.ret))
  .ret
}
#' Fit data item handling
#'
#' @param datId data text for the `data = {item, item2}`
#' @param quote integer to see if this is quoted
#' @return noting, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.fitDatId <- function(datId, quote) {
  .monolix2rx$fitDat <- c(.monolix2rx$fitDat, datId)
  .monolix2rx$fitQuote <- c(.monolix2rx$fitQuote, as.logical(quote))
}
#' Add model id item
#'
#' @param modelId model id
#' @param quote character vector representing if the id quoted
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.fitModelId <- function(modelId, quote) {
  .monolix2rx$modelDat <- c(.monolix2rx$modelDat, modelId)
  .monolix2rx$modelQuote <- c(.monolix2rx$modelQuote, as.logical(quote))
}
#' @export
as.character.monolix2rxFit <- function(x, ...) {
  c(paste0("data = {", paste(vapply(seq_along(x$data),
                                    function(i) {
                                      if (x$dataQuote[i]) {
                                        return(paste0("'",x$data[i], "'"))
                                      }
                                      x$data[i]
                                    }, character(1), USE.NAMES = FALSE),
                             collapse=", "), "}"),
    paste0("model = {", paste(vapply(seq_along(x$model),
                                     function(i) {
                                       if (x$modelQuote[i]) {
                                         return(paste0("'",x$model[i], "'"))
                                       }
                                       x$model[i]
                                     }, character(1), USE.NAMES = FALSE), collapse=", "), "}"))
}

#' @export
print.monolix2rxFit <- function(x, ...) {
  cat(paste(as.character.monolix2rxFit(x), collapse="\n"),"\n", sep="")
  invisible(x)
}

#' @export
as.data.frame.monolix2rxFit <- function(x, row.names = NULL, optional = FALSE, ...) {
  .x <- x
  class(.x) <- "data.frame"
  .x
}
