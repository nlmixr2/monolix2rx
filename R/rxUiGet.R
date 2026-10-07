#' @export
rxUiGet.monolixModelIwres <- function(x, ...) {
  .ui <- x[[1]]
  if (is.null(.ui$predDf)) {
    .ui$simulationModel
  } else {
    .ns <- loadNamespace("rxode2")
    .env <- new.env(parent=.ns)
    .env$.ui <- .ui
    if (any(grepl("\\bmix[(]", deparse(.ui$lstExpr)))) {
      # a mix() model cannot be rebuilt as a ui from its model variables
      # (mix() needs the ini block), so add the lines to the expression
      return(rxode2::rxode2(.monolixIwresMix(with(.env, rxode2::rxCombineErrorLines(.ui, modelVars=TRUE)),
                                             .env)))
    }
    .ui <- with(.env,eval(rxode2::rxCombineErrorLines(.ui, modelVars=TRUE)))
    DV <- sim <- iwres <- rxdv <- rx_pred_ <- rx_r_ <- NULL
    if (length(.ui$predDf$cond) == 1) {
      .ret <- suppressMessages(rxode2::model(.ui, iwres <- (DV-rx_pred_)/sqrt(rx_r_),
                                             append=sim, auto=FALSE))
      .ret <- suppressMessages(rxode2::model(.ret, ires <- DV-rx_pred_,
                                             append=sim, auto=FALSE))
    } else {
      .ret <- suppressMessages(rxode2::as.rxUi(.ui))
      .lstExpr <- .ret$lstExpr
      .l <- length(.lstExpr)
      while(identical(.lstExpr[[.l]][[1]], quote(`dvid`)) ||
              identical(.lstExpr[[.l]][[1]], quote(`cmt`))) .l <- .l - 1
      .lstOut <- c(list(quote(`{`)),
                   lapply(seq_len(.l), function(i) .lstExpr[[i]]),
                   list(quote(iwres <- (DV-rx_pred_)/sqrt(rx_r_)),
                        quote(ires <- DV-rx_pred_)),
                   lapply(seq(.l+1, length(.lstExpr)), function(i) .lstExpr[[i]]))
      .lstOut <- as.call(list(quote(`model`), as.call(.lstOut)))
      rxode2::model(.ret) <- .lstOut
      .ret
    }
    .ret <- rxode2::rxModelVars(.ret)
    .ret <- rxode2::rxode2(.ret)
    .ret
  }
}
# Default is list
#attr(rxUiGet.monolixModelIwres, "rstudio") <- list()

#' Add iwres/ires to a combined-error `rxModelVars({...})` expression
#'
#' @param expr `rxModelVars({...})` call from `rxCombineErrorLines()`
#' @param env environment to evaluate it in
#' @return rxModelVars
#' @noRd
#' @author Matthew L. Fidler
.monolixIwresMix <- function(expr, env) {
  .l <- as.list(expr[[2]])
  .n <- length(.l)
  while (.n > 1L && is.call(.l[[.n]]) &&
           (identical(.l[[.n]][[1]], quote(`dvid`)) || identical(.l[[.n]][[1]], quote(`cmt`)))) {
    .n <- .n - 1L
  }
  .l <- c(.l[seq_len(.n)],
          list(quote(iwres <- (DV-rx_pred_)/sqrt(rx_r_)),
               quote(ires <- DV-rx_pred_)),
          .l[-seq_len(.n)])
  expr[[2]] <- as.call(.l)
  eval(expr, envir=env)
}
