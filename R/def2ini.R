.parsTransformValue <- function(value, distribution, min, max) {
  if (distribution == "lognormal") {
    return(log(value))
  } else if (distribution == "logitnormal") {
    if (is.null(min)) min <- 0
    if (is.null(max)) max <- 1
    return(rxode2::logit(value, min, max))
  } else if (distribution == "normal") {
    return(value)
  } else if (distribution == "probitnormal") {
    return(qnorm(value))
  }
  NA_real_
}

.parsGetValue <- function(pars, name) {
  .w <- which(pars$name == name)
  if (length(.w) == 1L) {
    return(pars[.w, "value"])
  }
  NA_real_
}

.def2ini <- function(def, pars, longDef) {
  .env <- new.env(parent=emptyenv())
  .var <- def$var
  .n <- names(.var)
  .env$err <- character(0)
  lapply(seq_along(longDef$endpoint), function(i) {
    .env$err <- c(.env$err, longDef$endpoint[[1]]$err$typical)
  })
  .pop <- c(list(quote(`{`)),
            lapply(.n, function(n) {
              .cur <- .var[[n]]
              if (!is.null(.cur$typical)) {
                .var <- .cur$typical
                .val <- .parsTransformValue(.parsGetValue(pars, .var), .cur$distribution,
                                            min=.cur$min, max=.cur$max)
              } else if (!is.null(.cur$mean)) {
                .var <- .cur$mean
                .val <- .parsGetValue(pars, .var)
              }
              bquote(.(str2lang(.var)) <- .(.val))
            }),
            lapply(.env$err,
                   function(e) {
                     bquote(.(str2lang(e)) <- .(.parsGetValue(pars, e)))
                   }))
  .pop <- as.call(c(list(str2lang("lotri::lotri")), as.call(.pop)))
  .pop
}
