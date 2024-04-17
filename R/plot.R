.cmpDataOverall <- function(ui) {
  .d1 <- ui$predIpredData[, c("id", "time", "cmt", "monolixPred", "pred")]
  .d2 <- ui$predIpredData[, c("id", "time", "cmt", "monolixIpred", "ipred")]
  .d3 <- ui$predIpredData[, c("id", "time", "cmt", "monolixIwres", "iwres")]
  names(.d1) <- names(.d2) <- names(.d3) <- c("id", "time", "cmt", "monolix", "rxode2")
  .d1$type <- "pred"
  .d2$type <- "ipred"
  .d3$type <- "iwres"
  rbind(.d1, .d2, .d3)
}


#' Autoplot monolix2rx object
#'
#' @param ... ignored parameters for `monolix2rx` objects
#' @param page number of page(s) for the individual plots, by default
#'   (`FALSE`) no pages are print; You can use `TRUE` for all pages to
#'   print, or list which pages you want to print
#' @return a ggplot2 object
#' @inheritParams rxode2::plot.rxSolve
#' @inheritParams ggplot2::autoplot
#' @inheritParams ggforce::facet_wrap_paginate
#' @keywords internal
autoplot.monolix2rx <- function(object, ...,
                                ncol=3, nrow=3, log="", xlab = "Time", ylab = "Predictions",
                                page=FALSE) {
  stopifnot(length(log) == 1)
  stopifnot(is.character(log))
  if (is.logical(page)) {
    if (page) {
      page <- NULL
    }
  } else {
    checkmate::assertIntegerish(page, lower=1, any.missing = FALSE, null.ok=TRUE)
  }
  stopifnot(log %in% c("", "x", "y", "xy", "yx"))
  .useLogX <- nchar(log) == 2 | log == "x"
  .useLogY <- nchar(log) == 2 | log == "y"
  .data <- .cmpDataOverall(object)
  if (is.logical(page) && !page) {
    return(ggplot(data=.data, aes(.data$rxode2, .data$monolix)) +
             geom_point() +
             facet_wrap(cmt~type, scales="free") +
             rxode2::rxTheme() +
             ylab("Monolix") +
             xlab("rxode2"))
  }

  .ids <- unique(.data$id)
  .npage <- ceiling(length(.ids)/(ncol*nrow))

  .useXgxr <-
    getOption("rxode2.xgxr", TRUE) &&
    requireNamespace("xgxr", quietly = TRUE)
  .logx <- NULL
  .logy <- NULL
  if (.useLogX) {
    .dat <- .data[.data$time > 0, ]
    if (.useXgxr) {
      .logx <- xgxr::xgx_scale_x_log10()
    } else {
      .logx <- ggplot2::scale_x_log10()
    }
  }
  if (.useLogY) {
    if (.useXgxr) {
      .logy <- xgxr::xgx_scale_y_log10()
    } else {
      .logy <- ggplot2::scale_y_log10()
    }
  }
  if (is.null(page)) {
    .pages <- seq_len(.npage)
  } else {
    .pages <- page
    .pages <- .pages[.pages <= .npage]
  }

  .data <- .data[.data$type != "iwres", ]

  .ret <- lapply(.pages,
                 function(p) {
                   .ret <- ggplot(data=.data, aes(.data$time, .data$rxode2, col=.data$type)) +
                     geom_point() +
                     ggforce::facet_wrap_paginate(.data$cmt ~ .data$id,
                                                  ncol=ncol, nrow=nrow, page=p) +
                     geom_line(aes(.data$time, .data$monolix)) +
                     ylab(ylab) +
                     xlab(xlab) +
                     rxode2::rxTheme() +
                     theme(legend.position="top") + .logy +
                     .logx +
                     ggtitle(paste0("Lines: Monolix; Points: rxode2; Page ", p, " of ", .npage))
                 })
  if (length(.ret) == 1L) return(.ret[[1]])
  .ret
}

#' @export
plot.monolix2rx <- function(x, ..., ncol=3, nrow=3, log="",  xlab = "Time", ylab = "Predictions", page=FALSE) {
  .ret <- autoplot.monolix2rx(object=x, ..., ncol=ncol, nrow=nrow, log=log, xlab=xlab, ylab=ylab, page=page)
  if (inherits(.ret, "ggplot")) {
    print(.ret)
  } else {
    lapply(seq_along(.ret), function(i) {
      print(.ret[[i]])
    })
  }
  invisible()
}
