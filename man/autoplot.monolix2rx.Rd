% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{autoplot.monolix2rx}
\alias{autoplot.monolix2rx}
\title{Autoplot monolix2rx object}
\usage{
\method{autoplot}{monolix2rx}(
  object,
  ...,
  ncol = 3,
  nrow = 3,
  log = "",
  xlab = "Time",
  ylab = "Predictions",
  page = FALSE
)
}
\arguments{
\item{object}{an object, whose class will determine the behaviour of autoplot}

\item{...}{ignored parameters for \code{monolix2rx} objects}

\item{nrow, ncol}{Number of rows and columns}

\item{log}{Should "" (neither x nor y), "x", "y", or "xy" (or "yx") be
log-scale?}

\item{xlab, ylab}{The x and y axis labels}

\item{page}{number of page(s) for the individual plots, by default
(\code{FALSE}) no pages are print; You can use \code{TRUE} for all pages to
print, or list which pages you want to print}
}
\value{
a ggplot2 object
}
\description{
Autoplot monolix2rx object
}
\keyword{internal}
