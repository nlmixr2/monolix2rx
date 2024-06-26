% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mlxtran.R
\name{mlxtran}
\alias{mlxtran}
\title{Read and parse mlxtran lines}
\usage{
mlxtran(file, equation = FALSE, update = FALSE)
}
\arguments{
\item{file}{mlxtran file to process}

\item{equation}{parse the equation block to rxode2 (some models cannot be translated)}

\item{update}{when true, try to update the parameter block to the final parameter estimates}
}
\value{
mlxtran object
}
\description{
Read and parse mlxtran lines
}
\examples{
# First load in the model; in this case the theo model
# This is modified from the Monolix demos by saving the model
# File as a text file (hence you can access without model library)
# setup.
#
# This example is also included in the monolix2rx package, so
# you refer to the location with `system.file()`:

pkgTheo <- system.file("theo", package="monolix2rx")

mlx <- mlxtran(file.path(pkgTheo, "theophylline_project.mlxtran"))

mlx
}
\author{
Matthew L. Fidler
}
