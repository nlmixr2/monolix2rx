#' Monolix Covariance (Linearity, Raw)
#'
#' @param mlx mlxtran parsed information
#' @return linearized covariance matrix
#' @author Matthew L. Fidler
#' @noRd
.mlxtranCovLin <- function(mlx) {
  .exportPath <- mlx$MONOLIX$SETTINGS$GLOBAL$exportpath
  if (!file.exists(.exportPath)) return(NULL)
  .covLin <- file.path(.exportPath, "FisherInformation", "covarianceEstimatesLin.txt")
  if (!file.exists(.covLin)) return(NULL)
  .c <- read.csv(.covLin, header=FALSE)
  .n <- .c[, 1]
  .c <- as.matrix(.c[, -1])
  dimnames(.c) <- list(.n, .n)
  .c
}
#' Get the raw Covariance (Simulated Annealing)
#'
#' @param mlx parsed monolix file
#' @return monolix parsed object
#' @noRd
#' @author Matthew L. Fidler
.mlxtranCovSA <- function(mlx) {
  .exportPath <- mlx$MONOLIX$SETTINGS$GLOBAL$exportpath
  if (!file.exists(.exportPath)) return(NULL)
  .covSA <- file.path(.exportPath, "FisherInformation", "covarianceEstimatesSA.txt")
  if (!file.exists(.covSA)) return(NULL)
  .c <- read.csv(.covSA, header=FALSE)
  .n <- .c[, 1]
  .c <- as.matrix(.c[, -1])
  dimnames(.c) <- list(.n, .n)
  .c
}
#' Determine if the covariance is untransformed
#'
#' @param ver Monlix version run
#' @param sa Is this a simulated annealing matrix?
#' @return boolean to if the covariance output is untransformed
#' @noRd
#' @author Matthew L. Fidler
.mlxtranCovarianceIsUntransformed <- function(ver, sa) {
  # This is used according to the table https://monolix.lixoft.com/tasks/result-files-generated-monolix/
  #
  # fim type | 2018        | 2019        | 2020        | 2021      |
  # ---------+-------------+-------------+-------------+-----------+
  # SA       | untransform | untransform | transform   | transform |
  # Lin      | untransform | untransform | untransform | transform |
  # ---------+-------------+-------------+-------------+-----------+
  if (!is.character(ver)) return(TRUE)
  .reg <- ".*([0-9][0-9][0-9][0-9]).*"
  if (regexpr(.reg, ver) != -1) {
    .num <- as.numeric(sub(.reg, "\\1", ver))
    if (sa) {
      return(.num < 2020)
    } else {
      return(.num <= 2020)
    }
  }
  TRUE
}
#' Get the mlxtran transformed covariance
#'
#' @param mat matrix that may need to be transformed
#' @param jac diagonal jacobian from output
#' @param ver version of monolix
#' @param sa is this a simulated annealing covariance
#' @return transformed covariance matrix
#' @noRd
#' @author Matthew L. Fidler
.mlxtranCovTransformed <- function(mat, jac, ver, sa) {
  if (is.null(mat)) return(NULL)
  if (!.mlxtranCovarianceIsUntransformed(ver, sa)) return(mat)
  .dn <- dimnames(mat)[[1]]
  .jInv <- vapply(.dn,
                  function(n) {
                    if (is.na(jac[n])) return(1)
                    1/jac[n]
                  }, numeric(1), USE.NAMES = TRUE)
  .jInv <- diag(.jInv)
  .cov <- .jInv %*% mat %*% .jInv
  dimnames(.cov) <- dimnames(mat)
  .cov
}
#'  Get the mlxtran untransformed covariance
#'
#' @param mat matrix that may need to be transformed
#' @param jac diagonal jacobian from output
#' @param ver version of monolix
#' @param sa is this a simulated annealing covariance
#' @return untransformed covariance
#' @noRd
#' @author Matthew L. Fidler
.mlxtranCovUntransformed <- function(mat, jac, ver, sa) {
  if (is.null(mat)) return(NULL)
  if (.mlxtranCovarianceIsUntransformed(ver, sa)) return(mat)
  .dn <- dimnames(mat)[[1]]
  .j <- vapply(.dn,
               function(n) {
                 if (is.na(jac[n])) return(1)
                 jac[n]
               }, numeric(1), USE.NAMES = TRUE)
  .j <- diag(.j)
  .cov <- .j %*% mat %*% .j
  dimnames(.cov) <- dimnames(mat)
  .cov
}

.mlxtranCov <- function(mlx) {
  .mlx <- .mlxtranSumary(mlx)
  .ver <- attr(.mlx, "version")
  .lin <- .mlxtranCovLin(mlx)
  .sa  <- .mlxtranCovSA(mlx)
  .jac <- .mlxtranJacobianDiag(.mlx$MODEL$INDIVIDUAL$DEFINITION,
                               .mlx$PARAMETER$PARAMETER)
  .saTransformed   <- .mlxtranCovTransformed(.sa, .jac, .ver, TRUE)
  .saUntransformed <- .mlxtranCovUntransformed(.sa, .jac, .ver, TRUE)

  .linTransformed   <- .mlxtranCovTransformed(.lin, .jac, .ver, FALSE)
  .linUntransformed <- .mlxtranCovUntransformed(.lin, .jac, .ver, FALSE)

  attr(.mlx, "covSaTransformed") <- .saTransformed
  attr(.mlx, "covSaUntransformed") <- .saUntransformed
  attr(.mlx, "covLinTransformed") <- .linTransformed
  attr(.mlx, "covLinUntransformed") <- .linUntransformed
  .mlx
}
