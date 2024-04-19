#' Get the monolix project working direcotry
#'
#' @param x monolix2rx object to get the working directory
#' @return working directory for the current project
#' @noRd
#' @author Matthew L. Fidler
.monolixGetPwd <- function(x) {
  if (inherits(x, "monolix2rx")) x <- x$mlxtran
  if (!inherits(x, "monolix2rxMlxtran")) return(getwd())
  .wd <- attr(x, "dirn")
  if (checkmate::testDirectoryExists(.wd)) return(.wd)
  getwd()
}


#' Update the parameters based on final output
#'
#' @param mlx mlxtran object to update
#' @return updated mlxtran object where the values are updated to the final model values
#' @noRd
#' @author Matthew L. Fidler
.parameterUpdate <- function(mlx) {
  if (inherits(mlx, "rxUi")) mlx <- mlx$mlxtran
  if (is.null(mlx)) return(invisible())
  .wd <- .monolixGetPwd(mlx)
  withr::with_dir(.wd, {
    .exportPath <- mlx$MONOLIX$SETTINGS$GLOBAL$exportpath
    .popPar <- file.path(.exportPath, "populationParameters.txt")
    if (file.exists(.popPar)) {
      .minfo("updating model values to final parameter estimates")
      .popPar <- read.csv(.popPar)
      mlx$PARAMETER$PARAMETER$value <-
        vapply(mlx$PARAMETER$PARAMETER$name,
               function(n) {
                 .w <- which(.popPar$parameter == n)
                 if (length(.w) == 1L) {
                   return(.popPar[.w, "value"])
                 }
                 return(NA_real_)
               }, numeric(1), USE.NAMES = FALSE)
      .minfo("done")
    }
  })
  mlx
}
