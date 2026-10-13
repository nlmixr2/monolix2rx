#' Set the number of threads to 2 for CRAN checks
#'
#' CRAN requires packages to use at most 2 threads.  Its example file
#' sorts first, so the limit holds for every example that follows.  It is
#' not intended for general use.
#'
#' When testing with `devtools::test()` or `testthat::test_package()`,
#' `NOT_CRAN` is `"true"`, so the number of threads is not limited.
#'
#' @return nothing, called for the side effect of limiting the threads
#'   to 2 for CRAN checks
#' @keywords internal
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' # Set the number of threads to 2 for CRAN checks
#' aaaCranMonolix2rxThreads()
#'
aaaCranMonolix2rxThreads <- function() {
  if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
    rxode2::setRxThreads(2L)
    if (requireNamespace("data.table", quietly=TRUE)) {
      data.table::setDTthreads(2L)
    }
    Sys.setenv(OMP_NUM_THREADS = "2")
    Sys.setenv(MKL_NUM_THREADS = "2")
    if (identical(Sys.info()["sysname"], "Darwin")) {
      rxode2::rxUnloadAll(set = FALSE)
    }
  }
  invisible()
}
