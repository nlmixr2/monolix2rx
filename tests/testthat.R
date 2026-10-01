# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(monolix2rx)

# CRAN/R-hub work-arounds, mirroring rxode2's own tests/testthat.R: keep
# rxode2, OpenMP and MKL to one thread, and on macOS stop rxode2 from
# unloading the model dlls, which the ASAN checks trip over.
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  rxode2::setRxThreads(1L)
  Sys.setenv(OMP_NUM_THREADS = "1")
  Sys.setenv(MKL_NUM_THREADS = "1")
  if (identical(Sys.info()[["sysname"]], "Darwin")) {
    rxode2::rxUnloadAll(set = FALSE)
  }
}

test_check("monolix2rx")
