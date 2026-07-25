.monolix2rx <- function(..., save=FALSE) {
  suppressWarnings(suppressMessages(monolix2rx(...)))
}

.rxSolve <-  function(...) {
  suppressMessages(suppressWarnings(rxSolve(...)))
}

test_that("solving makes sense", {
  skip_on_cran()

  f <- .monolix2rx(system.file("theo/theophylline_project.mlxtran", package="monolix2rx"))

  s <- .rxSolve(f)
  expect_true(inherits(s, "rxSolve"))

  expect_equal(s$env$.args$covsInterpolation, 1L)

  s <- .rxSolve(f, nStud=1)

  expect_equal(s$env$.args$dfObs, 120)
  expect_equal(s$env$.args$dfSub, 12)

  expect_equal(s$env$.args$thetaMat, f$thetaMat)
  expect_equal(s$env$.args$omega, f$omega)

  # the Monolix-style steady state limits are forwarded to the solve
  expect_equal(s$env$.args$maxSS, .getNbdoses(f) + 1)
  expect_equal(s$env$.args$minSS, .getNbdoses(f))

  # user-specified steady state limits are honored, not overwritten
  s <- .rxSolve(f, nStud=1, minSS=7L, maxSS=20L)
  expect_equal(s$env$.args$maxSS, 20L)
  expect_equal(s$env$.args$minSS, 7L)

  for (v in names(f$theta)) {
    expect_true(all(s$params[[v]] == f$theta[v]))
  }

})

test_that("rxSolve falls back to model-level dfObs/thetaMat when meta lacks them", {
  skip_on_cran()

  f <- .monolix2rx(system.file("theo/theophylline_project.mlxtran", package="monolix2rx"))
  f <- rxode2::rxUiDecompress(f)
  # rxUiDecompress() drops the monolix2rx class; restore it for dispatch
  class(f) <- c("monolix2rx", class(f))

  # move dfObs off the meta environment onto the model environment
  .dfObs <- f$meta$dfObs
  rm("dfObs", envir=f$meta)
  assign("dfObs", .dfObs + 1, envir=f)

  # move thetaMat off the meta environment onto the model environment
  if (exists("thetaMat", envir=f$meta)) rm("thetaMat", envir=f$meta)
  .iniDf <- f$iniDf
  .nm <- .iniDf$name[is.na(.iniDf$neta1) & !.iniDf$fix]
  .tm <- diag(1e-4, length(.nm))
  dimnames(.tm) <- list(.nm, .nm)
  assign("thetaMat", .tm, envir=f)

  s <- .rxSolve(f, nStud=1)

  expect_equal(s$env$.args$dfObs, .dfObs + 1)
  expect_equal(s$env$.args$thetaMat, .tm)

  # without any thetaMat, nStud > 1 warns that uncertainty is missing
  # (rxSolve() strips the monolix2rx class by reference on environment
  #  uis, so restore it before dispatching again)
  class(f) <- c("monolix2rx", class(f))
  rm("thetaMat", envir=f)
  expect_warning(try(suppressMessages(rxSolve(f, nStud=2)), silent=TRUE),
                 "simulating without parameter uncertainty")
})
