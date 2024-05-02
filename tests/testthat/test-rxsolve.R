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

  ## expect_equal(s$env$.args$dfObs, 2280)
  ## expect_equal(s$env$.args$dfSub, 120)

  expect_equal(s$env$.args$thetaMat, f$thetaMat)
  expect_equal(s$env$.args$omega, f$omega)

  for (v in names(f$theta)) {
    expect_true(all(s$params[[v]] == f$theta[v]))
  }

  # now test model that does not translate the error
  # force rik's model
})
