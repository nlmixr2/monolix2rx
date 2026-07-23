test_that("thetaMat NaN/NA pruning keeps a 1x1 matrix (drop=FALSE regression)", {

  .m <- matrix(c(1, 0.1, 0.1, NaN), 2, 2,
               dimnames=list(c("a", "b"), c("a", "b")))

  expect_warning(.p <- .thetaMatPrune(.m), "NaN/NA")
  expect_true(is.matrix(.p))
  expect_equal(dim(.p), c(1L, 1L))
  expect_equal(dimnames(.p), list("a", "a"))
  expect_equal(.p[1, 1], 1)

})

test_that("thetaMat pruning drops parameters with NaN/NA off-diagonal covariances", {

  .m <- matrix(c(1,   0.1, NaN,
                 0.1, 2,   NaN,
                 NaN, NaN, 3), 3, 3,
               dimnames=list(c("a", "b", "c"), c("a", "b", "c")))

  expect_warning(.p <- .thetaMatPrune(.m), "'c'")
  expect_equal(dim(.p), c(2L, 2L))
  expect_equal(dimnames(.p), list(c("a", "b"), c("a", "b")))
  expect_false(anyNA(.p))

})

test_that("thetaMat pruning of an all-NaN matrix gives a 0x0 matrix", {

  .m <- matrix(NaN, 2, 2, dimnames=list(c("a", "b"), c("a", "b")))

  expect_warning(.p <- .thetaMatPrune(.m), "NaN/NA")
  expect_true(is.matrix(.p))
  expect_equal(dim(.p), c(0L, 0L))

})

test_that("thetaMat pruning leaves a clean matrix untouched", {

  .m <- matrix(c(1, 0.1, 0.1, 2), 2, 2,
               dimnames=list(c("a", "b"), c("a", "b")))

  expect_warning(.p <- .thetaMatPrune(.m), NA)
  expect_equal(.p, .m)

})

test_that("monolix2rx imports a 1x1 thetaMat when all but one parameter is NaN", {
  skip_on_cran()
  skip_if_not(requireNamespace("rxode2", quietly=TRUE))

  .f <- system.file("theo/parent_metabolite_project.mlxtran", package="monolix2rx")
  .v <- suppressWarnings(suppressMessages(mlxtran(.f, equation=TRUE)))

  .cov <- attr(.v, "covSaUntransformed")
  skip_if(is.null(.cov))

  # make every diagonal element but the first NaN
  .n <- dim(.cov)[1]
  diag(.cov)[-1] <- NaN
  attr(.v, "covSaUntransformed") <- .cov
  if (!is.null(attr(.v, "covLinUntransformed"))) {
    attr(.v, "covLinUntransformed") <- .cov
  }

  expect_warning(.rx <- suppressMessages(monolix2rx(.v, update=FALSE)),
                 "NaN/NA")
  expect_true(inherits(.rx$thetaMat, "matrix"))
  expect_equal(dim(.rx$thetaMat), c(1L, 1L))
  expect_equal(dimnames(.rx$thetaMat)[[1]], dimnames(.cov)[[1]][1])

  # when every parameter is NaN the covariance information is ignored
  diag(.cov) <- NaN
  attr(.v, "covSaUntransformed") <- .cov
  if (!is.null(attr(.v, "covLinUntransformed"))) {
    attr(.v, "covLinUntransformed") <- .cov
  }

  .ws <- capture_warnings(.rx <- suppressMessages(monolix2rx(.v, update=FALSE)))
  expect_true(any(grepl("ignoring the Monolix covariance step", .ws)))
  expect_true(is.null(.rx$thetaMat))

})
