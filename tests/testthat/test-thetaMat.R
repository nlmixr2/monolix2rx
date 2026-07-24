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

test_that("thetaMat off-diagonal pruning keeps a 1x1 matrix", {

  .m <- matrix(c(1, NaN, NaN, 2), 2, 2,
               dimnames=list(c("a", "b"), c("a", "b")))

  expect_warning(.p <- .thetaMatPrune(.m), "is dropped")
  expect_true(is.matrix(.p))
  expect_equal(dim(.p), c(1L, 1L))
  expect_equal(dimnames(.p), list("b", "b"))
  expect_equal(.p[1, 1], 2)

})

test_that("thetaMat pruning handles column-only NaN patterns", {

  # NaN only below the diagonal: 'a' participates in both NaN pairs, so
  # dropping it keeps the clean {b, c} block
  .m <- matrix(c(1,  0, 0,
                 NA, 1, 0,
                 NA, 0, 1), 3, 3, byrow=TRUE,
               dimnames=list(c("a", "b", "c"), c("a", "b", "c")))

  expect_warning(.p <- .thetaMatPrune(.m), "'a'")
  expect_equal(dimnames(.p), list(c("b", "c"), c("b", "c")))
  expect_false(anyNA(.p))

})

test_that("thetaMat pruning of an all-NaN matrix gives a 0x0 matrix", {

  .m <- matrix(NaN, 2, 2, dimnames=list(c("a", "b"), c("a", "b")))

  expect_warning(.p <- .thetaMatPrune(.m), "NaN/NA")
  expect_true(is.matrix(.p))
  expect_equal(dim(.p), c(0L, 0L))

})

test_that("thetaMat pruning drops non-model parameters first on ties", {

  # the bad covariance pairs 'ka' with 'unused'; without a keep
  # preference the tie would drop the real model parameter 'ka'
  .m <- matrix(c(1,  0, NA,
                 0,  4, 0,
                 NA, 0, 9), 3, 3, byrow=TRUE,
               dimnames=list(c("ka", "cl", "unused"), c("ka", "cl", "unused")))

  expect_warning(.p <- .thetaMatPrune(.m, keep=c("ka", "cl")), "'unused'")
  expect_equal(dimnames(.p), list(c("ka", "cl"), c("ka", "cl")))
  expect_true(all(is.finite(.p)))

})

test_that("thetaMat pruning also drops Inf (co)variances", {

  .m <- matrix(c(Inf, 0.1, 0.1, 2), 2, 2,
               dimnames=list(c("a", "b"), c("a", "b")))

  expect_warning(.p <- .thetaMatPrune(.m), "NaN/NA/Inf")
  expect_equal(dimnames(.p), list("b", "b"))

  .m <- matrix(c(1, Inf, Inf, 2), 2, 2,
               dimnames=list(c("a", "b"), c("a", "b")))

  expect_warning(.p <- .thetaMatPrune(.m), "is dropped")
  expect_equal(dim(.p), c(1L, 1L))
  expect_true(all(is.finite(.p)))

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
  .cov0 <- .cov

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

  # a clean diagonal with NaN off-diagonal covariances drops the parameter
  .cov <- .cov0
  .cov[1, 2] <- .cov[2, 1] <- NaN
  attr(.v, "covSaUntransformed") <- .cov
  if (!is.null(attr(.v, "covLinUntransformed"))) {
    attr(.v, "covLinUntransformed") <- .cov
  }

  .ws <- capture_warnings(.rx <- suppressMessages(monolix2rx(.v, update=FALSE)))
  expect_true(any(grepl("is dropped from the thetaMat covariance matrix", .ws)))
  expect_true(inherits(.rx$thetaMat, "matrix"))
  expect_equal(dim(.rx$thetaMat), dim(.cov) - 1L)
  expect_false(anyNA(.rx$thetaMat))

})
