
pkgTheo <- system.file("theo", package="monolix2rx")

test_that("single endpoint import", {

  rx <- monolix2rx(file.path(pkgTheo, "theophylline_project.mlxtran"))
  expect_true(inherits(rx, "rxUi"))
  if (requireNamespace("vdiffr", quietly=TRUE)) {
    vdiffr::expect_doppelganger("single-endpoint-theo",
                                plot(rx))
    vdiffr::expect_doppelganger("single-endpoint-theo-p1",
                                plot(rx, page=1))
    vdiffr::expect_doppelganger("single-endpoint-theo-pall",
                                plot(rx, page=TRUE))
  }
})

test_that("multiple endpoint import", {

  rx <- monolix2rx(file.path(pkgTheo, "parent_metabolite_project.mlxtran"))
  expect_true(inherits(rx, "rxUi"))
  if (requireNamespace("vdiffr", quietly=TRUE)) {
    vdiffr::expect_doppelganger("multiple-endpoint-theo",
                                plot(rx))
    vdiffr::expect_doppelganger("multiple-endpoint-theo-p1",
                                plot(rx, page=1))
    vdiffr::expect_doppelganger("multiple-endpoint-theo-pall",
                                plot(rx, page=TRUE))
  }
})
