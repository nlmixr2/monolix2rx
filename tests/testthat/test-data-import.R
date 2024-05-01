
pkgTheo <- system.file("theo", package="monolix2rx")

test_that("single endpoint import", {
  rx <- monolix2rx(file.path(pkgTheo, "theophylline_project.mlxtran"))
  expect_true(inherits(rx, "rxUi"))
})

test_that("multiple endpoint import", {
  rx <- monolix2rx(file.path(pkgTheo, "parent_metabolite_project.mlxtran"))
  expect_true(inherits(rx, "rxUi"))
})
