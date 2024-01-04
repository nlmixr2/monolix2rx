test_that("longitudinal()", {

  f <- .longitudinal("input={pkadd__err, prop__err, pdadd__err}
file='pk.turnover.emax3-monolix.txt'")

  expect_equal(f$input, c("pkadd__err", "prop__err", "pdadd__err"))
  expect_equal(f$file, "pk.turnover.emax3-monolix.txt")
  expect_true(inherits(f, "monolix2rxLongitudinal"))

  expect_snapshot(print(f))
  expect_error(as.list(f), NA)

})
