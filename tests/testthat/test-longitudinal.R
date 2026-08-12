test_that("longitudinal()", {

  f <- .longitudinal("input={pkadd__err, prop__err, pdadd__err}
file='pk.turnover.emax3-monolix.txt'")

  expect_equal(f$input, c("pkadd__err", "prop__err", "pdadd__err"))
  expect_equal(f$file, "pk.turnover.emax3-monolix.txt")
  expect_true(inherits(f, "monolix2rxLongitudinal"))

  expect_snapshot(print(f))
  expect_error(as.list(f), NA)

})

test_that("longitudinal() supports the Monolix 2024 file={path=} syntax (#43)", {

  f <- .longitudinal("input={pkadd__err, prop__err, pdadd__err}
file={path='pk.turnover.emax3-monolix.txt'}")

  expect_equal(f$input, c("pkadd__err", "prop__err", "pdadd__err"))
  expect_equal(f$file, "pk.turnover.emax3-monolix.txt")

  # the model file can also come from the Monolix library
  f <- .longitudinal("input={a, b}
file = {path = 'lib:oral1_1cpt_kaVCl.txt'}")

  expect_equal(f$file, "lib:oral1_1cpt_kaVCl.txt")

})
