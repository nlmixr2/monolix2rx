test_that("mlxtranOp", {

  .tmp <- .mlxtranOp("exportpath = 'pk.turnover.emax3-monolix'
  exploratoryautostop = no
  smoothingautostop = no
  burniniterations = 5
  exploratoryiterations = 250
  simulatedannealingiterations = 250
  smoothingiterations = 200
  exploratoryalpha = 0
  exploratoryinterval = 200
  omegatau = 0.95
  errormodeltau = 0.95")

  expect_snapshot(print(.tmp))
  expect_error(as.list(.tmp), NA)

  .tmp2 <- list(exportpath = "pk.turnover.emax3-monolix",
                exploratoryautostop = FALSE,
                smoothingautostop = FALSE,
                burniniterations = 5,
                exploratoryiterations = 250,
                simulatedannealingiterations = 250,
                smoothingiterations = 200,
                exploratoryalpha = 0,
                exploratoryinterval = 200,
                omegatau = 0.95,
                errormodeltau = 0.95)
  class(.tmp2) <- "monolix2rxOp"

  expect_equal(.tmp, .tmp2)

  .tmp <- .mlxtranOp("abc = {a, 'b', 1}")

  expect_equal(as.character(.tmp),
               "abc = {a, 'b', 1}")

})
