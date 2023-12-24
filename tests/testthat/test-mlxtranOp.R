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

})
