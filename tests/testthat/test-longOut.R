test_that("long output", {
  .ret <- .longOut("output = {Conc, Effect}
table  = {Ap, T12}")

  expect_snapshot(print(.ret))
  expect_error(as.list(.ret), NA)

  .ret2 <- list(output = c("Conc", "Effect"), table = c("Ap", "T12"))
  class(.ret2) <- "monolix2rxLongOut"
  expect_equal(.ret, .ret2)

})
