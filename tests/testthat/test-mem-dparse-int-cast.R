test_that("the equation parser handles ordinary input", {
  # The trans_* entry-points range-check strlen() in size_t before
  # narrowing it for dparse()'s int buf_len.  The guard itself cannot be
  # tripped from R -- a CHARSXP is capped at INT_MAX bytes -- so what is
  # testable here is that adding the check left the normal parse path and
  # the syntax-error path alone.
  expect_equal(.equation("ddt_A1 = -k*A1\n")$rx,
               "d/dt(A1) <-  - k * A1")

  expect_error(.equation("ddt_A1 = -*\n"), "syntax error")
})
