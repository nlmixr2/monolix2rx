test_that("sbuf overflow guards raise clean R errors", {
  # The guards in src/sbuf.c only trigger for ~2GB inputs, so
  # `_monolix2rx_sbufGuardTest` (src/sbufTest.c) fakes buffer state near
  # INT_MAX and calls each guarded function directly.  Before the guards
  # these size calculations overflowed int and R_Realloc() received a
  # negative (huge) size.
  .guard <- function(which) {
    .Call(`_monolix2rx_sbufGuardTest`, which)
  }
  expect_error(.guard(1L), "negative length passed to 'sAppendN'")
  expect_error(.guard(2L), "string buffer size overflow: input too large")
  expect_error(.guard(3L), "string buffer size overflow: input too large")
  expect_error(.guard(4L), "string buffer size overflow: input too large")
  expect_error(.guard(5L), "line array size overflow: too many lines")
  expect_error(.guard(0L), "unknown sbuf guard test: 0")
  # the normal (non-overflow) resize paths still grow the buffer: 2 appends
  # of SBUF_MXBUF + 1 single characters each, with SBUF_MXBUF = 48000
  expect_equal(.guard(6L), 96002L)
})

test_that("sbuf handles moderately large inputs without error", {
  # Sanity check: the overflow guards must not trigger on realistic inputs,
  # which grow the buffer well past its initial SBUF_MXBUF bytes.
  large_input <- paste(rep("a=1\n", 2e4), collapse = "")
  expect_no_error(.Call(`_monolix2rx_trans_equation`, large_input, "rxode2"))
})
