# rc_dup_str() (src/shared.c) copies either a [s, e) segment or, when `e` is
# NULL, the whole NUL-terminated string.  Its INT_MAX length guards cannot be
# reached from R (R strings are capped below INT_MAX bytes), so these tests
# check that both copy paths return exactly the requested bytes.

test_that("rc_dup_str copies [s, e) segments exactly", {
  # Grammar actions duplicate identifiers and numbers as segments of the
  # source; any off-by-one in the length would show up in the translation.
  .ret <- .equation("a_long_name = b1 + 12.5*c\nz = exp(-k*t)", .pk(""))
  expect_equal(.ret$rx, c("a_long_name <- b1 + 12.5 * c", "z <- exp( - k * time)"))
})

test_that("rc_dup_str copies whole NUL-terminated strings when e is NULL", {
  # finalizeSyntaxError() duplicates the full error report with
  # rc_dup_str(firstErr.s, 0); the message must survive intact.
  expect_error(
    capture.output(.equation("x = 1\ny = !", .pk(""))),
    "syntax error"
  )
})
