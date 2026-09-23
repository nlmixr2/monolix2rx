# rc_dup_str() (src/shared.c) copies either a [s, e) segment or, when `e` is
# NULL, the whole NUL-terminated string.  Its INT_MAX length guards cannot be
# reached from R (R strings are capped below INT_MAX bytes), so these tests
# check that both copy paths return exactly the requested bytes.
# .equation() does not release the rc_dup_str() pool itself (mlxTxt() does on
# exit), so each test frees it explicitly.

test_that("rc_dup_str copies [s, e) segments exactly", {
  # Grammar actions duplicate identifiers and numbers as segments of the
  # source; any off-by-one in the length would show up in the translation.
  .ret <- .equation("a_long_name = b1 + 12.5*c\nz = exp(-k*t)", .pk(""))
  .Call(`_monolix2rx_r_parseFree`)
  expect_equal(.ret$rx, c("a_long_name <- b1 + 12.5 * c", "z <- exp( - k * time)"))
})

test_that("rc_dup_str copies whole NUL-terminated strings when e is NULL", {
  # finalizeSyntaxError() duplicates the full error report with
  # rc_dup_str(firstErr.s, 0); the whole report, including the highlighted
  # source line and caret, must reach the R error message.
  .msg <- tryCatch(
    capture.output(.equation("x = 1\ny = !", .pk(""))),
    error = function(e) conditionMessage(e)
  )
  .Call(`_monolix2rx_r_parseFree`)
  expect_identical(.msg, paste0(
    "[LONGITUDINAL] EQUATION: syntax error:\n",
    "\n:002: y = !\n          ^\n",
    "more errors could be listed above"
  ))
})

test_that("a later syntax error does not replace the first one's highlight", {
  .msg <- tryCatch(
    capture.output(.equation("x = 1\ny = !\nz = )", .pk(""))),
    error = function(e) conditionMessage(e)
  )
  .Call(`_monolix2rx_r_parseFree`)
  expect_match(.msg, ":002: y = !", fixed = TRUE)
  expect_no_match(.msg, ":003:", fixed = TRUE)
})
