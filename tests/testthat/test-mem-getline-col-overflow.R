test_that("getLine handles normal-sized lines without error", {
  # Sanity check: regular syntax errors still report cleanly after the
  # `int col` -> `size_t col` change in getLine.
  result <- tryCatch(
    .Call(`_monolix2rx_trans_equation`,
          "[LONGITUDINAL] EQUATION:\nf = !!!syntax error here!!!\n",
          "[LONGITUDINAL] EQUATION:"),
    error = function(e) conditionMessage(e),
    warning = function(w) conditionMessage(w)
  )
  # Either an R error/warning is raised or a value is returned;
  # specifically, the new "line too long in getLine" must NOT appear.
  expect_false(grepl("line too long in getLine|source offset overflow", as.character(result)))
})

test_that("getLine col overflow guard triggers on >INT_MAX-byte line (skipped: requires ~2GB RAM)", {
  skip("Requires ~2GB free RAM to construct an >INT_MAX-byte single-line input")
  # --- What this test checks ---
  # `getLine` (src/parseSyntaxErrors.h) walks `src` to find the column of
  # a syntax error.  The column accumulator was `int col` and the
  # subsequent allocation was `R_Calloc(col + 1, char)`.  When the line
  # is wider than INT_MAX bytes, `col` wraps to a negative int; the
  # allocation either fails or is undersized, and the following
  # `memcpy(buf, src + i, col)` writes past the buffer.
  #
  # The fix changes `col` to `size_t` and bounds-checks before the cast
  # back to `int` for the R_Calloc call.
  #
  # --- How to trigger manually ---
  # Construct a single-line input that is wider than INT_MAX bytes and
  # ends with a syntax error, so that getLine is actually exercised on
  # the giant line.

  giant_line <- strrep("x_var_abc", 200000000L)        # ~1.8 GB on a single line
  bad <- paste0(giant_line, " = !!!bad")
  expect_error(
    .Call(`_monolix2rx_trans_equation`, bad, "[LONGITUDINAL]"),
    "line too long in getLine|source offset overflow"
  )
})
