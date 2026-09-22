# Tests for integer overflow and memory safety fixes
#
# Issues addressed:
#   - rc_dup_str: implicit ptrdiff_t to int truncation (shared.c)
#   - dparse: (int)strlen(gBuf) overflow in all 13 parser entry-points
#   - sbuf: signed integer overflow in size arithmetic (sbuf.c)
#   - getLine: int index/col overflow in parseSyntaxErrors.h
#   - syntax error highlighting: embedded NUL on an empty error line
#
# NOTE on the >2GB skipped tests: R's internal CHARSXP type uses a signed
# 32-bit integer for string length, capping individual R strings at
# INT_MAX (2,147,483,647) bytes. Because of this, the overflow guards in
# the C code protect primarily against direct C-level misuse (e.g., calls
# from C code that bypasses R's string limit). The tests below document
# the boundary behaviour and need several GB of free RAM, so they only run
# when MONOLIX2RX_BIG_MEMORY_TESTS is set.

test_that("rc_dup_str handles normal strings without error", {
  # Regression: short strings must work correctly after the overflow guards
  .ret <- .equation("x_0 = V\nddt_x = -k*x", .pk(""))
  expect_type(.ret$rx, "character")
  expect_true(length(.ret$rx) > 0)
})

test_that("equation parser handles multi-statement input correctly", {
  # Regression: multi-statement equations still parse cleanly after all guards
  .ret <- .equation(
    "x_0 = V\ny_0 = 1\nddt_x = -k*x\nddt_y = k*x - k2*y",
    .pk("")
  )
  expect_type(.ret$rx, "character")
  expect_true(any(grepl("d/dt", .ret$rx)))
})

test_that("syntax error on an empty last line does not truncate the report", {
  # A syntax error reported past the last character used to emit getLine()'s
  # terminating NUL into the highlight buffer, silently cutting off the rest
  # of the report.
  .out <- capture.output(
    expect_error(.equation("x = \n", .pk("")))
  )
  expect_true(any(grepl("^", .out, fixed = TRUE)))
})

test_that("rc_dup_str strings stay intact across many duplications", {
  # 1500 statements duplicate well over 1024 strings, so the rc_dup_str()
  # pointer array is reallocated mid-parse; every earlier string must survive.
  .i <- 1:1500
  .eq <- paste0("v", .i, " = a", .i, " + b", .i, collapse = "\n")
  .ret <- .equation(.eq, .pk(""))
  expect_equal(.ret$rx, paste0("v", .i, " <- a", .i, " + b", .i))
})

test_that("repeated parses with cleanup in between stay correct", {
  # rc_dup_str() strings are freed by _monolix2rx_r_parseFree between parses;
  # the next parse must start from a clean pool.  (The curDdt reset in
  # _monolix2rx_trans_equation is defensive: this input sets curDdt on its first
  # assignment, so it cannot observe a stale curDdt.)
  for (.i in 1:3) {
    .ret <- .equation("x_0 = V\nddt_x = -k*x", .pk(""))
    expect_true(any(grepl("d/dt(x)", .ret$rx, fixed = TRUE)))
    .Call(`_monolix2rx_r_parseFree`)
  }
})

test_that("many syntax errors in one parse are reported without crashing", {
  # Each reported error copies its source line with getLine(); the copies are
  # released per error rather than held until the .Call() returns.
  .bad <- strrep("x = !\n", 2000L)
  .out <- capture.output(expect_error(.equation(.bad, .pk(""))))
  expect_true(any(grepl("syntax error", .out, fixed = TRUE)))
})

test_that("a later parse reports its syntax error header after an earlier error", {
  # lastSyntaxErrorLine used to carry over, so the second report lost its
  # header and the source lines before the error.
  capture.output(expect_error(.equation(strrep("x = !\n", 20L), .pk(""))))
  .out <- capture.output(expect_error(.equation("y = 1\ny = !", .pk(""))))
  expect_true(any(grepl("^=+$", .out)))
  expect_true(any(grepl(":001: y = 1", .out, fixed = TRUE)))
})

test_that("integer overflow protection: dparse input approaching INT_MAX bytes", {
  skip_if_not(nzchar(Sys.getenv("MONOLIX2RX_BIG_MEMORY_TESTS")), paste(
    "requires ~2GB free RAM;",
    "tests the (int)strlen(gBuf) overflow guard before each dparse() call.",
    "Without the fix the cast silently wraps to a negative value for inputs",
    "> INT_MAX bytes, causing dparse to crash. R strings are internally capped",
    "at INT_MAX-1 bytes so the guard fires for C-level misuse; this test",
    "exercises the largest string R can construct to verify the path is safe.",
    "NOTE: use strrep() not paste0(rep()) to avoid a large intermediate vector."
  ))
  # 6 bytes x 357,913,941 = 2,147,483,646 bytes (INT_MAX - 1): the largest
  # string strrep can produce before R itself errors on string length.
  # The overflow guard fires only for > INT_MAX, so this input passes through;
  # the test confirms no crash or corruption occurs near the boundary.
  huge_str <- strrep("a = b\n", 357913941L)
  expect_no_error(
    .equation(huge_str, .pk(""))
  )
})

test_that("integer overflow protection: sbuf size arithmetic near INT_MAX", {
  skip_if_not(nzchar(Sys.getenv("MONOLIX2RX_BIG_MEMORY_TESTS")), paste(
    "requires ~2GB free RAM;",
    "tests signed integer overflow guards in sbuf.c sAppend/sAppendN/addLine.",
    "Without the fix, sbb->o + n wraps to a negative int when the accumulated",
    "buffer approaches INT_MAX, causing R_Realloc to allocate a tiny buffer and",
    "corrupt the heap. The guard fires before that arithmetic.",
    "NOTE: use strrep() not paste0(rep()) to avoid a large intermediate vector."
  ))
  # 35 bytes x 51,000,000 = 1,785,000,000 bytes (~1.78GB); feeds the parser
  # with enough content to accumulate near the sbuf overflow boundary.
  huge_eq <- strrep("var_a_b_c_d_e_f_g = var_h_i_j_k_l\n", 51000000L)
  # The equation parser processes all lines; cumulative rc_dup_str calls
  # grow _dupStrs toward INT_MAX. The guard prevents heap corruption.
  expect_no_error(
    .equation(huge_eq, .pk(""))
  )
})

test_that("integer overflow protection: syntax error on a near-INT_MAX line", {
  skip_if_not(nzchar(Sys.getenv("MONOLIX2RX_BIG_MEMORY_TESTS")), paste(
    "requires ~4GB free RAM;",
    "reports a syntax error on a single ~1.8GB line, so getLine() copies the",
    "whole line and the error highlighter pushes it through sbuf, whose",
    "overflow guard fires. getLine()'s own col == INT_MAX guard cannot be",
    "reached from R (R strings are capped below INT_MAX bytes); this checks",
    "that the path raises a clean R error instead of crashing (it does not",
    "measure whether the line buffer is reclaimed on the longjmp).",
    "NOTE: use strrep() not paste0(rep()) to avoid a large intermediate vector."
  ))
  # 9 bytes x 200,000,000 = 1,800,000,000 bytes on one line (under INT_MAX),
  # with an invalid RHS to force a syntax error on that line.
  giant_line <- strrep("x_var_abc", 200000000L)
  expect_error(
    .equation(paste0(giant_line, " = !!!bad"), .pk(""))
  )
})
