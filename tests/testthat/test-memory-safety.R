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
  # the next parse must start from a clean pool.
  for (.i in 1:3) {
    .ret <- .equation("x_0 = V\nddt_x = -k*x", .pk(""))
    expect_true(any(grepl("d/dt(x)", .ret$rx, fixed = TRUE)))
    .Call(`_monolix2rx_r_parseFree`)
  }
  # A conditional first line calls pushModel() before any assignment resets
  # curDdt, so without the per-parse reset it would read the ddt string freed
  # above (and could inject depot lines for a stale compartment).
  .ret <- .equation("if t > 0\ny = 1\nend", .pk(""))
  expect_equal(.ret$rx, c("if (time > 0) {", "y <- 1", "}"))
})

test_that("many syntax errors in one parse are reported without crashing", {
  # Exercises the per-error getLine()/vmaxset() path end to end.  It checks
  # that many reports in one parse complete cleanly; it cannot observe whether
  # each line copy is released early (that needs a memory profiler).
  # each stray ")" line is reported (and highlighted) as its own error
  .bad <- strrep("x = 1\n)\n", 500L)
  .out <- capture.output(expect_error(.equation(.bad, .pk(""))))
  expect_gt(sum(grepl("^", .out, fixed = TRUE)), 100)
})

test_that("a later parse reports its syntax error header after an earlier error", {
  # lastSyntaxErrorLine used to carry over, so the second report lost its
  # header and the source lines before the error.
  capture.output(expect_error(.equation(strrep("x = !\n", 20L), .pk(""))))
  .out <- capture.output(expect_error(.equation("y = 1\ny = !", .pk(""))))
  expect_true(any(grepl("^=+$", .out)))
  expect_true(any(grepl(":001: y = 1", .out, fixed = TRUE)))
})

test_that("near-INT_MAX boundary smoke test: dparse input of INT_MAX - 1 bytes", {
  skip_if_not(nzchar(Sys.getenv("MONOLIX2RX_BIG_MEMORY_TESTS")), paste(
    "needs several GB of RAM and a long run time; set",
    "MONOLIX2RX_BIG_MEMORY_TESTS to run. Smoke test only: R strings are",
    "capped below INT_MAX bytes, so this cannot reach the overflow guard in",
    "monolix2rxParseLen(); it checks the largest R string parses without a crash."
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

test_that("near-INT_MAX boundary smoke test: ~1.8GB of translated output", {
  skip_if_not(nzchar(Sys.getenv("MONOLIX2RX_BIG_MEMORY_TESTS")), paste(
    "needs several GB of RAM and a long run time; set",
    "MONOLIX2RX_BIG_MEMORY_TESTS to run. Smoke test only: the output stays",
    "below INT_MAX, so the sbuf overflow guards are not reached; it checks a",
    "large parse near the boundary completes without a crash."
  ))
  # 35 bytes x 51,000,000 = 1,785,000,000 bytes (~1.78GB); feeds the parser
  # with enough content to accumulate near the sbuf overflow boundary.
  huge_eq <- strrep("var_a_b_c_d_e_f_g = var_h_i_j_k_l\n", 51000000L)
  expect_no_error(
    .equation(huge_eq, .pk(""))
  )
})

test_that("integer overflow protection: syntax error on a near-INT_MAX line", {
  skip_if_not(nzchar(Sys.getenv("MONOLIX2RX_BIG_MEMORY_TESTS")), paste(
    "needs several GB of RAM and a long run time;",
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
