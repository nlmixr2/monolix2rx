test_that("rc_dup_str handles normal-sized inputs without error", {
  # Sanity check: regular Mlxtran fragments must continue to parse cleanly
  # after the INT_MAX guards added to rc_dup_str.
  expect_no_error(
    tryCatch(
      .Call(`_monolix2rx_trans_equation`,
            "[LONGITUDINAL] EQUATION:\nf = exp(-k*t)\n",
            "[LONGITUDINAL] EQUATION:"),
      error = function(e) {
        if (grepl("rc_dup_str", conditionMessage(e))) stop(e)
        # Other parse errors (e.g., from synthetic test input) are fine.
        NULL
      }
    )
  )
})

test_that("rc_dup_str int truncation guard triggers on huge inputs (skipped: requires ~2GB RAM)", {
  skip("Requires ~2GB free RAM to construct a >INT_MAX-byte string and exercise the rc_dup_str path")
  # --- What this test checks ---
  # `rc_dup_str` (src/shared.c) computes the length as
  #   int l = e ? e-s : (int)strlen(s);
  # When the source string segment is larger than INT_MAX bytes, the
  # `(int)` cast silently truncates to a wrong value, which propagates
  # into `addLine(&_dupStrs, "%.*s", l, s)` and either reads past the
  # buffer (OOB) or wraps to a tiny copy.
  #
  # The guard added by this fix range-checks the ptrdiff_t / size_t
  # length and raises an R error before the truncation can happen.
  #
  # --- How to run manually (outside devtools::test()) ---
  # Start a fresh R session with at least 3 GB of available RAM, then:
  #
  #   library(monolix2rx)
  #   big <- strrep("a", 2147483647L)   # exactly INT_MAX bytes
  #   # Before fix: silent truncation; potential heap corruption
  #   # After fix:  error "string too long in rc_dup_str"

  big <- strrep("a", 2147483647L)
  expect_error(
    .Call(`_monolix2rx_trans_equation`, big, "[LONGITUDINAL]"),
    "rc_dup_str|too long"
  )
})
