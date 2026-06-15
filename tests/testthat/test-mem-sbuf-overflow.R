test_that("sbuf overflow guard triggers a graceful error (skipped: requires ~2.5GB RAM)", {
  skip("Requires ~2.5GB RAM to construct a 2.147 GB identifier string that triggers integer overflow in buffer size calculation")
  # --- What this test checks ---
  # `sAppendN`/`sAppend`/`addLine` in src/sbuf.c computed the new allocation
  # size as:
  #   int mx = sbb->o + 2 + n + SBUF_MXBUF;
  # When the user-controlled `n` exceeds `INT_MAX - sbb->o - 2 - SBUF_MXBUF`,
  # this expression overflows `int` to a negative value.  R_Realloc then
  # converts it to a huge `size_t` and either crashes (older R) or fails the
  # allocation cleanly (modern R).  Either way it is a memory bug rather than
  # a controlled error path.
  #
  # The overflow guard added by this fix checks BEFORE the calculation:
  #   if (n > INT_MAX - sbb->o - 2 - SBUF_MXBUF) Rf_error(...)
  # so the crash is replaced by a controlled R error.
  #
  # --- How to run manually (outside devtools::test()) ---
  # Start a fresh R session with at least 3 GB of available RAM, then:
  #
  #   library(monolix2rx)
  #   big <- strrep("x", 2147435646L)   # ~2.147 GB
  #   src <- sprintf("[LONGITUDINAL]\ninput={%s}\nEQUATION:\nf=%s\n", big, big)
  #   # Before fix: SIGSEGV from R_Realloc with negative size
  #   # After fix:  error "string buffer size overflow: input too large"
  #   try(.Call(`_monolix2rx_trans_equation`, src, "rxode2"))

  big <- strrep("x", 2147435646L)
  src <- sprintf("[LONGITUDINAL]\ninput={%s}\nEQUATION:\nf=%s\n", big, big)
  expect_error(
    .Call(`_monolix2rx_trans_equation`, src, "rxode2"),
    "string buffer size overflow|too large"
  )
})

test_that("sbuf handles moderately large inputs without error", {
  # Sanity check: the overflow guard must not trigger on realistic inputs.
  large_input <- paste(rep("a=1\n", 1e4), collapse = "")
  expect_no_error(
    tryCatch(
      .Call(`_monolix2rx_trans_equation`, large_input, "rxode2"),
      error = function(e) {
        if (grepl("buffer size overflow", conditionMessage(e))) stop(e)
        # Any other parse error is acceptable for this malformed input.
        NULL
      }
    )
  )
})
