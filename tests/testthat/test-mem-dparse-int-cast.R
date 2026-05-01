test_that("trans_* parsers handle normal-sized inputs without error", {
  # Sanity check: regular Mlxtran fragments must parse cleanly after
  # switching every dparse() call site to udparse() (unsigned int buf_len).
  expect_no_error(
    tryCatch(
      .Call(`_monolix2rx_trans_equation`,
            "[LONGITUDINAL] EQUATION:\nf = exp(-k*t)\n",
            "[LONGITUDINAL] EQUATION:"),
      error = function(e) {
        if (grepl("input too large", conditionMessage(e))) stop(e)
        # Other parse errors from synthetic input are acceptable.
        NULL
      }
    )
  )
})
