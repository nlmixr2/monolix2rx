test_that("test single endpoint handling", {

  expect_equal(.handleSingleEndpoint(list(var = "rx_prd_cp",
                                          dist = "normal",
                                          pred = "rx_pred_cp",
                                          err = list(errName = "combined2",
                                                     typical = c("pkadd__err", "prop__err")))),
               "rx_pred_cp ~ add(pkadd__err) + prop(prop__err) + combined2() | rx_prd_cp")

  expect_equal(.handleSingleEndpoint(list(var = "rx_prd_cp",
                                          dist = "normal",
                                          pred = "rx_pred_cp",
                                          err = list(errName = "combined1",
                                                     typical = c("pkadd__err", "prop__err")))),
               "rx_pred_cp ~ add(pkadd__err) + prop(prop__err) + combined1() | rx_prd_cp")

  expect_equal(.handleSingleEndpoint(list(var = "rx_prd_cp",
                                          dist = "lognormal",
                                          pred = "rx_pred_cp",
                                          err = list(errName = "combined2",
                                                     typical = c("pkadd__err", "prop__err")))),
               "rx_pred_cp ~ lnorm(pkadd__err) + prop(prop__err) + combined2() | rx_prd_cp")

  expect_equal(.handleSingleEndpoint(list(var = "rx_prd_cp",
                                          dist = "logitnormal",
                                          pred = "rx_pred_cp",
                                          err = list(errName = "combined2",
                                                     typical = c("pkadd__err", "prop__err")))),
               "rx_pred_cp ~ logitNorm(pkadd__err) + prop(prop__err) + combined2() | rx_prd_cp")

  .ret <- list(var = "rx_prd_cp",
               dist = "probitnormal",
               pred = "rx_pred_cp",
               err = list(errName = "combined2",
                          typical = c("pkadd__err", "prop__err")))

  expect_equal(.handleSingleEndpoint(.ret),
               "rx_pred_cp ~ probitNorm(pkadd__err) + prop(prop__err) + combined2() | rx_prd_cp")

  .ret <- list(var = "rx_prd_cp",
               dist = "logitnormal",
               pred = "rx_pred_cp",
               err = list(errName = "proportional",
                          typical = c("prop__err")))

  expect_equal(.handleSingleEndpoint(.ret),
               "rx_pred_cp ~ logitNorm(NA) + prop(prop__err) | rx_prd_cp")

  .ret <- list(var = "rx_prd_cp",
               dist = "normal",
               pred = "rx_pred_cp",
               err = list(errName = "proportional",
                          typical = c("prop__err")))

  expect_equal(.handleSingleEndpoint(.ret),
               "rx_pred_cp ~ prop(prop__err) | rx_prd_cp")

  .ret <- list(var = "rx_prd_cp",
               dist = "normal",
               pred = "rx_pred_cp",
               err = list(errName = "constant",
                          typical = c("add__err")))

  expect_equal(.handleSingleEndpoint(.ret),
               "rx_pred_cp ~ add(add__err) | rx_prd_cp")

  .ret <- list(var = "rx_prd_cp",
               dist = "normal",
               pred = "rx_pred_cp",
               err = list(errName = "combined2c",
                          typical = c("pkadd__err", "prop__err", "tc")))

  expect_equal(.handleSingleEndpoint(.ret),
               "rx_pred_cp ~ add(pkadd__err) + pow(prop__err, tc) + combined2() | rx_prd_cp")

})
