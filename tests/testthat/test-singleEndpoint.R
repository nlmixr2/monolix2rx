test_that("test single endpoint handling", {

  expect_equal(.handleSingleEndpoint(list(var = "rx_prd_cp",
                                          dist = "normal",
                                          pred = "rx_pred_cp",
                                          err = list(errName = "combined2",
                                                     typical = c("pkadd__err", "prop__err")))),
               "rx_prd_cp <- rx_pred_cp\nrx_prd_cp ~ add(pkadd__err) + prop(prop__err) + combined2()")

  expect_equal(.handleSingleEndpoint(list(var = "rx_prd_cp",
                                          dist = "normal",
                                          pred = "rx_pred_cp",
                                          err = list(errName = "combined1",
                                                     typical = c("pkadd__err", "prop__err")))),
               "rx_prd_cp <- rx_pred_cp\nrx_prd_cp ~ add(pkadd__err) + prop(prop__err) + combined1()")

  expect_equal(.handleSingleEndpoint(list(var = "rx_prd_cp",
                                          dist = "lognormal",
                                          pred = "rx_pred_cp",
                                          err = list(errName = "combined2",
                                                     typical = c("pkadd__err", "prop__err")))),
               "rx_prd_cp <- rx_pred_cp\nrx_prd_cp ~ lnorm(pkadd__err) + prop(prop__err) + combined2()")

  expect_equal(.handleSingleEndpoint(list(var = "rx_prd_cp",
                                          dist = "logitnormal",
                                          pred = "rx_pred_cp",
                                          err = list(errName = "combined2",
                                                     typical = c("pkadd__err", "prop__err")))),
               "rx_prd_cp <- rx_pred_cp\nrx_prd_cp ~ logitNorm(pkadd__err) + prop(prop__err) + combined2()")

  .ret <- list(var = "rx_prd_cp",
               dist = "probitnormal",
               pred = "rx_pred_cp",
               err = list(errName = "combined2",
                          typical = c("pkadd__err", "prop__err")))

  expect_equal(.handleSingleEndpoint(.ret),
               "rx_prd_cp <- rx_pred_cp\nrx_prd_cp ~ probitNorm(pkadd__err) + prop(prop__err) + combined2()")

  .ret <- list(var = "rx_prd_cp",
               dist = "logitnormal",
               pred = "rx_pred_cp",
               err = list(errName = "proportional",
                          typical = c("prop__err")))

  expect_equal(.handleSingleEndpoint(.ret),
               "rx_prd_cp <- rx_pred_cp\nrx_prd_cp ~ logitNorm(NA) + prop(prop__err)")

  .ret <- list(var = "rx_prd_cp",
               dist = "normal",
               pred = "rx_pred_cp",
               err = list(errName = "proportional",
                          typical = c("prop__err")))

  expect_equal(.handleSingleEndpoint(.ret),
               "rx_prd_cp <- rx_pred_cp\nrx_prd_cp ~ prop(prop__err)")

  .ret <- list(var = "rx_prd_cp",
               dist = "normal",
               pred = "rx_pred_cp",
               err = list(errName = "constant",
                          typical = c("add__err")))

  expect_equal(.handleSingleEndpoint(.ret),
               "rx_prd_cp <- rx_pred_cp\nrx_prd_cp ~ add(add__err)")

  .ret <- list(var = "rx_prd_cp",
               dist = "normal",
               pred = "rx_pred_cp",
               err = list(errName = "combined2c",
                          typical = c("pkadd__err", "prop__err", "tc")))

  expect_equal(.handleSingleEndpoint(.ret),
               "rx_prd_cp <- rx_pred_cp\nrx_prd_cp ~ add(pkadd__err) + pow(prop__err, tc) + combined2()")

  tmp <- .longDef("Seizure = {type = event, eventType = intervalCensored, maxEventNumber = 1,
rightCensoringTime = 120, intervalLength = 10, hazard = haz}")

  expect_error(.handleSingleEndpoint(tmp$endpoint[[1]]))

  tmp <- .longDef("State = {type = categorical, categories = {1,2,3}, dependence = Markov
P(State_1=1) = a1
P(State_1=2) = a2
logit(P(State <=1|State_p=1)) = a11
logit(P(State <=2|State_p=1)) = a11+a12
logit(P(State <=1|State_p=2)) = a21
logit(P(State <=2|State_p=2)) = a21+a22
logit(P(State <=1|State_p=3)) = a31
logit(P(State <=2|State_p=3)) = a31+a32}")

  expect_error(.handleSingleEndpoint(tmp$endpoint[[1]]))

  tmp <- .longDef("y = {type=count, P(y=k) = exp(-lambda)*(lambda^k)/factorial(k)}")

  expect_error(.handleSingleEndpoint(tmp$endpoint[[1]]))

  tmp <- .longDef("rx_prd_cp={distribution = normal, prediction = rx_pred_cp, errorModel=combined2c(pkadd__err,prop__err, tc)}")

  expect_equal(.handleSingleEndpoint(tmp$endpoint[[1]]),
               "rx_prd_cp <- rx_pred_cp\nrx_prd_cp ~ add(pkadd__err) + pow(prop__err, tc) + combined2()")

})
