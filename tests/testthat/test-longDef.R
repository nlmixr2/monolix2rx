test_that("[LONGITUDINAL] DEFINITION:", {

  tmp <- .longDef("rx_prd_cp={distribution = normal, prediction = rx_pred_cp, errorModel=combined2(pkadd__err,prop__err)}
           rx_prd_effect={distribution = normal, prediction = rx_pred_effect, errorModel=constant(pdadd__err)}")

  tmp2 <- list(endpoint = list(list(var = "rx_prd_cp",
                                    dist = "normal",
                                    pred = "rx_pred_cp",
                                    err = list(errName = "combined2",
                                               typical = c("pkadd__err", "prop__err"))),
                               list(var = "rx_prd_effect",
                                    dist = "normal",
                                    pred = "rx_pred_effect",
                                    err = list(errName = "constant", typical = "pdadd__err"))),
               fixed = numeric(0))
  class(tmp2) <- "monolix2rxLongDef"

  expect_equal(tmp, tmp2)

  tmp <- .longDef("rx_prd_cp={distribution = normal, prediction = rx_pred_cp, errorModel=combined2(pkadd__err,1)}
           rx_prd_effect={distribution = normal, prediction = rx_pred_effect, errorModel=constant(pdadd__err)}")

  tmp2 <- list(endpoint = list(list(var = "rx_prd_cp",
                                    dist = "normal",
                                    pred = "rx_pred_cp",
                                    err = list(errName = "combined2",
                                               typical = c("pkadd__err", "rx_rx_prd_cp_combined2_2"))),
                               list(var = "rx_prd_effect",
                                    dist = "normal",
                                    pred = "rx_pred_effect",
                                    err = list(errName = "constant", typical = "pdadd__err"))),
               fixed = c(rx_rx_prd_cp_combined2_2 = 1))
  class(tmp2) <- "monolix2rxLongDef"

  expect_equal(tmp, tmp2)


  tmp <- .longDef("Seizure = {type = event, eventType = intervalCensored, maxEventNumber = 1,
rightCensoringTime = 120, intervalLength = 10, hazard = haz}")

  tmp2 <- list(endpoint = list(list(var = "Seizure",
                                    dist = "event",
                                    pred = "haz",
                                    err = list(eventType = "intervalCensored",
                                               maxEventNumber = 1L,
                                               rightCensoringTime = 120,
                                               intervalLength = 10))),
               fixed = numeric(0))
  class(tmp2) <- "monolix2rxLongDef"

  expect_equal(tmp, tmp2)

  tmp <- .longDef("Seizure = {type = event, hazard = haz}")

  tmp2 <- list(endpoint = list(list(var = "Seizure",
                                    dist = "event",
                                    pred = "haz",
                                    err = list(eventType = "exact",
                                               maxEventNumber = NA_integer_,
                                               rightCensoringTime = NA_real_,
                                               intervalLength = NA_real_))),
               fixed = numeric(0))
  class(tmp2) <- "monolix2rxLongDef"

  expect_equal(tmp, tmp2)


  tmp <- .longDef("level = {type = categorical, categories = {0, 1, 2, 3},
logit(P(level <=0)) = th1
logit(P(level <=1)) = th1 + th2
logit(P(level <=2)) = th1 + th2 + th3}")

  tmp2 <- list(endpoint = list(list(var = "level",
                                    dist = "categorical",
                                    pred = NA_character_,
                                    err = list(categories = 0:3,
                                               code = c("logit(P(level <=0)) = th1",
                                                        "logit(P(level <=1)) = th1 + th2",
                                                        "logit(P(level <=2)) = th1 + th2 + th3")))),
               fixed = numeric(0))
  class(tmp2) <- "monolix2rxLongDef"
  expect_equal(tmp, tmp2)

  tmp <- .longDef("State = {type = categorical, categories = {1,2,3}, dependence = Markov
P(State_1=1) = a1
P(State_1=2) = a2
logit(P(State <=1|State_p=1)) = a11
logit(P(State <=2|State_p=1)) = a11+a12
logit(P(State <=1|State_p=2)) = a21
logit(P(State <=2|State_p=2)) = a21+a22
logit(P(State <=1|State_p=3)) = a31
logit(P(State <=2|State_p=3)) = a31+a32}")

  tmp2 <- list(endpoint = list(list(var = "State",
                                    dist = "categorical",
                                    pred = NA_character_,
                                    err = list(categories = 1:3,
                                               code = c("dependence = Markov",
                                                        "P(State_1=1) = a1",
                                                        "P(State_1=2) = a2",
                                                        "logit(P(State <=1|State_p=1)) = a11",
                                                        "logit(P(State <=2|State_p=1)) = a11+a12",
                                                        "logit(P(State <=1|State_p=2)) = a21",
                                                        "logit(P(State <=2|State_p=2)) = a21+a22",
                                                        "logit(P(State <=1|State_p=3)) = a31",
                                                        "logit(P(State <=2|State_p=3)) = a31+a32")))),
               fixed = numeric(0))
  class(tmp2) <- "monolix2rxLongDef"
  expect_equal(tmp, tmp2)

  # FIXME; in the example dependence=Markov doesn't have a comma after it; is that legal or not
  tmp <- .longDef("State = {type = categorical, categories = {1,2}, dependence = Markov
P(State_1=1) = p1
transitionRate(1,2) = q12
transitionRate(2,1) = q21}")

  tmp2 <- list(endpoint = list(list(var = "State",
                                   dist = "categorical",
                                   pred = NA_character_,
                                   err = list(categories = 1:2,
                                              code = c("dependence = Markov",
                                                       "P(State_1=1) = p1",
                                                       "transitionRate(1,2) = q12",
                                                       "transitionRate(2,1) = q21")))),
              fixed = numeric(0))
  class(tmp2) <- "monolix2rxLongDef"
  expect_equal(tmp, tmp2)


  tmp <- .longDef("y = {type=count, P(y=k) = exp(-lambda)*(lambda^k)/factorial(k)}")

  tmp2 <- list(endpoint = list(list(var = "y",
                                    dist = "count",
                                    pred = NA_character_,
                                    err = list(code = "P(y=k) = exp(-lambda)*(lambda^k)/factorial(k)"))),
               fixed = numeric(0))
  class(tmp2) <- "monolix2rxLongDef"
  expect_equal(tmp, tmp2)

})