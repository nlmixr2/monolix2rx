test_that("[LONGITUDINAL] DEFINITION:", {

  tmp <- .longDef("rx_prd_cp={distribution = normal, prediction = rx_pred_cp, errorModel=combined2(pkadd__err,prop__err)}
           rx_prd_effect={distribution = normal, prediction = rx_pred_effect, errorModel=constant(pdadd__err)}")

  tmp2 <- list(endpoint = list(list(var = "rx_prd_cp",
                                    dist = "normal",
                                    pred = "rx_pred_cp",
                                    err = list(errName = "combined2",
                                               typical = c("pkadd__err", "prop__err")),
                                    autocor=character(0)),
                               list(var = "rx_prd_effect",
                                    dist = "normal",
                                    pred = "rx_pred_effect",
                                    err = list(errName = "constant", typical = "pdadd__err"),
                                    autocor=character(0))),
               fixed = numeric(0))
  class(tmp2) <- "monolix2rxLongDef"

  expect_equal(tmp, tmp2)

  tmp <- .longDef("rx_prd_cp={distribution = normal, prediction = rx_pred_cp, errorModel=combined2(pkadd__err,1)}
           rx_prd_effect={distribution = normal, prediction = rx_pred_effect, errorModel=constant(pdadd__err)}")

  tmp2 <- list(endpoint = list(list(var = "rx_prd_cp",
                                    dist = "normal",
                                    pred = "rx_pred_cp",
                                    err = list(errName = "combined2",
                                               typical = c("pkadd__err", "rx_rx_prd_cp_combined2_2")),
                                    autocor=character(0)),
                               list(var = "rx_prd_effect",
                                    dist = "normal",
                                    pred = "rx_pred_effect",
                                    err = list(errName = "constant", typical = "pdadd__err"),
                                    autocor=character(0))),
               fixed = c(rx_rx_prd_cp_combined2_2 = 1))
  class(tmp2) <- "monolix2rxLongDef"

  expect_equal(tmp, tmp2)


  tmp <- .longDef("Seizure = {type = event, eventType = intervalCensored, maxEventNumber = 1,
rightCensoringTime = 120, intervalLength = 10, hazard = haz}")

  expect_snapshot(print(tmp))
  expect_error(as.list(tmp), NA)

  tmp2 <- list(endpoint = list(list(var = "Seizure",
                                    dist = "event",
                                    pred = "haz",
                                    err = list(eventType = "intervalCensored",
                                               maxEventNumber = 1L,
                                               rightCensoringTime = 120,
                                               intervalLength = 10),
                                    autocor=character(0))),
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
                                               intervalLength = NA_real_),
                                    autocor=character(0))),
               fixed = numeric(0))
  class(tmp2) <- "monolix2rxLongDef"

  expect_equal(tmp, tmp2)


  tmp <- .longDef("level = {type = categorical, categories = {0, 1, 2, 3},
logit(P(level <=0)) = th1
logit(P(level <=1)) = th1 + th2
logit(P(level <=2)) = th1 + th2 + th3}")

  expect_equal(as.character(tmp), "level = {type=categorical, categories={0, 1, 2, 3},\nlogit(P(level <=0)) = th1\nlogit(P(level <=1)) = th1 + th2\nlogit(P(level <=2)) = th1 + th2 + th3}")


  tmp <- .longDef("Level = {type = categorical, categories={1,2,3}
            logit(P(Level<=1)) = lp1
            logit(P(Level<=2)) = lp2
}")

  expect_equal(as.character(tmp),
               "Level = {type=categorical, categories={1, 2, 3},\nlogit(P(Level<=1)) = lp1\n            logit(P(Level<=2)) = lp2}")


  tmp2 <- list(endpoint = list(list(var = "Level",
                                    dist = "categorical",
                                    pred = NA_character_,
                                    err = list(categories = 1:3,
                                               code = c("logit(P(Level<=1)) = lp1",
                                                        "            logit(P(Level<=2)) = lp2")),
                                    autocor=character(0))),
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
                                                        "logit(P(State <=2|State_p=3)) = a31+a32")),
                                    autocor=character(0))),
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
                                                       "transitionRate(2,1) = q21")),
                                   autocor=character(0))),
              fixed = numeric(0))
  class(tmp2) <- "monolix2rxLongDef"
  expect_equal(tmp, tmp2)


  tmp <- .longDef("y = {type=count, P(y=k) = exp(-lambda)*(lambda^k)/factorial(k)}")

  expect_equal(as.character(tmp),
               "y = {type=count,\nP(y=k) = exp(-lambda)*(lambda^k)/factorial(k)}")

  tmp2 <- list(endpoint = list(list(var = "y",
                                    dist = "count",
                                    pred = NA_character_,
                                    err = list(code = "P(y=k) = exp(-lambda)*(lambda^k)/factorial(k)"),
                                    autocor=character(0)
                                    )),
               fixed = numeric(0))
  class(tmp2) <- "monolix2rxLongDef"
  expect_equal(tmp, tmp2)

})


test_that(".setCategoriesInt()", {
  .monolix2rx$categoriesInt <- 100L
  expect_error(.setCategoriesInt("1"))
  .monolix2rx$categoriesInt <- integer(0)
})

test_that("combined1()", {

  tmp <- .longDef("rx_prd_cp={distribution = normal, prediction = rx_pred_cp, errorModel=combined1(pkadd__err,1)}
           rx_prd_effect={distribution = normal, prediction = rx_pred_effect, errorModel=proportional(pdadd__err)}")

  expect_equal(tmp$endpoint[[1]]$err$errName, "combined1")

  expect_equal(tmp$endpoint[[2]]$err$errName, "proportional")


  tmp <- .longDef("rx_prd_cp={distribution = normal, prediction = rx_pred_cp, errorModel=combined1c(pkadd__err, pkprop_sd,4)}
           rx_prd_effect={distribution = normal, prediction = rx_pred_effect, errorModel=proportional(pdadd__err)}")

  expect_equal(tmp$endpoint[[1]]$err$errName,
               "combined1c")
})

test_that("as.list works", {

  tmp <- .longDef("rx_prd_cp={distribution = normal, prediction = rx_pred_cp, errorModel=combined1c(pkadd__err, pkprop_sd,4)}
           rx_prd_effect={distribution = normal, prediction = rx_pred_effect, errorModel=proportional(pdadd__err)}")

  expect_error(as.list(tmp), NA)

})

test_that("autocorrelation works", {

  expect_equal(as.character(.longDef("rx_prd_effect={distribution = normal, prediction = rx_pred_effect, errorModel=proportional(pdadd__err), autoCorrCoef = r}")),
               "rx_prd_effect = {distribution=normal, prediction=rx_pred_effect, errorModel=proportional(pdadd__err), autoCorrCoef=r}")
})

test_that("min/max works with logitnorm", {
  expect_equal(as.character(.longDef("Effect = {distribution=logitNormal, min=0, max=100, prediction=E, errorModel=constant(a)}")),
               "Effect = {distribution=logitnormal, prediction=E, min=0, max=100, errorModel=constant(a)}")
})


test_that("event with missing comma works, even though it technically is malformed", {
  expect_equal(as.character(.longDef("Event = {type=event, hazard = h
  eventType=intervalCensored,
  intervalLength=5     ; used for the graphics (not mandatory)
}")),
"Event = {type=event, eventType=intervalCensored, intervalLength=5, hazard=h}")
})


test_that("complex count def", {
  expect_equal(as.character(.longDef("Event = {type=count,
if k>0
  lpk = - HAZ + k*log(HAZ) - factln(k)
else
  lpk = -HAZ
end
log(P(Event=k)) = lpk
}")), "Event = {type=count,\nif k>0\n  lpk = - HAZ + k*log(HAZ) - factln(k)\nelse\n  lpk = -HAZ\nend\nlog(P(Event=k)) = lpk}")
})


test_that("covariate transform", {

  tmp <- .longDef("tSex = {
     transform = 'sex',
     categories = {
        'F' = {'0'},
         'M' = {'1'}  },
    reference = 'M'}

tSTUDYIDN =
{
  transform = STUDYID,
  categories = {
  S1 = 1,
  S2_3 = {2, 3},
  S4 = S4,
  S5 = S5
  },
  reference = S2_3
}
")

  expect_equal(as.character(tmp),
               c("tSex = {transform='sex', categories={'F'={'0'}, 'M'={'1'}}, reference='M'}",
                 "tSTUDYIDN = {transform=STUDYID, categories={S1=1, S2_3={2, 3}, S4=S4, S5=S5}, reference=S2_3}"
                 ))

  tmp <- .longDef(" tSex = {
     transform = 'sex',
     categories = {
        'F' = {'0'},
         'M' = {'1'}  },
    reference = 'M'}")

  expect_equal(as.character(tmp),
               "tSex = {transform='sex', categories={'F'={'0'}, 'M'={'1'}}, reference='M'}")

  tmp <- .longDef(" tSex = {
     transform = sex,
     categories = {
        F = {0},
         M = {1}  },
    reference = M}")


  expect_equal(as.character(tmp),
               "tSex = {transform=sex, categories={F={0}, M={1}}, reference=M}")

  tmp <- .longDef(" tSex = {
     transform = sex,
     categories = {
        F = 0,
        'M' = '1'  },
    reference = M}")

  expect_equal(as.character(tmp),
               "tSex = {transform=sex, categories={F=0, 'M'='1'}, reference=M}")

  tmp <- .longDef("tAPGAR =
{
  transform = APGAR,
  categories = {
  High = {10, 8, 9},
  Low = {1, 2, 3},
  Med = {4, 5, 6, 7}  },
  reference = Med
}")

  expect_equal(as.character(tmp),
               "tAPGAR = {transform=APGAR, categories={High={10, 8, 9}, Low={1, 2, 3}, Med={4, 5, 6, 7}}, reference=Med}")


  tmp <- .longDef("Event = {type=event,
              hazard=1/Te,
              eventType=intervalCensored,
              intervalLength=5,     ; used for the graphics (not mandatory)
              rightCensoringTime=200  ; used for the graphics (not mandatory)
}")

  expect_equal(as.character(tmp),
               "Event = {type=event, eventType=intervalCensored, rightCensoringTime=200, intervalLength=5, hazard=1/Te}")

})
