test_that("content", {

  tmp <- .content("DV = {use=observation, name=y1, yname='1', type=continuous}")

  expect_equal(as.character(tmp),
               "DV = {use=observation, name=y1, yname='1', type=continuous}" )

  tmp <- .content("SS = {use=steadystate, nbdoses=10}")

  expect_equal(as.character(tmp),
               "SS = {use=steadystate, nbdoses=10}" )

  tmp <- .content("WT={use=covariate, type=continuous}")
  expect_equal(as.character(tmp),
               "WT = {use=covariate, type=continuous}")

  tmp <- .content("ID = {use=identifier}
TIME = {use=time}
EVID = {use=eventidentifier}
AMT = {use=amount}
DV = {use=observation, name={y1, y2}, yname={'1', '2'}, type={continuous, continuous}}
ADM = {use=administration}
YTYPE = {use=observationtype}
WT={use=covariate, type=continuous}
CRCL={use=covariate, type=continuous}
E0 = {use = regressor}
Emax = {use = regressor}
Race = {type=categorical, categories={Caucasian, Black, Latin}}
Sex = {type=categorical, categories={M, F}}")

  expect_snapshot(print(tmp))
  expect_error(as.list(tmp), NA)

  tmp2 <- list(use1 = c(identifier = "ID", time = "TIME", eventidentifier = "EVID",
                        amount = "AMT", interdoseinterval = NA, censored = NA, limit = NA,
                        observationtype = "YTYPE", administration = "ADM", steadystate = NA,
                        observation = "DV", occasion=NA, rate=NA, additionaldose=NA, missingdependentvariable=NA),
               cont = c("WT", "CRCL"),
               cat = list(Race = list(cat = c("Caucasian", "Black", "Latin"),
                                      quote = c(FALSE, FALSE, FALSE)),
                          Sex = list(cat = c("M", "F"),
                                     quote = c(FALSE, FALSE))),
               reg = c("E0", "Emax"),
               nbdoses = 7L,
               yname = c("1", "2"),
               ynameQuote=c(TRUE, TRUE),
               ytype=character(0),
               ytypeQuote=logical(0),
               name = c("y1", "y2"),
               type = c("continuous",  "continuous"))
  class(tmp2) <- "monolix2rxContent"

  expect_equal(tmp, tmp2)

  tmp <- .content("ID = {use=identifier}
TIME = {use=time}
EVID = {use=eventidentifier}
AMT = {use=amount}
DV = {use=observation, name={y1, y2}, yname={'1', '2'}, type={continuous, continuous}}
II = {use=interdoseinterval}
SS = {use=steadystate, nbdoses=10}
ADM = {use=administration}
YTYPE = {use=observationtype}
WT={use=covariate, type=continuous}
CRCL = {use=covariate, type=continuous}
E0 = {use = regressor}
Emax = {use = regressor}
Race = {type=categorical, categories={Caucasian, Black, Latin}}
Sex = {type=categorical, categories={M, F}}
cens = {use=censored}
limit = {use=limit}")

  tmp2 <- list(use1 = c(identifier = "ID", time = "TIME", eventidentifier = "EVID",
                        amount = "AMT", interdoseinterval = "II", censored = "cens", limit = "limit",
                        observationtype = "YTYPE", administration = "ADM", steadystate = "SS", observation = "DV",
                        occasion=NA, rate=NA, additionaldose=NA, missingdependentvariable=NA),
               cont = c("WT", "CRCL"),
               cat = list(Race = list(cat = c("Caucasian", "Black", "Latin"),
                                     quote = c(FALSE, FALSE, FALSE)),
                          Sex = list(cat = c("M", "F"),
                                     quote = c(FALSE, FALSE))),
               reg = c("E0", "Emax"),
               nbdoses = 10L,
               yname = c("1", "2"),
               ynameQuote=c(TRUE, TRUE),
               ytype=character(0),
               ytypeQuote=logical(0),
               name = c("y1",  "y2"),
               type = c("continuous", "continuous"))
  class(tmp2) <- "monolix2rxContent"

  expect_equal(tmp, tmp2)

  expect_error(.content("DV = {use=observation, name={y1, y2, y3}, yname={'1', '2'}, type={continuous, continuous}}"))
  expect_error(.content("DV = {use=observation, name=y1, yname='1', type=continuous}"), NA)

  tmp <- .content("ID = {use=identifier}
TIME = {use=time}
EVID = {use=eventidentifier}
AMT = {use=amount}
DV = {use=observation, name={y1, y2}, yname={'1', '2'}, type={continuous, event}}
ADM = {use=administration}
YTYPE = {use=observationtype}
WT={use=covariate, type=continuous}
CRCL={use=covariate, type=continuous}
E0 = {use = regressor}
Emax = {use = regressor}
Race = {type=categorical, categories={Caucasian, Black, Latin}}
Sex = {type=categorical, categories={M, F}}")

  expect_equal(tmp$type, c("continuous", "event"))


})

test_that("single name=y works", {
  expect_equal(as.character(.content("y={use=observation, name=y, type=continuous}")),
               "y = {use=observation, name=y, type=continuous}")
})

test_that("{use=covariate, type=categorical} works", {
  expect_equal(as.character(.content("sex={use=covariate,type=categorical}")),
               "sex = {use=covariate, type=categorical}")
})

test_that("complex dv observation works", {
  expect_equal(as.character(.content("dv={use=observation, yname={'1', '2'}, type={continuous, continuous}}")),
               "dv = {use=observation, yname={'1', '2'}, type={continuous, continuous}}")
})


test_that("use occ", {
  expect_equal(as.character(.content("DOSE_OCC={use = occasion}")),
               "DOSE_OCC = {use=occasion}")
})

test_that("use rate", {
  expect_equal(as.character(.content("RATE={use=rate}")),
               "RATE = {use=rate}")
})

test_that("use ytype", {
  expect_equal(as.character(.content("Y = {use=observation, name={y1, y2, y3}, ytype={1, 2, 3}, type={continuous, continuous, continuous}}")),
               "Y = {use=observation, name={y1, y2, y3}, ytype={1, 2, 3}, type={continuous, continuous, continuous}}")
})

test_that("use addl = {use=additionaldose}", {
  expect_equal(as.character(.content("addl={use=additionaldose}")),
               "addl = {use=additionaldose}")
})

test_that("discrete", {
  expect_equal(as.character(.content("Y = {use=observation, name={Concentration, Seizure}, yname={'1', '2'}, type={continuous, discrete}}")),
               "Y = {use=observation, name={Concentration, Seizure}, yname={'1', '2'}, type={continuous, discrete}}")
})

test_that("mdv", {
  expect_equal(as.character(.content("mdv = {use=missingdependentvariable}")),
                "mdv = {use=missingdependentvariable}")
})
