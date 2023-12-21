test_that("content", {

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

  tmp2 <- list(use1 = c(identifier = "ID", time = "TIME", eventidentifier = "EVID",
                                  amount = "AMT", interdoseinterval = NA, censored = NA, limit = NA,
                                  observationtype = "YTYPE", administration = "ADM", steadystate = NA,
                        observation = "DV"),
               cont = c("WT", "CRCL"),
               cat = list(Race = list(cat = c("Caucasian", "Black", "Latin"),
                                      quote = c(FALSE, FALSE, FALSE))),
               reg = c("E0", "Emax"),
               nbdoses = 7L,
               yname = c("1", "2"),
               name = c("y1", "y2"))
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
                        amount = "AMT", interdoseinterval = "II", censored = "cens",
                        limit = "limit", observationtype = "YTYPE", administration = "ADM",
                        steadystate = "SS", observation = "DV"),
               cont = c("WT", "CRCL"),
               cat = list(Race = list(cat = c("Caucasian", "Black", "Latin"),
                                      quote = c(FALSE, FALSE, FALSE))),
               reg = c("E0", "Emax"),
               nbdoses = 10L,
               yname = c("1", "2"),
               name = c("y1", "y2"))

  class(tmp2) <- "monolix2rxContent"
  expect_equal(tmp, tmp2)

  expect_error(.content("DV = {use=observation, name={y1, y2, y3}, yname={'1', '2'}, type={continuous, continuous}}"))
  expect_error(.content("DV = {use=observation, name=y1, yname='1', type=continuous}"), NA)

})
