.discTrans <- function(text) {
  vapply(.longDef(text)$endpoint, .handleSingleEndpoint, character(1))
}

test_that("Poisson count endpoints become pois()", {
  expect_equal(.discTrans("Y = {type=count, log(P(Y=k)) = -lambda + k*log(lambda) - factln(k)}"),
               "Y ~ pois(lambda)")
  expect_equal(.discTrans("Y = {type=count, P(Y = k) = exp(-lambda)*lambda^k/factorial(k)}"),
               "Y ~ pois(lambda)")
})

test_that("other count endpoints become ll()", {
  expect_equal(.discTrans("Y = {type=count, log(P(Y=k)) = -lambda + k*log(lambda) - factln(k) + a}"),
               "Y_logp <- -lambda + DV * log(lambda) - lfactorial(DV) + a\nll(Y) ~ Y_logp")
  expect_equal(.discTrans("Y = {type=count, P(Y=k) = p^k*(1-p)}"),
               "Y_p <- p^DV * (1 - p)\nll(Y) ~ log(Y_p)")
  .zip <- .discTrans("Y = {type=count,
if k == 0
  lpk = log(p0 + (1-p0)*exp(-lambda))
else
  lpk = log(1-p0) - lambda + k*log(lambda) - factln(k)
end
log(P(Y=k)) = lpk}")
  .zip <- strsplit(.zip, "\n")[[1]]
  expect_equal(.zip[1], "if (DV == 0) {")
  expect_equal(.zip[length(.zip) - 1], "Y_logp <- lpk")
  expect_equal(.zip[length(.zip)], "ll(Y) ~ Y_logp")
  expect_error(.discTrans("Y = {type=count, P(Y=k|Yp=j) = 1}"), "Markov")
})

test_that("categorical endpoints become a named ordinal c()", {
  expect_equal(.discTrans("Level = {type=categorical, categories={0, 1, 2},
logit(P(Level<=0)) = lp0,
logit(P(Level <= 1)) = lp0 + th2}"),
               paste("Level_le0 <- expit(lp0)",
                     "Level_le1 <- expit(lp0 + th2)",
                     "Level_p0 <- Level_le0",
                     "Level_p1 <- Level_le1 - Level_le0",
                     "Level ~ c(Level_p0=0, Level_p1=1, 2)", sep="\n"))
  expect_equal(.discTrans("Level = {type=categorical, categories={1, 2, 3}, P(Level=1) = p1\nP(Level=2) = max(p2, 0)}"),
               paste("Level_p1 <- p1",
                     "Level_p2 <- max(p2, 0)",
                     "Level ~ c(Level_p1=1, Level_p2=2, 3)", sep="\n"))
  expect_match(.discTrans("Level = {type=categorical, categories={1, 2}, probit(P(Level<=1)) = a}"),
               "Level_le1 <- pnorm(a)", fixed=TRUE)
  expect_match(.discTrans("Level = {type=categorical, categories={1, 2}, log(P(Level=1)) = a}"),
               "Level_p1 <- exp(a)", fixed=TRUE)
  expect_equal(.discTrans("Y = {type=categorical, categories={0, 1}, logit(P(Y=1)) = lp}"),
               "Y_p1 <- expit(lp)\nY ~ c(Y_p1=1, 0)")
  expect_error(.discTrans("Level = {type=categorical, categories={1, 2, 3}, P(Level=1) = p1}"),
               "categories 1, 2, 3")
  expect_error(.discTrans("Level = {type=categorical, categories={1, 2, 3}, P(Level=1) = p1, P(Level<=2) = p2}"),
               "categories 1, 2, 3")
  expect_error(.discTrans("Level = {type=categorical, categories={1, 2, 3}, P(Level<=2) = p1, P(Level<=3) = p2}"),
               "categories 1, 2, 3")
  .e <- tryCatch(.discTrans("Level = {type=categorical, categories={1, 2}, P(Level=1) = p1, P(Level=2) = p2}"),
                 error=function(e) conditionMessage(e))
  expect_false(grepl("Markov", .e))
})

test_that("discrete endpoints translate to an rxode2 model", {
  skip_if_not_installed("rxode2")
  .m <- function(endpoint) {
    eval(str2lang(paste0("function() {\nini({a <- 1})\nmodel({\nlambda <- a\nlp0 <- a\np1 <- 0.2\n",
                         endpoint, "\n})\n}")))
  }
  .ui <- rxode2::rxode2(.m(.discTrans("Y = {type=count, log(P(Y=k)) = -lambda + k*log(lambda) - factln(k)}")))
  expect_equal(as.character(.ui$predDf$distribution), "pois")
  .ui <- rxode2::rxode2(.m(.discTrans("Y = {type=count, log(P(Y=k)) = -lambda + k*log(lambda) - factln(k) + a}")))
  expect_equal(as.character(.ui$predDf$distribution), "LL")
  .ui <- rxode2::rxode2(.m(.discTrans("Level = {type=categorical, categories={0, 1}, logit(P(Level<=0)) = lp0}")))
  expect_equal(as.character(.ui$predDf$distribution), "ordinal")
})

test_that("a discrete endpoint has no prediction to alias", {
  .ld <- .longDef("y1 = {distribution=normal, prediction=Cc, errorModel=constant(a)}
Y = {type=categorical, categories={0, 1}, logit(P(Y=1)) = lp}")
  expect_equal(unname(.getMonolixPreds(.ld)), "Cc")
})

test_that("event endpoints become ll() of the hazard since the previous record", {
  .e <- .longDef("Event = {type=event, hazard=h}")$endpoint[[1]]
  .l <- strsplit(.handleSingleEndpoint(.e, 3L), "\n")[[1]]
  expect_equal(.l[1:3], c("Event_haz <- h", "d/dt(Event_cumhaz) <- Event_haz",
                          "Event_E <- (CMT == 3)"))
  expect_true("  Event_ll <- log(Event_haz) - Event_dH" %in% .l)
  expect_equal(.l[length(.l)], "ll(Event) ~ Event_ll")
  .e <- .longDef("Event = {type=event, eventType=intervalCensored, maxEventNumber=1, hazard=1/Te}")$endpoint[[1]]
  .l <- strsplit(.handleSingleEndpoint(.e, 2L), "\n")[[1]]
  expect_equal(.l[1], "Event_haz <- 1 / Te")
  expect_true("  Event_ll <- log(1 - exp(-Event_dH))" %in% .l)
})

test_that("an event hazard is not a prediction", {
  .ld <- .longDef("y1 = {distribution=normal, prediction=Cc, errorModel=constant(a)}
Event = {type=event, hazard=h}")
  expect_equal(unname(.getMonolixPreds(.ld)), "Cc")
})

test_that("an event project imports with its records and likelihood", {
  skip_if_not_installed("rxode2")
  .dir <- withr::local_tempdir()
  writeLines(c("ID,TIME,AMT,DV,EVID",
               "1,0,.,0,0", "1,3,10,.,1", "1,5,.,1,0", "1,7,.,1,0", "1,10,.,0,0",
               "2,2,.,0,0", "2,6,.,0,0",
               "3,0,.,0,0", "3,2,.,.,0", "3,5,.,1,0",
               "4,0,.,0,0", "4,5,.,0,0", "4,6,10,.,4", "4,8,.,1,0",
               "5,5,.,1,0", "5,10,.,0,0"),
             file.path(.dir, "data.csv"))
  writeLines(c("[LONGITUDINAL]", "input = {Te}", "", "EQUATION:",
               "ddt_A = -A", "h = 1/Te", "", "DEFINITION:",
               "Event = {type=event, hazard=h}", "", "OUTPUT:", "output = Event"),
             file.path(.dir, "model.txt"))
  writeLines(c("<DATAFILE>", "", "[FILEINFO]", "file = 'data.csv'", "delimiter = comma",
               "header = {ID, TIME, AMT, DV, EVID}", "", "[CONTENT]", "ID = {use=identifier}",
               "TIME = {use=time}", "AMT = {use=amount}", "EVID = {use=eventidentifier}",
               "DV = {use=observation, name=Event, type=event}", "", "<MODEL>", "",
               "[INDIVIDUAL]", "input = {Te_pop}", "", "DEFINITION:",
               "Te = {distribution=logNormal, typical=Te_pop, no-variability}", "",
               "[LONGITUDINAL]", "file = 'model.txt'", "", "<FIT>", "data = Event",
               "model = Event", "", "<PARAMETER>", "Te_pop = {value=10, method=MLE}"),
             file.path(.dir, "run.mlxtran"))
  .m <- suppressMessages(suppressWarnings(monolix2rx(file.path(.dir, "run.mlxtran"))))
  .k <- .m$predDf$cmt
  expect_true(paste0("Event_E <- (CMT == ", .k, ")") %in%
                vapply(.m$lstExpr, deparse1, character(1)))
  .d <- .m$monolixData
  .rec <- !is.na(.d$dv)
  expect_equal(.d$cmt[.rec], rep("Event", sum(.rec)))
  expect_false(any(.d$cmt[!.rec] %in% "Event"))
  .s <- suppressMessages(rxode2::rxSolve(.m, .d, returnType="data.frame"))
  .s <- .s[!is.na(.s$DV) | !"DV" %in% names(.s), ]
  .ll <- stats::setNames(.s$Event_ll, paste(.s$id, .s$time))
  ## the first record starts the observation; doses and a missing DV are
  ## not records; the washout restarts the hazard; an event on the first
  ## record counts from time 0
  .h <- log(0.1)
  expect_equal(unname(.ll[c("1 0", "1 5", "1 7", "1 10", "2 2", "2 6", "3 0", "3 5",
                            "4 0", "4 5", "4 8", "5 5", "5 10")]),
               c(0, .h - 0.5, .h - 0.2, -0.3, 0, -0.4, 0, .h - 0.5,
                 0, -0.5, .h - 0.2, .h - 0.5, -0.5),
               tolerance=1e-6)
})
