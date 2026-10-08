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
