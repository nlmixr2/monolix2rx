.mixMlx <- function(def) {
  list(MODEL=list(INDIVIDUAL=list(DEFINITION=.indDef(def))))
}

test_that("wsmm() is the weighted prediction", {
  .mlx <- .mixMlx("p1 = {distribution=logitNormal, typical=p1_pop, sd=omega_p1}")
  .m <- .mixtureRewrite(quote(model({Cc <- wsmm(C1, p1, max(C2, 1), 1 - p1)})), .mlx)
  expect_null(.m$prob)
  expect_equal(.m$model, quote(model({Cc <- (p1) * (C1) + (1 - p1) * (max(C2, 1))})))
})

test_that("bsmm() is mix() with population probabilities", {
  .mlx <- .mixMlx(paste("p1 = {distribution=logitNormal, typical=p1_pop, no-variability}",
                        "p2 = {distribution=normal, typical=p2_pop, no-variability}",
                        "V = {distribution=logNormal, typical=V_pop, sd=omega_V}", sep="\n"))
  .m <- .mixtureRewrite(quote(model({
    p1 <- expit(p1_pop, 0, 1)
    p2 <- p2_pop
    Cc <- bsmm(C1, p1, C2, p2, C3, 1 - p1 - p2)
    Ce <- bsmm(E1, p1, E2, p2, E3, 1 - p1 - p2)
  })), .mlx)
  expect_equal(.m$prob$name, c("p1_pop", "p2_pop"))
  expect_equal(.m$model, quote(model({
    p1 <- p1_pop
    p2 <- p2_pop
    Cc <- mix(C1, p1_pop, C2, p2_pop, C3)
    Ce <- mix(E1, p1_pop, E2, p2_pop, E3)
  })))
  # a literal probability is a fixed parameter
  .l <- .mixtureRewrite(quote(model({Cc <- bsmm(C1, 0.3, C2, 0.7)})), .mlx)
  expect_equal(.l$model, quote(model({Cc <- mix(C1, rxBsmmP1, C2)})))
  .pars <- .parameter("p1_pop = {value=0.3, method=MLE}\np2_pop = {value=0.2, method=FIXED}")
  .ini <- .mixtureIni(quote(ini({p1_pop <- -0.8; p2_pop <- 0.2; V <- 1})), .m$prob, .pars)
  expect_equal(.ini, quote(ini({p1_pop <- 0.3; p2_pop <- fixed(0.2); V <- 1})))
  .ini <- .mixtureIni(quote(ini({V <- 1})), .l$prob, .pars)
  expect_equal(.ini, quote(ini({V <- 1; rxBsmmP1 <- fixed(0.3)})))
})

test_that("bsmm() probabilities that vary by subject are refused", {
  .mlx <- .mixMlx(paste("p1 = {distribution=logitNormal, typical=p1_pop, sd=omega_p1}",
                        "q = {distribution=logitNormal, typical=q_pop, covariate=wt, coefficient=b, no-variability}",
                        sep="\n"))
  expect_error(.mixtureRewrite(quote(model({Cc <- bsmm(C1, p1, C2, 1 - p1)})), .mlx),
               "without variability")
  expect_error(.mixtureRewrite(quote(model({Cc <- bsmm(C1, q, C2, 1 - q)})), .mlx),
               "without variability")
  expect_error(.mixtureRewrite(quote(model({Cc <- bsmm(C1, 2 * p, C2, 1 - p)})), .mlx),
               "without variability")
  .ok <- .mixMlx(paste("p1 = {distribution=logitNormal, typical=p1_pop, no-variability}",
                       "p2 = {distribution=logitNormal, typical=p2_pop, no-variability}",
                       "V = {distribution=logNormal, typical=V_pop, sd=omega_V}", sep="\n"))
  expect_error(.mixtureRewrite(quote(model({
    Cc <- bsmm(C1, p1, C2, 1 - p1)
    Ce <- bsmm(E1, p2, E2, 1 - p2)
  })), .ok), "same probabilities")
})

test_that("malformed bsmm() calls and missing etas give clear errors", {
  .ok <- .mixMlx(paste("p1 = {distribution=logitNormal, typical=p1_pop, no-variability}",
                       "V = {distribution=logNormal, typical=V_pop, sd=omega_V}", sep="\n"))
  expect_error(.mixtureRewrite(quote(model({Cc <- bsmm(C1, 1)})), .ok), "at least two")
  expect_warning(.mixtureRewrite(quote(model({Cc <- bsmm(C1, p1, C2, p2)})), .ok),
                 "1 minus the others")
  expect_warning(.mixtureRewrite(quote(model({Cc <- bsmm(C1, 0.3, C2, 0.3)})), .ok),
                 "1 minus the others")
  expect_warning(.mixtureRewrite(quote(model({Cc <- bsmm(C1, 0.3, C2, 0.7)})), .ok), NA)
  .two <- .mixMlx(paste("p1 = {distribution=logitNormal, typical=p1_pop, no-variability}",
                        "p2 = {distribution=logitNormal, typical=p2_pop, no-variability}",
                        "V = {distribution=logNormal, typical=V_pop, sd=omega_V}", sep="\n"))
  for (.last in list(quote(1 - (p1 + p2)), quote(1 - p2 - p1), quote((1 - p1) - p2))) {
    .e <- bquote(model({Cc <- bsmm(C1, p1, C2, p2, C3, .(.last))}))
    expect_warning(.mixtureRewrite(.e, .two), NA)
  }
  .noEta <- .mixMlx("p1 = {distribution=logitNormal, typical=p1_pop, no-variability}")
  expect_error(.mixtureRewrite(quote(model({Cc <- bsmm(C1, p1, C2, 1 - p1)})), .noEta),
               "between-subject variability")
})

test_that("the equation block passes bsmm()/wsmm() through", {
  expect_equal(.equation("Cc = bsmm(C1, p1, C2, 1-p1)\nCw = wsmm(C1, p1, t, 1-p1)")$rx,
               c("Cc <- bsmm(C1, p1, C2, 1 - p1)", "Cw <- wsmm(C1, p1, time, 1 - p1)"))
})
