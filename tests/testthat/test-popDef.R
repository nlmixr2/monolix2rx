test_that("MAP priors in [POPULATION] DEFINITION: become ini() priors", {
  skip_on_cran()
  .dir <- file.path(tempfile(), "theo")
  dir.create(.dir, recursive = TRUE)
  on.exit(unlink(dirname(.dir), recursive = TRUE))
  file.copy(
    list.files(system.file("theo", package = "monolix2rx"), full.names = TRUE),
    .dir,
    recursive = TRUE
  )
  .p <- file.path(.dir, "theophylline_project.mlxtran")
  .ref <- suppressMessages(monolix2rx(.p))
  # as a Monolix 2024R1 GUI-saved project writes a prior
  .l <- readLines(.p)
  .l <- sub("^<MODEL>$", paste0("<MODEL>\n\n[POPULATION]\nDEFINITION:\n",
                                "ka_pop = {distribution=logNormal, typical=3, sd=0.25}\n",
                                "V_pop = {distribution=logNormal, typical=30, sd=0.3}"), .l)
  .l <- sub("^(ka_pop = \\{value=[^,]+), method=MLE\\}", "\\1, method=MAP}", .l)
  .l <- sub("^(V_pop = \\{value=[^,]+), method=MLE\\}", "\\1, method=MAP}", .l)
  writeLines(.l, .p)
  .m <- suppressMessages(mlxtran(.p))
  expect_equal(as.character(.m$MODEL$POPULATION$DEFINITION),
               c("ka_pop = {distribution=lognormal, typical=3, sd=0.25}",
                 "V_pop = {distribution=lognormal, typical=30, sd=0.3}"))
  .par <- .m$PARAMETER$PARAMETER
  expect_equal(.par$method[.par$name %in% c("ka_pop", "V_pop")], c("MAP", "MAP"))
  # the prior is on the imported (log-scale) theta; nothing else changes
  .rx <- suppressMessages(monolix2rx(.p))
  .i <- .rx$iniDf
  expect_equal(.i$prior[match(c("ka_pop", "V_pop", "Cl_pop"), .i$name)],
               c("dnorm(log(3), 0.25)", "dnorm(log(30), 0.3)", NA))
  expect_equal(.i[, names(.i) != "prior"], .ref$iniDf[, names(.ref$iniDf) != "prior"])
  expect_equal(.rx$thetaMat, .ref$thetaMat)
  expect_true(.rx$ipredAtol < 1e-4)
})

test_that("a prior takes its parameter's scale; omega and residual priors are skipped", {
  .ind <- .indDef(paste("ka = {distribution=logNormal, typical=ka_pop, sd=omega_ka}",
                        "V = {distribution=normal, typical=V_pop, covariate=WT, coefficient=beta_V_WT, sd=omega_V}",
                        "p = {distribution=logitNormal, typical=p_pop, no-variability}",
                        "f = {distribution=logitNormal, typical=f_pop, min=0, max=10, no-variability}",
                        "q = {distribution=probitNormal, typical=q_pop, no-variability}", sep="\n"))
  .pd <- .popDef(paste("ka_pop = {distribution=logNormal, typical=3, sd=0.25}",
                       "V_pop = {distribution=normal, typical=30, sd=5}",
                       "p_pop = {distribution=logitNormal, typical=0.7, sd=0.5}",
                       "f_pop = {distribution=logitNormal, typical=5, sd=0.5, min=0, max=10}",
                       "q_pop = {distribution=probitNormal, typical=0.2, sd=0.3}",
                       "beta_V_WT = {distribution=normal, typical=0.1, sd=0.05}", sep="\n"))
  .pars <- data.frame(name=c("ka_pop", "V_pop", "p_pop", "f_pop", "q_pop", "beta_V_WT"),
                      value=1, method="MAP")
  expect_equal(vapply(.popPrior(.pd, .pars, .ind), deparse1, character(1)),
               c("prior(ka_pop) ~ dnorm(log(3), 0.25)",
                 "prior(V_pop) ~ dnorm(30, 5)",
                 paste0("prior(p_pop) ~ dnorm(", signif(qlogis(0.7), 15), ", 0.5)"),
                 "prior(f_pop) ~ dnorm(0, 0.5)",
                 paste0("prior(q_pop) ~ dnorm(", signif(qnorm(0.2), 15), ", 0.3)"),
                 "prior(beta_V_WT) ~ dnorm(0.1, 0.05)"))
  # a prior with another distribution, an omega, a residual parameter or a
  # name outside ini() is skipped with a warning
  .bad <- .popDef(paste("ka_pop = {distribution=normal, typical=3, sd=0.25}",
                        "omega_ka = {distribution=logNormal, typical=0.5, sd=0.25}",
                        "a = {distribution=logNormal, typical=0.5, sd=0.25}", sep="\n"))
  .bp <- data.frame(name=c("ka_pop", "omega_ka", "a"), value=1, method="MAP")
  expect_warning(expect_warning(expect_warning(
    .r <- .popPrior(.bad, .bp, .ind), "differs"), "not a typical value"), "not a typical value")
  expect_equal(.r, list())
  expect_warning(.popPrior(.pd, .pars[1, ], .ind, theta = "V_pop"), "not an ini")
  # a non-MAP parameter has no prior
  expect_equal(.popPrior(.pd, data.frame(name="ka_pop", value=1, method="MLE"), .ind), list())
  expect_equal(.popPrior(NULL, .pars), list())
})

test_that("an import with a MAP prior simulates uncertainty with usePrior=FALSE", {
  skip_on_cran()
  .dir <- file.path(tempfile(), "theo")
  dir.create(.dir, recursive = TRUE)
  on.exit(unlink(dirname(.dir), recursive = TRUE))
  file.copy(
    list.files(system.file("theo", package = "monolix2rx"), full.names = TRUE),
    .dir,
    recursive = TRUE
  )
  .p <- file.path(.dir, "theophylline_project.mlxtran")
  .l <- readLines(.p)
  .l <- sub("^<MODEL>$", "<MODEL>\n\n[POPULATION]\nDEFINITION:\nka_pop = {distribution=logNormal, typical=3, sd=0.25}", .l)
  .l <- sub("^(ka_pop = \\{value=[^,]+), method=MLE\\}", "\\1, method=MAP}", .l)
  writeLines(.l, .p)
  .rx <- suppressMessages(monolix2rx(.p))
  .ev <- rxode2::et(amt = 320) |> rxode2::et(seq(0, 24, by = 4))
  # rxode2 simulates the prior (whose mean is not the MAP estimate) unless
  # told to use the imported thetaMat
  expect_error(suppressWarnings(rxode2::rxSolve(.rx, .ev, nStud = 2, nSub = 2)))
  expect_error(rxode2::rxSolve(.rx, .ev, nStud = 2, nSub = 2, usePrior = FALSE), NA)
})
