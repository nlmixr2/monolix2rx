.covDef <- function(txt) {
  list(MODEL=list(COVARIATE=list(DEFINITION=.longDef(txt, "<MODEL> [COVARIATE] DEFINITION:"))))
}

test_that("latent covariates become mix()", {
  .m <- .latentMix(.covDef("lcat = {type=categorical, categories={1, 2}, P(lcat=1)=plcat1}"))
  expect_equal(.m$model, "lcat <- mix(1, plcat1, 2)")
  expect_equal(.m$prob, "plcat1")
  .m <- .latentMix(.covDef("lcat = {type=categorical, categories={1, 2, 3}, P(lcat=1)=p1\nP(lcat=2)=p2}"))
  expect_equal(.m$model, "lcat <- mix(1, p1, 2, p2, 3)")
  expect_equal(.m$prob, c("p1", "p2"))
  expect_error(.latentMix(.covDef("lcat = {type=categorical, categories={1, 2, 3}, P(lcat=1)=p1}")),
               "P\\(lcat=k\\)")
})

test_that("categorical coefficients follow the declared categories", {
  .d <- .indDef("Cl = {distribution=logNormal, typical=Cl_pop, covariate=lcat, coefficient={0, beta_Cl_lcat_2}, sd=omega_Cl}",
                list(lcat=c("1", "2")))
  expect_true(grepl("beta_Cl_lcat_2 * (lcat == 2)", .d$rx, fixed=TRUE))
  .d <- .indDef("Cl = {distribution=logNormal, typical=Cl_pop, covariate=sex, coefficient={beta_Cl_sex_F, 0}, sd=omega_Cl}",
                list(sex=c("F", "M")))
  expect_true(grepl("beta_Cl_sex_F * (sex == 'F')", .d$rx, fixed=TRUE))
})

test_that("a latent covariate project imports and solves by class", {
  skip_on_cran()
  .dir <- withr::local_tempdir()
  .d <- data.frame(ID=rep(1:2, each=3), TIME=rep(c(0, 1, 4), 2),
                   AMT=c(100, ".", ".", 100, ".", "."), DV=c(".", 2, 1, ".", 2, 1))
  utils::write.csv(.d, file.path(.dir, "data.csv"), row.names=FALSE, quote=FALSE)
  writeLines(c("[LONGITUDINAL]", "input = {V, Cl}", "EQUATION:", "Cc = pkmodel(V, Cl)",
               "OUTPUT:", "output = Cc"), file.path(.dir, "model.txt"))
  writeLines(c("<DATAFILE>", "[FILEINFO]", "file = 'data.csv'", "delimiter = comma",
               "header = {ID, TIME, AMT, DV}", "[CONTENT]", "ID = {use=identifier}",
               "TIME = {use=time}", "AMT = {use=amount}",
               "DV = {use=observation, name=CONC, type=continuous}",
               "<MODEL>", "[COVARIATE]", "input = plcat1", "DEFINITION:",
               "lcat = {type=categorical, categories={1, 2}, P(lcat=1)=plcat1}",
               "[INDIVIDUAL]", "input = {V_pop, omega_V, Cl_pop, beta_Cl_lcat_2, lcat}",
               "lcat = {type=categorical, categories={1, 2}}", "DEFINITION:",
               "V = {distribution=logNormal, typical=V_pop, sd=omega_V}",
               "Cl = {distribution=logNormal, typical=Cl_pop, covariate=lcat, coefficient={0, beta_Cl_lcat_2}, no-variability}",
               "[LONGITUDINAL]", "input = {b}", "file = 'model.txt'", "DEFINITION:",
               "CONC = {distribution=normal, prediction=Cc, errorModel=proportional(b)}",
               "<FIT>", "data = CONC", "model = CONC", "<PARAMETER>",
               "V_pop = {value=10, method=MLE}", "omega_V = {value=0.2, method=MLE}",
               "Cl_pop = {value=1, method=MLE}", "beta_Cl_lcat_2 = {value=1, method=MLE}",
               "plcat1 = {value=0.7, method=MLE}", "b = {value=0.1, method=MLE}",
               "<MONOLIX>", "[SETTINGS]", "GLOBAL:", "exportpath = 'run'"),
             file.path(.dir, "run.mlxtran"))
  .m <- suppressWarnings(suppressMessages(monolix2rx(file.path(.dir, "run.mlxtran"))))
  expect_equal(.m$iniDf$est[.m$iniDf$name == "plcat1"], 0.7)
  expect_true(any(grepl("lcat <- mix(1, plcat1, 2)", deparse(.m$lstExpr), fixed=TRUE)))
  .data <- .m$monolixData
  .data$mixest <- ifelse(.data$id == 1, 1L, 2L)
  .p <- c(getFromNamespace(".addRxerr", "monolix2rx")(.m, .m$theta), omega_V=0)
  .s <- rxode2::rxSolve(.m$monolixModelIwres, .p, .data, returnType="data.frame", addDosing=FALSE)
  expect_true(all(c("iwres", "ires") %in% names(.s)))
  expect_equal(unique(.s$lcat[.s$id == 2]), 2)
  .cl <- tapply(.s$Cl, .s$id, unique)
  expect_equal(as.vector(.cl), c(1, exp(1)))
})

test_that("latent probabilities on one line, quoted categories and fixed coefficients", {
  .m <- .latentMix(.covDef("lcat = {type=categorical, categories={1, 2, 3}, P(lcat=1)=p1, P(lcat=2)=p2}"))
  expect_equal(.m$model, "lcat <- mix(1, p1, 2, p2, 3)")
  expect_equal(.catLiteral(c("1", "it's", "M")), c("1", "'it\\'s'", "'M'"))
  .d <- .indDef("Cl = {distribution=logNormal, typical=Cl_pop, covariate=sex, coefficient={beta_Cl_sex_F, 0.2}, sd=omega_Cl}",
                list(sex=c("F", "M")))
  expect_true(grepl("beta_Cl_sex_F * (sex == 'F') + rxCov_Cl_sex_2 * (sex == 'M')", .d$rx, fixed=TRUE))
})
