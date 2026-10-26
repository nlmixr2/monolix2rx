test_that("MAP priors in [POPULATION] DEFINITION: parse and leave the import unchanged", {
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
  # the prior is not carried: the parameters and their covariance are as
  # without it
  .rx <- suppressMessages(monolix2rx(.p))
  expect_equal(.rx$iniDf, .ref$iniDf)
  expect_equal(.rx$thetaMat, .ref$thetaMat)
  expect_true(.rx$ipredAtol < 1e-4)
})
