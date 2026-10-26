test_that("MAP priors in [POPULATION] DEFINITION: import at Monolix's estimates", {
  skip_on_cran()
  .dir <- file.path(tempfile(), "theo")
  dir.create(.dir, recursive = TRUE)
  on.exit(unlink(dirname(.dir), recursive = TRUE))
  file.copy(
    list.files(system.file("theo", package = "monolix2rx"), full.names = TRUE),
    .dir,
    recursive = TRUE
  )
  # as a Monolix 2024R1 GUI-saved project writes a prior
  .p <- file.path(.dir, "theophylline_project.mlxtran")
  .l <- readLines(.p)
  .l <- sub("^<MODEL>$", paste0("<MODEL>\n\n[POPULATION]\nDEFINITION:\n",
                                "ka_pop = {distribution=logNormal, typical=3, sd=0.25}\n",
                                "V_pop = {distribution=normal, typical=30, sd=5}"), .l)
  .l <- sub("^(ka_pop = \\{value=[^,]+), method=MLE\\}", "\\1, method=MAP}", .l)
  .l <- sub("^(V_pop = \\{value=[^,]+), method=MLE\\}", "\\1, method=MAP}", .l)
  writeLines(.l, .p)
  .m <- suppressMessages(mlxtran(.p))
  expect_equal(as.character(.m$MODEL$POPULATION$DEFINITION),
               c("ka_pop = {distribution=lognormal, typical=3, sd=0.25}",
                 "V_pop = {distribution=normal, typical=30, sd=5}"))
  expect_equal(.m$PARAMETER$PARAMETER$method[.m$PARAMETER$PARAMETER$name == "ka_pop"], "MAP")
  .rx <- suppressMessages(monolix2rx(.p))
  .i <- .rx$iniDf
  expect_false(any(.i$fix[.i$name %in% c("ka_pop", "V_pop")]))
  expect_true(.rx$ipredAtol < 1e-4)
})
