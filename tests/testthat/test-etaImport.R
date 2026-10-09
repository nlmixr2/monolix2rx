test_that("random effects map to the sd=/var= eta names", {
  .def <- .indDef("ka = {distribution=logNormal, typical=ka_pop, sd=0.3}
Cl = {distribution=logNormal, typical=Cl_pop, varlevel=id*occ, sd=gamma_Cl}
V = {distribution=logNormal, typical=V_pop, varlevel={id, id*occ}, sd={omega_V, gamma_V}}
Tlag = {distribution=logNormal, typical=Tlag_pop, varlevel={id*occ, id}, sd={gamma_Tlag, omega_Tlag}}
Q = {distribution=logNormal, typical=Q_pop, var=omega2_Q}
k = {distribution=logNormal, typical=k_pop, no-variability}")
  expect_equal(.etaImportSd(list(MODEL=list(INDIVIDUAL=list(DEFINITION=.def)))),
               c(ka="rxVar_ka_1", V="omega_V", Tlag="omega_Tlag", Q="omega2_Q"))
  expect_equal(.etaImportSd(list(MODEL=list())), character(0))
})

test_that("a var= random effect is imported and validated", {
  skip_on_cran()
  .dir <- file.path(tempfile(), "theo")
  dir.create(.dir, recursive = TRUE)
  on.exit(unlink(dirname(.dir), recursive = TRUE))
  file.copy(
    list.files(system.file("theo", package = "monolix2rx"), full.names = TRUE),
    .dir,
    recursive = TRUE
  )
  .f <- file.path(.dir, "theophylline_project.mlxtran")
  .l <- readLines(.f)
  .l <- gsub("omega_Cl", "omega2_Cl", .l, fixed = TRUE)
  .l <- sub("sd=omega2_Cl", "var=omega2_Cl", .l, fixed = TRUE)
  writeLines(.l, .f)
  .rx <- suppressMessages(monolix2rx(.f))
  expect_true("omega2_Cl" %in% names(.rx$etaData))
  expect_false("omega_Cl" %in% names(.rx$etaData))
  expect_true(.rx$ipredAtol < 0.05)
})
