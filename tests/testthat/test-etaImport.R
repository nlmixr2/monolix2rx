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

test_that("a project without random effects keeps a data frame of ids", {
  skip_on_cran()
  .dir <- file.path(tempfile(), "theo")
  dir.create(.dir, recursive = TRUE)
  on.exit(unlink(dirname(.dir), recursive = TRUE))
  file.copy(
    list.files(system.file("theo", package = "monolix2rx"), full.names = TRUE),
    .dir,
    recursive = TRUE
  )
  .f <- file.path(.dir, "tp", "IndividualParameters", "estimatedRandomEffects.txt")
  .ids <- read.csv(.f)$id
  writeLines(c("id", .ids), .f)
  .eta <- monolixEtaImport(file.path(.dir, "theophylline_project.mlxtran"))
  expect_true(is.data.frame(.eta))
  expect_equal(names(.eta), "id")
  expect_equal(.eta$id, .ids)
})

test_that("individual parameters follow the data's subject order with numeric ids", {
  .ui <- list(theta=c(tka=1),
              etaData=data.frame(id=c("S-2", "S-1"), eta.ka=c(0.2, 0.1)),
              monolixData=data.frame(id=c("S-1", "S-1", "S-2"), time=c(0, 1, 0)))
  .p <- .parameterThetaEta(.ui)
  expect_equal(.p$id, 1:2)
  expect_equal(.p$eta.ka, c(0.1, 0.2))
  expect_equal(.p$tka, c(1, 1))
})

test_that("the conditional modes are imported, the SAEM etas kept in $etaSaemData", {
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
  .re <- read.csv(file.path(.dir, "tp", "IndividualParameters", "estimatedRandomEffects.txt"))
  .rx <- suppressMessages(monolix2rx(.f))
  expect_equal(.rx$etaData$omega_Cl, .re$eta_Cl_mode)
  expect_equal(.rx$etaSaemData$omega_Cl, .re$eta_Cl_SAEM)
  expect_null(attr(.rx$etaData, "est"))
  expect_true(.rx$ipredAtol < 1e-4)
  # without the modes in one of the files, both the etas and the ipred are SAEM
  .noMode <- function(.p) {
    .d <- read.csv(.p, check.names = FALSE)
    write.csv(.d[, !grepl("_mode$", names(.d))], .p, row.names = FALSE, quote = FALSE)
  }
  .noMode(file.path(.dir, "tp", "predictions.txt"))
  .rx <- suppressMessages(monolix2rx(.f))
  expect_equal(.rx$etaData$omega_Cl, .re$eta_Cl_SAEM)
  expect_null(.rx$etaSaemData)
  expect_true(.rx$ipredAtol < 0.05)
  .noMode(file.path(.dir, "tp", "IndividualParameters", "estimatedRandomEffects.txt"))
  .rx <- suppressMessages(monolix2rx(.f))
  expect_equal(.rx$etaData$omega_Cl, .re$eta_Cl_SAEM)
  expect_true(.rx$ipredAtol < 0.05)
})

test_that("occasion rows repeating the random effects collapse to one per subject", {
  skip_on_cran()
  .dir <- file.path(tempfile(), "theo")
  dir.create(.dir, recursive = TRUE)
  on.exit(unlink(dirname(.dir), recursive = TRUE))
  file.copy(
    list.files(system.file("theo", package = "monolix2rx"), full.names = TRUE),
    .dir,
    recursive = TRUE
  )
  .p <- file.path(.dir, "tp", "IndividualParameters", "estimatedRandomEffects.txt")
  .re <- read.csv(.p)
  .occ <- rbind(cbind(.re[, 1, drop = FALSE], OCCevid = 1L, .re[, -1]),
                cbind(.re[, 1, drop = FALSE], OCCevid = 2L, .re[, -1]))
  write.csv(.occ[order(.occ$id), ], .p, row.names = FALSE, quote = FALSE)
  .rx <- suppressMessages(monolix2rx(file.path(.dir, "theophylline_project.mlxtran")))
  expect_equal(.rx$etaData$id, .re$id)
  expect_equal(.rx$etaData$omega_Cl, .re$eta_Cl_mode)
  expect_true(.rx$ipredAtol < 1e-4)
})

test_that("results written next to the project are found without an exportpath", {
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
  .e <- grep("^exportpath", .l)
  expect_length(.e, 1L)
  writeLines(.l[-.e], .f)
  file.copy(list.files(file.path(.dir, "tp"), full.names = TRUE), .dir, recursive = TRUE)
  unlink(file.path(.dir, "tp"), recursive = TRUE)
  .rx <- suppressMessages(monolix2rx(.f))
  expect_false(is.null(.rx$etaData))
  expect_true(.rx$ipredAtol < 1e-4)
  expect_true(.rx$predAtol < 1e-4)
})
