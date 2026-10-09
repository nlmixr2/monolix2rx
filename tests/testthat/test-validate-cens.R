test_that("censored observations are left out of both validation sides", {
  skip_on_cran()
  .dir <- file.path(tempfile(), "theo")
  dir.create(.dir, recursive = TRUE)
  on.exit(unlink(dirname(.dir), recursive = TRUE))
  file.copy(
    list.files(system.file("theo", package = "monolix2rx"), full.names = TRUE),
    .dir,
    recursive = TRUE
  )
  .d <- file.path(.dir, "data", "theophylline_data.txt")
  .l <- readLines(.d)
  .obs <- c(FALSE, grepl("^[0-9]+\t[.]\t", .l[-1]))
  .cens <- ifelse(.obs & seq_along(.l) %% 5 == 0, "1", "0")
  .cens[1] <- "CENS"
  writeLines(paste(.l, .cens, sep = "\t"), .d)
  .f <- file.path(.dir, "theophylline_project.mlxtran")
  .l <- readLines(.f)
  .l <- sub("SEX}", "SEX, CENS}", .l, fixed = TRUE)
  .l <- sub("SEX = {use=covariate, type=categorical}",
            "SEX = {use=covariate, type=categorical}\nCENS = {use=censored}",
            .l, fixed = TRUE)
  writeLines(.l, .f)
  .rx <- suppressMessages(monolix2rx(.f))
  expect_true(any(.rx$monolixData$cens == 1))
  expect_null(.rx$monolixNotMatched)
  expect_true(.rx$ipredAtol < 0.05)
})
