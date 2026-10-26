for (.doseCens in c("0", ".")) {
test_that(paste0("censored observations are left out of both validation sides, CENS=", .doseCens, " on doses"), {
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
  .cens <- ifelse(.obs, ifelse(seq_along(.l) %% 5 == 0, "1", "0"), .doseCens)
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
}

test_that("a subject without observations is left out of the validation", {
  skip_on_cran()
  .dir <- file.path(tempfile(), "theo")
  dir.create(.dir, recursive = TRUE)
  on.exit(unlink(dirname(.dir), recursive = TRUE))
  file.copy(
    list.files(system.file("theo", package = "monolix2rx"), full.names = TRUE),
    .dir,
    recursive = TRUE
  )
  # a dose-only subject, which Monolix gives no random effects
  .d <- file.path(.dir, "data", "theophylline_data.txt")
  .l <- readLines(.d)
  writeLines(c(.l, "99\t4\t0\t.\t70\tM"), .d)
  # rxode2 warns that it drops the subject
  .rx <- suppressWarnings(suppressMessages(monolix2rx(file.path(.dir, "theophylline_project.mlxtran"))))
  expect_true(99 %in% .rx$monolixData$id)
  expect_null(.rx$monolixNotMatched)
  expect_true(.rx$ipredAtol < 1e-4)
})

test_that("EVID=3 with a missing amount is ignored, with AMT=0 it resets; EVID=4 without one resets", {
  skip_on_cran()
  .run <- function(amt, evid = 3L) {
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
    .evid <- c("EVID", ifelse(grepl("^[0-9]+\t[.]\t", .l[-1]), "0", "1"))
    # a reset line for subject 1 at t = 5
    .l <- c(paste(.l, .evid, sep = "\t"), paste0("1\t", amt, "\t5\t.\t79.6\tM\t", evid))
    writeLines(.l, .d)
    .f <- file.path(.dir, "theophylline_project.mlxtran")
    .p <- readLines(.f)
    .p <- sub("SEX}", "SEX, EVID}", .p, fixed = TRUE)
    .p <- sub("SEX = {use=covariate, type=categorical}",
              "SEX = {use=covariate, type=categorical}\nEVID = {use=eventidentifier}",
              .p, fixed = TRUE)
    writeLines(.p, .f)
    .rx <- suppressMessages(monolix2rx(.f))
    .rx$monolixData$evid[.rx$monolixData$id == 1 & .rx$monolixData$time == 5]
  }
  expect_equal(.run("."), 2L)
  expect_equal(.run("0"), 3L)
  # an EVID=4 washout without an amount is a reset
  expect_equal(.run(".", 4L), 3L)
})
