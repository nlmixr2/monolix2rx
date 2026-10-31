## The stress kit without Monolix: translate mode (simulate each case with
## rxode2, write the project and data, import with monolix2rx and compare
## with the truth), and the run/import plumbing with the mock Monolix.  The
## smoke cases by default; every case with MONOLIX2RX_STRESS_ALL=true.  See
## inst/stress/README.md.

.stressLoad <- function() {
  .stress <- system.file("stress", "stress.R", package="monolix2rx")
  skip_if(.stress == "", "stress kit not installed")
  suppressMessages(source(.stress, local=TRUE))
  withr::defer(detach("monolix2rx-stress", character.only=TRUE), envir=parent.frame())
  .stress
}

.stressBad <- function(res) {
  .bad <- res[res$status %in% c("FAIL", "ERROR", "XPASS"), , drop=FALSE]
  expect_equal(nrow(.bad), 0L,
               info=paste(.bad$case, .bad$status, .bad$note, collapse="\n"))
}

test_that("stress kit translate mode", {
  skip_on_cran()
  skip_if_not_installed("withr")
  .stressLoad()
  .all <- identical(Sys.getenv("MONOLIX2RX_STRESS_ALL"), "true")
  .res <- suppressMessages(stressKit(modes="translate",
                                     tags=if (.all) NULL else "smoke",
                                     bundle=FALSE, out=withr::local_tempdir()))
  .stressBad(.res)
  expect_true(nrow(.res) >= 2L)
})

test_that("stress kit run and replay with the mock Monolix", {
  skip_on_cran()
  skip_if_not_installed("withr")
  .stress <- .stressLoad()
  .rscript <- file.path(R.home("bin"), "Rscript")
  skip_if_not(file.exists(.rscript) || file.exists(paste0(.rscript, ".exe")))
  .mock <- paste(shQuote(.rscript),
                 shQuote(file.path(dirname(.stress), "mock", "fake-monolix.R")),
                 "{mlxtran}")
  .out <- file.path(withr::local_tempdir(), "mock")
  .res <- suppressMessages(stressKit(monolix=.mock,
                                     cases="^(pkmodel-oral-1cmt|tasks-no-exportpath|disc-mixed-continuous|tte-pk-joint|data-ytype-swapped|data-string-id)$",
                                     bundle=FALSE, out=.out))
  .stressBad(.res)
  expect_equal(.res$status, rep("PASS", 6L))
  expect_true(all(.res$resaved))
  expect_equal(.res$dfSub, .res$expSub)
  expect_true(all(is.finite(.res$ipredRtol)))
  .rep <- suppressMessages(stressReplay(.out))
  .stressBad(.rep)
  expect_equal(.rep$mode, rep("import", 6L))
})
