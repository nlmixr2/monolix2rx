.theoCopy <- function() {
  .dir <- file.path(tempfile(), "theo")
  dir.create(.dir, recursive = TRUE)
  file.copy(
    system.file("theo", package = "monolix2rx"),
    dirname(.dir),
    recursive = TRUE
  )
  .dir
}

.theoMlxtran <- function(dir) {
  suppressMessages(mlxtran(file.path(dir, "theophylline_project.mlxtran")))
}

test_that(".monolixNaApply converts numeric-like character columns", {
  .d <- data.frame(
    time = c("0", "1"),
    amt = c("1", " . "),
    dv = c("1", "abc"),
    WT = c("1", "2")
  )
  .r <- .monolixNaApply(.d, c("NA", "."), NULL)
  expect_equal(.r$time, c(0, 1))
  expect_equal(.r$amt, c(1, NA))
  expect_equal(.r$dv, c("1", "abc"))
  expect_equal(.r$WT, c("1", "2"))
})

test_that(".monolixNaApply matches na.strings literally (#56)", {
  .d <- data.frame(
    time = c("0", "1", NA),
    amt = c("1", "*", "5"),
    dv = c("1", "x", "2")
  )
  .r <- .monolixNaApply(.d, c("NA", "."), NULL)
  expect_equal(.r$time, c(0, 1, NA))
  expect_equal(.r$amt, c("1", "*", "5"))
  expect_equal(.r$dv, c("1", "x", "2"))
  .r <- .monolixNaApply(.d, c("*", "(", "x"), NULL)
  expect_equal(.r$amt, c(1, NA, 5))
  expect_equal(.r$dv, c(1, NA, 2))
  .d$dv <- c("1", "\t.\t", "2")
  .r <- .monolixNaApply(.d, c("NA", " . "), NULL)
  expect_equal(.r$dv, c(1, NA, 2))
})

test_that(".monolixDataLoad handles header variants", {
  skip_on_cran()
  .dir <- .theoCopy()
  on.exit(unlink(dirname(.dir), recursive = TRUE))
  .m <- .theoMlxtran(.dir)
  .f <- file.path(.dir, "data", "theophylline_data.txt")
  .l <- readLines(.f)
  .ref <- .monolixDataLoad(.m)
  expect_equal(names(.ref), c("ID", "AMT", "TIME", "CONC", "WEIGHT", "SEX"))

  # case mismatch in the header
  writeLines(c(tolower(.l[1]), .l[-1]), .f)
  expect_warning(.r <- .monolixDataLoad(.m), "header does not match")
  expect_equal(.r, .ref)

  # no header
  writeLines(.l[-1], .f)
  expect_equal(.monolixDataLoad(.m), .ref)

  # wrong number of columns
  writeLines(
    c(sub("\tSEX$", "\tSEX\tX", tolower(.l[1])), paste0(.l[-1], "\t1")),
    .f
  )
  expect_error(
    suppressWarnings(.monolixDataLoad(.m)),
    "length of the headers"
  )
  writeLines(paste0(.l[-1], "\t1"), .f)
  expect_error(suppressWarnings(.monolixDataLoad(.m)), "length of the headers")

  # missing data file
  unlink(.f)
  expect_null(.monolixDataLoad(.m))
})

test_that("monolixDataImport input checks", {
  skip_on_cran()
  .dir <- .theoCopy()
  on.exit(unlink(dirname(.dir), recursive = TRUE))
  .rx <- suppressMessages(suppressWarnings(
    monolix2rx(file.path(.dir, "theophylline_project.mlxtran"))
  ))
  .f <- function() {
    model({
      a <- 1
    })
  }
  expect_error(monolixDataImport(rxode2::rxode2(.f)), "imported from monolix")

  .d <- utils::read.table(
    file.path(.dir, "data", "theophylline_data.txt"),
    na.strings = ".",
    header = TRUE
  )
  # NA time is replaced by zero
  .d$TIME[1] <- NA
  .r <- suppressMessages(monolixDataImport(.rx, .d))
  expect_equal(.r$time[1], 0)
  # no time column gets a dummy time
  .r <- suppressMessages(monolixDataImport(.rx, .d[, names(.d) != "TIME"]))
  expect_equal(.r$time, seq_len(nrow(.r)))
})
