demoDir <- getOption("monolix2rx.demo", NULL)
.env <- new.env(parent=emptyenv())
.env$models <- NULL
demoDir <- sub("[/\\]+$", "", demoDir)
getMlxtran <- function(dd=demoDir, env=.env) {
  .files <- list.files(dd)
  for (.f in .files) {
    .next <- file.path(dd, .f)
    if (dir.exists(.next)) {
      getMlxtran(dd=.next, env=env)
    }
    .nc <- nchar(.f)
    if (.nc < 9) next
    if (substr(.f, .nc-7, .nc) == ".mlxtran") {
      env$models <- c(env$models, .next)
    }
  }
}

getMlxtran()
models <- .env$models

unparsed <- function(x, ...) {
  .p <- .unparsedMlxtran(x, ...)
  .p <- .p[(.p %in% c("$MONOLIX$COMMENTS$COMMENTS", "$DATA_FORMATTING$OPERATIONS$TREATMENTS",
                      "$DATA_FORMATTING$OPERATIONS$OBSERVATIONS", "$DATA_FORMATTING$SETTINGS$LINES",
                      "$DATA_FORMATTING$SETTINGS$LIES",
                      "$MODEL$LONGITUDINAL$EQUATION"))]
}

for (f2 in models) {
  if (file.exists(f2)) {
    test_that(paste("mlxtran without equation", f2), {
      m <- mlxtran(f2)
      expect_true(inherits(m, "monolix2rxMlxtran"))
      expect_equal(unparsed(m), character(0))
    })
    m <- try(mlxtran(f2, equation=TRUE), silent=TRUE)
    if (inherits(m, "try-error")) {
      msg <- attr(m, "condition")$message
      test_that(paste0("error is known: ", msg),{
        expect_true(grepl("(delay|bsmm|wsmm)", msg))
      })
    } else {
      test_that("mlxtran model parsed", {
        expect_true(inherits(m, "monolix2rxMlxtran"))
        expect_equal(unparsed(m), character(0))
      })
      if (requireNamespace("rxode2", quietly = TRUE)) {
        m2 <- try(monolix2rx(m), silent=TRUE)
        if (inherits(m2, "try-error")) {
          msg <- attr(m2, "condition")$message
          test_that(paste0("error is known: ", msg, "\n", f2),{
            expect_true(grepl("(count|categorical|event|equations to translate)", msg))
          })
        } else {
          test_that(paste0("monolix2rx: ", f2), {
            expect_true(inherits(m2, "rxUi"))
          })
        }
      }
    }
  }
}
