libDir <- getOption("monolix2rx.library", NULL)
.env <- new.env(parent=emptyenv())
.env$txt <- NULL
libDir <- sub("[/\\]+$", "", libDir)
getMlxTxt <- function(dd=libDir, env=.env) {
  .files <- list.files(dd)
  for (.f in .files) {
    .next <- file.path(dd, .f)
    if (dir.exists(.next)) {
      getMlxTxt(dd=.next, env=env)
    }
    .nc <- nchar(.f)
    if (.nc < 4) next
    if (substr(.f, .nc-3, .nc) == ".txt") {
      env$txt <- c(env$txt, .next)
    }
  }
}

getMlxTxt()
txt <- .env$txt

unparsed <- function(x, ...) {
  .p <- .unparsedMlxtran(x, ...)
  .p <- .p[(.p %in% c("$MONOLIX$COMMENTS$COMMENTS", "$DATA_FORMATTING$OPERATIONS$TREATMENTS",
                      "$DATA_FORMATTING$OPERATIONS$OBSERVATIONS", "$DATA_FORMATTING$SETTINGS$LINES",
                      "$DATA_FORMATTING$SETTINGS$LIES"))]
}

# ~/src/monolix/library/tmdd/oral0_1cpt_constRtot_TlagTk0VkintkonKDR0Cl_outputLtot.txt
## cur <- "~/src/monolix/library/tmdd/oral0_1cpt_constRtot_TlagTk0VkintkonKDR0Cl_outputLtot.txt"
## cur <- "~/src/monolix/library/tmdd/oral1_1cpt_constRtot_kaVkintkonKDR0Cl_outputLtot.txt"
cur <- ""

.w <- which(cur == txt)
if (length(.w) == 1L) {
  txt <- txt[seq(.w - 1, length(txt))]
}


for (f2 in txt) {
  if (file.exists(f2)) {
    message(f2)
    m <- try(mlxTxt(f2))
    test_that(paste("mlxtran without equation", f2), {
      expect_true(inherits(m, "monolix2rxMlxtran"))
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
    rxode2::rxUnloadAll()
  }
}
