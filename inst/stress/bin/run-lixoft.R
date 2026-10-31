#!/usr/bin/env Rscript
## Run one Monolix project with lixoftConnectors (in its own process).
##
##   Rscript run-lixoft.R run.mlxtran   load, run, then resave as run-resaved.mlxtran
##   Rscript run-lixoft.R --version     print "version: <Monolix version>"
##
## lixoftConnectors reports a failure by returning FALSE and printing
## [ERROR] lines, not by signalling an error; the reason is written to
## monolix.failed so the kit can report it.

.args <- commandArgs(trailingOnly=TRUE)

.fail <- function(...) {
  .msg <- paste0(...)
  writeLines(.msg, "monolix.failed")
  message(.msg)
  quit(status=1)
}

## run `expr`, keeping what lixoftConnectors prints
.capture <- function(expr) {
  .msg <- character(0)
  .out <- utils::capture.output({
    .value <- try(withCallingHandlers(expr,
      message=function(m) .msg <<- c(.msg, conditionMessage(m)),
      warning=function(w) .msg <<- c(.msg, conditionMessage(w))),
      silent=TRUE)
  })
  if (length(.out) > 0L) cat(.out, sep="\n")
  if (inherits(.value, "try-error")) .msg <- c(.msg, attr(.value, "condition")$message)
  list(value=.value, text=trimws(unlist(strsplit(c(.out, .msg), "\n"))))
}

.reason <- function(text) {
  text <- text[nzchar(text)]
  .err <- grep("ERROR", text, value=TRUE)
  if (length(.err) == 0L) .err <- text
  if (length(.err) == 0L) return("(lixoftConnectors printed no reason)")
  paste(.err, collapse=" ")
}

.bad <- function(x) inherits(x$value, "try-error") || isFALSE(x$value)

if (!requireNamespace("lixoftConnectors", quietly=TRUE)) {
  if (identical(.args[1], "--version")) quit(status=1)
  .fail("lixoftConnectors is not installed")
}
.init <- .capture(lixoftConnectors::initializeLixoftConnectors(software="monolix",
                                                                 force=TRUE))
if (identical(.args[1], "--version")) {
  ## no monolix.failed here: this runs in the user's directory
  if (.bad(.init)) quit(status=1)
  .s <- try(lixoftConnectors::getLixoftConnectorsState(quietly=TRUE), silent=TRUE)
  if (!inherits(.s, "try-error") && !is.null(.s$version)) cat("version: ", .s$version, "\n", sep="")
  quit(status=0)
}
if (.bad(.init)) .fail("initializeLixoftConnectors() failed: ", .reason(.init$text))

.mlxtran <- .args[1]
if (is.na(.mlxtran) || !file.exists(.mlxtran)) .fail("no project file: ", .mlxtran)

.x <- .capture(lixoftConnectors::loadProject(.mlxtran))
if (.bad(.x)) .fail("loadProject() failed: ", .reason(.x$text))
.x <- .capture(lixoftConnectors::runScenario())
if (.bad(.x)) .fail("runScenario() failed: ", .reason(.x$text))

## Monolix's own rewrite of the project; its failure is not a run failure
.x <- .capture(lixoftConnectors::saveProject(projectFile="run-resaved.mlxtran"))
if (.bad(.x)) {
  writeLines(paste("saveProject() failed:", .reason(.x$text)), "resave.failed")
}
quit(status=0)
