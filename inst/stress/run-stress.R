#!/usr/bin/env Rscript
# monolix2rx stress kit runner: rxode2 simulation -> Monolix -> monolix2rx.
#
# The same as stressCheck(), stressList() and stressKit() from an R session
# after sourcing stress.R; see README.md in this directory.
#
# Usage:
#   Rscript run-stress.R [options]
#
# Options:
#   --kit                     everything for a Monolix machine: run every
#                             case with the Monolix that is found and zip
#                             the output (--bundle)
#   --check                   report the versions and whether Monolix is
#                             found, then exit
#   --list                    list the cases and exit
#   --mode=translate|run      translate: write the projects/data and check
#                             the translation (no Monolix); run: also run
#                             Monolix and import the output
#                             (default: translate; run with --kit)
#   --monolix=COMMAND         lixoftConnectors (the default when it is
#                             installed) or a command template with {mlxtran}
#   --cases=REGEX             only the cases whose name matches REGEX
#   --tags=t1,t2              only the cases with any of these tags
#   --est=full|fixed          estimation preset (default: full)
#   --nsub=N                  subjects per case (default: 20)
#   --jobs=N                  cases run in parallel (default: 1)
#   --timeout=SECONDS         Monolix timeout per case (default: 3600)
#   --out=DIR                 output directory
#   --bundle                  zip the output directory to send back
#   --replay=ZIP              re-import a returned zip (no Monolix)
#   --installed               use the installed monolix2rx, not the source
#                             tree the kit is in
#
# The exit status is 1 when any case is FAIL or ERROR.

.args <- commandArgs(trailingOnly=TRUE)
.opt <- function(name, default=NULL) {
  .w <- which(grepl(paste0("^--", name, "(=|$)"), .args))
  if (length(.w) == 0L) return(default)
  .a <- .args[.w[1]]
  if (grepl("=", .a)) return(sub(paste0("^--", name, "="), "", .a))
  if (.w[1] < length(.args) && !startsWith(.args[.w[1] + 1], "--")) {
    return(.args[.w[1] + 1])
  }
  TRUE
}
.split <- function(x) if (is.null(x)) NULL else strsplit(x, ",")[[1]]

.stressDir <- local({
  .f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value=TRUE))
  if (length(.f) == 1) dirname(normalizePath(.f)) else "inst/stress"
})

if (isTRUE(.opt("installed"))) suppressMessages(library(monolix2rx))
source(file.path(.stressDir, "stress.R"))

if (isTRUE(.opt("check"))) {
  stressCheck(monolix=.opt("monolix"))
  quit(status=0)
}
if (isTRUE(.opt("list"))) {
  stressList(cases=.opt("cases"), tags=.split(.opt("tags")))
  quit(status=0)
}

.replay <- .opt("replay")
if (!is.null(.replay)) {
  .res <- stressReplay(.replay, cases=.opt("cases"),
                       jobs=as.integer(.opt("jobs", 1L)))
} else {
  .kit <- isTRUE(.opt("kit"))
  .mode <- .opt("mode", if (.kit) "run" else "translate")
  .args2 <- list(monolix=.opt("monolix"),
                 modes=if (.mode == "run") c("translate", "run") else "translate",
                 cases=.opt("cases"), tags=.split(.opt("tags")),
                 est=.opt("est", "full"), nSub=as.integer(.opt("nsub", 20L)),
                 jobs=as.integer(.opt("jobs", 1L)),
                 timeout=as.numeric(.opt("timeout", 3600)),
                 bundle=.kit || isTRUE(.opt("bundle")))
  if (!is.null(.opt("out"))) .args2$out <- .opt("out")
  .res <- do.call(stressKit, .args2)
}
.counts <- attr(.res, "counts")
quit(status=if (.counts[["FAIL"]] + .counts[["ERROR"]] > 0) 1 else 0)
