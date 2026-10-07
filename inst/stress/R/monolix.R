## Monolix discovery and execution.
##
## Monolix always runs in a child process: lixoftConnectors::runScenario()
## blocks (no timeout in process) and keeps global state (no forking).
## The run command is a template with {mlxtran}; "lixoftConnectors" runs
## bin/run-lixoft.R, which also resaves the project as
## run-resaved.mlxtran.

#' How Monolix is run: a command template, "lixoftConnectors" or ""
stressFindMonolix <- function() {
  for (.opt in c("monolix2rx.monolix", "babelmixr2.monolix")) {
    .o <- getOption(.opt, "")
    if (is.character(.o) && nzchar(.o)) return(.o)
  }
  if (requireNamespace("lixoftConnectors", quietly=TRUE)) return("lixoftConnectors")
  ""
}

.kitRscript <- function() {
  file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
}

## The command template (with {mlxtran}) for a Monolix setting
.kitMonolixCmd <- function(monolix) {
  if (identical(monolix, "lixoftConnectors")) {
    return(paste(shQuote(.kitRscript()),
                 shQuote(file.path(.kitDir, "bin", "run-lixoft.R")), "{mlxtran}"))
  }
  if (grepl("{mlxtran}", monolix, fixed=TRUE)) return(monolix)
  paste(monolix, "{mlxtran}")
}

## Monolix version reported by the runner ("2024R1", "mock"), or NA
.kitMonolixVersion <- function(monolix) {
  if (!identical(monolix, "lixoftConnectors") && !grepl("fake-monolix", monolix)) {
    return(NA_character_)
  }
  .cmd <- gsub("{mlxtran}", "--version", .kitMonolixCmd(monolix), fixed=TRUE)
  .v <- suppressWarnings(tryCatch(system(.cmd, intern=TRUE, ignore.stderr=TRUE),
                                  error=function(e) character(0)))
  .v <- grep("^version: ", .v, value=TRUE)
  if (length(.v) == 0L) return(NA_character_)
  sub("^version: ", "", .v[length(.v)])
}

## "2024R1" -> 2024.1; NA when not a Monolix year version
.kitMonolixVersionNum <- function(v) {
  if (is.null(v) || is.na(v)) return(NA_real_)
  .m <- regmatches(v, regexec("^([0-9]{4})R([0-9])", v))[[1]]
  if (length(.m) != 3L) return(NA_real_)
  as.numeric(.m[2]) + as.numeric(.m[3]) / 10
}

## Run Monolix on run.mlxtran in `dir`
kitRunMonolix <- function(dir, cmd, timeout=3600) {
  .old <- setwd(dir)
  on.exit(setwd(.old))
  unlink(c(.kitExport, "monolix.failed", "resave.failed", "run-resaved.mlxtran",
           "run-resaved"), recursive=TRUE)
  .cmd <- gsub("{mlxtran}", "run.mlxtran", cmd, fixed=TRUE)
  .t0 <- Sys.time()
  if (.Platform$OS.type == "windows") {
    ## cmd /c drops the first and last quote of a line with more than two,
    ## so the whole command is quoted once more
    .status <- suppressWarnings(shell(paste0("\"", .cmd, " > monolix.log 2>&1\""),
                                      wait=TRUE))
  } else {
    .to <- Sys.which("timeout")
    if (nzchar(.to)) .cmd <- paste(.to, "--kill-after=30", timeout, "sh -c", shQuote(.cmd))
    .status <- suppressWarnings(system(paste(.cmd, "> monolix.log 2>&1"),
                                       timeout=if (nzchar(.to)) 0 else timeout))
  }
  ## a killed or crashed run can leave partial results: the exit status
  ## must be 0 too (timeout exits 124)
  .ok <- identical(as.integer(.status), 0L) && !file.exists("monolix.failed") &&
    file.exists(file.path(.kitExport, "populationParameters.txt")) &&
    length(Sys.glob(file.path(.kitExport, "predictions*.txt"))) > 0L
  list(status=.status, ok=.ok,
       seconds=as.numeric(Sys.time() - .t0, units="secs"))
}

## Why Monolix failed: the runner's reason, Monolix's [ERROR] lines, or
## the last line of monolix.log
.kitMonolixError <- function(dir) {
  .read <- function(f) {
    f <- file.path(dir, f)
    if (file.exists(f)) readLines(f, warn=FALSE) else character(0)
  }
  .f <- trimws(.read("monolix.failed"))
  .f <- .f[nzchar(.f)]
  if (length(.f)) return(substr(paste(.f, collapse=" "), 1, 300))
  .l <- trimws(.read("monolix.log"))
  .l <- .l[nzchar(.l)]
  .e <- grep("ERROR|^Error", .l, value=TRUE)
  if (length(.e)) return(substr(paste(.e, collapse=" "), 1, 300))
  if (length(.l) == 0L) return("no output (see monolix.log)")
  substr(.l[length(.l)], 1, 160)
}
