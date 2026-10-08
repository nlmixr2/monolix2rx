## Monolix project generation.

.kitExport <- "run"

.kitPlots <- "plotResult(method = {indfits, obspred, residualsscatter})"

## [TASKS] for an estimation preset
.kitTasks <- function(est) {
  switch(est,
         full=paste("[TASKS]", "populationParameters()",
                    "individualParameters(method = {conditionalMean, conditionalMode })",
                    "fim(method = StochasticApproximation)",
                    "logLikelihood(method = ImportanceSampling)",
                    .kitPlots, sep="\n"),
         fixed=paste("[TASKS]", "populationParameters()",
                     "individualParameters(method = {conditionalMean, conditionalMode })",
                     .kitPlots, sep="\n"),
         stop("unknown estimation preset: ", est, call.=FALSE))
}

## a case's results directory: its exportpath, or Monolix's default (the
## project name) when the project has none
.kitCaseExport <- function(case) {
  .e <- case$exportpath
  if (is.null(.e) || is.na(.e)) .kitExport else .e
}

.kitSettings <- function(case) {
  if (is.na(case$exportpath)) return("")
  paste0("[SETTINGS]\nGLOBAL:\nexportpath = '", case$exportpath, "'")
}

## est="fixed": every <PARAMETER> estimated by MLE becomes FIXED
.kitFixParameters <- function(lines) {
  .start <- grep("^<PARAMETER>", lines)
  if (length(.start) != 1L) return(lines)
  .next <- grep("^<", lines)
  .next <- .next[.next > .start]
  .end <- if (length(.next)) .next[1] - 1L else length(lines)
  .i <- seq(.start, .end)
  lines[.i] <- gsub("method *= *MLE", "method=FIXED", lines[.i])
  lines
}

.kitExpand <- function(txt, sub, name) {
  for (.n in names(sub)) {
    txt <- gsub(paste0("{{", .n, "}}"), sub[[.n]], txt, fixed=TRUE)
  }
  if (grepl("{{", txt, fixed=TRUE)) {
    stop("unexpanded placeholder in the project of ", name, call.=FALSE)
  }
  txt
}

## Write run.mlxtran (and model.txt) in `dir`
kitWriteProject <- function(case, header, dir, est="full") {
  .tasks <- if (identical(case$est, "default")) .kitTasks(est) else case$est
  .sub <- c(PROBLEM=paste("kit case", case$name, "--", case$covers),
            DATA=case$dataFile, HEADER=paste(header, collapse=", "),
            MODEL="model.txt", TASKS=.tasks, SETTINGS=.kitSettings(case))
  .txt <- .kitExpand(case$mlxtran, .sub, case$name)
  .lines <- strsplit(.txt, "\n", fixed=TRUE)[[1]]
  if (est == "fixed" && identical(case$est, "default")) {
    .lines <- .kitFixParameters(.lines)
  }
  writeLines(.lines, file.path(dir, "run.mlxtran"))
  if (!is.null(case$model)) {
    writeLines(.kitExpand(case$model, .sub, case$name), file.path(dir, "model.txt"))
  }
  invisible(file.path(dir, "run.mlxtran"))
}
