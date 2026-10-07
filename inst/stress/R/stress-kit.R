## The kit for a Monolix machine, matching the nonmem2rx and babelmixr2
## kits: stressCheck(), stressList(), stressKit() and, back home,
## stressReplay().

#' Package versions for the report
#'
#' @return markdown list lines
stressVersions <- function() {
  .p <- c("monolix2rx", "rxode2", "lotri", "dparser", "nlmixr2est",
          "lixoftConnectors")
  .v <- vapply(.p, function(p) {
    if (requireNamespace(p, quietly=TRUE)) as.character(utils::packageVersion(p)) else "-"
  }, character(1))
  .how <- ""
  .dir <- Sys.getenv("MLXKIT_PKGDIR", "")
  if (nzchar(.dir)) {
    .git <- function(...) suppressWarnings(tryCatch(
      system2("git", c("-C", shQuote(.dir), ...), stdout=TRUE, stderr=FALSE),
      error=function(e) character(0)))
    .sha <- .git("rev-parse", "--short", "HEAD")
    .br <- .git("rev-parse", "--abbrev-ref", "HEAD")
    .how <- paste0(" (source ", .dir,
                   if (length(.sha) == 1L) paste0("; ", .br, " ", .sha), ")")
  } else {
    .sha <- utils::packageDescription("monolix2rx")$RemoteSha
    if (!is.null(.sha)) .how <- paste0(" (", substr(.sha, 1, 7), ")")
  }
  c(paste0("- ", .p, " ", .v, ifelse(.p == "monolix2rx", .how, "")),
    paste0("- rxode2 delay(): ", .kitHas("rxode2", "delay")),
    paste0("- R ", getRversion(), " on ", R.version$platform))
}

.kitHas <- function(pkg, fun) {
  requireNamespace(pkg, quietly=TRUE) && exists(fun, envir=asNamespace(pkg))
}

#' Report the versions and whether Monolix is found
#'
#' @param monolix "lixoftConnectors" or a Monolix command template with
#'   {mlxtran}; NULL looks for it (see stressFindMonolix())
#' @return the Monolix setting ("" when not found), invisibly
stressCheck <- function(monolix=NULL) {
  .mlx <- if (is.null(monolix)) stressFindMonolix() else monolix
  message(paste(stressVersions(), collapse="\n"))
  message("- Monolix: ", if (nzchar(.mlx)) .mlx else
    "not found (install lixoftConnectors or give a command with monolix=)")
  if (nzchar(.mlx)) {
    .v <- .kitMonolixVersion(.mlx)
    message("- Monolix version: ", if (is.na(.v)) "unknown" else .v)
  }
  message("- cases: ", length(.kitEnv$cases))
  invisible(.mlx)
}

.stressSelect <- function(cases=NULL, tags=NULL) {
  .c <- kitCases(tags=tags)
  if (!is.null(cases)) .c <- .c[grepl(cases, names(.c))]
  .c
}

#' List the stress cases
#'
#' @param cases regular expression of case names (NULL is all)
#' @param tags character vector of tags (cases with any of them)
#' @return data frame of the cases, invisibly
stressList <- function(cases=NULL, tags=NULL) {
  .c <- .stressSelect(cases, tags)
  .ret <- data.frame(
    case=names(.c),
    tags=vapply(.c, function(x) paste(x$tags, collapse=","), ""),
    known=vapply(.c, function(x) {
      if (!is.null(x$known)) "always" else if (!is.null(x$knownRun)) "run" else ""
    }, ""),
    description=vapply(.c, function(x) x$covers, ""),
    row.names=NULL, stringsAsFactors=FALSE)
  print(.ret, right=FALSE)
  invisible(.ret)
}

#' Zip the output directory to send back
#'
#' @param out output directory
#' @return the zip file, invisibly (NULL when zipping failed)
stressBundle <- function(out) {
  .zip <- paste0(out, ".zip")
  .old <- setwd(dirname(out))
  .ok <- try(utils::zip(.zip, basename(out), flags="-r9Xq"), silent=TRUE)
  setwd(.old)
  if (inherits(.ok, "try-error") || !file.exists(.zip)) {
    message("could not zip the output; send the directory ", out, " instead")
    return(invisible(NULL))
  }
  message("send this file back: ", .zip)
  invisible(.zip)
}

#' Run the stress kit
#'
#' Simulates every case with rxode2, writes the Monolix project and data,
#' checks the monolix2rx translation, runs Monolix, imports the run with
#' monolix2rx and compares Monolix's predictions with rxode2.  Then zips
#' the output to send back.
#'
#' @param monolix "lixoftConnectors" or a Monolix command template with
#'   {mlxtran}; NULL looks for it
#' @param modes "translate" (no Monolix) and/or "run" (also run Monolix);
#'   "run" includes the translation checks
#' @inheritParams stressList
#' @param out output directory
#' @param est estimation preset: "full" (SAEM, FIM, log-likelihood) or
#'   "fixed" (population parameters fixed at the truth; faster)
#' @param nSub subjects per case
#' @param jobs cases run in parallel (socket workers)
#' @param timeout Monolix timeout per case (seconds; not on Windows)
#' @param bundle zip the output directory
#' @return data frame of the results (invisibly) with the attributes `out`
#'   and `zip`
stressKit <- function(monolix=NULL, modes=c("translate", "run"), cases=NULL,
                      tags=NULL,
                      out=paste0("monolix2rx-stress-", format(Sys.time(), "%Y%m%d-%H%M%S")),
                      est=c("full", "fixed"), nSub=20L, jobs=1L,
                      timeout=3600, bundle=TRUE) {
  modes <- match.arg(modes, c("translate", "run"), several.ok=TRUE)
  est <- match.arg(est)
  .mlx <- if (is.null(monolix)) stressFindMonolix() else monolix
  .run <- "run" %in% modes
  if (.run && !nzchar(.mlx)) {
    stop("Monolix is not found; install lixoftConnectors or give monolix=, ",
         "or use modes = \"translate\"", call.=FALSE)
  }
  .cases <- .stressSelect(cases, tags)
  if (length(.cases) == 0L) stop("no stress cases selected", call.=FALSE)
  dir.create(out, showWarnings=FALSE, recursive=TRUE)
  out <- normalizePath(out)
  writeLines(utils::capture.output(utils::sessionInfo()),
             file.path(out, "sessionInfo.txt"))
  message(paste(stressVersions(), collapse="\n"))
  .vline <- NULL
  if (.run) {
    .kitEnv$mlxVersion <- .kitMonolixVersion(.mlx)
    on.exit(.kitEnv$mlxVersion <- NULL, add=TRUE)
    .vline <- paste0("- Monolix: ", .mlx, " (version ",
                     if (is.na(.kitEnv$mlxVersion)) "unknown" else .kitEnv$mlxVersion, ")")
    message(.vline)
  }
  .res <- runKit(mode=if (.run) "full" else "dry", cases=names(.cases),
                 cmd=if (.run) .kitMonolixCmd(.mlx), est=est, nSub=nSub,
                 jobs=jobs, out=out, timeout=timeout)
  utils::write.csv(.res, file.path(out, "results.csv"), row.names=FALSE)
  .md <- readLines(file.path(out, "summary.md"))
  .md <- append(.md, c("", "## Versions", "", stressVersions(), .vline), after=2L)
  writeLines(.md, file.path(out, "summary.md"))
  .bad <- .res[.res$status %in% c("FAIL", "ERROR", "XPASS"), , drop=FALSE]
  if (nrow(.bad) > 0L) {
    message("\nto look at:\n",
            paste(sprintf("  %-28s %-5s %s", .bad$case, .bad$status,
                          ifelse(is.na(.bad$note), "", substr(.bad$note, 1, 90))),
                  collapse="\n"))
  }
  attr(.res, "out") <- out
  attr(.res, "zip") <- if (bundle) stressBundle(out)
  invisible(.res)
}

#' Re-import a returned stress-kit zip (no Monolix needed)
#'
#' @param zip the zip file (or an unzipped output directory)
#' @param cases regular expression of the case names (NULL is all)
#' @param out directory to unzip into
#' @param jobs cases imported in parallel
#' @return data frame of the results, invisibly
stressReplay <- function(zip, cases=NULL, out=tempfile("monolix2rx-replay-"),
                         jobs=1L) {
  dir.create(out, showWarnings=FALSE, recursive=TRUE)
  if (dir.exists(zip)) {
    ## a copy, so the returned results and summaries are not overwritten
    file.copy(normalizePath(zip), out, recursive=TRUE)
    .dir <- file.path(out, basename(zip))
  } else {
    utils::unzip(zip, exdir=out)
    .top <- list.dirs(out, recursive=FALSE)
    .dir <- if (length(.top) == 1L) .top else out
  }
  .ran <- basename(list.dirs(.dir, recursive=FALSE))
  .done <- vapply(.ran, function(r) {
    file.exists(file.path(.dir, r, .kitExport, "populationParameters.txt"))
  }, logical(1))
  if (any(!.done)) {
    message("Monolix did not finish (not replayed): ",
            paste(.ran[!.done], collapse=", "))
  }
  .ran <- .ran[.done]
  .known <- intersect(.ran, names(.kitEnv$cases))
  if (length(setdiff(.ran, .known)) > 0L) {
    message("not cases of this kit (skipped): ",
            paste(setdiff(.ran, .known), collapse=", "))
  }
  if (!is.null(cases)) .known <- .known[grepl(cases, .known)]
  if (length(.known) == 0L) stop("no Monolix runs to replay in ", .dir, call.=FALSE)
  runKit(mode="import", cases=.known, out=.dir, jobs=jobs)
}
