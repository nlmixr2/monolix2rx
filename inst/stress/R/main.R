## runKit(): the lower-level runner behind stressKit()

#' Run the monolix2rx round-trip kit
#'
#' @param mode "dry" (no Monolix), "full" (run Monolix) or "import"
#'   (re-import Monolix output already in `out`)
#' @param cases character vector of case names (NULL is all)
#' @param tags character vector of tags; cases with any of them are run
#' @param cmd Monolix command template with {mlxtran} (see .kitMonolixCmd())
#' @param est estimation preset: "full" or "fixed"
#' @param nSub subjects per case
#' @param seed base seed (each case derives a stable seed from its name)
#' @param jobs cases run in parallel (socket workers)
#' @param out output directory
#' @param timeout Monolix timeout per case (seconds)
#' @return data.frame with one row per case (invisibly); summary.md and
#'   summary.csv are written to `out`
runKit <- function(mode=c("dry", "full", "import"), cases=NULL, tags=NULL,
                   cmd=NULL, est=c("full", "fixed"), nSub=20L, seed=42L,
                   jobs=1L, out="kit-runs", timeout=3600) {
  mode <- match.arg(mode)
  est <- match.arg(est)
  if (mode == "full" && is.null(cmd)) {
    stop("mode=\"full\" needs a Monolix command", call.=FALSE)
  }
  .cases <- kitCases(names=cases, tags=tags)
  if (length(.cases) == 0L) stop("no kit cases selected", call.=FALSE)
  out <- normalizePath(out, mustWork=FALSE)
  ## the solves are single threaded; restore the session's settings
  .rxThreads <- rxode2::getRxThreads()
  on.exit(rxode2::setRxThreads(.rxThreads), add=TRUE)
  rxode2::setRxThreads(1L)
  message(sprintf("monolix2rx kit: %d case(s), mode=%s, out=%s",
                  length(.cases), mode, out))
  .res <- kitRun(.cases, out, mode=mode, nSub=as.integer(nSub),
                 seed=as.integer(seed), est=est, cmd=cmd,
                 jobs=as.integer(jobs), timeout=as.numeric(timeout))
  .counts <- kitReport(.res, out)
  message(paste(paste0(names(.counts), ": ", .counts), collapse=" | "))
  message("summary: ", file.path(out, "summary.md"))
  attr(.res, "counts") <- .counts
  invisible(.res)
}
