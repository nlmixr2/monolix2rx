## Case registry for the monolix2rx round-trip kit.
##
## A case is one "simulate -> Monolix -> monolix2rx" round trip:
##
## - `sim`: rxode2 ui function; its `ini()` values are the truth.  Name
##   the etas like the Monolix omega parameters (`omega_Cl ~ 0.09`) and the
##   residual parameters like the Monolix ones (`a`, `b`), so the
##   translate check can compare them by name.
## - `data`: function(nSub) returning a data.frame from the builders in
##   data.R (ROWID is added automatically).
## - `mlxtran`: project text.  Placeholders:
##     {{PROBLEM}}  case name and description (for DESCRIPTION:)
##     {{DATA}}     data file name (data.csv)
##     {{HEADER}}   header column list (from the written data)
##     {{MODEL}}    model file name (model.txt)
##     {{TASKS}}    the <MONOLIX> [TASKS] section (see .kitTasks())
##     {{SETTINGS}} the <MONOLIX> [SETTINGS] section (exportpath)
## - `model`: model file text written to model.txt (NULL for `lib:`);
##   the same placeholders are expanded.
##
## Optional:
##
## - `columns`: data columns written (default ID TIME AMT DV).
## - `write`: function(d) -> data.frame or lines written instead.
## - `postSim`: function(d, s) -> d for custom DV handling.
## - `tol`: thresholds overriding .kitTolDefault (import.R).
## - `known`: known failure (XFAIL); `knownRun`: only once Monolix output
##   is involved.
## - `est`: "default" uses the run-wide preset; any other string is used
##   verbatim as the [TASKS] section.
## - `dryPred`/`dryOmega`: FALSE skips that translate check.
## - `nbSSDoses`: Monolix steady-state doses (default 7).
## - `minMonolix`: Monolix version needed (like "2024R1"); a failed run
##   on an older Monolix is SKIP.
## - `solve`: extra rxSolve() options for the truth (like method=).

.kitEnv <- new.env(parent=emptyenv())
.kitEnv$cases <- list()

kitCase <- function(name, covers, tags=character(0), sim, data, mlxtran,
                    model=NULL, columns=c("ID", "TIME", "AMT", "DV"),
                    write=NULL, postSim=NULL, tol=list(), known=NULL,
                    knownRun=NULL, est="default", dryPred=TRUE,
                    dryOmega=TRUE, nSub=NULL, nbSSDoses=7L,
                    minMonolix=NULL, solve=list()) {
  stopifnot(is.character(name), length(name) == 1L,
            !grepl("[^A-Za-z0-9_-]", name))
  if (!is.null(.kitEnv$cases[[name]])) {
    stop("duplicate kit case name: ", name, call.=FALSE)
  }
  .kitEnv$cases[[name]] <- list(name=name, covers=covers, tags=tags,
                                sim=sim, data=data, mlxtran=mlxtran,
                                model=model, columns=columns, write=write,
                                postSim=postSim, tol=tol, known=known,
                                knownRun=knownRun, est=est,
                                dryPred=dryPred, dryOmega=dryOmega,
                                nSub=nSub, nbSSDoses=nbSSDoses,
                                minMonolix=minMonolix, solve=solve,
                                file=if (is.null(.kitEnv$curFile)) NA_character_ else .kitEnv$curFile)
  invisible(name)
}

kitLoadCases <- function(dir) {
  .kitEnv$cases <- list()
  for (.f in sort(list.files(dir, pattern="[.][Rr]$", full.names=TRUE))) {
    .kitEnv$curFile <- basename(.f)
    sys.source(.f, envir=environment(kitCase))
  }
  .kitEnv$curFile <- NULL
  invisible(names(.kitEnv$cases))
}

kitCases <- function(names=NULL, tags=NULL) {
  .c <- .kitEnv$cases
  if (!is.null(names)) {
    .bad <- setdiff(names, base::names(.c))
    if (length(.bad) > 0) stop("unknown kit case(s): ",
                               paste(.bad, collapse=", "), call.=FALSE)
    .c <- .c[names]
  }
  if (!is.null(tags)) {
    .c <- .c[vapply(.c, function(x) any(tags %in% x$tags), logical(1))]
  }
  .c
}

## Register a variant of an existing case, overriding some fields
kitVariant <- function(base, name, covers, ..., known=NULL, knownRun=NULL) {
  .c <- .kitEnv$cases[[base]]
  if (is.null(.c)) stop("unknown base case: ", base, call.=FALSE)
  .c$name <- name
  .c$covers <- covers
  .c$known <- known
  .c$knownRun <- knownRun
  .over <- list(...)
  .c[names(.over)] <- .over
  .c$file <- NULL
  do.call(kitCase, .c)
}
