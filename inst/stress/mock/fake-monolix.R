#!/usr/bin/env Rscript
## KIT SELF-TEST ONLY -- this is NOT Monolix.
##
## Stands in for Monolix so the kit's run/import plumbing can be checked
## without a license:
##   stressKit(monolix = paste("Rscript", system.file("stress", "mock", "fake-monolix.R",
##                                                     package = "monolix2rx"), "{mlxtran}"))
##
## Run in a case directory, it writes Monolix-format results (population
## parameters = the <PARAMETER> values; random effects and predictions from
## the rxode2 truth in sim.rds) and a "resaved" copy of the project.
## It says nothing about how Monolix itself behaves.

.args <- commandArgs(trailingOnly=TRUE)
if (identical(.args[1], "--version")) {
  cat("version: mock\n")
  quit(status=0)
}
.f <- .args[1]
.fail <- function(...) {
  writeLines(paste0(...), "monolix.failed")
  quit(status=1)
}
if (is.na(.f) || !file.exists(.f)) .fail("no project file: ", .f)
if (!file.exists("sim.rds")) .fail("the mock needs the kit's sim.rds")

.pkg <- Sys.getenv("MLXKIT_PKGDIR", "")
if (nzchar(.pkg)) {
  suppressMessages(pkgload::load_all(.pkg, quiet=TRUE))
} else {
  suppressMessages(library(monolix2rx))
}

.mlx <- suppressMessages(monolix2rx::mlxtran(.f))
.export <- .mlx$MONOLIX$SETTINGS$GLOBAL$exportpath
if (is.null(.export)) .export <- sub("[.]mlxtran$", "", basename(.f))
.ui <- suppressMessages(monolix2rx::monolix2rx(.f))
.sim <- readRDS("sim.rds")

dir.create(file.path(.export, "IndividualParameters"), recursive=TRUE, showWarnings=FALSE)
.w <- function(d, ...) utils::write.csv(d, file.path(.export, ...), row.names=FALSE, quote=FALSE)

.par <- .mlx$PARAMETER$PARAMETER
.w(data.frame(parameter=.par$name, value=.par$value), "populationParameters.txt")

## individual predictions and random effects are the truth's; IWRES is
## the translated model's at the true etas
.eta <- rownames(.ui$omega)
.theta <- utils::getFromNamespace(".addRxerr", "monolix2rx")(.ui, .ui$theta)
.ids <- unique(as.character(.ui$monolixData$id))
if (length(.eta) && !is.null(.sim$etas)) {
  .p <- .sim$etas[match(.ids, .sim$etas$id), .eta, drop=FALSE]
  for (.n in names(.theta)) .p[[.n]] <- .theta[[.n]]
} else {
  .p <- c(.theta, stats::setNames(rep(0, length(.eta)), .eta))
}
## Monolix writes no predictions of discrete observations; with several
## continuous endpoints one predictions_<observation>.txt each
.pd <- .ui$predDf
.cont <- !as.character(.pd$distribution) %in% c("pois", "ordinal", "LL")
.t <- .sim$pred
.tDvid <- .sim$data$DVID[match(.t$ROWID, .sim$data$ROWID)]
.end <- .mlx$MODEL$LONGITUDINAL$DEFINITION$endpoint
.endVar <- vapply(.end, function(e) e$var, character(1))
## the truth's DVID of imported endpoint k: by its prediction name when
## several endpoints are continuous (their order may differ)
.truthDvid <- function(k) {
  if (sum(.cont) < 2L) return(.pd$dvid[k])
  .e <- .end[[match(as.character(.pd$cond[k]), .endVar)]]
  .dv <- match(.e$pred, .sim$endpoints)
  if (length(.dv) != 1L || is.na(.dv)) .fail("no truth endpoint for ", .pd$cond[k])
  .dv
}
if (any(.cont)) {
  .s <- suppressMessages(rxode2::rxSolve(.ui$monolixModelIwres, .p, .ui$monolixData,
                                         returnType="data.frame", addDosing=FALSE,
                                         covsInterpolation="locf"))
  .key <- function(id, time) {
    paste(id, sprintf("%.12g", time),
          stats::ave(seq_along(id), id, time, FUN=seq_along), sep="|")
  }
  for (.k in which(.cont)) {
    .obs <- as.character(.pd$var[.k])
    .tk <- .t
    .sk <- .s
    if (nrow(.pd) > 1L) {
      .tk <- .t[.tDvid == .truthDvid(.k), ]
      .sk <- .s[.s$CMT == .pd$cmt[.k], ]
    }
    .m <- match(.key(.tk$ID, .tk$TIME), .key(as.character(.sk$id), .sk$time))
    .pred <- data.frame(id=.tk$ID, time=.tk$TIME,
                        dv=.sim$data$DV[match(.tk$ROWID, .sim$data$ROWID)],
                        popPred=.tk$simPred, indivPred_SAEM=.tk$simIpred,
                        indWRes_SAEM=.sk$iwres[.m])
    names(.pred)[3] <- .obs
    .w(.pred, if (sum(.cont) > 1L) paste0("predictions_", .pd$cond[.k], ".txt") else "predictions.txt")
  }
}

.re <- data.frame(id=.ids)
## Monolix names the random effect after the parameter, not its sd= name
.sdName <- monolix2rx:::.etaImportSd(.mlx)
for (.e in .eta) {
  .pn <- names(.sdName)[match(.e, .sdName)]
  if (is.na(.pn)) .pn <- sub("^omega_", "", .e)
  .re[[paste0("eta_", .pn, "_SAEM")]] <-
    if (is.null(.sim$etas)) 0 else .sim$etas[[.e]][match(.ids, .sim$etas$id)]
}
.w(.re, "IndividualParameters", "estimatedRandomEffects.txt")
.w(data.frame(id=.ids), "IndividualParameters", "estimatedIndividualParameters.txt")

## observations per endpoint
.dvid <- if (nrow(.pd) > 1L) .sim$data$DVID[match(.sim$pred$ROWID, .sim$data$ROWID)] else 1L
.nObs <- vapply(seq_len(nrow(.pd)), function(k) {
  paste0("Number of observations (", .pd$var[k], "): ",
         if (nrow(.pd) > 1L) sum(.dvid == .truthDvid(k)) else nrow(.sim$pred))
}, character(1))
.nDose <- sum(.sim$data$EVID %in% c(1L, 4L))
writeLines(c(strrep("*", 80),
             paste0("*  ", basename(.f)),
             "*  Monolix version : mock",
             strrep("*", 80), "",
             "DATASET INFORMATION",
             paste0("Number of individuals: ", length(unique(.sim$data$ID))),
             .nObs,
             paste0("Number of doses: ", .nDose), ""),
           file.path(.export, "summary.txt"))
## the resaved copy points at the results (what Monolix writes there
## without an exportpath is to confirm)
.rs <- readLines(.f, warn=FALSE)
if (is.null(.mlx$MONOLIX$SETTINGS$GLOBAL$exportpath)) {
  .rs <- c(.rs, "", "[SETTINGS]", "GLOBAL:", paste0("exportpath = '", .export, "'"))
}
writeLines(.rs, "run-resaved.mlxtran")
quit(status=0)
