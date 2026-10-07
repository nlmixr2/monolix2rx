## Orchestrate simulate -> Monolix -> monolix2rx for each case.
##
## Modes
##   "dry"    no Monolix: write the data and project, import with
##            monolix2rx and compare the translated PRED, omegas and
##            residual parameters with the truth.
##   "full"   also run Monolix, then import with validation (the project
##            as written and, when Monolix resaved it, run-resaved.mlxtran).
##   "import" re-import Monolix output already in the output directory.

.kitTranslateCols <- c("sim", "dryImport", "dryError", "dryMaxRel", "dryNobs",
                       "dryNexpected", "dryOmegaDiff", "dryErrDiff", "dryIpredMaxRel")

kitRunCase <- function(case, outDir, mode="dry", nSub=20L, seed=42L,
                       est="full", cmd=NULL, timeout=3600) {
  .dir <- file.path(outDir, case$name)
  .res <- list(case=case$name, file=case$file,
               tags=paste(case$tags, collapse=","),
               known=!is.null(case$known), mode=mode,
               sim=NA, monolix=NA, mlxSeconds=NA_real_, import=NA,
               importError=NA_character_, dryImport=NA,
               dryError=NA_character_, dryMaxRel=NA_real_, dryIpredMaxRel=NA_real_,
               dryNobs=NA_integer_, dryNexpected=NA_integer_,
               dryOmegaDiff=NA_real_, dryErrDiff=NA_real_,
               ipredRtol=NA_real_, ipredQ95=NA_real_, ipredMax=NA_real_,
               predRtol=NA_real_, predQ95=NA_real_, predMax=NA_real_,
               pkgIpredRtol=NA_real_, pkgPredRtol=NA_real_,
               iwresAtol=NA_real_, iwresRtol=NA_real_, nNotMatched=NA_integer_,
               dfSub=NA_real_, dfObs=NA_real_, expSub=NA_real_, expObs=NA_real_,
               thetaMatPd=NA, nTheta=NA_integer_, nEta=NA_integer_,
               nWarn=NA_integer_, resaved=NA, resavedError=NA_character_,
               resavedIpredRtol=NA_real_, status=NA_character_,
               note=NA_character_)
  .tol <- .kitTol(case)
  if (mode != "import") {
    unlink(.dir, recursive=TRUE)
    dir.create(.dir, recursive=TRUE, showWarnings=FALSE)
    .sim <- try(kitSimulate(case, nSub=nSub, seed=seed), silent=TRUE)
    if (inherits(.sim, "try-error")) {
      .res$sim <- FALSE
      .res$note <- paste("simulation failed:", trimws(.sim))
      return(.kitFinish(.res, case, .tol, .dir))
    }
    .res$sim <- TRUE
    saveRDS(.sim, file.path(.dir, "sim.rds"))
    .header <- kitWriteData(case, .sim$data, file.path(.dir, "data.csv"))
    kitWriteProject(case, .header, .dir, est=est)
    .dry <- kitImport(.dir, log="import-dry.log")
    .res$dryImport <- is.na(.dry$error)
    .res$dryError <- .dry$error
    if (.res$dryImport) {
      .mat <- try(kitDryMatrices(.dry$value, case), silent=TRUE)
      if (inherits(.mat, "try-error")) {
        .res$dryError <- paste("omega/error check:", trimws(.mat))
      } else {
        .res$dryOmegaDiff <- .mat[["omega"]]
        .res$dryErrDiff <- .mat[["err"]]
      }
    }
    if (.res$dryImport && isTRUE(case$dryPred)) {
      .dp <- try(kitDryPred(.dry$value, .sim, case), silent=TRUE)
      if (inherits(.dp, "try-error")) {
        .res$dryError <- trimws(.dp)
      } else {
        utils::write.csv(.dp$cmp, file.path(.dir, "dry-compare.csv"), row.names=FALSE)
        .res$dryMaxRel <- .dp$maxRel
        .res$dryIpredMaxRel <- .dp$maxRelIpred
        .res$dryNobs <- .dp$nObs
        .res$dryNexpected <- .dp$nExpected
        if (.dp$rowsExtra + .dp$rowsMissing > 0) {
          .res$dryError <- sprintf("imported data has %d unexpected and is missing %d simulated observations",
                                   .dp$rowsExtra, .dp$rowsMissing)
        }
      }
    }
  } else {
    ## keep the translate results of the run being re-imported
    .prev <- file.path(.dir, "result.rds")
    if (file.exists(.prev)) {
      .prev <- readRDS(.prev)
      .keep <- intersect(.kitTranslateCols, names(.prev))
      .res[.keep] <- .prev[1, .keep]
    }
  }
  if (mode == "full") {
    if (is.null(cmd)) stop("full mode needs a Monolix command", call.=FALSE)
    .mlx <- kitRunMonolix(.dir, cmd, timeout=timeout)
    .res$monolix <- .mlx$ok
    .res$mlxSeconds <- .mlx$seconds
    if (!.mlx$ok) {
      .why <- .kitMonolixError(.dir)
      .res$note <- paste("Monolix did not finish:", .why)
      .need <- .kitMonolixVersionNum(case$minMonolix)
      .have <- .kitMonolixVersionNum(.kitEnv$mlxVersion)
      if (!is.na(.need) && !is.na(.have) && .have < .need) {
        .res$status <- "SKIP"
        .res$note <- paste0("needs Monolix ", case$minMonolix, " (Monolix ",
                            .kitEnv$mlxVersion, ": ", .why, ")")
      }
      return(.kitFinish(.res, case, .tol, .dir))
    }
  }
  if (mode %in% c("full", "import")) {
    .sim <- file.path(.dir, "sim.rds")
    if (file.exists(.sim)) {
      .sim <- readRDS(.sim)
      .res$expSub <- length(unique(.sim$data$ID))
      .res$expObs <- nrow(.sim$pred)
    }
    .imp <- kitImport(.dir)
    .res$import <- is.na(.imp$error)
    .res$importError <- .imp$error
    .res$nWarn <- length(.imp$warnings)
    if (.res$import) {
      .met <- kitImportMetrics(.imp$value)
      .res[names(.met)] <- .met
    }
    if (file.exists(file.path(.dir, "run-resaved.mlxtran"))) {
      .rs <- kitImport(.dir, file="run-resaved.mlxtran", log="import-resaved.log")
      .res$resaved <- is.na(.rs$error)
      .res$resavedError <- .rs$error
      if (.res$resaved) .res$resavedIpredRtol <- kitImportMetrics(.rs$value)$ipredRtol
    } else if (file.exists(file.path(.dir, "resave.failed"))) {
      ## Monolix could not resave: reported, not graded
      .res$resavedError <- paste(readLines(file.path(.dir, "resave.failed"), warn=FALSE),
                                 collapse=" ")
    }
  }
  .kitFinish(.res, case, .tol, .dir)
}

.kitPassDry <- function(res, case, tol) {
  if (!isTRUE(res$sim) || !isTRUE(res$dryImport)) return(FALSE)
  .matOk <- function(x) is.na(x) || x <= tol$mat
  if (!.matOk(res$dryOmegaDiff) || !.matOk(res$dryErrDiff)) return(FALSE)
  if (!is.na(res$dryError)) return(FALSE)
  if (!isTRUE(case$dryPred)) return(TRUE)
  if (!is.na(res$dryIpredMaxRel) && res$dryIpredMaxRel > tol$dry) return(FALSE)
  is.finite(res$dryMaxRel) && res$dryMaxRel <= tol$dry &&
    identical(as.integer(res$dryNobs), as.integer(res$dryNexpected))
}

.kitPassFull <- function(res, tol) {
  if (!isTRUE(res$import)) return(FALSE)
  if (isFALSE(res$resaved)) return(FALSE)
  if (isFALSE(tol$validate)) return(TRUE)
  .chk <- function(v, t) is.na(t) || (is.finite(v) && v <= t)
  if (!is.finite(res$ipredRtol) && !is.finite(res$predRtol)) return(FALSE)
  if (!identical(as.integer(res$nNotMatched), 0L)) return(FALSE)
  if (isFALSE(res$thetaMatPd)) return(FALSE)
  .same <- function(a, b) is.na(b) || identical(as.numeric(a), as.numeric(b))
  if (!.same(res$dfSub, res$expSub) || !.same(res$dfObs, res$expObs)) return(FALSE)
  .chk(res$ipredRtol, tol$ipred) && .chk(res$predRtol, tol$pred) &&
    .chk(res$ipredQ95, tol$ipredQ95) && .chk(res$predQ95, tol$predQ95) &&
    .chk(res$iwresAtol, tol$iwres) &&
    (!isTRUE(res$resaved) || .chk(res$resavedIpredRtol, tol$ipred))
}

.kitFinish <- function(res, case, tol, dir) {
  ## knownRun only applies once Monolix output is involved
  .known <- case$known
  if (res$mode %in% c("full", "import") && !is.null(case$knownRun)) {
    .known <- paste(c(.known, case$knownRun), collapse="; ")
  }
  res$known <- !is.null(.known)
  if (!identical(res$status, "SKIP")) {
    .pass <- switch(res$mode,
                    dry=.kitPassDry(res, case, tol),
                    full=.kitPassDry(res, case, tol) && .kitPassFull(res, tol),
                    import=(!isTRUE(res$sim) || .kitPassDry(res, case, tol)) &&
                      .kitPassFull(res, tol))
    res$status <- if (.pass) {
      if (res$known) "XPASS" else "PASS"
    } else {
      if (res$known) "XFAIL" else "FAIL"
    }
    if (!.pass && is.na(res$note)) res$note <- .kitWhy(res, tol)
    if (res$known) {
      res$note <- if (is.na(res$note)) .known else paste0(.known, " [", res$note, "]")
    }
  }
  .df <- as.data.frame(res, stringsAsFactors=FALSE)
  if (dir.exists(dir)) saveRDS(.df, file.path(dir, "result.rds"))
  .df
}

## The first failed check, for the note
.kitWhy <- function(res, tol) {
  .w <- .kitWhyDry(res, tol)
  if (is.na(.w) && res$mode != "dry") .kitWhyRun(res, tol) else .w
}

.kitWhyDry <- function(res, tol) {
  if (isFALSE(res$sim)) return("simulation failed")
  if (isFALSE(res$dryImport)) return(paste("translate import failed:", res$dryError))
  if (!is.na(res$dryError)) return(res$dryError)
  if (is.finite(res$dryOmegaDiff) && res$dryOmegaDiff > tol$mat ||
        identical(res$dryOmegaDiff, Inf)) return("omega differs from the truth")
  if (is.finite(res$dryErrDiff) && res$dryErrDiff > tol$mat ||
        identical(res$dryErrDiff, Inf)) return("residual parameters differ from the truth")
  if (!is.na(res$dryMaxRel) && res$dryMaxRel > tol$dry) {
    return(sprintf("translated PRED differs from the truth by %.3g %%", res$dryMaxRel))
  }
  if (!is.na(res$dryIpredMaxRel) && res$dryIpredMaxRel > tol$dry) {
    return(sprintf("translated IPRED at the true etas differs from the truth by %.3g %%",
                   res$dryIpredMaxRel))
  }
  NA_character_
}

.kitWhyRun <- function(res, tol) {
  if (isFALSE(res$import)) return(paste("import failed:", res$importError))
  if (isFALSE(res$resaved)) return(paste("resaved import failed:", res$resavedError))
  if (isTRUE(res$import) && !is.finite(res$ipredRtol) && !is.finite(res$predRtol)) {
    return("monolix2rx did not validate the import")
  }
  if (!is.na(res$nNotMatched) && res$nNotMatched > 0) {
    return(paste(res$nNotMatched, "Monolix predictions not matched by rxode2"))
  }
  if (isFALSE(res$thetaMatPd)) return("thetaMat is not positive definite")
  if (!is.na(res$expSub) && !identical(as.numeric(res$dfSub), as.numeric(res$expSub)) ||
        !is.na(res$expObs) && !identical(as.numeric(res$dfObs), as.numeric(res$expObs))) {
    return(sprintf("dfSub/dfObs %s/%s, data has %s/%s", res$dfSub, res$dfObs,
                   res$expSub, res$expObs))
  }
  .over <- function(v, t) !is.na(t) && (!is.finite(v) || v > t)
  if (.over(res$ipredRtol, tol$ipred) || .over(res$ipredQ95, tol$ipredQ95)) {
    return(sprintf("ipred differs from Monolix: median %.3g %%, 95th percentile %.3g %%",
                   res$ipredRtol, res$ipredQ95))
  }
  if (.over(res$predRtol, tol$pred) || .over(res$predQ95, tol$predQ95)) {
    return(sprintf("pred differs from Monolix: median %.3g %%, 95th percentile %.3g %%",
                   res$predRtol, res$predQ95))
  }
  if (.over(res$iwresAtol, tol$iwres)) {
    return(sprintf("iwres differs from Monolix by %.3g", res$iwresAtol))
  }
  if (isTRUE(res$resaved) && .over(res$resavedIpredRtol, tol$ipred)) {
    return(if (is.finite(res$resavedIpredRtol)) {
      sprintf("resaved project ipred differs from Monolix: median %.3g %%", res$resavedIpredRtol)
    } else {
      "resaved project imported but was not validated (no Monolix results found)"
    })
  }
  NA_character_
}

kitRun <- function(cases, outDir, mode="dry", nSub=20L, seed=42L,
                   est="full", cmd=NULL, jobs=1L, timeout=3600) {
  dir.create(outDir, recursive=TRUE, showWarnings=FALSE)
  .one <- function(case) {
    .t0 <- Sys.time()
    .r <- tryCatch(kitRunCase(case, outDir, mode=mode, nSub=nSub,
                              seed=seed, est=est, cmd=cmd, timeout=timeout),
                   error=function(e) {
                     data.frame(case=case$name, file=case$file,
                                tags=paste(case$tags, collapse=","),
                                known=!is.null(case$known), mode=mode,
                                status="ERROR", note=conditionMessage(e))
                   })
    message(sprintf("[%-5s] %-32s %6.1fs %s", .r$status, case$name,
                    as.numeric(Sys.time() - .t0, units="secs"),
                    if (!is.null(.r$note) && !is.na(.r$note)) .r$note else ""))
    .r
  }
  .l <- if (jobs > 1L && .Platform$OS.type == "unix") {
    parallel::mclapply(cases, .one, mc.cores=jobs, mc.preschedule=FALSE)
  } else {
    lapply(cases, .one)
  }
  ## a crashed fork gives NULL or a try-error instead of a result row
  .l <- lapply(seq_along(.l), function(i) {
    .x <- .l[[i]]
    if (is.data.frame(.x)) return(.x)
    data.frame(case=cases[[i]]$name, mode=mode, status="ERROR",
               note=paste("worker failed:",
                          if (is.null(.x)) "no result (crashed?)" else trimws(as.character(.x))))
  })
  .all <- unique(unlist(lapply(.l, names)))
  .l <- lapply(.l, function(d) {
    for (.n in setdiff(.all, names(d))) d[[.n]] <- NA
    d[, .all]
  })
  do.call(rbind, .l)
}
