## Import a case with monolix2rx and score it.

## percent thresholds: ipred/pred on the median, ipredQ95/predQ95 on the
## 95th percentile; iwres is an absolute median difference (off by
## default); dry is the translate-mode PRED threshold; mat the omega and
## residual-parameter relative threshold; validate=FALSE accepts an
## import monolix2rx did not validate
.kitTolDefault <- list(ipred=1, pred=1, ipredQ95=5, predQ95=5, iwres=NA,
                       dry=0.01, mat=1e-6, validate=TRUE)

.kitTol <- function(case) {
  .t <- .kitTolDefault
  .t[names(case$tol)] <- case$tol
  .t
}

## Run `expr`, collecting messages/warnings to `logFile`
.kitCapture <- function(expr, logFile) {
  .log <- character(0)
  .warn <- character(0)
  .out <- utils::capture.output(.val <- tryCatch(
    withCallingHandlers(expr,
      message=function(m) {
        .log <<- c(.log, sub("\n$", "", conditionMessage(m)))
        invokeRestart("muffleMessage")
      },
      warning=function(w) {
        .warn <<- c(.warn, conditionMessage(w))
        .log <<- c(.log, paste("WARNING:", conditionMessage(w)))
        invokeRestart("muffleWarning")
      }),
    error=function(e) {
      .log <<- c(.log, paste("ERROR:", conditionMessage(e)))
      structure(conditionMessage(e), class="kitError")
    }))
  writeLines(c(.log, if (length(.out)) c("---- stdout ----", .out)), logFile)
  if (inherits(.val, "kitError")) {
    .err <- c(unclass(.val), grep(":ERR:|syntax error", .out, value=TRUE))
    return(list(value=NULL, error=paste(unique(.err), collapse=" "),
                warnings=.warn))
  }
  list(value=.val, error=NA_character_, warnings=.warn)
}

kitImport <- function(dir, file="run.mlxtran", log="import.log") {
  .kitCapture(monolix2rx::monolix2rx(file.path(dir, file)),
              file.path(dir, log))
}

.kitNum <- function(x) if (is.null(x) || length(x) != 1L) NA_real_ else as.numeric(x)

## percent differences of a against b; a missing rxode2 value is Inf
.kitRel <- function(cmp, a, b) {
  if (is.null(cmp) || !all(c(a, b) %in% names(cmp))) return(NULL)
  .d <- abs(cmp[[a]] - cmp[[b]])
  .r <- ifelse(abs(cmp[[b]]) > 1e-8, 100 * .d / abs(cmp[[b]]),
               ifelse(.d <= 1e-6, 0, Inf))
  .r[is.finite(cmp[[b]]) & !is.finite(.r)] <- Inf
  .r[is.finite(cmp[[b]])]
}

.kitQ <- function(r, p) if (length(r) == 0) NA_real_ else
  unname(stats::quantile(r, p, type=1))

.kitPd <- function(m) {
  if (is.null(m)) return(NA)
  .e <- try(eigen(as.matrix(m), symmetric=TRUE, only.values=TRUE)$values, silent=TRUE)
  !inherits(.e, "try-error") && all(is.finite(.e)) && all(.e > 0)
}

## Validation metrics from monolix2rx's Monolix vs rxode2 comparison; the
## kit takes its own percentiles of the compared rows (monolix2rx's
## predRtol is relative to Monolix's IPRED) and keeps monolix2rx's values
## (fractions) as pkg*
kitImportMetrics <- function(m) {
  .ri <- .kitRel(m$ipredCompare, "ipred", "monolixIpred")
  .rp <- .kitRel(m$predCompare, "pred", "monolixPred")
  list(ipredRtol=.kitQ(.ri, 0.5), ipredQ95=.kitQ(.ri, 0.95),
       ipredMax=.kitQ(.ri, 1),
       predRtol=.kitQ(.rp, 0.5), predQ95=.kitQ(.rp, 0.95),
       predMax=.kitQ(.rp, 1),
       pkgIpredRtol=100 * .kitNum(m$ipredRtol), pkgPredRtol=100 * .kitNum(m$predRtol),
       iwresAtol=.kitNum(m$iwresAtol), iwresRtol=100 * .kitNum(m$iwresRtol),
       nNotMatched=NROW(m$monolixNotMatched),
       dfSub=.kitNum(m$meta$dfSub), dfObs=.kitNum(m$meta$dfObs),
       thetaMatPd=.kitPd(m$thetaMat),
       nTheta=length(m$theta), nEta=NROW(m$omega))
}

## (id, time, repeat index) row key: monolix2rx drops undeclared data
## columns, so a ROWID column cannot be carried through the import
.kitKey <- function(id, time) {
  .k <- stats::ave(seq_along(id), id, time, FUN=seq_along)
  paste(id, sprintf("%.12g", time), .k, sep="|")
}

## steady-state doses monolix2rx read from the project
.kitNbdoses <- function(m) {
  .n <- utils::getFromNamespace(".getNbdoses", "monolix2rx")(m)
  if (length(.n) != 1L || is.na(.n)) 7L else as.integer(.n)
}

## Solve the imported model with the theta values and `etas` (a data
## frame with id and the eta columns, or NULL for zero random effects)
kitImportSolve <- function(m, etas, case) {
  ## m$omega is a list by level with inter-occasion variability
  .ini <- m$iniDf
  .ini <- .ini[!is.na(.ini$neta1) & .ini$neta1 == .ini$neta2, ]
  .iov <- .ini$condition != "id"
  .eta <- .ini$name[!.iov]
  .theta <- utils::getFromNamespace(".addRxerr", "monolix2rx")(m, m$theta)
  .d <- m$monolixData
  if (is.null(.d)) stop("monolix2rx did not read the data", call.=FALSE)
  if (is.null(etas) || nrow(.ini) == 0L) {
    .p <- c(.theta, stats::setNames(rep(0, nrow(.ini)), .ini$name))
  } else {
    .miss <- setdiff(.eta, names(etas))
    if (length(.miss)) stop("imported etas not in the truth: ",
                            paste(.miss, collapse=", "), call.=FALSE)
    ## one row per subject, in the data's subject order
    .id <- unique(as.character(.d$id))
    .p <- etas[match(.id, etas$id), .eta, drop=FALSE]
    if (anyNA(.p)) stop("subjects without true etas", call.=FALSE)
    for (.n in names(.theta)) .p[[.n]] <- .theta[[.n]]
    ## each row gets its occasion's eta as a data column
    for (.i in which(.iov)) {
      .d[[.ini$name[.i]]] <- .kitIovColumn(.d, etas, .ini$name[.i], .ini$condition[.i])
    }
  }
  .s <- suppressMessages(do.call(rxode2::rxSolve,
                                 c(list(m$monolixModelIwres, .p, .d,
                                        returnType="data.frame", addDosing=FALSE),
                                   .kitSolveOpts(.kitNbdoses(m)), case$solve)))
  data.frame(key=.kitKey(as.character(.s$id), .s$time), mlx=.s$ipredSim,
             iwres=if (is.null(.s$iwres)) NA_real_ else .s$iwres)
}

## The true inter-occasion eta of each data row: the truth's
## `eta(OCC==k)` columns matched on the imported occasion column's value
.kitIovColumn <- function(d, etas, eta, level) {
  .re <- paste0("^", eta, "[(][^=]*==(.*)[)]$")
  .cols <- grep(.re, names(etas), value=TRUE)
  if (length(.cols) == 0L) {
    stop("imported etas not in the truth: ", eta, call.=FALSE)
  }
  if (is.null(d[[level]])) {
    stop("the imported data has no occasion column '", level, "'", call.=FALSE)
  }
  .occ <- sub(.re, "\\1", .cols)
  .row <- match(as.character(d$id), etas$id)
  .col <- match(as.character(d[[level]]), .occ)
  if (anyNA(.row) || anyNA(.col)) stop("rows without true occasion etas", call.=FALSE)
  as.matrix(etas[, .cols, drop=FALSE])[cbind(.row, .col)]
}

.kitMaxRel <- function(a, b) {
  .d <- abs(a - b)
  .d[!is.finite(.d)] <- Inf
  .r <- ifelse(abs(b) > 1e-8, 100 * .d / abs(b), ifelse(.d <= 1e-8, 0, Inf))
  if (length(.r)) max(.r) else NA_real_
}

## Monolix-free check: solve the imported model at the <PARAMETER> values
## (the truth) and compare with the truth's PRED (zero random effects) and
## IPRED (the true etas, which also checks how etas enter the parameters).
kitDryPred <- function(m, sim, case) {
  .t <- sim$pred
  .t$key <- .kitKey(as.character(.t$ID), .t$TIME)
  .pop <- kitImportSolve(m, NULL, case)
  .cmp <- merge(.t, stats::setNames(.pop[, 1:2], c("key", "mlxPred")), by="key")
  .nEta <- sum(!is.na(m$iniDf$neta1))
  .ind <- if (!is.null(sim$etas) && .nEta > 0L) kitImportSolve(m, sim$etas, case)
  .cmp$mlxIpred <- if (is.null(.ind)) NA_real_ else .ind$mlx[match(.cmp$key, .ind$key)]
  ## the truth has etas the import lost: IPRED cannot match
  .lost <- !is.null(sim$etas) && is.null(.ind)
  list(cmp=.cmp[, c("ID", "TIME", "simPred", "mlxPred", "simIpred", "mlxIpred")],
       nObs=nrow(.cmp), nExpected=nrow(.t),
       rowsExtra=nrow(.pop) - nrow(.cmp), rowsMissing=nrow(.t) - nrow(.cmp),
       maxRel=.kitMaxRel(.cmp$mlxPred, .cmp$simPred),
       maxRelIpred=if (.lost) Inf else if (is.null(.ind)) NA_real_ else
         .kitMaxRel(.cmp$mlxIpred, .cmp$simIpred))
}

## largest relative difference of named values; a missing name is Inf
.kitNamedDiff <- function(a, b) {
  if (length(b) == 0L && length(a) == 0L) return(0)
  if (!setequal(names(a), names(b))) return(Inf)
  .a <- a[names(b)]
  max(abs(.a - b) / pmax(1, abs(b)))
}

## omega entries named "eta1|eta2"; a list (inter-occasion levels) is
## flattened, so differently named levels still compare
.kitOmegaFlat <- function(o) {
  if (is.null(o)) return(numeric(0))
  if (is.list(o)) return(unlist(lapply(unname(o), .kitOmegaFlat)))
  .n <- rownames(o)
  stats::setNames(as.vector(o), paste0(rep(.n, length(.n)), "|", rep(.n, each=length(.n))))
}

## omega difference by eta name; different eta names are Inf
.kitOmegaDiff <- function(mo, so) {
  .kitNamedDiff(.kitOmegaFlat(mo), .kitOmegaFlat(so))
}

## Residual error model: each parameter with its type, and the
## combined1/combined2 form of add + prop endpoints
.kitErrModel <- function(ui) {
  .ini <- ui$iniDf
  .ini <- .ini[!is.na(.ini$err) & is.na(.ini$neta1), ]
  .p <- ui$predDf
  .ap <- as.character(.p$addProp)
  .ap[.ap == "default"] <- getOption("rxode2.addProp", "combined2")
  .ap <- .ap[grepl("add", .p$errType) & grepl("prop", .p$errType)]
  paste(c(sort(paste0(.ini$name, ":", .ini$err)), .ap), collapse=", ")
}

## Compare the imported omega and residual-error parameters with the truth
## by name (PRED does not depend on them); a different error model is Inf
kitDryMatrices <- function(m, case) {
  .ui <- suppressMessages(rxode2::assertRxUi(case$sim))
  .ret <- c(omega=NA_real_, err=NA_real_)
  if (isTRUE(case$dryOmega)) .ret["omega"] <- .kitOmegaDiff(m$omega, .ui$omega)
  .ini <- .ui$iniDf
  .ini <- .ini[!is.na(.ini$err) & is.na(.ini$neta1), ]
  .mi <- m$iniDf
  .mi <- .mi[!is.na(.mi$err) & is.na(.mi$neta1), ]
  .ret["err"] <- .kitNamedDiff(stats::setNames(.mi$est, .mi$name),
                               stats::setNames(.ini$est, .ini$name))
  .em <- c(truth=.kitErrModel(.ui), import=.kitErrModel(m))
  if (.em[1] != .em[2]) .ret["err"] <- Inf
  attr(.ret, "errModel") <- .em
  .ret
}
