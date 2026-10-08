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
kitImportSolve <- function(m, etas, case, mix=NULL) {
  ## m$omega is a list by level with inter-occasion variability
  .ini <- m$iniDf
  .ini <- .ini[!is.na(.ini$neta1) & .ini$neta1 == .ini$neta2, ]
  .iov <- .ini$condition != "id"
  .eta <- .ini$name[!.iov]
  .theta <- utils::getFromNamespace(".addRxerr", "monolix2rx")(m, m$theta)
  .d <- m$monolixData
  if (is.null(.d)) stop("monolix2rx did not read the data", call.=FALSE)
  ## each subject's true mixture class
  if (!is.null(mix)) .d$mixest <- mix$mixest[match(as.character(.d$id), mix$id)]
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
  ## only observations (MDV=1 rows are imported as evid 2)
  .d$kitObs <- if (is.null(.d[["evid"]])) 1L else as.integer(.d$evid %in% 0L)
  ## rows of the endpoint checked by likelihood instead
  .d$kitLik <- 0L
  if (is.character(case$dryLik)) .d$kitLik <- as.integer(.kitEndpointRows(.d, m, .kitLikIndex(m, case)))
  .s <- suppressMessages(do.call(rxode2::rxSolve,
                                 c(list(m$monolixModelIwres, .p, .d,
                                        returnType="data.frame", addDosing=FALSE,
                                        keep=c("kitObs", "kitLik")),
                                   .kitSolveOpts(.kitNbdoses(m)), case$solve)))
  .s <- .s[.s$kitObs == 1L, ]
  .ret <- data.frame(key=.kitKey(as.character(.s$id), .s$time), mlx=.s$ipredSim,
                     iwres=if (is.null(.s$iwres)) NA_real_ else .s$iwres)
  .ret[.s$kitLik == 0L, ]
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
  ## keyed over all observations, as the import is
  .t$key <- .kitKey(as.character(.t$ID), .t$TIME)
  if (is.character(case$dryLik)) .t <- .t[!.kitPredLikRows(sim, case), ]
  .pop <- kitImportSolve(m, NULL, case, sim$mix)
  .cmp <- merge(.t, stats::setNames(.pop[, 1:2], c("key", "mlxPred")), by="key")
  .nEta <- sum(!is.na(m$iniDf$neta1))
  .ind <- if (!is.null(sim$etas) && .nEta > 0L) kitImportSolve(m, sim$etas, case, sim$mix)
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

## The endpoint checked by the likelihood: dryLik=TRUE is the only one,
## a name picks one of several
.kitLikIndex <- function(ui, case) {
  .p <- ui$predDf
  if (isTRUE(case$dryLik)) {
    if (nrow(.p) != 1L) stop("dryLik=TRUE needs one endpoint; name it", call.=FALSE)
    return(1L)
  }
  .k <- which(as.character(.p$var) == case$dryLik)
  if (length(.k) != 1L) stop("no endpoint '", case$dryLik, "'", call.=FALSE)
  .k
}

## observation rows of endpoint k: the truth's DVID or the import's cmt
.kitEndpointRows <- function(d, ui, k) {
  .p <- ui$predDf
  if (nrow(.p) == 1L) return(rep(TRUE, nrow(d)))
  if (!is.null(d$dvid)) return(d$dvid %in% .p$dvid[k])
  as.character(d$cmt) %in% as.character(.p$var[k])
}

## the truth's observations (sim$pred rows) of the likelihood endpoint
.kitPredLikRows <- function(sim, case) {
  .ui <- suppressMessages(rxode2::assertRxUi(case$sim))
  .d <- sim$data[match(sim$pred$ROWID, sim$data$ROWID), , drop=FALSE]
  names(.d) <- tolower(names(.d))
  .kitEndpointRows(.d, .ui, .kitLikIndex(.ui, case))
}

## The log-likelihood expression of endpoint k, from rxode2 itself
.kitLikExpr <- function(ui, k) {
  .ui <- rxode2::rxUiDecompress(ui)
  utils::getFromNamespace(".getQuotedDistributionAndLlikArgs", "rxode2")(.ui, ui$predDf[k, ])
}

## Per-observation log-likelihood of endpoint k of `ui` at its thetas and
## `etas`; the other endpoints are dropped
.kitLik <- function(ui, d, etas, case, k) {
  .l <- ui$lstExpr
  .line <- ui$predDf$line
  .l[[.line[k]]] <- bquote(kitLL <- .(.kitLikExpr(ui, k)))
  .l <- .l[setdiff(seq_along(.l), .line[-k])]
  .mod <- rxode2::rxode2(paste(vapply(.l, deparse1, character(1)), collapse="\n"))
  .ini <- ui$iniDf
  .theta <- stats::setNames(.ini$est, .ini$name)[is.na(.ini$neta1)]
  .eta <- .ini$name[!is.na(.ini$neta1) & .ini$neta1 == .ini$neta2]
  names(d) <- tolower(names(d))
  .id <- unique(as.character(d$id))
  .p <- if (length(.eta) == 0L) data.frame(row.names=seq_along(.id)) else
    etas[match(.id, etas$id), .eta, drop=FALSE]
  if (anyNA(.p)) stop("subjects without true etas", call.=FALSE)
  for (.n in names(.theta)) .p[[.n]] <- .theta[[.n]]
  .d <- d
  .zero <- function(x) if (is.null(x)) 0L else x
  .d$kitObs <- as.integer(.zero(.d$evid) %in% 0L & .zero(.d$mdv) %in% 0L)
  .d$kitLik <- as.integer(.kitEndpointRows(.d, ui, k))
  ## the endpoint compartments and ids are not in the likelihood model
  if (is.character(.d$cmt)) .d$cmt <- NULL
  .d$dvid <- NULL
  .s <- suppressMessages(do.call(rxode2::rxSolve,
                                 c(list(.mod, .p, .d, returnType="data.frame",
                                        addDosing=FALSE, keep=c("kitObs", "kitLik")),
                                   case$solve)))
  .s <- .s[.s$kitObs == 1L, ]
  .ret <- data.frame(key=.kitKey(as.character(.s$id), .s$time), ll=.s$kitLL)
  .ret[.s$kitLik == 1L, ]
}

## Translate check for discrete endpoints: the imported model's
## log-likelihood of each observation equals the truth's at the true etas
kitDryLik <- function(m, sim, case) {
  .ui <- suppressMessages(rxode2::assertRxUi(case$sim))
  if (is.null(m$monolixData)) stop("monolix2rx did not read the data", call.=FALSE)
  .t <- .kitLik(.ui, sim$data, sim$etas, case, .kitLikIndex(.ui, case))
  .i <- .kitLik(m, m$monolixData, sim$etas, case, .kitLikIndex(m, case))
  .cmp <- merge(stats::setNames(.t, c("key", "simLL")),
                stats::setNames(.i, c("key", "mlxLL")), by="key")
  .r <- 100 * abs(expm1(.cmp$mlxLL - .cmp$simLL))
  .r[!is.finite(.r)] <- Inf
  list(cmp=.cmp, nObs=nrow(.cmp), nExpected=sum(.kitPredLikRows(sim, case)), nImport=nrow(.i),
       maxRel=if (nrow(.cmp)) max(.r) else NA_real_)
}
