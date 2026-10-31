## Simulate a case's observations with rxode2.
##
## The solving options follow .validateModel() (R/validate.R), so the
## truth uses Monolix's event semantics: LOCF covariates and a fixed number
## of steady-state doses (nbSSDoses).

.kitSolveOpts <- function(nbSSDoses=7L) {
  ## an rxode2 with nlmixr2/rxode2#1492 fixed counts minSS = maxSS doses
  ## (later steady-state doses); the start is written out (.kitSsExpand())
  .fixed <- utils::getFromNamespace(".rxSsFixedCount", "monolix2rx")()
  list(covsInterpolation="locf", minSS=nbSSDoses,
       maxSS=if (.fixed) nbSSDoses else nbSSDoses + 1L,
       ssAtol=100, ssRtol=100, atol=1e-10, rtol=1e-10)
}

## stable per-case seed so cases are reproducible independently
.kitSeed <- function(name, seed) {
  seed + sum(utf8ToInt(name) * seq_along(utf8ToInt(name))) %% 100000L
}

## Monolix's steady state (nbSSDoses doses before the steady-state dose)
## as explicit doses, as monolix2rx solves it
.kitSsExpand <- function(d, nbSSDoses) {
  .nm <- names(d)
  .lc <- tolower(.nm)
  if (!all(c("id", "time", "amt", "ss", "ii") %in% .lc)) return(d)
  names(d) <- ifelse(.lc %in% c("id", "time", "amt", "ss", "ii", "evid", "addl"), .lc, .nm)
  d <- utils::getFromNamespace(".ssExpand", "monolix2rx")(d, nbSSDoses)
  names(d) <- .nm[match(tolower(names(d)), .lc)]
  d
}

.kitSolve <- function(ui, d, case, addDosing=FALSE, returnType="data.frame") {
  d <- .kitSsExpand(d, case$nbSSDoses)
  .args <- c(list(ui, d, keep="ROWID", returnType=returnType,
                  addDosing=addDosing),
             .kitSolveOpts(case$nbSSDoses), case$solve)
  suppressMessages(do.call(rxode2::rxSolve, .args))
}

## The simulated dataset (with ROWID), the true etas of each subject, and
## for the observation rows the population prediction (all random effects
## and residual errors zero) and the individual prediction (true etas).
kitSimulate <- function(case, nSub, seed=42L) {
  .seed <- .kitSeed(case$name, seed)
  set.seed(.seed)
  rxode2::rxSetSeed(.seed)
  ## after the seed: data functions may draw covariates
  .d <- case$data(if (is.null(case$nSub)) nSub else case$nSub)
  .d$ROWID <- seq_len(nrow(.d))
  .ui <- suppressMessages(rxode2::assertRxUi(case$sim))
  .s <- .kitSolve(.ui, .d, case, returnType="rxSolve")
  ## ui$omega drops the etas when there is inter-occasion variability
  .ini <- .ui$iniDf
  .par <- as.data.frame(.s$params)
  .eta <- intersect(.ini$name[!is.na(.ini$neta1) & .ini$neta1 == .ini$neta2], names(.par))
  ## inter-occasion etas are per-subject columns like gamma_Cl(OCC==2)
  .eta <- c(.eta, grep("[(].*==.*[)]$", names(.par), value=TRUE))
  .etas <- NULL
  if (length(.eta) > 0L) {
    .etas <- .par[, c("id", .eta), drop=FALSE]
    .etas$id <- as.character(.etas$id)
  }
  .s <- as.data.frame(.s)
  if (is.function(case$postSim)) {
    .d <- case$postSim(.d, .s)
  } else {
    .m <- match(.d$ROWID, .s$ROWID)
    .obs <- .d$EVID == 0 & .d$MDV == 0 & !is.na(.m)
    .d$DV[.obs] <- signif(.s$sim[.m[.obs]], 6)
  }
  .p <- suppressWarnings(.kitSolve(rxode2::zeroRe(.ui), .d, case))
  .obs <- .d[.d$EVID == 0 & .d$MDV == 0, c("ROWID", "ID", "TIME")]
  .obs$simPred <- .p$sim[match(.obs$ROWID, .p$ROWID)]
  .obs$simIpred <- .s$ipredSim[match(.obs$ROWID, .s$ROWID)]
  .mix <- NULL
  if (!is.null(case$mixest)) {
    .first <- !duplicated(.d$ID)
    .mix <- data.frame(id=as.character(.d$ID[.first]), mixest=as.integer(.d[[case$mixest]][.first]))
  }
  ## the truth's endpoints in DVID order
  .end <- if (!is.null(.ui$predDf)) as.character(.ui$predDf$var)
  list(data=.d, pred=.obs, etas=.etas, mix=.mix, seed=.seed, endpoints=.end)
}

## Monolix missing values: AMT only on doses, DV only on observations
.kitMonolixRows <- function(d) {
  .dose <- d$EVID %in% c(1L, 4L)
  .obs <- d$EVID == 0 & d$MDV == 0
  if (!is.null(d$AMT)) d$AMT[!.dose] <- NA
  if (!is.null(d$DV)) d$DV[!.obs] <- NA
  d
}

## Write the Monolix dataset; returns the header columns
kitWriteData <- function(case, d, file) {
  .w <- if (is.function(case$write)) case$write(d) else
    .kitMonolixRows(d)[, case$columns, drop=FALSE]
  if (is.character(.w)) {
    writeLines(.w, file)
    ## the header names without quotes or a UTF-8 byte order mark
    return(strsplit(gsub("\"|\ufeff", "", .w[1]), "[,;\t \r]+")[[1]])
  }
  ## full precision, so the imported times equal the truth's
  .w[] <- lapply(.w, function(x) {
    .r <- if (is.numeric(x)) trimws(formatC(x, digits=15, format="fg"))
          else as.character(x)
    .r[is.na(x)] <- "."
    .r
  })
  utils::write.csv(.w, file, row.names=FALSE, quote=FALSE)
  names(.w)
}
