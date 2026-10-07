## Monolix-style dataset builders.
##
## Every builder returns the columns ID TIME AMT RATE EVID CMT ADM SS II
## ADDL DV MDV: rxode2 simulates from the NONMEM-like event columns (CMT is
## the rxode2 compartment), and Monolix routes doses by ADM (the
## administration id the model's macros refer to).  kitWriteData() writes
## only the case's `columns`.

## Rows are subject-major.  Value arguments may have length 1, one value
## per row, one per ID or one per time; per-ID wins when they are equal.
.grid <- function(id, time) expand.grid(TIME=time, ID=id)[, c("ID", "TIME")]

.perRow <- function(x, g, id, time) {
  .n <- nrow(g)
  if (length(x) == 1L || length(x) == .n) return(rep_len(x, .n))
  if (length(x) == length(id)) return(x[match(g$ID, id)])
  if (length(x) == length(time)) return(x[match(g$TIME, time)])
  stop("argument length ", length(x), " matches neither rows, IDs nor times",
       call.=FALSE)
}

.extra <- function(d, extra) {
  for (.n in names(extra)) d[[.n]] <- rep_len(extra[[.n]], nrow(d))
  d
}

mlxDose <- function(id, time=0, amt=100, cmt=1L, adm=1L, rate=0, ss=0L,
                    ii=0, addl=0L, evid=1L, ...) {
  .d <- .grid(id, time)
  .p <- function(x) .perRow(x, .d, id, time)
  .ret <- data.frame(ID=.d$ID, TIME=.d$TIME, AMT=.p(amt), RATE=.p(rate),
                     EVID=.p(evid), CMT=.p(cmt), ADM=.p(adm), SS=.p(ss),
                     II=.p(ii), ADDL=.p(addl), DV=NA_real_, MDV=1L)
  .extra(.ret, lapply(list(...), .p))
}

mlxObs <- function(id, time, cmt=2L, mdv=0L, evid=0L, ...) {
  .d <- .grid(id, time)
  .p <- function(x) .perRow(x, .d, id, time)
  .ret <- data.frame(ID=.d$ID, TIME=.d$TIME, AMT=0, RATE=0,
                     EVID=.p(evid), CMT=.p(cmt), ADM=0L, SS=0L, II=0,
                     ADDL=0L, DV=0, MDV=.p(mdv))
  .extra(.ret, lapply(list(...), .p))
}

## Non-dose, non-observation events (EVID=2 other, EVID=3 reset)
mlxOther <- function(id, time, cmt=2L, evid=2L, ...) {
  mlxObs(id, time, cmt=cmt, mdv=1L, evid=evid, ...)
}

## Stack rows, sort by ID then TIME keeping input order for ties, and
## optionally attach per-subject covariates.
mlxBind <- function(..., cov=NULL, sort=TRUE) {
  .l <- list(...)
  .all <- unique(unlist(lapply(.l, names)))
  .l <- lapply(.l, function(d) {
    for (.n in setdiff(.all, names(d))) d[[.n]] <- 0
    d[, .all]
  })
  .d <- do.call(rbind, .l)
  if (sort) .d <- .d[order(.d$ID, .d$TIME, seq_len(nrow(.d))), ]
  if (!is.null(cov)) {
    .m <- match(.d$ID, cov$ID)
    for (.n in setdiff(names(cov), "ID")) .d[[.n]] <- cov[[.n]][.m]
  }
  rownames(.d) <- NULL
  .d
}

## Per-subject covariate table: named functions(n) or vectors
mlxCov <- function(nSub, ...) {
  .l <- list(...)
  .ret <- data.frame(ID=seq_len(nSub))
  for (.n in names(.l)) {
    .v <- .l[[.n]]
    .ret[[.n]] <- if (is.function(.v)) .v(nSub) else rep_len(.v, nSub)
  }
  .ret
}

pkTimes <- function(tmax=24) {
  .t <- c(0.25, 0.5, 1, 1.5, 2, 3, 4, 6, 8, 12, 16, 24, 36, 48, 72)
  .t[.t <= tmax]
}
