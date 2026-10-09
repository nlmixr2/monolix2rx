## Time-to-event observations (type=event)
##
## The truth integrates the hazard twice: Hall from the first record,
## to draw the event times by inversion, and H, reset by an EVID=5
## replace just after each event record, so each record's likelihood
## uses the hazard since the previous one (the first record only starts
## the observation; TTEFIRST marks it).  The grid rows only carry Hall.

.tteGrid <- function(end, by=0.05) seq(0, end, by=by)

## Rebuild the event records from the simulated Hall: `exact` gives the
## start, each event and the end (unless maxEvents was reached);
## `visits` gives interval censored records up to the first event.
.tteRows <- function(d, s, cmtH, start=0, end, maxEvents=Inf, visits=NULL,
                     dvid=NULL, eps=1e-8) {
  .grid <- d$EVID == 0 & d$MDV == 0 & d$TTEGRID == 1L
  .keep <- d[!.grid, , drop=FALSE]
  ## other observations keep their simulated values
  .m <- match(.keep$ROWID, s$ROWID)
  .obs <- .keep$EVID == 0 & .keep$MDV == 0 & !is.na(.m)
  .keep$DV[.obs] <- signif(s$sim[.m[.obs]], 6)
  .g <- s[s$ROWID %in% d$ROWID[.grid], c("id", "time", "Hall")]
  .rows <- lapply(unique(d$ID), function(i) {
    .gi <- .g[as.character(.g$id) == as.character(i), ]
    .hAt <- function(t) stats::approx(.gi$time, .gi$Hall, t, rule=2)$y
    .tAt <- function(h) stats::approx(.gi$Hall, .gi$time, h, ties="ordered")$y
    .ev <- numeric(0)
    .h <- .hAt(start)
    while (length(.ev) < maxEvents) {
      .h <- .h - log(stats::runif(1))
      if (.h >= .hAt(end)) break
      .ev <- c(.ev, .tAt(.h))
    }
    if (is.null(visits)) {
      .t <- c(start, .ev, if (length(.ev) < maxEvents) end)
      .dv <- c(0, rep(1, length(.ev)), if (length(.ev) < maxEvents) 0)
    } else {
      .t <- c(start, visits)
      .dv <- rep(0, length(.t))
      if (length(.ev)) {
        .k <- which(visits >= .ev[1])[1]
        .t <- .t[seq_len(.k + 1L)]
        .dv <- c(rep(0, .k), 1)
      }
    }
    .o <- mlxObs(i, .t, cmt=1L, TTEFIRST=as.integer(seq_along(.t) == 1L))
    .o$DV <- .dv
    .r <- mlxDose(i, .t + eps, amt=0, cmt=cmtH, evid=5L)
    if (!is.null(dvid)) {
      .o$DVID <- dvid
      .r$DVID <- dvid
    }
    mlxBind(.o, .r)
  })
  .keep$TTEFIRST <- rep(0L, nrow(.keep))
  ## kept rows keep their ROWID (the truth's IPRED is matched by it)
  .new <- do.call(mlxBind, .rows)
  .new$ROWID <- max(d$ROWID) + seq_len(nrow(.new))
  .ret <- mlxBind(.keep, .new)
  .ret$TTEGRID <- NULL
  .ret
}

## Monolix rows: no replace events or hidden columns
.tteWrite <- function(cols) {
  function(d) .kitMonolixRows(d[d$EVID != 5L, ])[, cols]
}

.tteContent <- function(amt=FALSE) {
  paste0("ID = {use=identifier}
TIME = {use=time}
", if (amt) "AMT = {use=amount}\n", "DV = {use=observation, name=CONC, type=event}")
}

.tteTol <- list(validate=FALSE)

kitCase(
  name="tte-weibull-exact",
  covers="single exact event (maxEventNumber=1) with a Weibull hazard, right censored at the end of the study",
  tags=c("tte", "discrete"),
  dryPred=FALSE,
  dryLik=TRUE,
  tol=.tteTol,
  sim=function() {
    ini({
      Te_pop <- 30; beta_pop <- 1.5
      omega_Te ~ 0.09
    })
    model({
      Te <- Te_pop * exp(omega_Te)
      beta <- beta_pop
      h <- (beta / Te) * (time / Te)^(beta - 1)
      d/dt(Hall) <- h
      d/dt(H) <- h
      if (DV == 1) {
        lle <- log(h) - H
      } else {
        lle <- -H
      }
      lle <- (1 - TTEFIRST) * lle
      ll(Event) ~ lle
    })
  },
  data=function(nSub) mlxObs(seq_len(nSub), .tteGrid(48), cmt=1L, TTEGRID=1L, TTEFIRST=0L),
  postSim=function(d, s) .tteRows(d, s, cmtH=2L, end=48, maxEvents=1),
  write=.tteWrite(c("ID", "TIME", "DV")),
  model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {Te, beta}

EQUATION:
h = (beta/Te)*(t/Te)^(beta-1)

DEFINITION:
Event = {type=event, eventType=exact, maxEventNumber=1, hazard=h}

OUTPUT:
output = Event
",
  mlxtran=.discProject(list(Te=.mlxPar(30, 0.3), beta=.mlxPar(1.5)), "Event",
                       content=.tteContent()))

kitCase(
  name="tte-interval-censored",
  covers="interval censored event (eventType=intervalCensored) seen at visits every 7 days, hazard=1/Te written inline",
  tags=c("tte", "discrete"),
  dryPred=FALSE,
  dryLik=TRUE,
  tol=.tteTol,
  sim=function() {
    ini({
      Te_pop <- 40
      omega_Te ~ 0.16
    })
    model({
      Te <- Te_pop * exp(omega_Te)
      h <- 1 / Te
      d/dt(Hall) <- h
      d/dt(H) <- h
      if (DV == 1) {
        lle <- log(1 - exp(-H))
      } else {
        lle <- -H
      }
      lle <- (1 - TTEFIRST) * lle
      ll(Event) ~ lle
    })
  },
  data=function(nSub) mlxObs(seq_len(nSub), .tteGrid(84), cmt=1L, TTEGRID=1L, TTEFIRST=0L),
  postSim=function(d, s) .tteRows(d, s, cmtH=2L, end=84, maxEvents=1, visits=seq(7, 84, by=7)),
  write=.tteWrite(c("ID", "TIME", "DV")),
  model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {Te}

DEFINITION:
Event = {type=event, eventType=intervalCensored, maxEventNumber=1, intervalLength=7, hazard=1/Te}

OUTPUT:
output = Event
",
  mlxtran=.discProject(list(Te=.mlxPar(40, 0.4)), "Event", content=.tteContent()))

kitCase(
  name="tte-repeated",
  covers="repeated exact events (no maxEventNumber) with a hazard decreasing in time",
  tags=c("tte", "discrete"),
  dryPred=FALSE,
  dryLik=TRUE,
  tol=.tteTol,
  sim=function() {
    ini({
      h0_pop <- 0.1; kh_pop <- 0.01
      omega_h0 ~ 0.25
    })
    model({
      h0 <- h0_pop * exp(omega_h0)
      kh <- kh_pop
      h <- h0 * exp(-kh * time)
      d/dt(Hall) <- h
      d/dt(H) <- h
      if (DV == 1) {
        lle <- log(h) - H
      } else {
        lle <- -H
      }
      lle <- (1 - TTEFIRST) * lle
      ll(Event) ~ lle
    })
  },
  data=function(nSub) mlxObs(seq_len(nSub), .tteGrid(100), cmt=1L, TTEGRID=1L, TTEFIRST=0L),
  postSim=function(d, s) .tteRows(d, s, cmtH=2L, end=100),
  write=.tteWrite(c("ID", "TIME", "DV")),
  model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {h0, kh}

EQUATION:
haz = h0*exp(-kh*t)

DEFINITION:
Event = {type=event, hazard=haz}

OUTPUT:
output = Event
",
  mlxtran=.discProject(list(h0=.mlxPar(0.1, 0.5), kh=.mlxPar(0.01)), "Event",
                       content=.tteContent()))

## the hazard follows the concentration of a one-compartment oral model
.tteSimPk <- function() {
  ini({
    ka_pop <- 1; V_pop <- 10; Cl_pop <- 2
    h0_pop <- 0.02; bc_pop <- 0.3
    omega_Cl ~ 0.09; omega_h0 ~ 0.25
  })
  model({
    ka <- ka_pop
    V <- V_pop
    Cl <- Cl_pop * exp(omega_Cl)
    h0 <- h0_pop * exp(omega_h0)
    bc <- bc_pop
    d/dt(depot) <- -ka * depot
    d/dt(central) <- ka * depot - Cl / V * central
    Cc <- central / V
    h <- h0 * exp(bc * Cc)
    d/dt(Hall) <- h
    d/dt(H) <- h
    if (DV == 1) {
      lle <- log(h) - H
    } else {
      lle <- -H
    }
    lle <- (1 - TTEFIRST) * lle
    ll(Event) ~ lle
  })
}

.tteModelPk <- "DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl, h0, bc}

EQUATION:
Cc = pkmodel(ka, V, Cl)
h = h0*exp(bc*Cc)

DEFINITION:
Event = {type=event, hazard=h}

OUTPUT:
output = Event
"

.tteParPk <- list(ka=.mlxPar(1), V=.mlxPar(10), Cl=.mlxPar(2, 0.3),
                  h0=.mlxPar(0.02, 0.5), bc=.mlxPar(0.3))

kitCase(
  name="tte-pk-hazard",
  covers="repeated events whose hazard follows pkmodel() concentrations over three daily doses",
  tags=c("tte", "discrete", "pk"),
  dryPred=FALSE,
  dryLik=TRUE,
  tol=.tteTol,
  sim=.tteSimPk,
  data=function(nSub) {
    .id <- seq_len(nSub)
    mlxBind(mlxDose(.id, c(0, 24, 48), amt=100, cmt=1L, TTEGRID=0L, TTEFIRST=0L),
            mlxObs(.id, .tteGrid(72), cmt=1L, TTEGRID=1L, TTEFIRST=0L))
  },
  postSim=function(d, s) .tteRows(d, s, cmtH=4L, end=72),
  write=.tteWrite(c("ID", "TIME", "AMT", "DV")),
  model=.tteModelPk,
  mlxtran=.discProject(.tteParPk, "Event", content=.tteContent(TRUE)))

kitCase(
  name="tte-late-start",
  covers="event observation starting at t=24, after the first dose: the hazard is integrated from the start record",
  tags=c("tte", "discrete", "pk"),
  dryPred=FALSE,
  dryLik=TRUE,
  tol=.tteTol,
  sim=.tteSimPk,
  data=function(nSub) {
    .id <- seq_len(nSub)
    mlxBind(mlxDose(.id, c(0, 24, 48), amt=100, cmt=1L, TTEGRID=0L, TTEFIRST=0L),
            mlxObs(.id, .tteGrid(72), cmt=1L, TTEGRID=1L, TTEFIRST=0L))
  },
  postSim=function(d, s) .tteRows(d, s, cmtH=4L, start=24, end=72),
  write=.tteWrite(c("ID", "TIME", "AMT", "DV")),
  model=.tteModelPk,
  mlxtran=.discProject(.tteParPk, "Event", content=.tteContent(TRUE)))

## concentrations (YTYPE 1) and events (YTYPE 2) in one data set
kitCase(
  name="tte-pk-joint",
  covers="continuous concentrations and repeated events in one project (type={continuous, event})",
  tags=c("tte", "discrete", "pk", "endpoints"),
  dryLik="Event",
  sim=function() {
    ini({
      ka_pop <- 1; V_pop <- 10; Cl_pop <- 2
      h0_pop <- 0.02; bc_pop <- 0.3
      omega_Cl ~ 0.09; omega_h0 ~ 0.25
      a1 <- 0.05; b1 <- 0.1
    })
    model({
      ka <- ka_pop
      V <- V_pop
      Cl <- Cl_pop * exp(omega_Cl)
      h0 <- h0_pop * exp(omega_h0)
      bc <- bc_pop
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - Cl / V * central
      Cc <- central / V
      h <- h0 * exp(bc * Cc)
      d/dt(Hall) <- h
      d/dt(H) <- h
      if (DV == 1) {
        lle <- log(h) - H
      } else {
        lle <- -H
      }
      lle <- (1 - TTEFIRST) * lle
      Cc ~ add(a1) + prop(b1) + combined1()
      ll(Event) ~ lle
    })
  },
  data=function(nSub) {
    .id <- seq_len(nSub)
    mlxBind(mlxDose(.id, c(0, 24, 48), amt=100, cmt=1L, DVID=1L, TTEGRID=0L, TTEFIRST=0L),
            mlxObs(.id, c(1, 4, 12, 25, 36, 49, 60, 72), cmt=2L, DVID=1L, TTEGRID=0L, TTEFIRST=0L),
            mlxObs(.id, .tteGrid(72), cmt=1L, DVID=2L, TTEGRID=1L, TTEFIRST=0L))
  },
  postSim=function(d, s) .tteRows(d, s, cmtH=4L, end=72, dvid=2L),
  write=function(d) {
    d <- .kitMonolixRows(d[d$EVID != 5L, ])
    d$YTYPE <- ifelse(is.na(d$DV), NA, d$DVID)
    d[, c("ID", "TIME", "AMT", "DV", "YTYPE")]
  },
  model=sub("output = Event", "output = {Cc, Event}", .tteModelPk, fixed=TRUE),
  mlxtran=local({
    .p <- .mlxProject(.tteParPk, err="combined1(a1, b1)", errPar=c(a1=0.05, b1=0.1),
                      content="ID = {use=identifier}
TIME = {use=time}
AMT = {use=amount}
DV = {use=observation, name={y1, Event}, yname={'1', '2'}, type={continuous, event}}
YTYPE = {use=observationtype}")
    .p <- sub("CONC = {", "y1 = {", .p, fixed=TRUE)
    .p <- sub("data = CONC", "data = {y1, Event}", .p, fixed=TRUE)
    .p <- sub("model = CONC", "model = {y1, Event}", .p, fixed=TRUE)
    if (grepl("CONC", .p, fixed=TRUE)) stop("joint project still names CONC", call.=FALSE)
    .p
  }))
