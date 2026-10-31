## Several endpoints and other special models

## parent (cmt 2) converted to a metabolite (cmt 3); YTYPE 1 is the
## parent, 2 the metabolite (rxode2 DVID in endpoint order)
kitCase(
  name="two-endpoints-parent-metabolite",
  covers="parent and metabolite observed through YTYPE (use=observationtype), one error model each",
  tags=c("endpoints", "error"),
  sim=function() {
    ini({
      ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3; Vm_pop <- 20; Clm_pop <- 2; fm_pop <- 0.6
      omega_V ~ 0.04; omega_Cl ~ 0.09; omega_Clm ~ 0.09
      a1 <- 0.05; b1 <- 0.1; a2 <- 0.02; b2 <- 0.15
    })
    model({
      ka <- ka_pop
      V <- V_pop * exp(omega_V)
      Cl <- Cl_pop * exp(omega_Cl)
      Vm <- Vm_pop
      Clm <- Clm_pop * exp(omega_Clm)
      fm <- fm_pop
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - Cl / V * central
      d/dt(metab) <- fm * Cl / V * central - Clm / Vm * metab
      Cc <- central / V
      Cm <- metab / Vm
      Cc ~ add(a1) + prop(b1) + combined1()
      Cm ~ add(a2) + prop(b2) + combined1()
    })
  },
  data=function(nSub) {
    .id <- seq_len(nSub)
    mlxBind(mlxDose(.id, 0, amt=100, cmt=1, DVID=1L),
            mlxObs(.id, pkTimes(48), cmt=2, DVID=1L),
            mlxObs(.id, c(1, 2, 4, 8, 12, 24, 36, 48), cmt=3, DVID=2L))
  },
  write=function(d) {
    d <- .kitMonolixRows(d)
    d$YTYPE <- ifelse(is.na(d$DV), NA, d$DVID)
    d[, c("ID", "TIME", "AMT", "DV", "YTYPE")]
  },
  model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl, Vm, Clm, fm}

PK:
depot(target=Ac, ka)

EQUATION:
ddt_Ac = -Cl/V*Ac
ddt_Am = fm*Cl/V*Ac - Clm/Vm*Am
Cc = Ac/V
Cm = Am/Vm

OUTPUT:
output = {Cc, Cm}
",
  mlxtran=.mlxProject(list(ka=.mlxPar(1.2), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3),
                           Vm=.mlxPar(20), Clm=.mlxPar(2, 0.3), fm=.mlxPar(0.6)),
                      pred=c(y1="Cc", y2="Cm"),
                      err=c("combined1(a1, b1)", "combined1(a2, b2)"),
                      errPar=c(a1=0.05, b1=0.1, a2=0.02, b2=0.15),
                      content="ID = {use=identifier}
TIME = {use=time}
AMT = {use=amount}
DV = {use=observation, name={y1, y2}, yname={'1', '2'}, type={continuous, continuous}}
YTYPE = {use=observationtype}"))

## Discrete observations: the observation model is in the model file, so
## the project has no [LONGITUDINAL] DEFINITION and no error parameters
.discProject <- function(par, obs, content="ID = {use=identifier}
TIME = {use=time}
DV = {use=observation, name=CONC, type=discrete}") {
  .p <- .mlxProject(par, content=content)
  .p <- sub("[LONGITUDINAL]\ninput = {a, b}\n\nfile = '{{MODEL}}'\n\nDEFINITION:\nCONC = {distribution=normal, prediction=Cc, errorModel=combined1(a, b)}",
            "[LONGITUDINAL]\nfile = '{{MODEL}}'", .p, fixed=TRUE)
  .p <- sub("a = {value=0.05, method=MLE}\nb = {value=0.1, method=MLE}\n", "", .p, fixed=TRUE)
  .p <- gsub("CONC", obs, .p, fixed=TRUE)
  if (grepl("errorModel", .p, fixed=TRUE) || grepl("b = {value", .p, fixed=TRUE)) {
    stop("discrete project still has a continuous observation model", call.=FALSE)
  }
  .p
}

## Monolix has no PRED/IPRED of discrete observations to validate
.discTol <- list(validate=FALSE)

kitCase(
  name="disc-count-poisson",
  covers="count observation: log(P(Y=k)) = -lambda + k*log(lambda) - factln(k), lambda decaying in time",
  tags=c("discrete", "count"),
  ## PRED of a random draw is not comparable
  dryPred=FALSE,
  dryLik=TRUE,
  tol=.discTol,
  sim=function() {
    ini({
      lambda0_pop <- 8; kdecay_pop <- 0.05
      omega_lambda0 ~ 0.09
    })
    model({
      lambda0 <- lambda0_pop * exp(omega_lambda0)
      kdecay <- kdecay_pop
      lambda <- lambda0 * exp(-kdecay * time)
      Y ~ pois(lambda)
    })
  },
  data=function(nSub) mlxObs(seq_len(nSub), c(1, 2, 4, 7, 10, 14, 21, 28), cmt=1),
  columns=c("ID", "TIME", "DV"),
  model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {lambda0, kdecay}

EQUATION:
lambda = lambda0*exp(-kdecay*t)

DEFINITION:
Y = {type=count, log(P(Y=k)) = -lambda + k*log(lambda) - factln(k)}

OUTPUT:
output = Y
",
  mlxtran=.discProject(list(lambda0=.mlxPar(8, 0.3), kdecay=.mlxPar(0.05)), "Y"))

## proportional odds on three ordered categories 0 < 1 < 2
kitCase(
  name="disc-categorical-ordinal",
  covers="ordered categorical observation (categories {0, 1, 2}) with logit(P(Level<=k)) proportional odds",
  tags=c("discrete", "categorical"),
  ## PRED of a random draw is not comparable
  dryPred=FALSE,
  dryLik=TRUE,
  tol=.discTol,
  sim=function() {
    ini({
      th1_pop <- -0.5; th2_pop <- 1.5; slope_pop <- 0.05
      omega_th1 ~ 0.25
    })
    model({
      th1 <- th1_pop + omega_th1
      th2 <- th2_pop
      slope <- slope_pop
      lp0 <- th1 - slope * time
      p0 <- expit(lp0)
      p1 <- expit(lp0 + th2) - p0
      Level ~ c(p0=0, p1=1, 2)
    })
  },
  data=function(nSub) mlxObs(seq_len(nSub), c(1, 2, 4, 7, 10, 14, 21, 28), cmt=1),
  columns=c("ID", "TIME", "DV"),
  model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {th1, th2, slope}

EQUATION:
lp0 = th1 - slope*t

DEFINITION:
Level = {type=categorical, categories={0, 1, 2},
logit(P(Level<=0)) = lp0,
logit(P(Level<=1)) = lp0 + th2}

OUTPUT:
output = Level
",
  mlxtran=.discProject(list(th1=.mlxPar(-0.5, 0.5, dist="normal"), th2=.mlxPar(1.5),
                            slope=.mlxPar(0.05)), "Level"))

## the likelihood is written in rxode2 as the truth; the counts are
## drawn in the model and copied to DV
kitCase(
  name="disc-count-zip",
  covers="zero-inflated Poisson count: if/else on k, log(P(Y=k)) = lpk (translated to ll())",
  tags=c("discrete", "count"),
  dryPred=FALSE,
  dryLik=TRUE,
  tol=.discTol,
  sim=function() {
    ini({
      lambda0_pop <- 6; kdecay_pop <- 0.05; p0_pop <- 0.25
      omega_lambda0 ~ 0.09
    })
    model({
      lambda0 <- lambda0_pop * exp(omega_lambda0)
      kdecay <- kdecay_pop
      p0 <- p0_pop
      lambda <- lambda0 * exp(-kdecay * time)
      ydraw <- (rxunif() > p0) * rxpois(lambda)
      if (DV == 0) {
        lpk <- log(p0 + (1 - p0) * exp(-lambda))
      } else {
        lpk <- log(1 - p0) + llikPois(DV, lambda)
      }
      ll(Y) ~ lpk
    })
  },
  data=function(nSub) mlxObs(seq_len(nSub), c(1, 2, 4, 7, 10, 14, 21, 28), cmt=1),
  postSim=function(d, s) {
    .m <- match(d$ROWID, s$ROWID)
    .obs <- d$EVID == 0 & d$MDV == 0 & !is.na(.m)
    d$DV[.obs] <- s$ydraw[.m[.obs]]
    d
  },
  columns=c("ID", "TIME", "DV"),
  model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {lambda0, kdecay, p0}

EQUATION:
lambda = lambda0*exp(-kdecay*t)

DEFINITION:
Y = {type=count,
if k > 0
  aux = -lambda + k*log(lambda) - factln(k)
  lpk = log(1 - p0) + aux
else
  lpk = log(p0 + (1 - p0)*exp(-lambda))
end
log(P(Y=k)) = lpk}

OUTPUT:
output = Y
",
  mlxtran=.discProject(list(lambda0=.mlxPar(6, 0.3), kdecay=.mlxPar(0.05),
                            p0=.mlxPar(0.25, dist="normal")), "Y"))

## nominal categories given by P(Y=c), the last is the remainder
kitCase(
  name="disc-categorical-p",
  covers="categorical observation (categories {1, 2, 3}) with P(Level=1), P(Level=2) from a multinomial logit",
  tags=c("discrete", "categorical"),
  dryPred=FALSE,
  dryLik=TRUE,
  tol=.discTol,
  sim=function() {
    ini({
      a1_pop <- 0.5; a2_pop <- -0.2; slope_pop <- 0.05
      omega_a1 ~ 0.25
    })
    model({
      a1 <- a1_pop + omega_a1
      a2 <- a2_pop
      slope <- slope_pop
      e1 <- exp(a1 - slope * time)
      e2 <- exp(a2)
      p1 <- e1 / (1 + e1 + e2)
      p2 <- e2 / (1 + e1 + e2)
      Level ~ c(p1=1, p2=2, 3)
    })
  },
  data=function(nSub) mlxObs(seq_len(nSub), c(1, 2, 4, 7, 10, 14, 21, 28), cmt=1),
  columns=c("ID", "TIME", "DV"),
  model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {a1, a2, slope}

EQUATION:
e1 = exp(a1 - slope*t)
e2 = exp(a2)

DEFINITION:
Level = {type=categorical, categories={1, 2, 3},
P(Level=1) = e1/(1 + e1 + e2),
P(Level=2) = e2/(1 + e1 + e2)}

OUTPUT:
output = Level
",
  mlxtran=.discProject(list(a1=.mlxPar(0.5, 0.5, dist="normal"), a2=.mlxPar(-0.2, dist="normal"),
                            slope=.mlxPar(0.05)), "Level"))

## binary response driven by a PK model, given as the probability of
## the last category
kitCase(
  name="disc-binary-pk",
  covers="binary observation (categories {0, 1}) with logit(P(Y=1)) driven by pkmodel() concentrations",
  tags=c("discrete", "categorical", "pk"),
  dryPred=FALSE,
  dryLik=TRUE,
  tol=.discTol,
  sim=function() {
    ini({
      ka_pop <- 1; V_pop <- 10; Cl_pop <- 2
      e0_pop <- -2; slope_pop <- 0.4
      omega_Cl ~ 0.09; omega_e0 ~ 0.25
    })
    model({
      ka <- ka_pop
      V <- V_pop
      Cl <- Cl_pop * exp(omega_Cl)
      e0 <- e0_pop + omega_e0
      slope <- slope_pop
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - Cl / V * central
      Cc <- central / V
      p1 <- expit(e0 + slope * Cc)
      Y ~ c(p1=1, 0)
    })
  },
  data=function(nSub) {
    .id <- seq_len(nSub)
    mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
            mlxObs(.id, pkTimes(48), cmt=2))
  },
  columns=c("ID", "TIME", "AMT", "DV"),
  model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl, e0, slope}

EQUATION:
Cc = pkmodel(ka, V, Cl)
lp = e0 + slope*Cc

DEFINITION:
Y = {type=categorical, categories={0, 1}, logit(P(Y=1)) = lp}

OUTPUT:
output = Y
",
  mlxtran=.discProject(list(ka=.mlxPar(1), V=.mlxPar(10), Cl=.mlxPar(2, 0.3),
                            e0=.mlxPar(-2, 0.5, dist="normal"), slope=.mlxPar(0.4)), "Y",
                       content="ID = {use=identifier}
TIME = {use=time}
AMT = {use=amount}
DV = {use=observation, name=CONC, type=discrete}"))

## the continuous observation is y1, the discrete one Y
.discMixedProject <- function(p) {
  p <- sub("CONC = {", "y1 = {", p, fixed=TRUE)
  p <- sub("data = CONC", "data = {y1, Y}", p, fixed=TRUE)
  p <- sub("model = CONC", "model = {y1, Y}", p, fixed=TRUE)
  if (grepl("CONC", p, fixed=TRUE)) stop("mixed project still names CONC", call.=FALSE)
  p
}

## a continuous and a discrete observation in one data set (YTYPE 1 is
## the concentration, 2 the binary response)
kitCase(
  name="disc-mixed-continuous",
  covers="continuous concentration and binary response in one project (type={continuous, discrete})",
  tags=c("discrete", "categorical", "endpoints"),
  dryLik="Y",
  sim=function() {
    ini({
      ka_pop <- 1; V_pop <- 10; Cl_pop <- 2
      e0_pop <- -2; slope_pop <- 0.4
      omega_Cl ~ 0.09; omega_e0 ~ 0.25
      a1 <- 0.05; b1 <- 0.1
    })
    model({
      ka <- ka_pop
      V <- V_pop
      Cl <- Cl_pop * exp(omega_Cl)
      e0 <- e0_pop + omega_e0
      slope <- slope_pop
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - Cl / V * central
      Cc <- central / V
      p1 <- expit(e0 + slope * Cc)
      Cc ~ add(a1) + prop(b1) + combined1()
      Y ~ c(p1=1, 0)
    })
  },
  data=function(nSub) {
    .id <- seq_len(nSub)
    mlxBind(mlxDose(.id, 0, amt=100, cmt=1, DVID=1L),
            mlxObs(.id, pkTimes(48), cmt=2, DVID=1L),
            mlxObs(.id, c(1, 2, 4, 8, 12, 24, 36, 48), cmt=2, DVID=2L))
  },
  write=function(d) {
    d <- .kitMonolixRows(d)
    d$YTYPE <- ifelse(is.na(d$DV), NA, d$DVID)
    d[, c("ID", "TIME", "AMT", "DV", "YTYPE")]
  },
  model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl, e0, slope}

EQUATION:
Cc = pkmodel(ka, V, Cl)
lp = e0 + slope*Cc

DEFINITION:
Y = {type=categorical, categories={0, 1}, logit(P(Y=1)) = lp}

OUTPUT:
output = {Cc, Y}
",
  mlxtran=.discMixedProject(.mlxProject(list(ka=.mlxPar(1), V=.mlxPar(10), Cl=.mlxPar(2, 0.3),
                                              e0=.mlxPar(-2, 0.5, dist="normal"), slope=.mlxPar(0.4)),
                                         err="combined1(a1, b1)", errPar=c(a1=0.05, b1=0.1),
                                         content="ID = {use=identifier}
TIME = {use=time}
AMT = {use=amount}
DV = {use=observation, name={y1, Y}, yname={'1', '2'}, type={continuous, discrete}}
YTYPE = {use=observationtype}")))
