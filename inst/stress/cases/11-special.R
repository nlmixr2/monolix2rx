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
.discProject <- function(par, obs) {
  .p <- .mlxProject(par, content="ID = {use=identifier}
TIME = {use=time}
DV = {use=observation, name=CONC, type=discrete}")
  .p <- sub("[LONGITUDINAL]\ninput = {a, b}\n\nfile = '{{MODEL}}'\n\nDEFINITION:\nCONC = {distribution=normal, prediction=Cc, errorModel=combined1(a, b)}",
            "[LONGITUDINAL]\nfile = '{{MODEL}}'", .p, fixed=TRUE)
  .p <- sub("a = {value=0.05, method=MLE}\nb = {value=0.1, method=MLE}\n", "", .p, fixed=TRUE)
  .p <- gsub("CONC", obs, .p, fixed=TRUE)
  if (grepl("errorModel", .p, fixed=TRUE) || grepl("b = {value", .p, fixed=TRUE)) {
    stop("discrete project still has a continuous observation model", call.=FALSE)
  }
  .p
}

.discKnown <- "discrete observations (count, categorical) are not translated (.handleSingleEndpoint)"

kitCase(
  name="disc-count-poisson",
  covers="count observation: log(P(Y=k)) = -lambda + k*log(lambda) - factln(k), lambda decaying in time",
  tags=c("discrete", "count"),
  known=.discKnown,
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

## proportional odds on three ordered categories 0 < 1 < 2; rxode2's
## ordinal draws 1..3, the data set holds 0..2
kitCase(
  name="disc-categorical-ordinal",
  covers="ordered categorical observation (categories {0, 1, 2}) with logit(P(Level<=k)) proportional odds",
  tags=c("discrete", "categorical"),
  known=.discKnown,
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
      Level ~ c(p0, p1)
    })
  },
  data=function(nSub) mlxObs(seq_len(nSub), c(1, 2, 4, 7, 10, 14, 21, 28), cmt=1),
  postSim=function(d, s) {
    .m <- match(d$ROWID, s$ROWID)
    .obs <- d$EVID == 0 & d$MDV == 0 & !is.na(.m)
    d$DV[.obs] <- s$sim[.m[.obs]] - 1
    d
  },
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
