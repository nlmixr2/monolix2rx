## Dosing records: ADDL/II, infusions, steady state

.ivTruth <- function() {
  ini({
    V_pop <- 30; Cl_pop <- 3
    omega_V ~ 0.04; omega_Cl ~ 0.09
    a <- 0.05; b <- 0.1
  })
  model({
    V <- V_pop * exp(omega_V)
    Cl <- Cl_pop * exp(omega_Cl)
    d/dt(central) <- -Cl / V * central
    Cc <- central / V
    Cc ~ add(a) + prop(b) + combined1()
  })
}

.ivModel <- "DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {V, Cl}

EQUATION:
Cc = pkmodel(V, Cl)

OUTPUT:
output = Cc
"

.ivPar <- list(V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3))

kitVariant("pkmodel-oral-1cmt", "dose-addl",
           "one dose record with ADDL/II (additionaldose, interdoseinterval)",
           tags=c("dosing"),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1, ii=12, addl=5),
                     mlxObs(.id, c(pkTimes(12), 60.5, 62, 66, 72, 84, 96), cmt=2))
           },
           columns=c("ID", "TIME", "AMT", "II", "ADDL", "DV"),
           mlxtran=.mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2),
                                    Cl=.mlxPar(3, 0.3)),
                               content=paste0(.mlxContent, "
II = {use=interdoseinterval}
ADDL = {use=additionaldose}")))

kitCase(
  name="dose-infusion-rate",
  covers="IV infusion given by a RATE column (use=rate)",
  tags=c("dosing", "infusion"),
  sim=.ivTruth,
  data=function(nSub) {
    .id <- seq_len(nSub)
    mlxBind(mlxDose(.id, 0, amt=100, rate=40, cmt=1),
            mlxObs(.id, c(0.5, 1, 2, 2.5, 3, 4, 6, 8, 12, 24, 36), cmt=1))
  },
  columns=c("ID", "TIME", "AMT", "RATE", "DV"),
  model=.ivModel,
  mlxtran=.mlxProject(.ivPar, content=paste0(.mlxContent, "
RATE = {use=rate}")))

## a long half-life relative to II, so the number of steady-state doses
## (nbdoses=10, not the default 7) changes the predictions
kitCase(
  name="dose-ss",
  covers="steady-state dose (use=steadystate, nbdoses=10) with II, then a second regimen",
  tags=c("dosing", "ss"),
  nbSSDoses=10L,
  sim=function() {
    ini({
      V_pop <- 30; Cl_pop <- 0.3
      omega_V ~ 0.04; omega_Cl ~ 0.09
      a <- 0.05; b <- 0.1
    })
    model({
      V <- V_pop * exp(omega_V)
      Cl <- Cl_pop * exp(omega_Cl)
      d/dt(central) <- -Cl / V * central
      Cc <- central / V
      Cc ~ add(a) + prop(b) + combined1()
    })
  },
  data=function(nSub) {
    .id <- seq_len(nSub)
    mlxBind(mlxDose(.id, 0, amt=100, cmt=1, ss=1L, ii=12),
            mlxDose(.id, 24, amt=200, cmt=1),
            mlxObs(.id, c(0.5, 2, 6, 11.5, 12.5, 18, 23.5, 25, 30, 36, 48), cmt=1))
  },
  columns=c("ID", "TIME", "AMT", "SS", "II", "DV"),
  model=.ivModel,
  mlxtran=.mlxProject(list(V=.mlxPar(30, 0.2), Cl=.mlxPar(0.3, 0.3)),
                      content=paste0(.mlxContent, "
SS = {use=steadystate, nbdoses=10}
II = {use=interdoseinterval}")))

kitCase(
  name="dose-infusion-time",
  covers="IV infusion given by its duration (use=infusiontime)",
  tags=c("dosing", "infusion"),
  sim=.ivTruth,
  data=function(nSub) {
    .id <- seq_len(nSub)
    mlxBind(mlxDose(.id, c(0, 24), amt=100, rate=50, cmt=1, TINF=2),
            mlxObs(.id, c(0.5, 1, 2, 2.5, 3, 4, 6, 8, 12, 24.5, 26, 30, 36, 48), cmt=1))
  },
  columns=c("ID", "TIME", "AMT", "TINF", "DV"),
  model=.ivModel,
  mlxtran=.mlxProject(.ivPar, content=paste0(.mlxContent, "
TINF = {use=infusiontime}")))

## EVID=4: the system is reset before the dose (washout)
kitVariant("pkmodel-oral-1cmt", "dose-evid4-reset",
           "EVID=4 dose resets the system (use=eventidentifier)",
           tags=c("dosing", "evid"),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxDose(.id, 6, amt=100, cmt=1, evid=4L),
                     mlxObs(.id, c(1, 2, 4, 5.5, 7, 8, 10, 12, 18, 24), cmt=2))
           },
           columns=c("ID", "TIME", "AMT", "EVID", "DV"),
           mlxtran=.mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2),
                                    Cl=.mlxPar(3, 0.3)),
                               content=paste0(.mlxContent, "
EVID = {use=eventidentifier}")))

## pre-dose samples and an observation tied with the second dose (the
## data lists the dose first); Monolix's tie order is to confirm
kitVariant("pkmodel-oral-1cmt", "dose-late-ties",
           "first dose at t=6 after baseline samples; an observation at the second dose time",
           tags=c("dosing", "ties"),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, c(6, 30), amt=100, cmt=1),
                     mlxObs(.id, c(0, 3, 6.5, 7, 8, 10, 14, 18, 30, 31, 34, 42, 54), cmt=2))
           })
