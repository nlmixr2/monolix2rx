## More dosing records (03-dosing.R has the common ones) and censoring

## half-life about 7 h: the steady state differs from one infusion, and
## rxode2's minSS matches Monolix's nbdoses repeated doses
kitCase(
  name="dose-ss-infusion",
  covers="steady-state infusion (SS=1 with RATE and II), then a single bolus",
  tags=c("dosing", "ss", "infusion"),
  sim=function() {
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
  },
  data=function(nSub) {
    .id <- seq_len(nSub)
    mlxBind(mlxDose(.id, 0, amt=100, rate=50, cmt=1, ss=1L, ii=12),
            mlxDose(.id, 24, amt=100, cmt=1),
            mlxObs(.id, c(0.5, 1.5, 2, 3, 6, 11.5, 12.5, 14, 18, 23.5, 25, 30, 36, 48), cmt=1))
  },
  columns=c("ID", "TIME", "AMT", "RATE", "SS", "II", "DV"),
  model=.ivModel,
  mlxtran=.mlxProject(list(V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3)),
                      content=paste0(.mlxContent, "
RATE = {use=rate}
SS = {use=steadystate, nbdoses=7}
II = {use=interdoseinterval}")))

kitVariant("pkmodel-oral-1cmt", "dose-ss-addl",
           "steady-state dose record that also carries ADDL (SS=1, II=12, ADDL=3)",
           tags=c("dosing", "ss"),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1, ss=1L, ii=12, addl=3),
                     mlxObs(.id, c(1, 4, 11.5, 13, 24.5, 36.5, 40, 48, 60, 72), cmt=2))
           },
           columns=c("ID", "TIME", "AMT", "SS", "II", "ADDL", "DV"),
           mlxtran=.mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2),
                                    Cl=.mlxPar(3, 0.3)),
                               content=paste0(.mlxContent, "
SS = {use=steadystate, nbdoses=7}
II = {use=interdoseinterval}
ADDL = {use=additionaldose}")))

## EVID=3 resets without a dose; drug is still on board at the reset
kitVariant("pkmodel-oral-1cmt", "dose-evid3-reset",
           "EVID=3 line (reset, no dose) with drug on board, then a new dose",
           tags=c("dosing", "evid"),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxOther(.id, 8, cmt=1, evid=3L),
                     mlxDose(.id, 10, amt=100, cmt=1),
                     mlxObs(.id, c(1, 2, 4, 7.5, 9, 11, 12, 14, 18, 24), cmt=2))
           },
           columns=c("ID", "TIME", "AMT", "EVID", "DV"),
           mlxtran=.mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2),
                                    Cl=.mlxPar(3, 0.3)),
                               content=paste0(.mlxContent, "
EVID = {use=eventidentifier}")))

kitCase(
  name="dose-infusion-tlag",
  covers="RATE infusions through iv(cmt=1, Tlag): the lag delays the infusion start",
  tags=c("dosing", "infusion", "macro"),
  sim=function() {
    ini({
      Tlag_pop <- 0.75; V_pop <- 30; Cl_pop <- 3
      omega_Tlag ~ 0.09; omega_V ~ 0.04; omega_Cl ~ 0.09
      a <- 0.05; b <- 0.1
    })
    model({
      Tlag <- Tlag_pop * exp(omega_Tlag)
      V <- V_pop * exp(omega_V)
      Cl <- Cl_pop * exp(omega_Cl)
      d/dt(central) <- -Cl / V * central
      alag(central) <- Tlag
      Cc <- central / V
      Cc ~ add(a) + prop(b) + combined1()
    })
  },
  data=function(nSub) {
    .id <- seq_len(nSub)
    mlxBind(mlxDose(.id, c(0, 12), amt=100, rate=40, cmt=1),
            mlxObs(.id, c(0.5, 1, 2, 3, 3.5, 4, 6, 12.5, 13, 14, 15, 16, 20, 24), cmt=1))
  },
  columns=c("ID", "TIME", "AMT", "RATE", "DV"),
  model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {Tlag, V, Cl}

PK:
compartment(cmt=1, amount=Ac)
iv(cmt=1, Tlag)
elimination(cmt=1, k=Cl/V)
Cc = Ac/V

OUTPUT:
output = Cc
",
  mlxtran=.mlxProject(list(Tlag=.mlxPar(0.75, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3)),
                      content=paste0(.mlxContent, "
RATE = {use=rate}")))

## Monolix and rxode2 may place the lagged steady-state doses differently;
## a short half-life keeps the minSS difference out of it
kitVariant("pkmodel-oral-1cmt", "dose-ss-tlag",
           "steady-state oral dose with an absorption lag (pkmodel(Tlag, ka, V, Cl))",
           tags=c("dosing", "ss"),
           sim=function() {
             ini({
               Tlag_pop <- 1; ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
               omega_ka ~ 0.09; omega_V ~ 0.04; omega_Cl ~ 0.09
               a <- 0.05; b <- 0.1
             })
             model({
               Tlag <- Tlag_pop
               ka <- ka_pop * exp(omega_ka)
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(omega_Cl)
               d/dt(depot) <- -ka * depot
               alag(depot) <- Tlag
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1, ss=1L, ii=12),
                     mlxObs(.id, c(0.5, 1, 1.5, 2, 3, 6, 11.5, 12.5, 14, 24, 36), cmt=2))
           },
           columns=c("ID", "TIME", "AMT", "SS", "II", "DV"),
           model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {Tlag, ka, V, Cl}

EQUATION:
Cc = pkmodel(Tlag, ka, V, Cl)

OUTPUT:
output = Cc
",
           mlxtran=.mlxProject(list(Tlag=.mlxPar(1), ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2),
                                    Cl=.mlxPar(3, 0.3)),
                               content=paste0(.mlxContent, "
SS = {use=steadystate, nbdoses=7}
II = {use=interdoseinterval}")))

## without a LIMIT column: CENS=1 below the LOQ (DV=LOQ), CENS=-1 above
## the upper limit (DV=ULOQ)
kitVariant("pkmodel-oral-1cmt", "data-cens-both",
           "left (CENS=1) and right (CENS=-1) censored observations without a LIMIT column",
           tags=c("data", "cens"),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxObs(.id, c(pkTimes(48), 72, 96), cmt=2))
           },
           columns=c("ID", "TIME", "AMT", "DV", "CENS"),
           postSim=function(d, s) {
             .m <- match(d$ROWID, s$ROWID)
             .obs <- d$EVID == 0 & d$MDV == 0 & !is.na(.m)
             d$DV[.obs] <- signif(s$sim[.m[.obs]], 6)
             d$CENS <- ifelse(.obs & d$DV < 0.1, 1L, ifelse(.obs & d$DV > 2.5, -1L, 0L))
             d$DV[d$CENS == 1L] <- 0.1
             d$DV[d$CENS == -1L] <- 2.5
             d
           },
           dryData=function(m, sim) {
             .t <- sim$data[sim$data$EVID == 0 & sim$data$MDV == 0, ]
             if (!all(c(-1L, 1L) %in% .t$CENS)) return("the simulation has no left or no right censoring")
             .d <- m$monolixData
             .d <- .d[!is.na(.d$dv), ]
             .m <- match(paste(.t$ID, .t$TIME), paste(.d$id, .d$time))
             if (anyNA(.m)) return("observations missing")
             .bad <- c(cens=!identical(as.integer(.d$cens[.m]), .t$CENS),
                       dv=!isTRUE(all.equal(.d$dv[.m], .t$DV)))
             if (any(.bad)) paste("differs from the data:", paste(names(.bad)[.bad], collapse=", "))
           },
           mlxtran=.dataProject(content=paste0(.mlxContent, "
CENS = {use=censored}")))
