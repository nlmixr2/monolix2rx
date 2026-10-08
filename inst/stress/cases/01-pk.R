## PK macros, pkmodel() and library models

.oralTruth <- function() {
  ini({
    ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
    omega_ka ~ 0.09; omega_V ~ 0.04; omega_Cl ~ 0.09
    a <- 0.05; b <- 0.1
  })
  model({
    ka <- ka_pop * exp(omega_ka)
    V <- V_pop * exp(omega_V)
    Cl <- Cl_pop * exp(omega_Cl)
    d/dt(depot) <- -ka * depot
    d/dt(central) <- ka * depot - Cl / V * central
    Cc <- central / V
    Cc ~ add(a) + prop(b) + combined1()
  })
}

.oralData <- function(nSub) {
  .id <- seq_len(nSub)
  mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
          mlxObs(.id, pkTimes(48), cmt=2))
}

## one endpoint CONC with a combined1 error
.oralProject <- .mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2),
                                 Cl=.mlxPar(3, 0.3)))

kitCase(
  name="pkmodel-oral-1cmt",
  covers="pkmodel(ka, V, Cl) one-compartment oral, logNormal parameters, combined1 error",
  tags=c("pk", "pkmodel", "smoke"),
  sim=.oralTruth,
  data=.oralData,
  model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl}

EQUATION:
Cc = pkmodel(ka, V, Cl)

OUTPUT:
output = Cc
",
  mlxtran=.oralProject)

kitVariant("pkmodel-oral-1cmt", "ode-oral-1cmt",
           "the same one-compartment oral model written as ddt_ equations with a depot() macro",
           tags=c("ode", "macro", "smoke"),
           model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl}

PK:
depot(target=Ad)

EQUATION:
ddt_Ad = -ka*Ad
ddt_Ac = ka*Ad - Cl/V*Ac
Cc = Ac/V

OUTPUT:
output = Cc
")

## IV bolus, two compartments
kitCase(
  name="pkmodel-iv-2cmt",
  covers="pkmodel(V, Cl, Q2=Q, V2) two-compartment IV bolus with clearances",
  tags=c("pk", "pkmodel"),
  sim=function() {
    ini({
      V_pop <- 10; Cl_pop <- 2; Q_pop <- 4; V2_pop <- 40
      omega_V ~ 0.04; omega_Cl ~ 0.09; omega_Q ~ 0.04; omega_V2 ~ 0.04
      a <- 0.05; b <- 0.1
    })
    model({
      V <- V_pop * exp(omega_V)
      Cl <- Cl_pop * exp(omega_Cl)
      Q <- Q_pop * exp(omega_Q)
      V2 <- V2_pop * exp(omega_V2)
      d/dt(central) <- -Cl / V * central - Q / V * central + Q / V2 * periph
      d/dt(periph) <- Q / V * central - Q / V2 * periph
      Cc <- central / V
      Cc ~ add(a) + prop(b) + combined1()
    })
  },
  data=function(nSub) {
    .id <- seq_len(nSub)
    mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
            mlxObs(.id, c(0.1, pkTimes(72)), cmt=1))
  },
  model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {V, Cl, Q, V2}

EQUATION:
Cc = pkmodel(V, Cl, Q2=Q, V2)

OUTPUT:
output = Cc
",
  mlxtran=.mlxProject(list(V=.mlxPar(10, 0.2), Cl=.mlxPar(2, 0.3),
                           Q=.mlxPar(4, 0.2), V2=.mlxPar(40, 0.2))))

kitVariant("pkmodel-iv-2cmt", "pkmodel-iv-2cmt-k",
           "pkmodel(V, Cl, k12, k21) two-compartment IV bolus, rates computed in EQUATION:",
           model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {V, Cl, Q, V2}

EQUATION:
k12 = Q/V
k21 = Q/V2
Cc = pkmodel(V, Cl, k12, k21)

OUTPUT:
output = Cc
")

kitVariant("pkmodel-oral-1cmt", "pkmodel-oral-tlag",
           "pkmodel(Tlag, ka, V, Cl) with a lag time with between-subject variability",
           tags=c("pk", "pkmodel"),
           sim=function() {
             ini({
               Tlag_pop <- 0.5; ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
               omega_Tlag ~ 0.04; omega_ka ~ 0.09; omega_V ~ 0.04; omega_Cl ~ 0.09
               a <- 0.05; b <- 0.1
             })
             model({
               Tlag <- Tlag_pop * exp(omega_Tlag)
               ka <- ka_pop * exp(omega_ka)
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(omega_Cl)
               d/dt(depot) <- -ka * depot
               lag(depot) <- Tlag
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {Tlag, ka, V, Cl}

EQUATION:
Cc = pkmodel(Tlag, ka, V, Cl)

OUTPUT:
output = Cc
",
           mlxtran=.mlxProject(list(Tlag=.mlxPar(0.5, 0.2), ka=.mlxPar(1.2, 0.3),
                                    V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3))))

## one-compartment oral truth with a parameterized elimination line
.oralTruthWith <- function(pars, omegas, elim, extra=NULL, depot=NULL) {
  eval(bquote(function() {
    ini(.(as.call(c(list(as.name("{")), pars, omegas,
                    list(quote(a <- 0.05), quote(b <- 0.1))))))
    model(.(as.call(c(list(as.name("{")), extra,
                      list(quote(d/dt(depot) <- -ka * depot)), depot,
                      list(bquote(d/dt(central) <- ka * depot - .(elim)),
                           quote(Cc <- central / V),
                           quote(Cc ~ add(a) + prop(b) + combined1()))))))
  }))
}

.pkModel <- function(input, eq, out="Cc") {
  paste0("DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {", input, "}

EQUATION:
", eq, "

OUTPUT:
output = ", out, "
")
}

kitVariant("pkmodel-oral-1cmt", "pkmodel-oral-k",
           "pkmodel(ka, V, k) with an elimination rate constant",
           tags=c("pk", "pkmodel"),
           sim=.oralTruthWith(
             list(quote(ka_pop <- 1.2), quote(V_pop <- 30), quote(k_pop <- 0.1)),
             list(quote(omega_ka ~ 0.09), quote(omega_V ~ 0.04), quote(omega_k ~ 0.09)),
             quote(k * central),
             extra=list(quote(ka <- ka_pop * exp(omega_ka)), quote(V <- V_pop * exp(omega_V)),
                        quote(k <- k_pop * exp(omega_k)))),
           model=.pkModel("ka, V, k", "Cc = pkmodel(ka, V, k)"),
           mlxtran=.mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2),
                                    k=.mlxPar(0.1, 0.3))))

kitVariant("pkmodel-oral-1cmt", "pkmodel-oral-p",
           "pkmodel(ka, p, V, Cl) with a logitNormal bioavailability",
           tags=c("pk", "pkmodel"),
           sim=.oralTruthWith(
             list(quote(ka_pop <- 1.2), quote(p_pop <- 0.7), quote(V_pop <- 30), quote(Cl_pop <- 3)),
             list(quote(omega_ka ~ 0.09), quote(omega_p ~ 0.25), quote(omega_V ~ 0.04),
                  quote(omega_Cl ~ 0.09)),
             quote(Cl / V * central),
             extra=list(quote(ka <- ka_pop * exp(omega_ka)),
                        quote(p <- expit(logit(p_pop) + omega_p)),
                        quote(V <- V_pop * exp(omega_V)), quote(Cl <- Cl_pop * exp(omega_Cl))),
             depot=list(quote(f(depot) <- p))),
           model=.pkModel("ka, p, V, Cl", "Cc = pkmodel(ka, p, V, Cl)"),
           mlxtran=.mlxProject(list(ka=.mlxPar(1.2, 0.3), p=.mlxPar(0.7, 0.5, dist="logitNormal"),
                                    V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3))))

kitVariant("pkmodel-oral-1cmt", "pkmodel-oral-mm",
           "pkmodel(ka, V, Vm, Km) with Michaelis-Menten elimination",
           tags=c("pk", "pkmodel", "nonlinear"),
           sim=.oralTruthWith(
             list(quote(ka_pop <- 1.2), quote(V_pop <- 30), quote(Vm_pop <- 10), quote(Km_pop <- 2)),
             list(quote(omega_ka ~ 0.09), quote(omega_V ~ 0.04), quote(omega_Vm ~ 0.09)),
             quote(Vm * (central / V) / (Km + central / V)),
             extra=list(quote(ka <- ka_pop * exp(omega_ka)), quote(V <- V_pop * exp(omega_V)),
                        quote(Vm <- Vm_pop * exp(omega_Vm)), quote(Km <- Km_pop))),
           model=.pkModel("ka, V, Vm, Km", "Cc = pkmodel(ka, V, Vm, Km)"),
           mlxtran=.mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2),
                                    Vm=.mlxPar(10, 0.3), Km=.mlxPar(2))))

## Savic transit: n + 1 compartments at rate Ktr, n = Mtt*Ktr - 1 (an
## integer here, so the truth can write the chain out)
kitVariant("pkmodel-oral-1cmt", "pkmodel-oral-transit",
           "pkmodel(Mtt, Ktr, ka, V, Cl) transit absorption",
           tags=c("pk", "pkmodel", "transit"),
           sim=function() {
             ini({
               Mtt_pop <- 2; Ktr_pop <- 2.5; ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
               omega_V ~ 0.04; omega_Cl ~ 0.09
               a <- 0.05; b <- 0.1
             })
             model({
               Mtt <- Mtt_pop
               Ktr <- Ktr_pop
               ka <- ka_pop
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(omega_Cl)
               d/dt(tr0) <- -Ktr * tr0
               d/dt(tr1) <- Ktr * tr0 - Ktr * tr1
               d/dt(tr2) <- Ktr * tr1 - Ktr * tr2
               d/dt(tr3) <- Ktr * tr2 - Ktr * tr3
               d/dt(tr4) <- Ktr * tr3 - Ktr * tr4
               d/dt(depot) <- Ktr * tr4 - ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxObs(.id, pkTimes(48), cmt=7))
           },
           model=.pkModel("Mtt, Ktr, ka, V, Cl", "Cc = pkmodel(Mtt, Ktr, ka, V, Cl)"),
           mlxtran=.mlxProject(list(Mtt=.mlxPar(2), Ktr=.mlxPar(2.5), ka=.mlxPar(1.2),
                                    V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3))))

## zero-order absorption: the truth infuses over Tk0 (RATE=-2, not written)
kitCase(
  name="pkmodel-tk0",
  covers="pkmodel(Tk0, V, Cl) zero-order absorption with an eta on Tk0",
  tags=c("pk", "pkmodel"),
  sim=function() {
    ini({
      Tk0_pop <- 2; V_pop <- 30; Cl_pop <- 3
      omega_Tk0 ~ 0.09; omega_V ~ 0.04; omega_Cl ~ 0.09
      a <- 0.05; b <- 0.1
    })
    model({
      Tk0 <- Tk0_pop * exp(omega_Tk0)
      V <- V_pop * exp(omega_V)
      Cl <- Cl_pop * exp(omega_Cl)
      d/dt(central) <- -Cl / V * central
      dur(central) <- Tk0
      Cc <- central / V
      Cc ~ add(a) + prop(b) + combined1()
    })
  },
  data=function(nSub) {
    .id <- seq_len(nSub)
    mlxBind(mlxDose(.id, 0, amt=100, rate=-2, cmt=1),
            mlxObs(.id, c(0.5, 1, 1.5, 2, 2.5, 3, 4, 6, 8, 12, 24, 36), cmt=1))
  },
  model=.pkModel("Tk0, V, Cl", "Cc = pkmodel(Tk0, V, Cl)"),
  mlxtran=.mlxProject(list(Tk0=.mlxPar(2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3))))

kitCase(
  name="pkmodel-iv-3cmt",
  covers="pkmodel(V, Cl, Q2, V2, Q3, V3) three-compartment IV bolus",
  tags=c("pk", "pkmodel"),
  sim=function() {
    ini({
      V_pop <- 10; Cl_pop <- 2; Q2_pop <- 4; V2_pop <- 40; Q3_pop <- 1; V3_pop <- 100
      omega_V ~ 0.04; omega_Cl ~ 0.09; omega_V2 ~ 0.04
      a <- 0.05; b <- 0.1
    })
    model({
      V <- V_pop * exp(omega_V)
      Cl <- Cl_pop * exp(omega_Cl)
      Q2 <- Q2_pop
      V2 <- V2_pop * exp(omega_V2)
      Q3 <- Q3_pop
      V3 <- V3_pop
      d/dt(central) <- -(Cl + Q2 + Q3) / V * central + Q2 / V2 * p1 + Q3 / V3 * p2
      d/dt(p1) <- Q2 / V * central - Q2 / V2 * p1
      d/dt(p2) <- Q3 / V * central - Q3 / V3 * p2
      Cc <- central / V
      Cc ~ add(a) + prop(b) + combined1()
    })
  },
  data=function(nSub) {
    .id <- seq_len(nSub)
    mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
            mlxObs(.id, c(0.1, pkTimes(72), 120, 168), cmt=1))
  },
  model=.pkModel("V, Cl, Q2, V2, Q3, V3", "Cc = pkmodel(V, Cl, Q2, V2, Q3, V3)"),
  mlxtran=.mlxProject(list(V=.mlxPar(10, 0.2), Cl=.mlxPar(2, 0.3), Q2=.mlxPar(4),
                           V2=.mlxPar(40, 0.2), Q3=.mlxPar(1), V3=.mlxPar(100))))

kitVariant("pkmodel-oral-1cmt", "pkmodel-effect",
           "{Cc, Ce} = pkmodel(ka, V, Cl, ke0) with the effect compartment observed",
           tags=c("pk", "pkmodel", "effect"),
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3; ke0_pop <- 0.3
               omega_ka ~ 0.09; omega_V ~ 0.04; omega_Cl ~ 0.09; omega_ke0 ~ 0.09
               a <- 0.05; b <- 0.1
             })
             model({
               ka <- ka_pop * exp(omega_ka)
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(omega_Cl)
               ke0 <- ke0_pop * exp(omega_ke0)
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               d/dt(Ce) <- ke0 * (Cc - Ce)
               Ce ~ add(a) + prop(b) + combined1()
             })
           },
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxObs(.id, pkTimes(48), cmt=3))
           },
           model=.pkModel("ka, V, Cl, ke0", "{Cc, Ce} = pkmodel(ka, V, Cl, ke0)", out="Ce"),
           mlxtran=.mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2),
                                    Cl=.mlxPar(3, 0.3), ke0=.mlxPar(0.3, 0.3)),
                               pred="Ce"))

## explicit macros, two administration routes (ADM 1 oral, ADM 2 IV bolus)
kitCase(
  name="macro-oral-iv-2cmt",
  covers="compartment/oral/iv/peripheral/elimination macros, ADM column routing two routes",
  tags=c("pk", "macro", "adm"),
  sim=function() {
    ini({
      ka_pop <- 1.2; V_pop <- 30; k_pop <- 0.1; k12_pop <- 0.3; k21_pop <- 0.1
      omega_ka ~ 0.09; omega_V ~ 0.04; omega_k ~ 0.09
      a <- 0.05; b <- 0.1
    })
    model({
      ka <- ka_pop * exp(omega_ka)
      V <- V_pop * exp(omega_V)
      k <- k_pop * exp(omega_k)
      k12 <- k12_pop
      k21 <- k21_pop
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - k * central - k12 * central + k21 * periph
      d/dt(periph) <- k12 * central - k21 * periph
      Cc <- central / V
      Cc ~ add(a) + prop(b) + combined1()
    })
  },
  data=function(nSub) {
    .id <- seq_len(nSub)
    mlxBind(mlxDose(.id, 0, amt=100, cmt=1, adm=1),
            mlxDose(.id, 48, amt=50, cmt=2, adm=2),
            mlxObs(.id, c(pkTimes(48), 48.25, 49, 52, 60, 72, 96), cmt=2))
  },
  columns=c("ID", "TIME", "AMT", "ADM", "DV"),
  model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, k, k12, k21}

PK:
compartment(cmt=1, amount=Ac)
oral(adm=1, cmt=1, ka)
iv(adm=2, cmt=1)
peripheral(k12, k21)
elimination(cmt=1, k)
Cc = Ac/V

OUTPUT:
output = Cc
",
  mlxtran=.mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), k=.mlxPar(0.1, 0.3),
                           k12=.mlxPar(0.3), k21=.mlxPar(0.1)),
                      content=paste0(.mlxContent, "
ADM = {use=administration}")))

kitVariant("pkmodel-oral-1cmt", "macro-depot-target",
           "depot(target=Ac, ka) into a compartment() macro",
           tags=c("pk", "macro"),
           model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl}

PK:
compartment(cmt=1, amount=Ac)
depot(target=Ac, ka)
elimination(cmt=1, k=Cl/V)
Cc = Ac/V

OUTPUT:
output = Cc
")

## one administration split between a zero-order and a first-order route
kitCase(
  name="macro-dual-absorption",
  covers="two absorption() macros on one adm: Tk0 (fraction F1) and ka (fraction 1-F1)",
  tags=c("pk", "macro", "absorption"),
  sim=function() {
    ini({
      F1_pop <- 0.4; Tk0_pop <- 2; ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
      omega_F1 ~ 0.25; omega_V ~ 0.04; omega_Cl ~ 0.09
      a <- 0.05; b <- 0.1
    })
    model({
      F1 <- expit(logit(F1_pop) + omega_F1)
      Tk0 <- Tk0_pop
      ka <- ka_pop
      V <- V_pop * exp(omega_V)
      Cl <- Cl_pop * exp(omega_Cl)
      d/dt(depot) <- -ka * depot
      f(depot) <- 1 - F1
      d/dt(central) <- ka * depot - Cl / V * central
      f(central) <- F1
      dur(central) <- Tk0
      Cc <- central / V
      Cc ~ add(a) + prop(b) + combined1()
    })
  },
  data=function(nSub) {
    .id <- seq_len(nSub)
    mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
            mlxDose(.id, 0, amt=100, cmt=2, rate=-2),
            mlxObs(.id, c(0.5, 1, 1.5, 2, 3, 4, 6, 8, 12, 24, 36), cmt=2))
  },
  ## Monolix sees one dose per time
  write=function(d) {
    d <- .kitMonolixRows(d)
    d <- d[!(d$EVID == 1L & d$CMT == 2L), c("ID", "TIME", "AMT", "DV")]
    d
  },
  model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {F1, Tk0, ka, V, Cl}

PK:
F2 = 1 - F1
compartment(cmt=1, amount=Ac)
absorption(adm=1, cmt=1, Tk0, p=F1)
absorption(adm=1, cmt=1, ka, p=F2)
elimination(cmt=1, k=Cl/V)
Cc = Ac/V

OUTPUT:
output = Cc
",
  mlxtran=.mlxProject(list(F1=.mlxPar(0.4, 0.5, dist="logitNormal"), Tk0=.mlxPar(2),
                           ka=.mlxPar(1.2), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3))))

## Monolix data with an ADM value and AMT=0 on the observation rows
kitVariant("pkmodel-tk0", "pkmodel-tk0-adm-rows",
           "Tk0 with ADM=1 and AMT=0 written on every observation row",
           tags=c("pk", "data"),
           write=function(d) {
             d <- .kitMonolixRows(d)
             d$AMT[is.na(d$AMT)] <- 0
             d$ADM <- 1L
             d[, c("ID", "TIME", "AMT", "ADM", "DV")]
           },
           mlxtran=.mlxProject(list(Tk0=.mlxPar(2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3)),
                               content=paste0(.mlxContent, "\nADM = {use=administration}")))
