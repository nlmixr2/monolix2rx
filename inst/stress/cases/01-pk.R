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
