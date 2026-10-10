## Delay differential equations.  rxode2 uses x(0) as the history only
## when it is a literal constant (otherwise 0), so the truths write
## literal initial conditions.

kitCase(
  name="dde-hutchinson",
  covers="delayed logistic growth without doses: x_0 history and t_0",
  tags=c("dde", "smoke"),
  known=if (utils::packageVersion("rxode2") < "5.1.8") "rxode2 < 5.1.8 delay() history is 0 when x(0) is not a literal constant (monolix2rx writes x(0) <- x_0); Monolix uses x_0 (nlmixr2/rxode2#1441)",
  solve=list(method="dop853"),
  sim=function() {
    ini({
      r_pop <- 0.5; K_pop <- 100; tau_pop <- 2
      omega_r ~ 0.04; omega_K ~ 0.04
      a <- 1; b <- 0.05
    })
    model({
      r <- r_pop * exp(omega_r)
      K <- K_pop * exp(omega_K)
      tau <- tau_pop
      x(0) <- 10
      d/dt(x) <- r * x * (1 - delay(x, tau) / K)
      x ~ add(a) + prop(b) + combined1()
    })
  },
  data=function(nSub) mlxObs(seq_len(nSub), c(1, 2, 4, 6, 8, 10, 12, 15, 20, 25, 30), cmt=1),
  columns=c("ID", "TIME", "DV"),
  model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {r, K, tau}

EQUATION:
t_0 = 0
x_0 = 10
ddt_x = r*x*(1 - delay(x, tau)/K)

OUTPUT:
output = x
",
  mlxtran=.mlxProject(list(r=.mlxPar(0.5, 0.2), K=.mlxPar(100, 0.2), tau=.mlxPar(2)),
                      errPar=c(a=1, b=0.05), pred="x",
                      content="ID = {use=identifier}
TIME = {use=time}
DV = {use=observation, name=CONC, type=continuous}"))

## The delayed-effect model: oral PK, indirect response on delay(Ac, tau)/V.
## `ode` is extra text at the start of EQUATION:
.ddeEffectModel <- function(ode="") {
  paste0("DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl, Kin, Kout, Imax, IC50, tau}

PK:
depot(target=Ad)

EQUATION:
", ode, "t_0 = 0
Ad_0 = 0
Ac_0 = 0
R_0 = Kin/Kout
ddt_Ad = -ka*Ad
ddt_Ac = ka*Ad - Cl/V*Ac
Cd = delay(Ac, tau)/V
ddt_R = Kin*(1 - Imax*Cd/(Cd + IC50)) - Kout*R

OUTPUT:
output = R
")
}

.ddeEffectProject <- function(content=.mlxContent, tau=4) {
  .mlxProject(list(ka=.mlxPar(1.2), V=.mlxPar(30), Cl=.mlxPar(3, 0.3),
                   Kin=.mlxPar(10), Kout=.mlxPar(0.1), Imax=.mlxPar(0.8),
                   IC50=.mlxPar(1), tau=.mlxPar(tau, 0.2)),
              errPar=c(a=1, b=0.05), pred="R", content=content)
}

kitCase(
  name="dde-delayed-effect",
  covers="oral PK driving an indirect response through delay(Ac, tau)/V, eta on tau",
  tags=c("dde", "smoke"),
  solve=list(method="dop853"),
  sim=function() {
    ini({
      ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
      Kin_pop <- 10; Kout_pop <- 0.1; Imax_pop <- 0.8; IC50_pop <- 1; tau_pop <- 4
      omega_Cl ~ 0.09; omega_tau ~ 0.04
      a <- 1; b <- 0.05
    })
    model({
      ka <- ka_pop
      V <- V_pop
      Cl <- Cl_pop * exp(omega_Cl)
      Kin <- Kin_pop
      Kout <- Kout_pop
      Imax <- Imax_pop
      IC50 <- IC50_pop
      tau <- tau_pop * exp(omega_tau)
      R(0) <- Kin / Kout
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - Cl / V * central
      Cd <- delay(central, tau) / V
      d/dt(R) <- Kin * (1 - Imax * Cd / (Cd + IC50)) - Kout * R
      R ~ add(a) + prop(b) + combined1()
    })
  },
  data=function(nSub) {
    .id <- seq_len(nSub)
    mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
            mlxObs(.id, c(pkTimes(72), 96, 120), cmt=3))
  },
  model=.ddeEffectModel(),
  mlxtran=.ddeEffectProject())

kitVariant("dde-delayed-effect", "dde-multidose",
           "ADDL/II doses (q12h, 6 doses) through delay(Ac, tau)",
           tags=c("dde", "dosing"),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1, ii=12, addl=5),
                     mlxObs(.id, c(pkTimes(12), 36, 60, 61, 62, 64, 66, 72, 96, 120, 144), cmt=3))
           },
           columns=c("ID", "TIME", "AMT", "II", "ADDL", "DV"),
           mlxtran=.ddeEffectProject(content=paste0(.mlxContent, "
II = {use=interdoseinterval}
ADDL = {use=additionaldose}")))

## records what each tool does: the steady state of a delayed system
## needs the history over the dosing interval (XPASS under the mock, whose
## Monolix results are the rxode2 truth)
kitVariant("dde-delayed-effect", "dde-ss",
           "steady-state dose (use=steadystate, q24h) through delay(Ac, tau)",
           tags=c("dde", "ss"),
           knownRun="rxode2 5.1.8 steady state with delay(): Ac reaches steady state but R starts at R_0 and the delay history is 0 (nlmixr2/rxode2#1447)",
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1, ss=1L, ii=24),
                     mlxObs(.id, c(pkTimes(24), 36, 48), cmt=3))
           },
           columns=c("ID", "TIME", "AMT", "SS", "II", "DV"),
           mlxtran=.ddeEffectProject(content=paste0(.mlxContent, "
SS = {use=steadystate}
II = {use=interdoseinterval}")))

kitVariant("dde-delayed-effect", "dde-stiff",
           "odeType=stiff with delay(Ac, tau)",
           tags=c("dde", "stiff"),
           model=.ddeEffectModel("odeType = stiff\n"))

kitVariant("dde-delayed-effect", "dde-pkmodel-mixed",
           "compartment/oral/elimination macros feeding delay(Ac, tau) in a ddt_ response",
           tags=c("dde", "macro"),
           model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl, Kin, Kout, Imax, IC50, tau}

PK:
k = Cl/V
compartment(cmt=1, amount=Ac)
oral(cmt=1, ka)
elimination(cmt=1, k)

EQUATION:
t_0 = 0
R_0 = Kin/Kout
Cd = delay(Ac, tau)/V
ddt_R = Kin*(1 - Imax*Cd/(Cd + IC50)) - Kout*R

OUTPUT:
output = R
")

kitVariant("dde-delayed-effect", "dde-delay-expr",
           "delay time given by an expression: delay(Ac, 0.5*tau)",
           tags=c("dde"),
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
               Kin_pop <- 10; Kout_pop <- 0.1; Imax_pop <- 0.8; IC50_pop <- 1; tau_pop <- 8
               omega_Cl ~ 0.09; omega_tau ~ 0.04
               a <- 1; b <- 0.05
             })
             model({
               ka <- ka_pop
               V <- V_pop
               Cl <- Cl_pop * exp(omega_Cl)
               Kin <- Kin_pop
               Kout <- Kout_pop
               Imax <- Imax_pop
               IC50 <- IC50_pop
               tau <- tau_pop * exp(omega_tau)
               R(0) <- Kin / Kout
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cd <- delay(central, 0.5 * tau) / V
               d/dt(R) <- Kin * (1 - Imax * Cd / (Cd + IC50)) - Kout * R
               R ~ add(a) + prop(b) + combined1()
             })
           },
           model=sub("delay(Ac, tau)", "delay(Ac, 0.5*tau)", .ddeEffectModel(), fixed=TRUE),
           mlxtran=.ddeEffectProject(tau=8))

## a delayed depot (a lag written as a delay) and a delayed effect
kitCase(
  name="dde-two-delays",
  covers="two delays on two states: delay(Ad, tlag) into the central compartment, delay(Ac, tau) on the effect",
  tags=c("dde"),
  solve=list(method="dop853"),
  sim=function() {
    ini({
      ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3; tlag_pop <- 0.75
      Kin_pop <- 10; Kout_pop <- 0.1; Imax_pop <- 0.8; IC50_pop <- 1; tau_pop <- 4
      omega_Cl ~ 0.09; omega_tlag ~ 0.04; omega_tau ~ 0.04
      a <- 1; b <- 0.05
    })
    model({
      ka <- ka_pop
      V <- V_pop
      Cl <- Cl_pop * exp(omega_Cl)
      tlag <- tlag_pop * exp(omega_tlag)
      Kin <- Kin_pop
      Kout <- Kout_pop
      Imax <- Imax_pop
      IC50 <- IC50_pop
      tau <- tau_pop * exp(omega_tau)
      R(0) <- Kin / Kout
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * delay(depot, tlag) - Cl / V * central
      Cd <- delay(central, tau) / V
      d/dt(R) <- Kin * (1 - Imax * Cd / (Cd + IC50)) - Kout * R
      R ~ add(a) + prop(b) + combined1()
    })
  },
  data=function(nSub) {
    .id <- seq_len(nSub)
    mlxBind(mlxDose(.id, c(0, 24), amt=100, cmt=1),
            mlxObs(.id, c(0.5, 1, 2, 4, 8, 12, 24.5, 25, 26, 28, 32, 48, 72, 96), cmt=3))
  },
  model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl, tlag, Kin, Kout, Imax, IC50, tau}

PK:
depot(target=Ad)

EQUATION:
t_0 = 0
Ad_0 = 0
Ac_0 = 0
R_0 = Kin/Kout
ddt_Ad = -ka*Ad
ddt_Ac = ka*delay(Ad, tlag) - Cl/V*Ac
Cd = delay(Ac, tau)/V
ddt_R = Kin*(1 - Imax*Cd/(Cd + IC50)) - Kout*R

OUTPUT:
output = R
",
  mlxtran=.mlxProject(list(ka=.mlxPar(1.2), V=.mlxPar(30), Cl=.mlxPar(3, 0.3),
                           tlag=.mlxPar(0.75, 0.2), Kin=.mlxPar(10), Kout=.mlxPar(0.1),
                           Imax=.mlxPar(0.8), IC50=.mlxPar(1), tau=.mlxPar(4, 0.2)),
                      errPar=c(a=1, b=0.05), pred="R"))
