## ddt_ systems

kitCase(
  name="ode-turnover",
  covers="indirect response (inhibited input) with R_0 = Kin/Kout, t_0, logitNormal Imax",
  tags=c("ode", "pd"),
  sim=function() {
    ini({
      ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
      Kin_pop <- 10; Kout_pop <- 0.1; Imax_pop <- 0.8; IC50_pop <- 1
      omega_V ~ 0.04; omega_Cl ~ 0.09; omega_Kout ~ 0.04; omega_Imax ~ 0.25
      a <- 1; b <- 0.05
    })
    model({
      ka <- ka_pop
      V <- V_pop * exp(omega_V)
      Cl <- Cl_pop * exp(omega_Cl)
      Kin <- Kin_pop
      Kout <- Kout_pop * exp(omega_Kout)
      Imax <- expit(logit(Imax_pop) + omega_Imax)
      IC50 <- IC50_pop
      R(0) <- Kin / Kout
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - Cl / V * central
      Cc <- central / V
      d/dt(R) <- Kin * (1 - Imax * Cc / (Cc + IC50)) - Kout * R
      R ~ add(a) + prop(b) + combined1()
    })
  },
  data=function(nSub) {
    .id <- seq_len(nSub)
    mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
            mlxObs(.id, c(pkTimes(72), 96, 120), cmt=3))
  },
  model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl, Kin, Kout, Imax, IC50}

PK:
depot(target=Ad)

EQUATION:
t_0 = 0
Ad_0 = 0
Ac_0 = 0
R_0 = Kin/Kout
Cc = Ac/V
ddt_Ad = -ka*Ad
ddt_Ac = ka*Ad - Cl/V*Ac
E = Imax*Cc/(Cc + IC50)
ddt_R = Kin*(1 - E) - Kout*R

OUTPUT:
output = R
",
  mlxtran=.mlxProject(list(ka=.mlxPar(1.2), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3),
                           Kin=.mlxPar(10), Kout=.mlxPar(0.1, 0.2),
                           Imax=.mlxPar(0.8, 0.5, dist="logitNormal"),
                           IC50=.mlxPar(1)),
                      errPar=c(a=1, b=0.05), pred="R"))

## a clearance that switches at t = 24 (if/elseif/else on t)
kitCase(
  name="ode-ifelse-time",
  covers="if/elseif/else on t inside EQUATION: (time-varying clearance)",
  tags=c("ode", "ifelse"),
  sim=function() {
    ini({
      ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3; fCl_pop <- 0.5
      omega_V ~ 0.04; omega_Cl ~ 0.09
      a <- 0.05; b <- 0.1
    })
    model({
      ka <- ka_pop
      V <- V_pop * exp(omega_V)
      Cl <- Cl_pop * exp(omega_Cl)
      fCl <- fCl_pop
      if (time < 24) {
        ClT <- Cl
      } else if (time < 48) {
        ClT <- Cl * fCl
      } else {
        ClT <- Cl * fCl * fCl
      }
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - ClT / V * central
      Cc <- central / V
      Cc ~ add(a) + prop(b) + combined1()
    })
  },
  data=function(nSub) {
    .id <- seq_len(nSub)
    mlxBind(mlxDose(.id, c(0, 24, 48), amt=100, cmt=1),
            mlxObs(.id, c(pkTimes(24), 26, 30, 36, 47, 50, 54, 60, 72), cmt=2))
  },
  model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl, fCl}

PK:
depot(target=Ad)

EQUATION:
if t < 24
  ClT = Cl
elseif t < 48
  ClT = Cl*fCl
else
  ClT = Cl*fCl*fCl
end
ddt_Ad = -ka*Ad
ddt_Ac = ka*Ad - ClT/V*Ac
Cc = Ac/V

OUTPUT:
output = Cc
",
  mlxtran=.mlxProject(list(ka=.mlxPar(1.2), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3),
                           fCl=.mlxPar(0.5))))

## stiff target-mediated disposition (fast binding)
kitCase(
  name="ode-stiff-tmdd",
  covers="odeType=stiff, TMDD with fast binding, several _0 initial conditions",
  tags=c("ode", "stiff"),
  sim=function() {
    ini({
      V_pop <- 3; Cl_pop <- 0.2; kon_pop <- 10; koff_pop <- 1; kint_pop <- 0.05
      ksyn_pop <- 1; kdeg_pop <- 0.25
      omega_V ~ 0.04; omega_Cl ~ 0.09; omega_ksyn ~ 0.04
      a <- 0.05; b <- 0.1
    })
    model({
      V <- V_pop * exp(omega_V)
      Cl <- Cl_pop * exp(omega_Cl)
      kon <- kon_pop
      koff <- koff_pop
      kint <- kint_pop
      ksyn <- ksyn_pop * exp(omega_ksyn)
      kdeg <- kdeg_pop
      R(0) <- ksyn / kdeg
      Cf <- central / V
      d/dt(central) <- -Cl / V * central - kon * Cf * R * V + koff * RC * V
      d/dt(R) <- ksyn - kdeg * R - kon * Cf * R + koff * RC
      d/dt(RC) <- kon * Cf * R - koff * RC - kint * RC
      Cc <- Cf
      Cc ~ add(a) + prop(b) + combined1()
    })
  },
  data=function(nSub) {
    .id <- seq_len(nSub)
    mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
            mlxObs(.id, c(0.1, 0.5, 1, 2, 4, 8, 24, 48, 96, 168), cmt=1))
  },
  solve=list(method="liblsoda"),
  model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {V, Cl, kon, koff, kint, ksyn, kdeg}

PK:
depot(target=Ac)

EQUATION:
odeType = stiff
Ac_0 = 0
R_0 = ksyn/kdeg
RC_0 = 0
Cf = Ac/V
ddt_Ac = -Cl/V*Ac - kon*Cf*R*V + koff*RC*V
ddt_R = ksyn - kdeg*R - kon*Cf*R + koff*RC
ddt_RC = kon*Cf*R - koff*RC - kint*RC
Cc = Cf

OUTPUT:
output = Cc
",
  mlxtran=.mlxProject(list(V=.mlxPar(3, 0.2), Cl=.mlxPar(0.2, 0.3), kon=.mlxPar(10),
                           koff=.mlxPar(1), kint=.mlxPar(0.05), ksyn=.mlxPar(1, 0.2),
                           kdeg=.mlxPar(0.25))))

## Monolix math functions inside the equations
kitCase(
  name="ode-math-functions",
  covers="exp, log, sqrt, abs, max, min, ^ and logistic/Hill terms in EQUATION:",
  tags=c("ode", "math"),
  sim=function() {
    ini({
      ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3; Emax_pop <- 2; EC50_pop <- 1; gam_pop <- 2
      omega_V ~ 0.04; omega_Cl ~ 0.09; omega_EC50 ~ 0.09
      a <- 0.05; b <- 0.1
    })
    model({
      ka <- ka_pop
      V <- V_pop * exp(omega_V)
      Cl <- Cl_pop * exp(omega_Cl)
      Emax <- Emax_pop
      EC50 <- EC50_pop * exp(omega_EC50)
      gam <- gam_pop
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - Cl / V * central
      Cc <- max(central / V, 0)
      lc <- log(1 + Cc)
      E <- Emax * Cc^gam / (EC50^gam + Cc^gam)
      Y <- sqrt(abs(lc) + 1) + exp(-E) * min(E, 1)
      Y ~ add(a) + prop(b) + combined1()
    })
  },
  data=function(nSub) {
    .id <- seq_len(nSub)
    mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
            mlxObs(.id, pkTimes(48), cmt=2))
  },
  model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl, Emax, EC50, gam}

PK:
depot(target=Ad)

EQUATION:
ddt_Ad = -ka*Ad
ddt_Ac = ka*Ad - Cl/V*Ac
Cc = max(Ac/V, 0)
lc = log(1 + Cc)
E = Emax*Cc^gam/(EC50^gam + Cc^gam)
Y = sqrt(abs(lc) + 1) + exp(-E)*min(E, 1)

OUTPUT:
output = Y
",
  mlxtran=.mlxProject(list(ka=.mlxPar(1.2), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3),
                           Emax=.mlxPar(2), EC50=.mlxPar(1, 0.3), gam=.mlxPar(2)),
                      pred="Y"))
