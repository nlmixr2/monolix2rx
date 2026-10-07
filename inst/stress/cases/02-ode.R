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
