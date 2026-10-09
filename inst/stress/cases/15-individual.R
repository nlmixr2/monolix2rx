## More individual parameters (05-params.R has the common ones) and a
## regressor driving an ODE

## logitNormal between min and max instead of 0 and 1
kitVariant("pkmodel-oral-1cmt", "param-logit-bounds",
           "logitNormal ka with min=0.5, max=3 (generalized logit)",
           tags=c("params", "logit"),
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
               omega_ka ~ 0.25; omega_V ~ 0.04; omega_Cl ~ 0.09
               a <- 0.05; b <- 0.1
             })
             model({
               ka <- expit(logit(ka_pop, 0.5, 3) + omega_ka, 0.5, 3)
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(omega_Cl)
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           mlxtran=.mlxProject(list(ka=.mlxPar(1.2, 0.5, dist="logitNormal", extra=", min=0.5, max=3"),
                                    V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3))))

## var= gives the variance instead of the standard deviation
kitVariant("pkmodel-oral-1cmt", "param-var",
           "random effect given as a variance (var=omega2_Cl) instead of sd=",
           tags=c("params"),
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
               omega_ka ~ 0.09; omega_V ~ 0.04; omega2_Cl ~ 0.09
               a <- 0.05; b <- 0.1
             })
             model({
               ka <- ka_pop * exp(omega_ka)
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(omega2_Cl)
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           mlxtran=gsub("sd=omega2_Cl", "var=omega2_Cl",
                        gsub("omega_Cl", "omega2_Cl",
                             .mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2),
                                              Cl=.mlxPar(3, 0.09))), fixed=TRUE), fixed=TRUE))

## param-corr3 (05-params.R) with a negative correlation
kitVariant("pkmodel-oral-1cmt", "param-corr-negative",
           "three-way correlation block with a negative r(V, Cl)",
           tags=c("params", "correlation"),
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
               omega_ka + omega_V + omega_Cl ~ c(0.09, 0.018, 0.04, 0.045, -0.024, 0.09)
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
           },
           mlxtran=.mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2),
                                    Cl=.mlxPar(3, 0.3)),
                               params=c(corr_ka_V=0.3, corr_ka_Cl=0.5, corr_V_Cl=-0.4),
                               indExtra="correlation = {level=id, r(ka, V)=corr_ka_V, r(ka, Cl)=corr_ka_Cl, r(V, Cl)=corr_V_Cl}"))

## the correlations listed out of parameter order
kitVariant("pkmodel-iv-2cmt", "param-corr-two-blocks",
           "two separate correlation blocks r(Q, V2) and r(V, Cl), listed out of parameter order",
           tags=c("params", "correlation"),
           sim=function() {
             ini({
               V_pop <- 10; Cl_pop <- 2; Q_pop <- 4; V2_pop <- 40
               omega_V + omega_Cl ~ c(0.04, 0.036, 0.09)
               omega_Q + omega_V2 ~ c(0.04, -0.012, 0.04)
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
           mlxtran=.mlxProject(list(V=.mlxPar(10, 0.2), Cl=.mlxPar(2, 0.3),
                                    Q=.mlxPar(4, 0.2), V2=.mlxPar(40, 0.2)),
                               params=c(corr_Q_V2=-0.3, corr_V_Cl=0.6),
                               indExtra="correlation = {level=id, r(Q, V2)=corr_Q_V2, r(V, Cl)=corr_V_Cl}"))

kitVariant("cov-wt-lw70", "param-cov-corr",
           "covariate (lw70) on Cl, which is correlated with V",
           tags=c("params", "covariate", "correlation"),
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3; beta_Cl_lw70 <- 0.75
               omega_ka ~ 0.09
               omega_V + omega_Cl ~ c(0.04, 0.03, 0.09)
               a <- 0.05; b <- 0.1
             })
             model({
               lw70 <- log(WT / 70)
               ka <- ka_pop * exp(omega_ka)
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(beta_Cl_lw70 * lw70 + omega_Cl)
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           mlxtran=.mlxProject(
             list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2),
                  Cl=.mlxPar(3, 0.3, extra=", covariate=lw70, coefficient=beta_Cl_lw70")),
             params=c(beta_Cl_lw70=0.75, corr_V_Cl=0.5), indInput="lw70",
             content=paste0(.mlxContent, "\nWT = {use=covariate, type=continuous}"),
             indExtra="correlation = {level=id, r(V, Cl)=corr_V_Cl}",
             covariate="[COVARIATE]
input = WT

EQUATION:
lw70 = log(WT/70)"))

## the regressor changes on lines with neither a dose nor an observation,
## so its value between them is carried forward
.regRate <- function(t) {
  ifelse(t < 2, 0, ifelse(t < 10, 5, ifelse(t < 20, 2, 0)))
}

kitCase(
  name="reg-ode-input",
  covers="regressor as a zero-order input rate in ddt_, changing on regressor-only lines (MDV=1, no dose, no observation)",
  tags=c("data", "regressor", "ode"),
  sim=function() {
    ini({
      V_pop <- 10; Cl_pop <- 2
      omega_V ~ 0.04; omega_Cl ~ 0.09
      a <- 0.05; b <- 0.1
    })
    model({
      V <- V_pop * exp(omega_V)
      Cl <- Cl_pop * exp(omega_Cl)
      d/dt(central) <- RIN - Cl / V * central
      Cc <- central / V
      Cc ~ add(a) + prop(b) + combined1()
    })
  },
  data=function(nSub) {
    .id <- seq_len(nSub)
    .d <- mlxBind(mlxDose(.id, 0, amt=50, cmt=1),
                  mlxObs(.id, c(1, 3, 6, 9, 12, 16, 22, 26, 30, 36, 48), cmt=1),
                  mlxOther(.id, c(2, 10, 20), cmt=1))
    .d$RIN <- .regRate(.d$TIME)
    .d
  },
  columns=c("ID", "TIME", "AMT", "DV", "MDV", "RIN"),
  model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {V, Cl, rin}
rin = {use=regressor}

PK:
depot(target=Ac)

EQUATION:
ddt_Ac = rin - Cl/V*Ac
Cc = Ac/V

OUTPUT:
output = Cc
",
  mlxtran=.mlxProject(list(V=.mlxPar(10, 0.2), Cl=.mlxPar(2, 0.3)),
                      content=paste0(.mlxContent, "\nMDV = {use=missingdependentvariable}\nRIN = {use=regressor}")))
