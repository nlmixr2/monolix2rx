## Parameter distributions, covariate effects and correlations

.covData <- function(nSub) {
  .id <- seq_len(nSub)
  mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
          mlxObs(.id, pkTimes(48), cmt=2),
          cov=mlxCov(nSub, WT=function(n) round(stats::runif(n, 45, 110), 1),
                     SEX=function(n) rep_len(0:1, n)))
}

kitVariant("pkmodel-oral-1cmt", "cov-wt-lw70",
           "continuous covariate transformed in [COVARIATE] EQUATION: (lw70 = log(WT/70)) on V and Cl",
           tags=c("covariate"),
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
               beta_V_lw70 <- 1; beta_Cl_lw70 <- 0.75
               omega_ka ~ 0.09; omega_V ~ 0.04; omega_Cl ~ 0.09
               a <- 0.05; b <- 0.1
             })
             model({
               lw70 <- log(WT / 70)
               ka <- ka_pop * exp(omega_ka)
               V <- V_pop * exp(beta_V_lw70 * lw70 + omega_V)
               Cl <- Cl_pop * exp(beta_Cl_lw70 * lw70 + omega_Cl)
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           data=.covData,
           columns=c("ID", "TIME", "AMT", "DV", "WT"),
           mlxtran=.mlxProject(
             list(ka=.mlxPar(1.2, 0.3),
                  V=.mlxPar(30, 0.2, extra=", covariate=lw70, coefficient=beta_V_lw70"),
                  Cl=.mlxPar(3, 0.3, extra=", covariate=lw70, coefficient=beta_Cl_lw70")),
             params=c(beta_V_lw70=1, beta_Cl_lw70=0.75), indInput="lw70",
             content=paste0(.mlxContent, "\nWT = {use=covariate, type=continuous}"),
             covariate="[COVARIATE]
input = WT

EQUATION:
lw70 = log(WT/70)"))

kitVariant("pkmodel-oral-1cmt", "cov-sex-categorical",
           "categorical covariate (SEX 0/1, reference 0) on Cl",
           tags=c("covariate"),
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3; beta_Cl_SEX_1 <- -0.4
               omega_ka ~ 0.09; omega_V ~ 0.04; omega_Cl ~ 0.09
               a <- 0.05; b <- 0.1
             })
             model({
               ka <- ka_pop * exp(omega_ka)
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(beta_Cl_SEX_1 * (SEX == 1) + omega_Cl)
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           data=.covData,
           columns=c("ID", "TIME", "AMT", "DV", "SEX"),
           mlxtran=.mlxProject(
             list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2),
                  Cl=.mlxPar(3, 0.3, extra=", covariate=SEX, coefficient={0, beta_Cl_SEX_1}")),
             params=c(beta_Cl_SEX_1=-0.4), indInput="SEX",
             content=paste0(.mlxContent, "\nSEX = {use=covariate, type=categorical}"),
             covariate="[COVARIATE]
input = SEX

SEX = {type=categorical, categories={0, 1}}",
             indExtra=NULL))

kitVariant("pkmodel-oral-1cmt", "param-corr",
           "correlated random effects: correlation = {level=id, r(ka, Cl)=corr_ka_Cl}",
           tags=c("params", "correlation"),
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
               omega_ka + omega_Cl ~ c(0.09, 0.045, 0.09)
               omega_V ~ 0.04
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
                               params=c(corr_ka_Cl=0.5),
                               indExtra="correlation = {level=id, r(ka, Cl)=corr_ka_Cl}"))

kitVariant("pkmodel-oral-1cmt", "param-normal-novar",
           "normal distribution (V) and a parameter without variability (ka)",
           tags=c("params"),
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
               omega_V ~ 16; omega_Cl ~ 0.09
               a <- 0.05; b <- 0.1
             })
             model({
               ka <- ka_pop
               V <- V_pop + omega_V
               Cl <- Cl_pop * exp(omega_Cl)
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           mlxtran=.mlxProject(list(ka=.mlxPar(1.2), V=.mlxPar(30, 4, dist="normal"),
                                    Cl=.mlxPar(3, 0.3))))
