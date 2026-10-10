## Covariate equations with if/else, and covariates on parameters without
## random effects

## if/else cannot be inlined into the parameter, so it starts the model
kitVariant("pkmodel-oral-1cmt", "cov-equation-ifelse",
           "weight band from if/elseif/else in [COVARIATE] EQUATION: (hWT) on Cl, next to lw70 on V",
           tags=c("covariate"),
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
               beta_V_lw70 <- 1; beta_Cl_hWT <- 0.4
               omega_ka ~ 0.09; omega_V ~ 0.04; omega_Cl ~ 0.09
               a <- 0.05; b <- 0.1
             })
             model({
               if (WT > 90) {
                 hWT <- 1
               } else if (WT > 60) {
                 hWT <- 0.5
               } else {
                 hWT <- 0
               }
               lw70 <- log(WT / 70)
               ka <- ka_pop * exp(omega_ka)
               V <- V_pop * exp(beta_V_lw70 * lw70 + omega_V)
               Cl <- Cl_pop * exp(beta_Cl_hWT * hWT + omega_Cl)
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
                  Cl=.mlxPar(3, 0.3, extra=", covariate=hWT, coefficient=beta_Cl_hWT")),
             params=c(beta_V_lw70=1, beta_Cl_hWT=0.4), indInput=c("lw70", "hWT"),
             content=paste0(.mlxContent, "\nWT = {use=covariate, type=continuous}"),
             covariate="[COVARIATE]
input = WT

EQUATION:
if WT > 90
  hWT = 1
elseif WT > 60
  hWT = 0.5
else
  hWT = 0
end
lw70 = log(WT/70)"))

kitVariant("pkmodel-oral-1cmt", "param-novar-cov",
           "covariates on parameters without random effects (lw70 on ka, SEX on V)",
           tags=c("params", "covariate"),
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
               beta_ka_lw70 <- -0.5; beta_V_SEX_1 <- 0.25
               omega_Cl ~ 0.09
               a <- 0.05; b <- 0.1
             })
             model({
               lw70 <- log(WT / 70)
               ka <- ka_pop * exp(beta_ka_lw70 * lw70)
               V <- V_pop * exp(beta_V_SEX_1 * (SEX == 1))
               Cl <- Cl_pop * exp(omega_Cl)
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           data=.covData,
           columns=c("ID", "TIME", "AMT", "DV", "WT", "SEX"),
           mlxtran=.mlxProject(
             list(ka=.mlxPar(1.2, extra=", covariate=lw70, coefficient=beta_ka_lw70"),
                  V=.mlxPar(30, extra=", covariate=SEX, coefficient={0, beta_V_SEX_1}"),
                  Cl=.mlxPar(3, 0.3)),
             params=c(beta_ka_lw70=-0.5, beta_V_SEX_1=0.25), indInput=c("lw70", "SEX"),
             content=paste0(.mlxContent, "\nWT = {use=covariate, type=continuous}",
                            "\nSEX = {use=covariate, type=categorical}"),
             covariate="[COVARIATE]
input = {WT, SEX}

SEX = {type=categorical, categories={0, 1}}

EQUATION:
lw70 = log(WT/70)",
             indDecl="SEX = {type=categorical, categories={0, 1}}"))

## whether Monolix estimates a model with no random effect is to confirm
kitVariant("pkmodel-oral-1cmt", "param-no-iiv",
           "no random effects at all (every parameter no-variability)",
           tags=c("params"),
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
               a <- 0.05; b <- 0.1
             })
             model({
               ka <- ka_pop
               V <- V_pop
               Cl <- Cl_pop
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           mlxtran=.mlxProject(list(ka=.mlxPar(1.2), V=.mlxPar(30), Cl=.mlxPar(3))))
