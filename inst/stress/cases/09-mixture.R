## Mixtures: monolix2rx refuses bsmm()/wsmm(); latent covariates are not translated

kitVariant("pkmodel-oral-1cmt", "bsmm-latent-cov-cl",
           "between-subject mixture as a latent categorical covariate (P(lcat=1)=plcat1) on Cl",
           tags=c("mixture", "bsmm", "smoke"),
           mixest="POP",
           knownRun="IPRED needs Monolix's estimated class per subject (not read yet)",
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3; beta_Cl_lcat_2 <- 1
               omega_ka ~ 0.09; omega_V ~ 0.04; omega_Cl ~ 0.04
               a <- 0.05; b <- 0.1
             })
             model({
               ka <- ka_pop * exp(omega_ka)
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(beta_Cl_lcat_2 * (POP == 2) + omega_Cl)
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           ## POP is the hidden true class (not written)
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxObs(.id, pkTimes(48), cmt=2),
                     cov=mlxCov(nSub, POP=function(n) 1L + stats::rbinom(n, 1, 0.4)))
           },
           mlxtran=.mlxProject(
             list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2),
                  Cl=.mlxPar(3, 0.2, extra=", covariate=lcat, coefficient={0, beta_Cl_lcat_2}")),
             params=c(beta_Cl_lcat_2=1), covParams=c(plcat1=0.6), indInput="lcat",
             indDecl="lcat = {type=categorical, categories={1, 2}}",
             covariate="[COVARIATE]
input = plcat1

DEFINITION:
lcat = {type=categorical, categories={1, 2}, P(lcat=1)=plcat1}"))
