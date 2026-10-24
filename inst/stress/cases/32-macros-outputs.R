## type= for adm= in PK macros, a bolus into a peripheral compartment, a
## lowercase observation distribution, an amount (not a concentration)
## observed

kitVariant("macro-oral-iv-2cmt", "macro-type-alias",
           "oral(type=1, ...) and iv(type=2, ...): type= instead of adm=",
           tags=c("pk", "macro", "adm", "syntax"),
           model=.kitSub("macro-oral-iv-2cmt", "model", c("oral(adm=1,", "iv(adm=2,"),
                         c("oral(type=1,", "iv(type=2,")))

## the second administration is a bolus into the peripheral compartment
kitVariant("macro-oral-iv-2cmt", "macro-iv-peripheral",
           "iv(adm=2, cmt=2) into the compartment a peripheral() macro defines",
           knownRun="Monolix 2024R1: Cannot find the compartment 2 (iv() before peripheral()); see macro-iv-peripheral-after",
           tags=c("pk", "macro", "adm"),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1, adm=1),
                     mlxDose(.id, 48, amt=50, cmt=3, adm=2),
                     mlxObs(.id, c(pkTimes(48), 48.25, 49, 52, 60, 72, 96), cmt=2))
           },
           model=.kitSub("macro-oral-iv-2cmt", "model", "iv(adm=2, cmt=1)", "iv(adm=2, cmt=2)"))

kitVariant("err-lognormal", "err-lognormal-lowercase",
           "distribution=lognormal (lowercase) in the observation model",
           tags=c("error", "lognormal", "syntax"),
           mlxtran=.kitSub("err-lognormal", "mlxtran", "{distribution=logNormal, prediction=",
                           "{distribution=lognormal, prediction="))

## the cumulative amount excreted in urine is an ODE state observed as is
kitVariant("two-endpoints-parent-metabolite", "two-endpoints-urine-amount",
           "plasma concentration and the cumulative urine amount (an ODE state) as two observations",
           tags=c("endpoints", "error"),
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3; fe_pop <- 0.4
               omega_V ~ 0.04; omega_Cl ~ 0.09
               a1 <- 0.05; b1 <- 0.1; a2 <- 0.5; b2 <- 0.1
             })
             model({
               ka <- ka_pop
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(omega_Cl)
               fe <- fe_pop
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               d/dt(urine) <- fe * Cl / V * central
               Cc <- central / V
               Ae <- urine
               Cc ~ add(a1) + prop(b1) + combined1()
               Ae ~ add(a2) + prop(b2) + combined1()
             })
           },
           model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl, fe}

PK:
depot(target=Ac, ka)

EQUATION:
ddt_Ac = -Cl/V*Ac
ddt_Ae = fe*Cl/V*Ac
Cc = Ac/V

OUTPUT:
output = {Cc, Ae}
",
           mlxtran=.mlxProject(list(ka=.mlxPar(1.2), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3),
                                    fe=.mlxPar(0.4)),
                               pred=c(y1="Cc", y2="Ae"),
                               err=c("combined1(a1, b1)", "combined1(a2, b2)"),
                               errPar=c(a1=0.05, b1=0.1, a2=0.5, b2=0.1),
                               content="ID = {use=identifier}
TIME = {use=time}
AMT = {use=amount}
DV = {use=observation, name={y1, y2}, yname={'1', '2'}, type={continuous, continuous}}
YTYPE = {use=observationtype}"))
