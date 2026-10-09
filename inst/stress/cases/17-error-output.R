## More residual error models (06-error.R has the common ones) and outputs

kitVariant("pkmodel-oral-1cmt", "err-custom-names",
           "combined1(a_CONC, b_CONC): error parameters not named a and b",
           tags=c("error"),
           sim=.oralErrTruth(quote(add(a_CONC) + prop(b_CONC) + combined1()),
                             list(quote(a_CONC <- 0.05), quote(b_CONC <- 0.1))),
           mlxtran=.mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3)),
                               err="combined1(a_CONC, b_CONC)", errPar=c(a_CONC=0.05, b_CONC=0.1)))

kitVariant("pkmodel-oral-1cmt", "err-fixed",
           "combined2(a, b) with a fixed (method=FIXED)",
           tags=c("error"),
           sim=.oralErrTruth(quote(add(a) + prop(b) + combined2()),
                             list(quote(a <- fix(0.05)), quote(b <- 0.1))),
           ## only a: est="fixed" fixes b too
           dryData=function(m, sim) {
             .i <- m$iniDf
             if (!isTRUE(.i$fix[.i$name == "a"])) "a should be fixed"
           },
           mlxtran=sub("a = {value=0.05, method=MLE}", "a = {value=0.05, method=FIXED}",
                       .mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3)),
                                   err="combined2(a, b)", errPar=c(a=0.05, b=0.1)), fixed=TRUE))

## a percent occupancy observed on (0, 100)
kitVariant("pkmodel-oral-1cmt", "err-logitnormal-percent",
           "logitNormal observation on (0, 100) (min=0, max=100) with constant(a) error",
           tags=c("error", "logitnormal"),
           ## the bounds change only the likelihood, and dryLik needs a
           ## non-normal endpoint, so they are checked directly
           dryData=function(m, sim) {
             .p <- m$predDf
             if (!identical(c(.p$trLow, .p$trHi), c(0, 100))) "the logitNormal bounds are not 0 and 100"
           },
           sim=.oralErrTruth(quote(logitNorm(a, 0, 100)), list(quote(a <- 0.3), quote(EC50_pop <- 1)),
                             out=quote(Occ), extra=list(quote(Occ <- 100 * Cc / (Cc + EC50_pop)))),
           model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl, EC50}

EQUATION:
Cc = pkmodel(ka, V, Cl)
Occ = 100*Cc/(Cc + EC50)

OUTPUT:
output = Occ
",
           mlxtran=.mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3),
                                    EC50=.mlxPar(1)),
                               err="constant(a)", errPar=c(a=0.3), pred="Occ",
                               obsDist="logitNormal", obsExtra=", min=0, max=100"))

## table= lists extra outputs; only output= is observed
kitVariant("pkmodel-oral-1cmt", "out-table",
           "OUTPUT: table = {AUC, Cl} next to output = Cc, with AUC an ODE state",
           tags=c("output", "ode"),
           model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl}

PK:
depot(target=Ad)

EQUATION:
ddt_Ad = -ka*Ad
ddt_Ac = ka*Ad - Cl/V*Ac
Cc = Ac/V
ddt_AUC = Cc

OUTPUT:
output = Cc
table = {AUC, Cl}
",
           dryData=function(m, sim) {
             if (nrow(m$predDf) != 1L) "table= items became endpoints"
           })

kitVariant("pkmodel-oral-1cmt", "pd-imax-direct",
           "only a direct Imax effect observed (prediction=E from pkmodel concentrations)",
           tags=c("error", "pd"),
           sim=.oralErrTruth(quote(add(a)),
                             list(quote(E0_pop <- 100), quote(IC50_pop <- 1.5), quote(omega_E0 ~ 0.01),
                                  quote(a <- 3)),
                             out=quote(E),
                             extra=list(quote(E0 <- E0_pop * exp(omega_E0)),
                                        quote(E <- E0 * (1 - 0.9 * Cc / (IC50_pop + Cc))))),
           model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl, E0, IC50}

EQUATION:
Cc = pkmodel(ka, V, Cl)
E = E0*(1 - 0.9*Cc/(IC50 + Cc))

OUTPUT:
output = E
",
           mlxtran=.mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3),
                                    E0=.mlxPar(100, 0.1), IC50=.mlxPar(1.5)),
                               err="constant(a)", errPar=c(a=3), pred="E"))
