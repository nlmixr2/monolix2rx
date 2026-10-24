## amtDose/tDose before the first dose and with a lag time; an empty() or
## reset() at the same time as a dose

## Monolix: amtDose and tDose are 0 before the first dose
kitVariant("ode-dose-keywords", "ode-dose-keywords-predose",
           "amtDose and tDose before the first dose (both 0) and at a dose time",
           tags=c("ode", "dosing"),
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3; s_pop <- 0.02; kd_pop <- 0.1
               omega_V ~ 0.04; omega_Cl ~ 0.09
               a <- 0.05; b <- 0.1
             })
             model({
               ka <- ka_pop
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(omega_Cl)
               s <- s_pop
               kd <- kd_pop
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Y <- central / V + s * (LDOSE + 20) * exp(-kd * (time - LTIME))
               Y ~ add(a) + prop(b) + combined1()
             })
           },
           data=function(nSub) {
             .id <- seq_len(nSub)
             .d <- .lastDose(mlxBind(mlxDose(.id, 12, amt=100, cmt=1),
                                     mlxDose(.id, 36, amt=50, cmt=1),
                                     mlxObs(.id, c(1, 4, 8, 12, 13, 14, 18, 24, 36, 37, 40, 48, 60), cmt=2)))
             .d$LDOSE[is.na(.d$LDOSE)] <- 0
             .d$LTIME[is.na(.d$LTIME)] <- 0
             .d
           },
           model=local({
             .m <- sub("s*amtDose*", "s*(amtDose + 20)*", .kitEnv$cases[["ode-dose-keywords"]]$model, fixed=TRUE)
             if (!grepl("(amtDose + 20)", .m, fixed=TRUE)) stop("ode-dose-keywords-predose: the base model changed")
             .m
           }))

## Monolix: tDose and amtDose ignore the lag time; rxode2's tlast0() and
## dose0() change at the lagged dose
kitVariant("ode-dose-keywords", "ode-dose-keywords-tlag",
           "amtDose and tDose with depot(Tlag) (Monolix: unaffected by the lag)",
           tags=c("ode", "dosing", "tlag"),
           known="rxode2 updates tlast0()/dose0() at the lagged dose time; Monolix's tDose/amtDose ignore Tlag (warned)",
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3; s_pop <- 0.02; kd_pop <- 0.1; Tlag_pop <- 1
               omega_V ~ 0.04; omega_Cl ~ 0.09
               a <- 0.05; b <- 0.1
             })
             model({
               ka <- ka_pop
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(omega_Cl)
               s <- s_pop
               kd <- kd_pop
               d/dt(depot) <- -ka * depot
               alag(depot) <- Tlag_pop
               d/dt(central) <- ka * depot - Cl / V * central
               Y <- central / V + s * LDOSE * exp(-kd * (time - LTIME))
               Y ~ add(a) + prop(b) + combined1()
             })
           },
           data=function(nSub) {
             .id <- seq_len(nSub)
             .lastDose(mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                               mlxDose(.id, 24, amt=50, cmt=1),
                               mlxObs(.id, c(0.5, 2, 4, 8, 12, 23, 24.5, 26, 30, 36, 48), cmt=2)))
           },
           model=local({
             .m <- .kitEnv$cases[["ode-dose-keywords"]]$model
             .m <- sub("input = {ka, V, Cl, s, kd}", "input = {ka, V, Cl, s, kd, Tlag}", .m, fixed=TRUE)
             .m <- sub("depot(target=Ad)", "depot(target=Ad, Tlag)", .m, fixed=TRUE)
             if (!grepl("Tlag)", .m, fixed=TRUE) || !grepl("kd, Tlag}", .m, fixed=TRUE)) {
               stop("ode-dose-keywords-tlag: the base model changed")
             }
             .m
           }),
           mlxtran=.mlxProject(list(ka=.mlxPar(1.2), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3),
                                    s=.mlxPar(0.02), kd=.mlxPar(0.1), Tlag=.mlxPar(1)),
                               pred="Y"))

## the data lists the reset before the dose at t = 6, so the new dose is
## kept; rxode2 resets first in either order, so only run mode can fail
kitVariant("pkmodel-oral-1cmt", "macro-reset-dose-tie",
           "reset(adm=3) and a dose on lines at the same time (reset first in the data)",
           knownRun="Monolix 2024R1: \"Conflicting reset definition\" for empty()/reset() with compartment() macros",
           tags=c("pk", "macro", "dosing"),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxDose(.id, 6, amt=0, cmt=1, evid=3L),
                     mlxDose(.id, 6, amt=50, cmt=1),
                     mlxObs(.id, c(0.5, 1, 2, 4, 5.5, 6.5, 7, 8, 10, 12, 16, 24, 36), cmt=2))
           },
           write=.macroEventWrite(3L),
           model=.macroOral("reset(adm=3)"),
           mlxtran=.mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3)),
                               content=.macroEventContent))

## the data lists the empty before the bolus at t = 24, so the new bolus
## is kept; rxode2 empties first in either order, so only run mode can fail
kitVariant("macro-empty-tlag", "macro-empty-dose-tie",
           "empty(adm=2, target=Ac) and an iv bolus into Ac on lines at the same time (empty first in the data)",
           tags=c("pk", "macro", "dosing"),
           knownRun="Monolix 2024R1: \"Conflicting reset definition\" for empty()/reset() with compartment() macros",
           sim=function() {
             ini({
               V_pop <- 30; Cl_pop <- 3
               omega_V ~ 0.04; omega_Cl ~ 0.09
               a <- 0.05; b <- 0.1
             })
             model({
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(omega_Cl)
               d/dt(central) <- -Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxDose(.id, 24, amt=0, cmt=1, adm=2L, evid=5L),
                     mlxDose(.id, 24, amt=50, cmt=1),
                     mlxObs(.id, c(0.25, 1, 2, 4, 8, 12, 23, 25, 26, 30, 36, 48), cmt=1))
           },
           model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {V, Cl}

PK:
compartment(cmt=1, amount=Ac)
iv(adm=1, cmt=1)
empty(adm=2, target=Ac)
elimination(cmt=1, k=Cl/V)

EQUATION:
Cc = Ac/V

OUTPUT:
output = Cc
",
           mlxtran=.mlxProject(list(V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3)),
                               content=.macroEventContent))

## acos() and tan(), the trigonometric functions no other case uses
kitVariant("pkmodel-oral-1cmt", "fun-acos-tan",
           "acos() and tan() in EQUATION:",
           tags=c("function"),
           model=.oralFunModel("Y = Cc*(1 + 0.1*acos(0.5) + 0.2*tan(0.3))"),
           sim=.oralErrTruth(quote(add(a) + prop(b) + combined1()),
                             list(quote(a <- 0.05), quote(b <- 0.1)),
                             out=quote(Y),
                             extra=list(quote(Y <- Cc * (1 + 0.1 * acos(0.5) + 0.2 * tan(0.3))))),
           mlxtran=.mlxProject(.oralFunPars, pred="Y"))
