## Residual error models across endpoints and edge values

## the parent/metabolite project with one additive parameter for both
kitVariant("two-endpoints-parent-metabolite", "err-shared-param",
           "two endpoints sharing their additive error parameter (combined1(a, b1) and combined1(a, b2))",
           tags=c("endpoints", "error"),
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3; Vm_pop <- 20; Clm_pop <- 2; fm_pop <- 0.6
               omega_V ~ 0.04; omega_Cl ~ 0.09; omega_Clm ~ 0.09
               a <- 0.05; b1 <- 0.1; b2 <- 0.15
             })
             model({
               ka <- ka_pop
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(omega_Cl)
               Vm <- Vm_pop
               Clm <- Clm_pop * exp(omega_Clm)
               fm <- fm_pop
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               d/dt(metab) <- fm * Cl / V * central - Clm / Vm * metab
               Cc <- central / V
               Cm <- metab / Vm
               ## rxode2 refuses one error parameter in two endpoints
               a2 <- a
               Cc ~ add(a) + prop(b1) + combined1()
               Cm ~ add(a2) + prop(b2) + combined1()
             })
           },
           mlxtran=.mlxProject(
             list(ka=.mlxPar(1.2), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3),
                  Vm=.mlxPar(20), Clm=.mlxPar(2, 0.3), fm=.mlxPar(0.6)),
             pred=c(y1="Cc", y2="Cm"),
             err=c("combined1(a, b1)", "combined1(a, b2)"),
             errPar=c(a=0.05, b1=0.1, b2=0.15),
             content="ID = {use=identifier}
TIME = {use=time}
AMT = {use=amount}
DV = {use=observation, name={y1, y2}, yname={'1', '2'}, type={continuous, continuous}}
YTYPE = {use=observationtype}"))

## two assays of the same concentration, each with its own error model
kitVariant("two-endpoints-parent-metabolite", "err-same-pred",
           "two observations of the same prediction (two assays of Cc) with different error models",
           tags=c("endpoints", "error"),
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
               omega_ka ~ 0.09; omega_V ~ 0.04; omega_Cl ~ 0.09
               a1 <- 0.05; b1 <- 0.1; b2 <- 0.2
             })
             model({
               ka <- ka_pop * exp(omega_ka)
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(omega_Cl)
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               C1 <- Cc
               C2 <- Cc
               C1 ~ add(a1) + prop(b1) + combined1()
               C2 ~ prop(b2)
             })
           },
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1, DVID=1L),
                     mlxObs(.id, pkTimes(48), cmt=3, DVID=1L),
                     mlxObs(.id, c(1, 4, 12, 24), cmt=4, DVID=2L))
           },
           model=.kitEnv$cases[["pkmodel-oral-1cmt"]]$model,
           mlxtran=.mlxProject(
             list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3)),
             pred=c(y1="Cc", y2="Cc"),
             err=c("combined1(a1, b1)", "proportional(b2)"),
             errPar=c(a1=0.05, b1=0.1, b2=0.2),
             content="ID = {use=identifier}
TIME = {use=time}
AMT = {use=amount}
DV = {use=observation, name={y1, y2}, yname={'1', '2'}, type={continuous, continuous}}
YTYPE = {use=observationtype}"))

## a combined1 error whose additive part is fixed at 0
kitVariant("pkmodel-oral-1cmt", "err-additive-fixed-zero",
           "combined1(a, b) with a fixed at 0 (proportional in effect)",
           tags=c("error"),
           sim=.oralErrTruth(quote(add(a) + prop(b) + combined1()),
                             list(quote(a <- fix(0)), quote(b <- 0.1))),
           mlxtran=local({
             .m <- sub("a = {value=0, method=MLE}", "a = {value=0, method=FIXED}",
                       .mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3)),
                                   errPar=c(a=0, b=0.1)), fixed=TRUE)
             if (!grepl("a = {value=0, method=FIXED}", .m, fixed=TRUE)) {
               stop("err-additive-fixed-zero: the project template changed")
             }
             .m
           }))

## residuals autocorrelated in time: the predictions do not change, only
## the residuals (and the likelihood)
kitVariant("pkmodel-oral-1cmt", "err-autocorr",
           "autocorrelated residuals (autoCorrCoef=r in [LONGITUDINAL] DEFINITION:)",
           tags=c("error"),
           mlxtran=local({
             .m <- .mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3)),
                               params=c(r=0.5))
             .m <- sub("errorModel=combined1(a, b)}", "errorModel=combined1(a, b), autoCorrCoef=r}", .m, fixed=TRUE)
             .m <- sub("input = {a, b}", "input = {a, b, r}", .m, fixed=TRUE)
             .m <- sub("input = {ka_pop, omega_ka, V_pop, omega_V, Cl_pop, omega_Cl, r}",
                       "input = {ka_pop, omega_ka, V_pop, omega_V, Cl_pop, omega_Cl}", .m, fixed=TRUE)
             if (!grepl("autoCorrCoef=r", .m, fixed=TRUE) || !grepl("input = {a, b, r}", .m, fixed=TRUE) ||
                   grepl("omega_Cl, r}", .m, fixed=TRUE)) {
               stop("err-autocorr: the project template changed")
             }
             .m
           }))
