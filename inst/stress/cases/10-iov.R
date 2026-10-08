## Inter-occasion variability

.iovContent <- paste0(.mlxContent, "
EVID = {use=eventidentifier}
OCC = {use=occasion}")

kitVariant("pkmodel-oral-1cmt", "iov-cl-basic",
           "use=occasion; Cl with varlevel={id, id*occ}, two occasions",
           tags=c("iov", "smoke"),
           knownRun="Monolix writes individual parameters per occasion; monolix2rx's validation expects one row per subject (to confirm on Monolix)",
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
               omega_ka ~ 0.09; omega_V ~ 0.04; omega_Cl ~ 0.09
               gamma_Cl ~ 0.04 | OCC
               a <- 0.05; b <- 0.1
             })
             model({
               ka <- ka_pop * exp(omega_ka)
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(omega_Cl + gamma_Cl)
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           ## occasion 2 starts with a washout (EVID=4 dose)
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1, OCC=1L),
                     mlxObs(.id, pkTimes(48), cmt=2, OCC=1L),
                     mlxDose(.id, 100, amt=100, cmt=1, evid=4L, OCC=2L),
                     mlxObs(.id, 100 + pkTimes(48), cmt=2, OCC=2L))
           },
           columns=c("ID", "TIME", "EVID", "AMT", "OCC", "DV"),
           mlxtran=.mlxProject(
             list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3, iov=0.2)),
             content=.iovContent))

.iovKnownRun <- "Monolix writes individual parameters per occasion; monolix2rx's validation expects one row per subject (to confirm on Monolix)"

kitVariant("iov-cl-basic", "iov-only",
           "occasion variability without subject variability on Cl (varlevel=id*occ)",
           tags=c("iov"),
           knownRun=.iovKnownRun,
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
               omega_ka ~ 0.09; omega_V ~ 0.04
               gamma_Cl ~ 0.04 | OCC
               a <- 0.05; b <- 0.1
             })
             model({
               ka <- ka_pop * exp(omega_ka)
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(gamma_Cl)
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           mlxtran=.mlxProject(
             list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, iov=0.2)),
             content=.iovContent))

## occasions 2, 5, 7 with 2, 1 and 3 doses; drug carries over between
## occasions (no reset)
kitVariant("iov-cl-basic", "iov-ka-v-unequal",
           "IOV on ka and V, occasions 2/5/7 of unequal length, no washout between occasions",
           tags=c("iov"),
           knownRun=.iovKnownRun,
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
               omega_ka ~ 0.09; omega_V ~ 0.04; omega_Cl ~ 0.09
               gamma_ka ~ 0.09 | OCC
               gamma_V ~ 0.01 | OCC
               a <- 0.05; b <- 0.1
             })
             model({
               ka <- ka_pop * exp(omega_ka + gamma_ka)
               V <- V_pop * exp(omega_V + gamma_V)
               Cl <- Cl_pop * exp(omega_Cl)
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, c(0, 12), amt=100, cmt=1, OCC=2L),
                     mlxObs(.id, c(1, 2, 4, 8, 13, 14, 16, 20), cmt=2, OCC=2L),
                     mlxDose(.id, 24, amt=100, cmt=1, OCC=5L),
                     mlxObs(.id, c(25, 26, 28, 32, 40), cmt=2, OCC=5L),
                     mlxDose(.id, c(48, 60, 72), amt=100, cmt=1, OCC=7L),
                     mlxObs(.id, c(49, 50, 61, 62, 73, 74, 76, 80, 96), cmt=2, OCC=7L))
           },
           columns=c("ID", "TIME", "AMT", "OCC", "DV"),
           mlxtran=.mlxProject(
             list(ka=.mlxPar(1.2, 0.3, iov=0.3), V=.mlxPar(30, 0.2, iov=0.1),
                  Cl=.mlxPar(3, 0.3)),
             content=paste0(.mlxContent, "\nOCC = {use=occasion}")))

kitVariant("iov-cl-basic", "iov-correlation",
           "correlated occasion effects: correlation = {level=id*occ, r(ka, Cl)}",
           tags=c("iov", "correlation"),
           knownRun=.iovKnownRun,
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
               omega_ka ~ 0.09; omega_V ~ 0.04; omega_Cl ~ 0.09
               gamma_ka + gamma_Cl ~ c(0.09, 0.03, 0.04) | OCC
               a <- 0.05; b <- 0.1
             })
             model({
               ka <- ka_pop * exp(omega_ka + gamma_ka)
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(omega_Cl + gamma_Cl)
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           mlxtran=.mlxProject(
             list(ka=.mlxPar(1.2, 0.3, iov=0.3), V=.mlxPar(30, 0.2),
                  Cl=.mlxPar(3, 0.3, iov=0.2)),
             params=c(corr2_ka_Cl=0.5),
             indExtra="correlation = {level=id*occ, r(ka, Cl)=corr2_ka_Cl}",
             content=.iovContent))
