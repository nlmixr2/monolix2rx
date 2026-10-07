## Inter-occasion variability

kitVariant("pkmodel-oral-1cmt", "iov-cl-basic",
           "use=occasion; Cl with varlevel={id, id*occ}, two occasions",
           tags=c("iov", "smoke"),
           known="IOV: the id*occ eta is renamed occ2 while the data column is occ, and validation ignores per-occasion parameters",
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
             list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2),
                  Cl=.mlxPar(3, 0.3)),
             content=paste0(.mlxContent, "
EVID = {use=eventidentifier}
OCC = {use=occasion}")) |>
             sub(pattern="Cl = {distribution=logNormal, typical=Cl_pop, sd=omega_Cl}",
                 replacement="Cl = {distribution=logNormal, typical=Cl_pop, varlevel={id, id*occ}, sd={omega_Cl, gamma_Cl}}",
                 fixed=TRUE) |>
             sub(pattern="omega_Cl}\n", replacement="omega_Cl, gamma_Cl}\n", fixed=TRUE) |>
             sub(pattern="omega_Cl = {value=0.3, method=MLE}",
                 replacement="omega_Cl = {value=0.3, method=MLE}\ngamma_Cl = {value=0.2, method=MLE}",
                 fixed=TRUE))
