## Residual error models on the oral one-compartment model of 01-pk.R

.errProject <- function(err, errPar) {
  .mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3)),
              err=err, errPar=errPar)
}

kitVariant("pkmodel-oral-1cmt", "err-constant", "constant(a) error",
           tags=c("error"),
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
               omega_ka ~ 0.09; omega_V ~ 0.04; omega_Cl ~ 0.09
               a <- 0.2
             })
             model({
               ka <- ka_pop * exp(omega_ka)
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(omega_Cl)
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a)
             })
           },
           mlxtran=.errProject("constant(a)", c(a=0.2)))

kitVariant("pkmodel-oral-1cmt", "err-proportional", "proportional(b) error",
           tags=c("error"),
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
               omega_ka ~ 0.09; omega_V ~ 0.04; omega_Cl ~ 0.09
               b <- 0.15
             })
             model({
               ka <- ka_pop * exp(omega_ka)
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(omega_Cl)
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ prop(b)
             })
           },
           mlxtran=.errProject("proportional(b)", c(b=0.15)))

kitVariant("pkmodel-oral-1cmt", "err-combined2", "combined2(a, b) error",
           tags=c("error"),
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
               omega_ka ~ 0.09; omega_V ~ 0.04; omega_Cl ~ 0.09
               a <- 0.05; b <- 0.1
             })
             model({
               ka <- ka_pop * exp(omega_ka)
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(omega_Cl)
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined2()
             })
           },
           mlxtran=.errProject("combined2(a, b)", c(a=0.05, b=0.1)))

## the oral truth with another residual error (`err`: the ~ right side;
## `pars`: its ini() assignments)
.oralErrTruth <- function(err, pars, out=quote(Cc), extra=NULL) {
  eval(bquote(function() {
    ini(.(as.call(c(list(as.name("{")),
                    list(quote(ka_pop <- 1.2), quote(V_pop <- 30), quote(Cl_pop <- 3),
                         quote(omega_ka ~ 0.09), quote(omega_V ~ 0.04),
                         quote(omega_Cl ~ 0.09)),
                    pars))))
    model(.(as.call(c(list(as.name("{")),
                      list(quote(ka <- ka_pop * exp(omega_ka)),
                           quote(V <- V_pop * exp(omega_V)),
                           quote(Cl <- Cl_pop * exp(omega_Cl)),
                           quote(d/dt(depot) <- -ka * depot),
                           quote(d/dt(central) <- ka * depot - Cl / V * central),
                           quote(Cc <- central / V)),
                      extra,
                      list(bquote(.(out) ~ .(err)))))))
  }))
}

kitVariant("pkmodel-oral-1cmt", "err-lognormal", "logNormal observation with constant(a) (exponential) error",
           tags=c("error", "lognormal"),
           sim=.oralErrTruth(quote(lnorm(a)), list(quote(a <- 0.2))),
           mlxtran=.mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3)),
                               err="constant(a)", errPar=c(a=0.2), obsDist="logNormal"))

kitVariant("pkmodel-oral-1cmt", "err-combined1c", "combined1c(a, b, c): a + b*f^c",
           tags=c("error"),
           sim=.oralErrTruth(quote(add(a) + pow(b, c) + combined1()),
                             list(quote(a <- 0.05), quote(b <- 0.1), quote(c <- 0.8))),
           mlxtran=.mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3)),
                               err="combined1c(a, b, c)", errPar=c(a=0.05, b=0.1, c=0.8)))

kitVariant("pkmodel-oral-1cmt", "err-combined2c", "combined2c(a, b, c): sqrt(a^2 + (b*f^c)^2)",
           tags=c("error"),
           sim=.oralErrTruth(quote(add(a) + pow(b, c) + combined2()),
                             list(quote(a <- 0.05), quote(b <- 0.1), quote(c <- 0.8))),
           mlxtran=.mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3)),
                               err="combined2c(a, b, c)", errPar=c(a=0.05, b=0.1, c=0.8)))

## an occupancy-like fraction observed on (0, 1)
kitVariant("pkmodel-oral-1cmt", "err-logitnormal", "logitNormal observation on (0, 1) with constant(a) error",
           tags=c("error", "logitnormal"),
           sim=.oralErrTruth(quote(logitNorm(a, 0, 1)), list(quote(a <- 0.3), quote(EC50_pop <- 1)),
                             out=quote(Fr), extra=list(quote(Fr <- Cc / (Cc + EC50_pop)))),
           model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl, EC50}

EQUATION:
Cc = pkmodel(ka, V, Cl)
Fr = Cc/(Cc + EC50)

OUTPUT:
output = Fr
",
           mlxtran=.mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3),
                                    EC50=.mlxPar(1)),
                               err="constant(a)", errPar=c(a=0.3), pred="Fr",
                               obsDist="logitNormal", obsExtra=", min=0, max=1"))
