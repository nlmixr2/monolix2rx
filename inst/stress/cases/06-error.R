## Residual error models on the oral one-compartment model of 01-pk.R

## .oralProject with another error model and residual parameter values
.errProject <- function(err, input, values) {
  .p <- sub("errorModel=combined1(a, b)", paste0("errorModel=", err), .oralProject, fixed=TRUE)
  .p <- sub("[LONGITUDINAL]\ninput = {a, b}",
            paste0("[LONGITUDINAL]\ninput = {", paste(input, collapse=", "), "}"), .p, fixed=TRUE)
  sub("a = {value=0.05, method=MLE}\nb = {value=0.1, method=MLE}\n",
      paste0(paste0(input, " = {value=", values, ", method=MLE}\n"), collapse=""), .p, fixed=TRUE)
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
           mlxtran=.errProject("constant(a)", "a", 0.2))

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
           mlxtran=.errProject("proportional(b)", "b", 0.15))

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
           mlxtran=.errProject("combined2(a, b)", c("a", "b"), c(0.05, 0.1)))
