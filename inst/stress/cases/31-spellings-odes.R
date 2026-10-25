## The camelCase use= spellings, delimiter = semicolumn, probitnormal and
## method=Mle; ODEs defined inside if/else; a negative binomial count

kitVariant("dose-ss", "dose-ss-camelcase",
           "SS = {use=steadyState, nbdoses=10} and II = {use=interDoseInterval}",
           knownRun="Monolix 2024R1 PRED with nbdoses=10 fits 11 doses (10 before the current one) at the final estimates; rxode2 minSS does not reproduce that count (nlmixr2/rxode2#1492)",
           tags=c("dosing", "ss", "syntax"),
           mlxtran=.kitSub("dose-ss", "mlxtran", c("use=steadystate", "use=interdoseinterval"),
                           c("use=steadyState", "use=interDoseInterval")))

kitVariant("dose-evid4-reset", "dose-evid4-camelcase",
           "EVID = {use=eventIdentifier} with an EVID=4 dose",
           tags=c("dosing", "evid", "syntax"),
           mlxtran=.kitSub("dose-evid4-reset", "mlxtran", "use=eventidentifier", "use=eventIdentifier"))

kitVariant("data-mdv", "data-mdv-camelcase",
           "MDV = {use=missingDependentVariable}",
           tags=c("data", "syntax"),
           mlxtran=.kitSub("data-mdv", "mlxtran", "use=missingdependentvariable",
                           "use=missingDependentVariable"))

kitVariant("data-semicolon-ignore", "data-semicolumn",
           "delimiter = semicolumn (the other spelling of semicolon)",
           tags=c("data", "syntax"),
           mlxtran=.kitSub("data-semicolon-ignore", "mlxtran", "delimiter = semicolon\n",
                           "delimiter = semicolumn\n"))

kitVariant("param-probit", "param-probitnormal-lowercase",
           "distribution=probitnormal (lowercase) on the bioavailability",
           tags=c("params", "probit", "syntax"),
           mlxtran=.kitSub("param-probit", "mlxtran", "distribution=probitNormal",
                           "distribution=probitnormal"))

kitVariant("pkmodel-oral-1cmt", "param-method-mle-mixed-case",
           "method=Mle on every parameter (Monolix writes MLE)",
           tags=c("params", "syntax"),
           mlxtran=.kitSub("pkmodel-oral-1cmt", "mlxtran", "method=MLE", "method=Mle"))

## the elimination doubles from t = 12; each branch defines every ODE.
## Monolix 2024R1 documents derivatives in an if/else as not allowed
kitVariant("pkmodel-oral-1cmt", "ode-ddt-in-if",
           "ddt_ equations defined in both branches of an if/else on t",
           tags=c("ode", "syntax"),
           knownRun="Monolix 2024R1 documents ddt_ inside if/else as not allowed (the import accepts it)",
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
               kel <- Cl / V * (1 + (time >= 12))
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - kel * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl}

PK:
depot(target=Ad)

EQUATION:
if t < 12
  ddt_Ad = -ka*Ad
  ddt_Ac = ka*Ad - Cl/V*Ac
else
  ddt_Ad = -ka*Ad
  ddt_Ac = ka*Ad - 2*Cl/V*Ac
end
Cc = Ac/V

OUTPUT:
output = Cc
")

## negative binomial with mean lambda and size 1/alpha, written out; a
## whole size since rxode2 simulates nbinomMu() with the size truncated
kitVariant("disc-count-poisson", "disc-count-negbin",
           "count observation: negative binomial log(P(Y=k)) with gammaln() and an overdispersion parameter",
           tags=c("discrete", "count"),
           sim=function() {
             ini({
               lambda0_pop <- 8; kdecay_pop <- 0.05; alpha_pop <- 0.25
               omega_lambda0 ~ 0.09
             })
             model({
               lambda0 <- lambda0_pop * exp(omega_lambda0)
               kdecay <- kdecay_pop
               alpha <- alpha_pop
               lambda <- lambda0 * exp(-kdecay * time)
               Y ~ nbinomMu(1 / alpha, lambda)
             })
           },
           model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {lambda0, kdecay, alpha}

EQUATION:
lambda = lambda0*exp(-kdecay*t)
r = 1/alpha

DEFINITION:
Y = {type=count, log(P(Y=k)) = gammaln(k + r) - gammaln(r) - factln(k) + r*log(r/(r + lambda)) + k*log(lambda/(r + lambda))}

OUTPUT:
output = Y
",
           mlxtran=.discProject(list(lambda0=.mlxPar(8, 0.3), kdecay=.mlxPar(0.05), alpha=.mlxPar(0.25)), "Y"))
