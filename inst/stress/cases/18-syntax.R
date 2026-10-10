## Less common spellings: comments, estimation methods, parameter names

kitVariant("pkmodel-oral-1cmt", "syntax-comments",
           "; comments on their own lines and after statements in the model file",
           tags=c("syntax"),
           model="DESCRIPTION: {{PROBLEM}}
; one compartment, first-order absorption

[LONGITUDINAL]
input = {ka, V, Cl} ; individual parameters

; the structural model
EQUATION:
Cc = pkmodel(ka, V, Cl) ; central concentration
; Cc = pkmodel(ka, V, Cl=2*Cl)

OUTPUT:
output = Cc ; observed
")

.fixedCheck <- function(name) {
  force(name)
  function(m, sim) {
    .i <- m$iniDf
    if (!isTRUE(.i$fix[which(.i$name == name)])) paste(name, "should be fixed")
  }
}

## the grammar accepts the method in any case
kitVariant("pkmodel-oral-1cmt", "param-fixed-lowercase",
           "population parameter fixed with a lowercase method=fixed",
           tags=c("params", "syntax"),
           dryData=.fixedCheck("V_pop"),
           mlxtran=sub("V_pop = {value=30, method=MLE}", "V_pop = {value=30, method=fixed}",
                       .mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3))),
                       fixed=TRUE))

kitVariant("pkmodel-oral-1cmt", "param-omega-fixed",
           "random effect standard deviation fixed (omega_V method=FIXED)",
           tags=c("params"),
           dryData=.fixedCheck("omega_V"),
           mlxtran=sub("omega_V = {value=0.2, method=MLE}", "omega_V = {value=0.2, method=FIXED}",
                       .mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3))),
                       fixed=TRUE))

## a prior on Cl_pop (MAP); the prior itself is not part of the rxode2 model
kitVariant("pkmodel-oral-1cmt", "param-bayes",
           "population parameter with a prior: method=BAYES and a [POPULATION] DEFINITION",
           tags=c("params", "syntax"),
           mlxtran=sub("[INDIVIDUAL]", "[POPULATION]
DEFINITION:
Cl_pop = {distribution=logNormal, typical=3, sd=0.1}

[INDIVIDUAL]",
                       sub("Cl_pop = {value=3, method=MLE}", "Cl_pop = {value=3, method=BAYES}",
                           .mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3))),
                           fixed=TRUE), fixed=TRUE))

## F is FALSE in R and bioavailability in NONMEM
kitVariant("pkmodel-oral-1cmt", "name-F",
           "bioavailability parameter named F (logitNormal) in depot(p=F)",
           tags=c("syntax", "macro"),
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3; F_pop <- 0.7
               omega_ka ~ 0.09; omega_V ~ 0.04; omega_Cl ~ 0.09; omega_F ~ 0.25
               a <- 0.05; b <- 0.1
             })
             model({
               ka <- ka_pop * exp(omega_ka)
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(omega_Cl)
               F <- expit(logit(F_pop) + omega_F)
               d/dt(depot) <- -ka * depot
               f(depot) <- F
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl, F}

PK:
depot(target=Ad, p=F)

EQUATION:
ddt_Ad = -ka*Ad
ddt_Ac = ka*Ad - Cl/V*Ac
Cc = Ac/V

OUTPUT:
output = Cc
",
           mlxtran=.mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3),
                                    F=.mlxPar(0.7, 0.5, dist="logitNormal"))))

## rxode2 has dose(), time and rate; Monolix keeps them as plain names
kitVariant("pkmodel-oral-1cmt", "name-rxode2-keywords",
           "model variables named dose, rate and time, and a state named ii",
           tags=c("syntax", "ode"),
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
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl}

PK:
depot(target=Ad)

EQUATION:
rate = Cl/V
time = ka
dose = Ad
ddt_Ad = -time*dose
ddt_Ac = time*dose - rate*Ac
Cc = Ac/V
ddt_ii = Cc

OUTPUT:
output = Cc
")

## a PK: block variable is renamed in the macro argument too
kitVariant("pkmodel-oral-1cmt", "name-rxode2-keywords-pk",
           "lag time named dur, set in PK: and passed to depot(Tlag=dur)",
           tags=c("syntax", "macro"),
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
               alag(depot) <- 0.5
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl}

PK:
dur = 0.5
depot(target=Ad, Tlag=dur)

EQUATION:
ddt_Ad = -ka*Ad
ddt_Ac = ka*Ad - Cl/V*Ac
Cc = Ac/V

OUTPUT:
output = Cc
")

## X_0 is the initial condition of the state X, also when X has an underscore
kitVariant("pkmodel-oral-1cmt", "ode-init-underscore",
           "endogenous baseline as the initial condition of a state with an underscore (A_c_0 = 10)",
           tags=c("ode", "syntax"),
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
               central(0) <- 10
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl}

PK:
depot(target=A_d)

EQUATION:
A_c_0 = 10
ddt_A_d = -ka*A_d
ddt_A_c = ka*A_d - Cl/V*A_c
Cc = A_c/V

OUTPUT:
output = Cc
")

## E_0 is a plain variable when there is no ddt_E
kitVariant("pd-imax-direct", "ode-init-not-state",
           "variable named E_0 with no state E (not an initial condition)",
           tags=c("ode", "syntax", "pd"),
           model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl, E0, IC50}

EQUATION:
Cc = pkmodel(ka, V, Cl)
E_0 = E0
E = E_0*(1 - 0.9*Cc/(IC50 + Cc))

OUTPUT:
output = E
")
