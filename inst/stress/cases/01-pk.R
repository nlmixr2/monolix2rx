## PK macros, pkmodel() and library models

.oralTruth <- function() {
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
}

.oralData <- function(nSub) {
  .id <- seq_len(nSub)
  mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
          mlxObs(.id, pkTimes(48), cmt=2))
}

## one endpoint CONC with a combined1 error; Monolix sd = sqrt(omega)
.oralProject <- "<DATAFILE>

[FILEINFO]
file = '{{DATA}}'
delimiter = comma
header = {{{HEADER}}}

[CONTENT]
ID = {use=identifier}
TIME = {use=time}
AMT = {use=amount}
DV = {use=observation, name=CONC, type=continuous}

<MODEL>

[INDIVIDUAL]
input = {ka_pop, omega_ka, V_pop, omega_V, Cl_pop, omega_Cl}

DEFINITION:
ka = {distribution=logNormal, typical=ka_pop, sd=omega_ka}
V = {distribution=logNormal, typical=V_pop, sd=omega_V}
Cl = {distribution=logNormal, typical=Cl_pop, sd=omega_Cl}

[LONGITUDINAL]
input = {a, b}

file = '{{MODEL}}'

DEFINITION:
CONC = {distribution=normal, prediction=Cc, errorModel=combined1(a, b)}

<FIT>
data = CONC
model = CONC

<PARAMETER>
ka_pop = {value=1.2, method=MLE}
V_pop = {value=30, method=MLE}
Cl_pop = {value=3, method=MLE}
omega_ka = {value=0.3, method=MLE}
omega_V = {value=0.2, method=MLE}
omega_Cl = {value=0.3, method=MLE}
a = {value=0.05, method=MLE}
b = {value=0.1, method=MLE}

<MONOLIX>

{{TASKS}}

{{SETTINGS}}
"

kitCase(
  name="pkmodel-oral-1cmt",
  covers="pkmodel(ka, V, Cl) one-compartment oral, logNormal parameters, combined1 error",
  tags=c("pk", "pkmodel", "smoke"),
  sim=.oralTruth,
  data=.oralData,
  model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl}

EQUATION:
Cc = pkmodel(ka, V, Cl)

OUTPUT:
output = Cc
",
  mlxtran=.oralProject)

kitVariant("pkmodel-oral-1cmt", "ode-oral-1cmt",
           "the same one-compartment oral model written as ddt_ equations with a depot() macro",
           tags=c("ode", "macro", "smoke"),
           model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl}

PK:
depot(target=Ad)

EQUATION:
ddt_Ad = -ka*Ad
ddt_Ac = ka*Ad - Cl/V*Ac
Cc = Ac/V

OUTPUT:
output = Cc
")
