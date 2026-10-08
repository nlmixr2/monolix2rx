## Mixtures: latent covariates and bsmm() become mix(), wsmm() the weighted prediction

kitVariant("pkmodel-oral-1cmt", "bsmm-latent-cov-cl",
           "between-subject mixture as a latent categorical covariate (P(lcat=1)=plcat1) on Cl",
           tags=c("mixture", "bsmm", "smoke"),
           mixest="POP",
           knownRun="IPRED needs Monolix's estimated class per subject (not read yet)",
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3; beta_Cl_lcat_2 <- 1
               omega_ka ~ 0.09; omega_V ~ 0.04; omega_Cl ~ 0.04
               a <- 0.05; b <- 0.1
             })
             model({
               ka <- ka_pop * exp(omega_ka)
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(beta_Cl_lcat_2 * (POP == 2) + omega_Cl)
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           ## POP is the hidden true class (not written)
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxObs(.id, pkTimes(48), cmt=2),
                     cov=mlxCov(nSub, POP=function(n) 1L + stats::rbinom(n, 1, 0.4)))
           },
           mlxtran=.mlxProject(
             list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2),
                  Cl=.mlxPar(3, 0.2, extra=", covariate=lcat, coefficient={0, beta_Cl_lcat_2}")),
             params=c(beta_Cl_lcat_2=1), covParams=c(plcat1=0.6), indInput="lcat",
             indDecl="lcat = {type=categorical, categories={1, 2}}",
             covariate="[COVARIATE]
input = plcat1

DEFINITION:
lcat = {type=categorical, categories={1, 2}, P(lcat=1)=plcat1}"))

## each group is its own one-compartment IV model: one dose goes to both
## (two depot() macros on adm 1); the truth doses both states
.bsmmData <- function(nSub, groups=2L, prob=c(0.4, 0.6)) {
  .id <- seq_len(nSub)
  mlxBind(do.call(mlxBind, lapply(seq_len(groups), function(g) mlxDose(.id, 0, amt=100, cmt=g))),
          mlxObs(.id, c(0.5, 1, 2, 4, 6, 8, 12, 24, 36), cmt=1),
          cov=mlxCov(nSub, POP=function(n) sample.int(groups, n, replace=TRUE, prob=prob)))
}

## Monolix sees one dose per time
.bsmmWrite <- function(d) {
  d <- .kitMonolixRows(d)
  d <- d[!(d$EVID == 1L & d$CMT != 1L), ]
  d[, c("ID", "TIME", "AMT", "DV")]
}

kitCase(
  name="bsmm-structural",
  covers="Cc = bsmm(C1, p1, C2, 1-p1) with p1 a logitNormal parameter without variability",
  tags=c("mixture", "bsmm"),
  mixest="POP",
  knownRun="IPRED needs Monolix's estimated class per subject (not read yet)",
  sim=function() {
    ini({
      V_pop <- 30; Cl1_pop <- 1; Cl2_pop <- 5
      omega_V ~ 0.04; omega_Cl1 ~ 0.09; omega_Cl2 ~ 0.09
      a <- 0.05; b <- 0.1
    })
    model({
      V <- V_pop * exp(omega_V)
      Cl1 <- Cl1_pop * exp(omega_Cl1)
      Cl2 <- Cl2_pop * exp(omega_Cl2)
      d/dt(A1) <- -Cl1 / V * A1
      d/dt(A2) <- -Cl2 / V * A2
      Cc <- (POP == 1) * A1 / V + (POP == 2) * A2 / V
      Cc ~ add(a) + prop(b) + combined1()
    })
  },
  data=function(nSub) .bsmmData(nSub),
  write=.bsmmWrite,
  model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {V, Cl1, Cl2, p1}

PK:
depot(adm=1, target=A1)
depot(adm=1, target=A2)

EQUATION:
ddt_A1 = -Cl1/V*A1
ddt_A2 = -Cl2/V*A2
C1 = A1/V
C2 = A2/V
Cc = bsmm(C1, p1, C2, 1-p1)

OUTPUT:
output = Cc
",
  mlxtran=.mlxProject(list(V=.mlxPar(30, 0.2), Cl1=.mlxPar(1, 0.3), Cl2=.mlxPar(5, 0.3),
                           p1=.mlxPar(0.4, dist="logitNormal"))))

kitCase(
  name="bsmm-3groups",
  covers="bsmm(C1, p1, C2, p2, C3, 1-p1-p2): three groups",
  tags=c("mixture", "bsmm"),
  mixest="POP",
  knownRun="IPRED needs Monolix's estimated class per subject (not read yet)",
  sim=function() {
    ini({
      V_pop <- 30; Cl1_pop <- 1; Cl2_pop <- 3; Cl3_pop <- 8
      omega_V ~ 0.04
      a <- 0.05; b <- 0.1
    })
    model({
      V <- V_pop * exp(omega_V)
      d/dt(A1) <- -Cl1_pop / V * A1
      d/dt(A2) <- -Cl2_pop / V * A2
      d/dt(A3) <- -Cl3_pop / V * A3
      Cc <- (POP == 1) * A1 / V + (POP == 2) * A2 / V + (POP == 3) * A3 / V
      Cc ~ add(a) + prop(b) + combined1()
    })
  },
  data=function(nSub) .bsmmData(nSub, 3L, c(0.3, 0.3, 0.4)),
  write=.bsmmWrite,
  model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {V, Cl1, Cl2, Cl3, p1, p2}

PK:
depot(adm=1, target=A1)
depot(adm=1, target=A2)
depot(adm=1, target=A3)

EQUATION:
ddt_A1 = -Cl1/V*A1
ddt_A2 = -Cl2/V*A2
ddt_A3 = -Cl3/V*A3
Cc = bsmm(A1/V, p1, A2/V, p2, A3/V, 1-p1-p2)

OUTPUT:
output = Cc
",
  mlxtran=.mlxProject(list(V=.mlxPar(30, 0.2), Cl1=.mlxPar(1), Cl2=.mlxPar(3), Cl3=.mlxPar(8),
                           p1=.mlxPar(0.3, dist="logitNormal"),
                           p2=.mlxPar(0.3, dist="logitNormal"))))

## the proportion varies by subject: no mixest, the prediction is the
## weighted sum
kitCase(
  name="wsmm-two-pred",
  covers="Cc = wsmm(C1, p1, C2, 1-p1) with a logitNormal p1 that has an eta",
  tags=c("mixture", "wsmm"),
  sim=function() {
    ini({
      V_pop <- 30; Cl1_pop <- 1; Cl2_pop <- 5; p1_pop <- 0.4
      omega_V ~ 0.04; omega_p1 ~ 0.25
      a <- 0.05; b <- 0.1
    })
    model({
      V <- V_pop * exp(omega_V)
      p1 <- expit(logit(p1_pop) + omega_p1)
      d/dt(A1) <- -Cl1_pop / V * A1
      d/dt(A2) <- -Cl2_pop / V * A2
      Cc <- p1 * A1 / V + (1 - p1) * A2 / V
      Cc ~ add(a) + prop(b) + combined1()
    })
  },
  data=function(nSub) .bsmmData(nSub),
  write=.bsmmWrite,
  model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {V, Cl1, Cl2, p1}

PK:
depot(adm=1, target=A1)
depot(adm=1, target=A2)

EQUATION:
ddt_A1 = -Cl1/V*A1
ddt_A2 = -Cl2/V*A2
Cc = wsmm(A1/V, p1, A2/V, 1-p1)

OUTPUT:
output = Cc
",
  mlxtran=.mlxProject(list(V=.mlxPar(30, 0.2), Cl1=.mlxPar(1), Cl2=.mlxPar(5),
                           p1=.mlxPar(0.4, 0.5, dist="logitNormal"))))

kitVariant("bsmm-structural", "bsmm-p-iiv",
           "bsmm() probability with between-subject variability",
           known="rxode2 mix() needs population probabilities; a bsmm() probability with an eta is refused",
           mlxtran=.mlxProject(list(V=.mlxPar(30, 0.2), Cl1=.mlxPar(1, 0.3), Cl2=.mlxPar(5, 0.3),
                                    p1=.mlxPar(0.4, 0.5, dist="logitNormal"))))
