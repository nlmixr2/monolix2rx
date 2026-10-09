## Covariate models beyond 05-params.R: grouped categories, several
## covariates on one parameter, untransformed covariates, and covariates
## on normal and logitNormal parameters.

## the oral truth with covariate coefficients `beta` (named values)
.covOral <- function(cl, v="V_pop * exp(omega_V)", beta=NULL) {
  .beta <- lapply(names(beta), function(n) call("<-", as.name(n), beta[[n]]))
  .ini <- as.call(c(list(as.name("{")),
                    list(quote(ka_pop <- 1.2), quote(V_pop <- 30), quote(Cl_pop <- 3)),
                    .beta,
                    list(quote(omega_ka ~ 0.09), quote(omega_V ~ 0.04), quote(omega_Cl ~ 0.09),
                         quote(a <- 0.05), quote(b <- 0.1))))
  eval(bquote(function() {
    ini(.(.ini))
    model({
      ka <- ka_pop * exp(omega_ka)
      V <- .(str2lang(v))
      Cl <- .(str2lang(cl))
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - Cl / V * central
      Cc <- central / V
      Cc ~ add(a) + prop(b) + combined1()
    })
  }))
}

kitVariant("pkmodel-oral-1cmt", "cov-transform-group",
           "categorical covariate grouped in [COVARIATE] DEFINITION: (transform=RACE, categories={A={1, 2}, B=3, C=4}, reference=A) on Cl",
           tags=c("covariate", "categorical"),
           known=paste("rxode2 5.1.8 numbers the string literals of comparisons apart from the",
                       "assigned strings, so tRACE == \"B\" is true for tRACE <- \"A\" (the B, C",
                       "coefficients compare against A, B; nlmixr2/rxode2#1456)"),
           sim=.covOral("Cl_pop * exp(beta_Cl_tRACE_B * (RACE == 3) + beta_Cl_tRACE_C * (RACE == 4) + omega_Cl)",
                        beta=c(beta_Cl_tRACE_B=-0.3, beta_Cl_tRACE_C=0.25)),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxObs(.id, pkTimes(48), cmt=2),
                     cov=mlxCov(nSub, RACE=function(n) rep_len(c(1L, 2L, 3L, 4L, 3L), n)))
           },
           columns=c("ID", "TIME", "AMT", "DV", "RACE"),
           mlxtran=.mlxProject(
             list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2),
                  Cl=.mlxPar(3, 0.3, extra=", covariate=tRACE, coefficient={0, beta_Cl_tRACE_B, beta_Cl_tRACE_C}")),
             params=c(beta_Cl_tRACE_B=-0.3, beta_Cl_tRACE_C=0.25), indInput="tRACE",
             content=paste0(.mlxContent, "\nRACE = {use=covariate, type=categorical}"),
             covariate="[COVARIATE]
input = RACE

RACE = {type=categorical, categories={1, 2, 3, 4}}

DEFINITION:
tRACE =
{
  transform = RACE,
  categories = {
  A = {1, 2},
  B = {3},
  C = {4}  },
  reference = A
}",
             indDecl="tRACE = {type=categorical, categories={A, B, C}}"))

## numeric labels compared unquoted: rxode2 reads them as level numbers,
## which match while the labels are 1, 2, ... in their assigned order
kitVariant("pkmodel-oral-1cmt", "cov-transform-numeric",
           "categorical transform with numeric labels (categories={'1'={1, 2}, '2'={3}, '3'={4}}, reference='1') on Cl",
           tags=c("covariate", "categorical"),
           sim=.covOral("Cl_pop * exp(beta_Cl_tRACE_2 * (RACE == 3) + beta_Cl_tRACE_3 * (RACE == 4) + omega_Cl)",
                        beta=c(beta_Cl_tRACE_2=-0.3, beta_Cl_tRACE_3=0.25)),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxObs(.id, pkTimes(48), cmt=2),
                     cov=mlxCov(nSub, RACE=function(n) rep_len(c(1L, 2L, 3L, 4L, 3L), n)))
           },
           columns=c("ID", "TIME", "AMT", "DV", "RACE"),
           mlxtran=.mlxProject(
             list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2),
                  Cl=.mlxPar(3, 0.3, extra=", covariate=tRACE, coefficient={0, beta_Cl_tRACE_2, beta_Cl_tRACE_3}")),
             params=c(beta_Cl_tRACE_2=-0.3, beta_Cl_tRACE_3=0.25), indInput="tRACE",
             content=paste0(.mlxContent, "\nRACE = {use=covariate, type=categorical}"),
             covariate="[COVARIATE]
input = RACE

RACE = {type=categorical, categories={1, 2, 3, 4}}

DEFINITION:
tRACE =
{
  transform = RACE,
  categories = {
  '1' = {1, 2},
  '2' = {3},
  '3' = {4}  },
  reference = '1'
}",
             indDecl="tRACE = {type=categorical, categories={'1', '2', '3'}}"))

kitVariant("pkmodel-oral-1cmt", "cov-multi",
           "two covariates on Cl (covariate={lw70, SEX}, coefficient={beta, {0, beta}}) and one on V",
           tags=c("covariate", "categorical"),
           sim=.covOral("Cl_pop * exp(beta_Cl_lw70 * log(WT / 70) + beta_Cl_SEX_1 * (SEX == 1) + omega_Cl)",
                        v="V_pop * exp(beta_V_lw70 * log(WT / 70) + omega_V)",
                        beta=c(beta_Cl_lw70=0.75, beta_Cl_SEX_1=-0.4, beta_V_lw70=1)),
           data=.covData,
           columns=c("ID", "TIME", "AMT", "DV", "WT", "SEX"),
           mlxtran=.mlxProject(
             list(ka=.mlxPar(1.2, 0.3),
                  V=.mlxPar(30, 0.2, extra=", covariate=lw70, coefficient=beta_V_lw70"),
                  Cl=.mlxPar(3, 0.3, extra=", covariate={lw70, SEX}, coefficient={beta_Cl_lw70, {0, beta_Cl_SEX_1}}")),
             params=c(beta_Cl_lw70=0.75, beta_Cl_SEX_1=-0.4, beta_V_lw70=1),
             indInput=c("lw70", "SEX"),
             content=paste0(.mlxContent, "\nWT = {use=covariate, type=continuous}",
                            "\nSEX = {use=covariate, type=categorical}"),
             covariate="[COVARIATE]
input = {WT, SEX}

SEX = {type=categorical, categories={0, 1}}

EQUATION:
lw70 = log(WT/70)",
             indDecl="SEX = {type=categorical, categories={0, 1}}"))

kitVariant("pkmodel-oral-1cmt", "cov-untransformed",
           "continuous covariate used as is (covariate=WT): log(Cl) is linear in WT",
           tags=c("covariate"),
           sim=.covOral("Cl_pop * exp(beta_Cl_WT * WT + omega_Cl)", beta=c(beta_Cl_WT=0.01)),
           data=.covData,
           columns=c("ID", "TIME", "AMT", "DV", "WT"),
           mlxtran=.mlxProject(
             list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2),
                  Cl=.mlxPar(3, 0.3, extra=", covariate=WT, coefficient=beta_Cl_WT")),
             params=c(beta_Cl_WT=0.01), indInput="WT",
             content=paste0(.mlxContent, "\nWT = {use=covariate, type=continuous}"),
             covariate="[COVARIATE]
input = WT"))

kitVariant("pkmodel-oral-1cmt", "cov-normal-param",
           "covariate on a normal parameter: V = V_pop + beta*WT + eta (additive)",
           tags=c("covariate", "params"),
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 10; Cl_pop <- 3; beta_V_WT <- 0.25
               omega_ka ~ 0.09; omega_V ~ 4; omega_Cl ~ 0.09
               a <- 0.05; b <- 0.1
             })
             model({
               ka <- ka_pop * exp(omega_ka)
               V <- V_pop + beta_V_WT * WT + omega_V
               Cl <- Cl_pop * exp(omega_Cl)
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           data=.covData,
           columns=c("ID", "TIME", "AMT", "DV", "WT"),
           mlxtran=.mlxProject(
             list(ka=.mlxPar(1.2, 0.3),
                  V=.mlxPar(10, 2, dist="normal", extra=", covariate=WT, coefficient=beta_V_WT"),
                  Cl=.mlxPar(3, 0.3)),
             params=c(beta_V_WT=0.25), indInput="WT",
             content=paste0(.mlxContent, "\nWT = {use=covariate, type=continuous}"),
             covariate="[COVARIATE]
input = WT"))

kitVariant("pkmodel-oral-1cmt", "cov-logit-param",
           "categorical covariate (FOOD) on a logitNormal bioavailability: logit(p) = logit(p_pop) + beta + eta",
           tags=c("covariate", "categorical", "params"),
           sim=function() {
             ini({
               ka_pop <- 1.2; p_pop <- 0.6; V_pop <- 30; Cl_pop <- 3; beta_p_FOOD_1 <- 0.8
               omega_ka ~ 0.09; omega_p ~ 0.25; omega_V ~ 0.04; omega_Cl ~ 0.09
               a <- 0.05; b <- 0.1
             })
             model({
               ka <- ka_pop * exp(omega_ka)
               p <- expit(logit(p_pop) + beta_p_FOOD_1 * (FOOD == 1) + omega_p)
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(omega_Cl)
               d/dt(depot) <- -ka * depot
               f(depot) <- p
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxObs(.id, pkTimes(48), cmt=2),
                     cov=mlxCov(nSub, FOOD=function(n) rep_len(c(0L, 1L), n)))
           },
           columns=c("ID", "TIME", "AMT", "DV", "FOOD"),
           model=.pkModel("ka, p, V, Cl", "Cc = pkmodel(ka, p, V, Cl)"),
           mlxtran=.mlxProject(
             list(ka=.mlxPar(1.2, 0.3),
                  p=.mlxPar(0.6, 0.5, dist="logitNormal",
                            extra=", covariate=FOOD, coefficient={0, beta_p_FOOD_1}"),
                  V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3)),
             params=c(beta_p_FOOD_1=0.8), indInput="FOOD",
             content=paste0(.mlxContent, "\nFOOD = {use=covariate, type=categorical}"),
             covariate="[COVARIATE]
input = FOOD

FOOD = {type=categorical, categories={0, 1}}",
             indDecl="FOOD = {type=categorical, categories={0, 1}}"))

## RACE is 1, 3 or 'U' (unknown, the reference): the data column is
## character, and the first subject is 3 so the data's level order differs
## from the categories
kitVariant("pkmodel-oral-1cmt", "cov-cat-mixed",
           "categorical covariate mixing numbers and a string (categories={'U', 1, 3}, reference 'U') on Cl",
           tags=c("covariate", "categorical"),
           sim=.covOral("Cl_pop * exp(beta_Cl_RACE_1 * (RACEN == 1) + beta_Cl_RACE_3 * (RACEN == 3) + omega_Cl)",
                        beta=c(beta_Cl_RACE_1=-0.3, beta_Cl_RACE_3=0.25)),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxObs(.id, pkTimes(48), cmt=2),
                     cov=mlxCov(nSub, RACEN=function(n) rep_len(c(3L, 1L, 0L), n)))
           },
           write=.writeDelim(c("ID", "TIME", "AMT", "DV", "RACE"), ",", function(w) {
             w$RACE <- ifelse(w$RACEN == 0L, "U", as.character(w$RACEN))
             w
           }),
           mlxtran=.mlxProject(
             list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2),
                  Cl=.mlxPar(3, 0.3, extra=", covariate=RACE, coefficient={0, beta_Cl_RACE_1, beta_Cl_RACE_3}")),
             params=c(beta_Cl_RACE_1=-0.3, beta_Cl_RACE_3=0.25), indInput="RACE",
             content=paste0(.mlxContent, "\nRACE = {use=covariate, type=categorical}"),
             covariate="[COVARIATE]
input = RACE

RACE = {type=categorical, categories={'U', '1', '3'}}",
             indDecl="RACE = {type=categorical, categories={'U', '1', '3'}}"))
