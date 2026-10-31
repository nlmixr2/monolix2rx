## Covariate and regressor data: missing values on some lines, a regressor
## given only where it changes, covariate columns the model does not use

## WT on Cl (cov-untransformed) and SEX in the data; `blank(w)` gives the
## rows written as missing.  rxode2 also fills them (with a warning), so
## the imported data is checked to have none
.covMissing <- function(name, covers, blank, cat=FALSE) {
  kitVariant("pkmodel-oral-1cmt", name, covers,
             tags=c("data", "covariate"),
             dryData=function(m, sim) {
               .c <- if (cat) "SEX" else "WT"
               if (anyNA(m$monolixData[[.c]])) paste0("the imported ", .c, " has missing values")
             },
             sim=if (cat) {
               .covOral("Cl_pop * exp(beta_Cl_SEX_1 * (SEX == 1) + omega_Cl)", beta=c(beta_Cl_SEX_1=-0.4))
             } else {
               .covOral("Cl_pop * exp(beta_Cl_WT * WT + omega_Cl)", beta=c(beta_Cl_WT=0.01))
             },
             data=.covData,
             write=function(d) {
               .w <- .kitMonolixRows(d)[, c("ID", "TIME", "AMT", "DV", "WT", "SEX")]
               .b <- blank(.w)
               .w$WT[.b] <- NA
               .w$SEX[.b] <- NA
               .w
             },
             mlxtran=if (cat) {
               .mlxProject(
                 list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2),
                      Cl=.mlxPar(3, 0.3, extra=", covariate=SEX, coefficient={0, beta_Cl_SEX_1}")),
                 params=c(beta_Cl_SEX_1=-0.4), indInput="SEX",
                 content=paste0(.mlxContent, "\nWT = {use=covariate, type=continuous}",
                                "\nSEX = {use=covariate, type=categorical}"),
                 covariate="[COVARIATE]
input = SEX

SEX = {type=categorical, categories={0, 1}}",
                 indDecl="SEX = {type=categorical, categories={0, 1}}")
             } else {
               .mlxProject(
                 list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2),
                      Cl=.mlxPar(3, 0.3, extra=", covariate=WT, coefficient=beta_Cl_WT")),
                 params=c(beta_Cl_WT=0.01), indInput="WT",
                 content=paste0(.mlxContent, "\nWT = {use=covariate, type=continuous}",
                                "\nSEX = {use=covariate, type=categorical}"),
                 covariate="[COVARIATE]
input = WT")
             })
}

## Monolix takes a subject's covariate from its lines that have one; that
## a missing value there is not an error is to confirm in run mode
.covMissing("cov-missing-dose-lines", "a continuous covariate (WT on Cl) missing ('.') on the dose lines",
            function(w) !is.na(w$AMT))

.covMissing("cov-first-line-only", "a continuous covariate (WT on Cl) given on each subject's first line only",
            function(w) duplicated(w$ID))

.covMissing("cov-cat-missing-dose-lines", "a categorical covariate (SEX on Cl) missing ('.') on the dose lines",
            function(w) !is.na(w$AMT), cat=TRUE)

## the covariates are in the data and [CONTENT] but not in the model
kitVariant("pkmodel-oral-1cmt", "cov-unused",
           "continuous and categorical covariate columns (WT, SEX) declared in [CONTENT] that no parameter uses",
           tags=c("data", "covariate"),
           data=.covData,
           columns=c("ID", "TIME", "AMT", "DV", "WT", "SEX"),
           mlxtran=.mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3)),
                               content=paste0(.mlxContent, "\nWT = {use=covariate, type=continuous}",
                                              "\nSEX = {use=covariate, type=categorical}")))

## a step in renal function at 24 h, written only on the first line and
## where it changes; Monolix carries a regressor forward
.regStep <- function(t, id) 60 + 10 * (id %% 5) - ifelse(t >= 24, 25, 0)

kitCase(
  name="reg-sparse",
  covers="a regressor (crcl on Cl) given only on each subject's first line and where it changes, '.' elsewhere",
  tags=c("data", "regressor"),
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
      Cli <- Cl * (CLCR / 100)^0.75
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - Cli / V * central
      Cc <- central / V
      Cc ~ add(a) + prop(b) + combined1()
    })
  },
  data=function(nSub) {
    .id <- seq_len(nSub)
    .d <- mlxBind(mlxDose(.id, c(0, 24), amt=100, cmt=1),
                  mlxObs(.id, c(pkTimes(24), 25, 26, 28, 32, 36, 48), cmt=2))
    .d$CLCR <- .regStep(.d$TIME, .d$ID)
    .d
  },
  write=function(d) {
    .w <- .kitMonolixRows(d)[, c("ID", "TIME", "AMT", "DV", "CLCR")]
    .keep <- !duplicated(.w$ID) | c(TRUE, diff(.w$CLCR) != 0)
    .w$CLCR[!.keep] <- NA
    .w
  },
  model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl, crcl}
crcl = {use=regressor}

PK:
depot(target=Ad)

EQUATION:
Cli = Cl*(crcl/100)^0.75
ddt_Ad = -ka*Ad
ddt_Ac = ka*Ad - Cli/V*Ac
Cc = Ac/V

OUTPUT:
output = Cc
",
  mlxtran=.dataProject(content=paste0(.mlxContent, "\nCLCR = {use=regressor}")))
