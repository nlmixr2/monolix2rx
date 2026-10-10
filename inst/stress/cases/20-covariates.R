## More covariate models (05-params.R and 13-covariates.R have the common
## ones) and occasions without inter-occasion variability

kitVariant("pkmodel-oral-1cmt", "cov-cat-two-params",
           "one categorical covariate (SEX 0/1, reference 0) on V and on Cl",
           tags=c("covariate", "categorical"),
           sim=.covOral("Cl_pop * exp(beta_Cl_SEX_1 * (SEX == 1) + omega_Cl)",
                        v="V_pop * exp(beta_V_SEX_1 * (SEX == 1) + omega_V)",
                        beta=c(beta_V_SEX_1=0.2, beta_Cl_SEX_1=-0.4)),
           data=.covData,
           columns=c("ID", "TIME", "AMT", "DV", "SEX"),
           mlxtran=.mlxProject(
             list(ka=.mlxPar(1.2, 0.3),
                  V=.mlxPar(30, 0.2, extra=", covariate=SEX, coefficient={0, beta_V_SEX_1}"),
                  Cl=.mlxPar(3, 0.3, extra=", covariate=SEX, coefficient={0, beta_Cl_SEX_1}")),
             params=c(beta_V_SEX_1=0.2, beta_Cl_SEX_1=-0.4), indInput="SEX",
             content=paste0(.mlxContent, "\nSEX = {use=covariate, type=categorical}"),
             covariate="[COVARIATE]
input = SEX

SEX = {type=categorical, categories={0, 1}}",
             indDecl="SEX = {type=categorical, categories={0, 1}}"))

## the drug is gone by the second occasion, so whether a new occasion
## resets the system does not matter here
kitVariant("pkmodel-oral-1cmt", "data-occ-no-iov",
           "an occasion column (two dosing periods) with no inter-occasion variability",
           tags=c("data", "iov"),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1, OCC=1L),
                     mlxObs(.id, pkTimes(48), cmt=2, OCC=1L),
                     mlxDose(.id, 168, amt=100, cmt=1, OCC=2L),
                     mlxObs(.id, 168 + pkTimes(48), cmt=2, OCC=2L))
           },
           columns=c("ID", "TIME", "AMT", "OCC", "DV"),
           mlxtran=.dataProject(content=paste0(.mlxContent, "\nOCC = {use=occasion}")))

## min() is elementwise in Monolix; a dplyr min() is over the whole column
kitVariant("pkmodel-oral-1cmt", "cov-equation-min",
           "covariate transform capped with min() in [COVARIATE] EQUATION: (log(min(WT, 90)/70))",
           tags=c("covariate"),
           sim=.covOral("Cl_pop * exp(beta_Cl_lwc * log(min(WT, 90) / 70) + omega_Cl)",
                        beta=c(beta_Cl_lwc=0.75)),
           data=.covData,
           columns=c("ID", "TIME", "AMT", "DV", "WT"),
           mlxtran=.mlxProject(
             list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2),
                  Cl=.mlxPar(3, 0.3, extra=", covariate=lwc, coefficient=beta_Cl_lwc")),
             params=c(beta_Cl_lwc=0.75), indInput="lwc",
             content=paste0(.mlxContent, "\nWT = {use=covariate, type=continuous}"),
             covariate="[COVARIATE]
input = WT

EQUATION:
lwc = log(min(WT, 90)/70)"))

kitVariant("pkmodel-oral-1cmt", "cov-equation-bmi",
           "covariate computed from two covariates through an intermediate (BMI from WT and HT, then log(BMI/25))",
           tags=c("covariate"),
           sim=.covOral("Cl_pop * exp(beta_Cl_lBMI * log(WT / (HT / 100)^2 / 25) + omega_Cl)",
                        beta=c(beta_Cl_lBMI=0.5)),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxObs(.id, pkTimes(48), cmt=2),
                     cov=mlxCov(nSub, WT=function(n) round(stats::runif(n, 45, 110), 1),
                                HT=function(n) round(stats::runif(n, 150, 195))))
           },
           columns=c("ID", "TIME", "AMT", "DV", "WT", "HT"),
           mlxtran=.mlxProject(
             list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2),
                  Cl=.mlxPar(3, 0.3, extra=", covariate=lBMI, coefficient=beta_Cl_lBMI")),
             params=c(beta_Cl_lBMI=0.5), indInput="lBMI",
             content=paste0(.mlxContent, "\nWT = {use=covariate, type=continuous}",
                            "\nHT = {use=covariate, type=continuous}"),
             covariate="[COVARIATE]
input = {WT, HT}

EQUATION:
BMI = WT/(HT/100)^2
lBMI = log(BMI/25)"))
