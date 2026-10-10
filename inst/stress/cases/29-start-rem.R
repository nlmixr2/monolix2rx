## A non-zero t_0, the t0 spelling, and rem() of a negative covariate

## R starts at 0 at t_0 = 12, before the first record (a dose at 24)
kitVariant("ode-no-t0-late-data", "ode-t0-nonzero",
           "t_0 = 12 with the first data record at t = 24 (the system starts at t_0)",
           tags=c("ode"),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxOther(.id, 12, cmt=3),
                     mlxOther(.id, 12, cmt=3, evid=3L),
                     mlxDose(.id, 24, amt=100, cmt=1),
                     mlxObs(.id, 24 + c(pkTimes(72), 96), cmt=3))
           },
           model=local({
             .m <- sub("EQUATION:\n", "EQUATION:\nt_0 = 12\n", .kitEnv$cases[["ode-no-t0-late-data"]]$model, fixed=TRUE)
             if (!grepl("t_0 = 12\n", .m, fixed=TRUE)) stop("ode-t0-nonzero: the base model changed")
             .m
           }))

## t0 (not t_0) with a comment: the system starts at 0, as rxode2 does
kitVariant("ode-t0-before-data", "ode-t0-alias",
           "t0 = 0 ; with a comment (the t0 spelling) and the first data record at t = 24",
           tags=c("ode", "syntax"),
           model=local({
             .m <- sub("t_0 = 0\n", "t0 = 0 ; start\n", .kitEnv$cases[["ode-t0-before-data"]]$model, fixed=TRUE)
             if (!grepl("t0 = 0 ; start\n", .m, fixed=TRUE)) stop("ode-t0-alias: the base model changed")
             .m
           }))

## rem() of a negative covariate has the sign of the covariate (C fmod),
## not R's floored %%
kitVariant("pkmodel-oral-1cmt", "cov-rem-negative",
           "rem(DT, 4) of a covariate with negative values in a [COVARIATE] EQUATION: (on Cl)",
           tags=c("covariate", "function"),
           sim=.covOral("Cl_pop * exp(beta_Cl_rDT * (DT - 4 * trunc(DT / 4)) + omega_Cl)",
                        beta=c(beta_Cl_rDT=0.1)),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxObs(.id, pkTimes(48), cmt=2),
                     cov=mlxCov(nSub, DT=function(n) round(stats::runif(n, -10, 10))))
           },
           columns=c("ID", "TIME", "AMT", "DV", "DT"),
           mlxtran=.mlxProject(
             list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2),
                  Cl=.mlxPar(3, 0.3, extra=", covariate=rDT, coefficient=beta_Cl_rDT")),
             params=c(beta_Cl_rDT=0.1), indInput="rDT",
             content=paste0(.mlxContent, "\nDT = {use=covariate, type=continuous}"),
             covariate="[COVARIATE]
input = DT

EQUATION:
rDT = rem(DT, 4)"))
