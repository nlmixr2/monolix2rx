## Inter-occasion variability

.iovContent <- paste0(.mlxContent, "
EVID = {use=eventidentifier}
OCC = {use=occasion}")

kitVariant("pkmodel-oral-1cmt", "iov-cl-basic",
           "use=occasion; Cl with varlevel={id, id*occ}, two occasions",
           tags=c("iov", "smoke"),
           knownRun="Monolix writes individual parameters per occasion; monolix2rx's validation expects one row per subject (to confirm on Monolix)",
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
               omega_ka ~ 0.09; omega_V ~ 0.04; omega_Cl ~ 0.09
               gamma_Cl ~ 0.04 | OCC
               a <- 0.05; b <- 0.1
             })
             model({
               ka <- ka_pop * exp(omega_ka)
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(omega_Cl + gamma_Cl)
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           ## occasion 2 starts with a washout (EVID=4 dose)
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1, OCC=1L),
                     mlxObs(.id, pkTimes(48), cmt=2, OCC=1L),
                     mlxDose(.id, 100, amt=100, cmt=1, evid=4L, OCC=2L),
                     mlxObs(.id, 100 + pkTimes(48), cmt=2, OCC=2L))
           },
           columns=c("ID", "TIME", "EVID", "AMT", "OCC", "DV"),
           mlxtran=.mlxProject(
             list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3, iov=0.2)),
             content=.iovContent))

.iovKnownRun <- "Monolix writes individual parameters per occasion; monolix2rx's validation expects one row per subject (to confirm on Monolix)"

kitVariant("iov-cl-basic", "iov-only",
           "occasion variability without subject variability on Cl (varlevel=id*occ)",
           tags=c("iov"),
           knownRun=.iovKnownRun,
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
               omega_ka ~ 0.09; omega_V ~ 0.04
               gamma_Cl ~ 0.04 | OCC
               a <- 0.05; b <- 0.1
             })
             model({
               ka <- ka_pop * exp(omega_ka)
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(gamma_Cl)
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           mlxtran=.mlxProject(
             list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, iov=0.2)),
             content=.iovContent))

## occasions 2, 5, 7 with 2, 1 and 3 doses; drug carries over between
## occasions (no reset)
kitVariant("iov-cl-basic", "iov-ka-v-unequal",
           "IOV on ka and V, occasions 2/5/6/7 of unequal length (6 has a dose and no observations), no washout between occasions",
           tags=c("iov"),
           knownRun=.iovKnownRun,
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
               omega_ka ~ 0.09; omega_V ~ 0.04; omega_Cl ~ 0.09
               gamma_ka ~ 0.09 | OCC
               gamma_V ~ 0.01 | OCC
               a <- 0.05; b <- 0.1
             })
             model({
               ka <- ka_pop * exp(omega_ka + gamma_ka)
               V <- V_pop * exp(omega_V + gamma_V)
               Cl <- Cl_pop * exp(omega_Cl)
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, c(0, 12), amt=100, cmt=1, OCC=2L),
                     mlxObs(.id, c(1, 2, 4, 8, 13, 14, 16, 20), cmt=2, OCC=2L),
                     mlxDose(.id, 24, amt=100, cmt=1, OCC=5L),
                     mlxObs(.id, c(25, 26, 28, 32, 40), cmt=2, OCC=5L),
                     mlxDose(.id, 42, amt=50, cmt=1, OCC=6L),
                     mlxDose(.id, c(48, 60, 72), amt=100, cmt=1, OCC=7L),
                     mlxObs(.id, c(49, 50, 61, 62, 73, 74, 76, 80, 96), cmt=2, OCC=7L))
           },
           columns=c("ID", "TIME", "AMT", "OCC", "DV"),
           mlxtran=.mlxProject(
             list(ka=.mlxPar(1.2, 0.3, iov=0.3), V=.mlxPar(30, 0.2, iov=0.1),
                  Cl=.mlxPar(3, 0.3)),
             content=paste0(.mlxContent, "\nOCC = {use=occasion}")))

kitVariant("iov-cl-basic", "iov-correlation",
           "correlated occasion effects: correlation = {level=id*occ, r(ka, Cl)}",
           tags=c("iov", "correlation"),
           knownRun=.iovKnownRun,
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
               omega_ka ~ 0.09; omega_V ~ 0.04; omega_Cl ~ 0.09
               gamma_ka + gamma_Cl ~ c(0.09, 0.03, 0.04) | OCC
               a <- 0.05; b <- 0.1
             })
             model({
               ka <- ka_pop * exp(omega_ka + gamma_ka)
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(omega_Cl + gamma_Cl)
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           mlxtran=.mlxProject(
             list(ka=.mlxPar(1.2, 0.3, iov=0.3), V=.mlxPar(30, 0.2),
                  Cl=.mlxPar(3, 0.3, iov=0.2)),
             params=c(corr2_ka_Cl=0.5),
             indExtra="correlation = {level=id*occ, r(ka, Cl)=corr2_ka_Cl}",
             content=.iovContent))

## IOV on an absorption lag (logNormal), ka (IOV only) and a logitNormal
## bioavailability (IOV only)
kitVariant("iov-cl-basic", "iov-ka-f-multi",
           "IOV on Tlag (with BSV), ka and logitNormal p (IOV only) in pkmodel(Tlag, ka, p, V, Cl)",
           tags=c("iov"),
           knownRun=.iovKnownRun,
           sim=function() {
             ini({
               Tlag_pop <- 0.5; ka_pop <- 1.2; p_pop <- 0.7; V_pop <- 30; Cl_pop <- 3
               omega_Tlag ~ 0.04; omega_V ~ 0.04; omega_Cl ~ 0.09
               gamma_Tlag ~ 0.04 | OCC
               gamma_ka ~ 0.09 | OCC
               gamma_p ~ 0.25 | OCC
               a <- 0.05; b <- 0.1
             })
             model({
               Tlag <- Tlag_pop * exp(omega_Tlag + gamma_Tlag)
               ka <- ka_pop * exp(gamma_ka)
               p <- expit(logit(p_pop) + gamma_p)
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(omega_Cl)
               d/dt(depot) <- -ka * depot
               alag(depot) <- Tlag
               f(depot) <- p
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           model=.pkModel("Tlag, ka, p, V, Cl", "Cc = pkmodel(Tlag, ka, p, V, Cl)"),
           mlxtran=.mlxProject(
             list(Tlag=.mlxPar(0.5, 0.2, iov=0.2), ka=.mlxPar(1.2, iov=0.3),
                  p=.mlxPar(0.7, dist="logitNormal", iov=0.5), V=.mlxPar(30, 0.2),
                  Cl=.mlxPar(3, 0.3)),
             content=.iovContent))

## a steady-state dose opens each occasion; the second comes 6 h after the
## last sample of the first, so a reset and an added dose differ
kitVariant("iov-cl-basic", "iov-ss",
           "steady-state dose (q24h) at the start of each of two occasions (drug on board at the second), IOV on Cl",
           tags=c("iov", "ss"),
           knownRun=.iovKnownRun,
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1, ss=1L, ii=24, OCC=1L),
                     mlxObs(.id, pkTimes(24), cmt=2, OCC=1L),
                     mlxDose(.id, 30, amt=100, cmt=1, ss=1L, ii=24, OCC=2L),
                     mlxObs(.id, 30 + pkTimes(24), cmt=2, OCC=2L))
           },
           columns=c("ID", "TIME", "AMT", "SS", "II", "OCC", "DV"),
           mlxtran=.mlxProject(
             list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3, iov=0.2)),
             content=paste0(.mlxContent, "
SS = {use=steadystate}
II = {use=interdoseinterval}
OCC = {use=occasion}")))

## body weight measured again at the second occasion (constant within an
## occasion, as Monolix needs for a covariate on a parameter with IOV)
kitVariant("iov-cl-basic", "iov-time-varying-cov",
           "covariate changing between occasions (lw70 on Cl) with IOV on Cl",
           tags=c("iov", "covariate"),
           knownRun=.iovKnownRun,
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3; beta_Cl_lw70 <- 0.75
               omega_ka ~ 0.09; omega_V ~ 0.04; omega_Cl ~ 0.09
               gamma_Cl ~ 0.04 | OCC
               a <- 0.05; b <- 0.1
             })
             model({
               lw70 <- log(WT / 70)
               ka <- ka_pop * exp(omega_ka)
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(beta_Cl_lw70 * lw70 + omega_Cl + gamma_Cl)
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           data=function(nSub) {
             .id <- seq_len(nSub)
             .wt <- round(stats::runif(nSub, 45, 110), 1)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1, OCC=1L, WT=.wt),
                     mlxObs(.id, pkTimes(48), cmt=2, OCC=1L, WT=.wt),
                     mlxDose(.id, 100, amt=100, cmt=1, evid=4L, OCC=2L, WT=.wt * 0.8),
                     mlxObs(.id, 100 + pkTimes(48), cmt=2, OCC=2L, WT=.wt * 0.8))
           },
           columns=c("ID", "TIME", "EVID", "AMT", "OCC", "WT", "DV"),
           mlxtran=.mlxProject(
             list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2),
                  Cl=.mlxPar(3, 0.3, iov=0.2, extra=", covariate=lw70, coefficient=beta_Cl_lw70")),
             params=c(beta_Cl_lw70=0.75), indInput="lw70",
             content=paste0(.iovContent, "\nWT = {use=covariate, type=continuous}"),
             covariate="[COVARIATE]
input = WT

EQUATION:
lw70 = log(WT/70)"))

## the edited project must really be nested
.iovNested <- function(txt) {
  for (.p in c("id*occ1*occ2", "gamma1_Cl = {value", "gamma2_Cl = {value", "omega_Cl, gamma1_Cl, gamma2_Cl}")) {
    if (!grepl(.p, txt, fixed=TRUE)) stop("iov-nested project lacks '", .p, "'", call.=FALSE)
  }
  txt
}

## nested occasions: periods (OCC1) split into sub-occasions (OCC2); the
## truth indexes the inner level by a hidden period/sub-occasion column
## (OCC12), numbered like the import's occ2 so the true etas line up
kitVariant("iov-cl-basic", "iov-nested",
           "nested occasions OCC1/OCC2: Cl with varlevel={id, id*occ1, id*occ1*occ2}",
           tags=c("iov"),
           knownRun=.iovKnownRun,
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
               omega_ka ~ 0.09; omega_V ~ 0.04; omega_Cl ~ 0.09
               gamma1_Cl ~ 0.04 | OCC1
               gamma2_Cl ~ 0.01 | OCC12
               a <- 0.05; b <- 0.1
             })
             model({
               ka <- ka_pop * exp(omega_ka)
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(omega_Cl + gamma1_Cl + gamma2_Cl)
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           data=function(nSub) {
             .id <- seq_len(nSub)
             .occ <- function(t0, o1, o2) {
               mlxBind(mlxDose(.id, t0, amt=100, cmt=1, OCC1=o1, OCC2=o2, OCC12=2L * (o1 - 1L) + o2),
                       mlxObs(.id, t0 + pkTimes(24), cmt=2, OCC1=o1, OCC2=o2, OCC12=2L * (o1 - 1L) + o2))
             }
             mlxBind(.occ(0, 1L, 1L), .occ(48, 1L, 2L), .occ(500, 2L, 1L), .occ(548, 2L, 2L))
           },
           columns=c("ID", "TIME", "AMT", "OCC1", "OCC2", "DV"),
           mlxtran=.iovNested(sub("varlevel={id, id*occ}, sd={omega_Cl, gamma_Cl}",
                       "varlevel={id, id*occ1, id*occ1*occ2}, sd={omega_Cl, gamma1_Cl, gamma2_Cl}",
                       sub("gamma_Cl = {value=0.2, method=MLE}",
                           "gamma1_Cl = {value=0.2, method=MLE}\ngamma2_Cl = {value=0.1, method=MLE}",
                           sub("omega_Cl, gamma_Cl}", "omega_Cl, gamma1_Cl, gamma2_Cl}",
                               .mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2),
                                                Cl=.mlxPar(3, 0.3, iov=0.2)),
                                           content=paste0(.mlxContent, "
OCC1 = {use=occasion}
OCC2 = {use=occasion}")), fixed=TRUE), fixed=TRUE), fixed=TRUE)))

## IOV with a delay: the occasion changes Cl while the delayed state
## carries its history across the boundary (no washout)
kitVariant("dde-delayed-effect", "iov-dde",
           "IOV on Cl in a delay() model; the second occasion starts with drug and delayed history on board",
           tags=c("iov", "dde"),
           knownRun=.iovKnownRun,
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
               Kin_pop <- 10; Kout_pop <- 0.1; Imax_pop <- 0.8; IC50_pop <- 1; tau_pop <- 4
               omega_Cl ~ 0.09; omega_tau ~ 0.04
               gamma_Cl ~ 0.04 | OCC
               a <- 1; b <- 0.05
             })
             model({
               ka <- ka_pop
               V <- V_pop
               Cl <- Cl_pop * exp(omega_Cl + gamma_Cl)
               Kin <- Kin_pop
               Kout <- Kout_pop
               Imax <- Imax_pop
               IC50 <- IC50_pop
               tau <- tau_pop * exp(omega_tau)
               R(0) <- Kin / Kout
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cd <- delay(central, tau) / V
               d/dt(R) <- Kin * (1 - Imax * Cd / (Cd + IC50)) - Kout * R
               R ~ add(a) + prop(b) + combined1()
             })
           },
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1, OCC=1L),
                     mlxObs(.id, c(pkTimes(24), 36), cmt=3, OCC=1L),
                     mlxDose(.id, 48, amt=100, cmt=1, OCC=2L),
                     mlxObs(.id, 48 + c(0.5, 1, 2, 4, 6, 8, 12, 24, 48, 72), cmt=3, OCC=2L))
           },
           columns=c("ID", "TIME", "AMT", "OCC", "DV"),
           mlxtran=.mlxProject(list(ka=.mlxPar(1.2), V=.mlxPar(30), Cl=.mlxPar(3, 0.3, iov=0.2),
                                    Kin=.mlxPar(10), Kout=.mlxPar(0.1), Imax=.mlxPar(0.8),
                                    IC50=.mlxPar(1), tau=.mlxPar(4, 0.2)),
                               errPar=c(a=1, b=0.05), pred="R",
                               content=paste0(.mlxContent, "\nOCC = {use=occasion}")))

## IOV inside a between-subject mixture: V varies by occasion in both groups
kitVariant("bsmm-structural", "iov-mixture",
           "bsmm(C1, p1, C2, 1-p1) with IOV on V over two dosing occasions",
           tags=c("iov", "mixture", "bsmm"),
           knownRun="IPRED needs Monolix's estimated class per subject and per-occasion individual parameters (not read yet)",
           sim=function() {
             ini({
               V_pop <- 30; Cl1_pop <- 1; Cl2_pop <- 5
               omega_V ~ 0.04; omega_Cl1 ~ 0.09; omega_Cl2 ~ 0.09
               gamma_V ~ 0.04 | OCC
               a <- 0.05; b <- 0.1
             })
             model({
               V <- V_pop * exp(omega_V + gamma_V)
               Cl1 <- Cl1_pop * exp(omega_Cl1)
               Cl2 <- Cl2_pop * exp(omega_Cl2)
               d/dt(A1) <- -Cl1 / V * A1
               d/dt(A2) <- -Cl2 / V * A2
               Cc <- (POP == 1) * A1 / V + (POP == 2) * A2 / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1, OCC=1L), mlxDose(.id, 0, amt=100, cmt=2, OCC=1L),
                     mlxObs(.id, c(0.5, 1, 2, 4, 8, 12, 24), cmt=1, OCC=1L),
                     mlxDose(.id, 48, amt=100, cmt=1, OCC=2L), mlxDose(.id, 48, amt=100, cmt=2, OCC=2L),
                     mlxObs(.id, 48 + c(0.5, 1, 2, 4, 8, 12, 24, 36), cmt=1, OCC=2L),
                     cov=mlxCov(nSub, POP=function(n) sample.int(2L, n, replace=TRUE, prob=c(0.4, 0.6))))
           },
           write=function(d) {
             d <- .kitMonolixRows(d)
             d <- d[!(d$EVID == 1L & d$CMT != 1L), ]
             d[, c("ID", "TIME", "AMT", "OCC", "DV")]
           },
           mlxtran=.mlxProject(list(V=.mlxPar(30, 0.2, iov=0.2), Cl1=.mlxPar(1, 0.3),
                                    Cl2=.mlxPar(5, 0.3), p1=.mlxPar(0.4, dist="logitNormal")),
                               content=paste0(.mlxContent, "\nOCC = {use=occasion}")))
