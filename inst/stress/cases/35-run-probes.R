## Probes for what the second Monolix 2024R1 run left open: where the DDE
## differences come from (oscillation in dde-hutchinson, a dose at t_0 in
## dde-two-delays), and an EVID=4 line without an amount

## the same delay (tau = 2) with r*tau = 0.3 < 1/e, which does not
## oscillate: the history window is the same, so a difference that
## shrinks against dde-hutchinson points at the solver
kitVariant("dde-hutchinson", "dde-hutchinson-damped",
           "delayed logistic growth with r*tau below 1/e (no oscillation), tau = 2",
           tags="dde",
           sim=function() {
             ini({
               r_pop <- 0.15; K_pop <- 100; tau_pop <- 2
               omega_r ~ 0.04; omega_K ~ 0.04
               a <- 1; b <- 0.05
             })
             model({
               r <- r_pop * exp(omega_r)
               K <- K_pop * exp(omega_K)
               tau <- tau_pop
               x(0) <- 10
               d/dt(x) <- r * x * (1 - delay(x, tau) / K)
               x ~ add(a) + prop(b) + combined1()
             })
           },
           data=function(nSub) mlxObs(seq_len(nSub), c(1, 2, 4, 8, 12, 16, 20, 25, 30, 40, 50, 60), cmt=1),
           mlxtran=.mlxProject(list(r=.mlxPar(0.15, 0.2), K=.mlxPar(100, 0.2), tau=.mlxPar(2)),
                               errPar=c(a=1, b=0.05), pred="x",
                               content="ID = {use=identifier}
TIME = {use=time}
DV = {use=observation, name=CONC, type=continuous}"))

## the doses after t_0, so the delayed depot has no bolus at the start of
## its history
kitVariant("dde-two-delays", "dde-two-delays-late",
           "two delays with the doses at t = 2 and 26, after t_0",
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, c(2, 26), amt=100, cmt=1),
                     mlxObs(.id, c(0.5, 1, 2.5, 4, 8, 12, 26.5, 27, 28, 30, 34, 48, 72, 96), cmt=3))
           })

## Monolix 2024R1 ignores the line, as it does EVID=3 with AMT=.
kitVariant("dose-evid4-reset", "dose-evid4-amt-missing",
           "EVID=4 line with AMT=.: ignored, no washout",
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxOther(.id, 6, cmt=1, evid=2L),
                     mlxDose(.id, 8, amt=100, cmt=1),
                     mlxObs(.id, c(1, 2, 4, 5.5, 7, 8.5, 10, 12, 18, 24), cmt=2))
           },
           write=function(d) {
             .w <- .kitMonolixRows(d)
             .w$EVID[.w$EVID == 2L] <- 4L
             .w[, c("ID", "TIME", "AMT", "EVID", "DV")]
           })
