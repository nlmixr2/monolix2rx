## The integration start (an explicit t_0 before the first record),
## time-to-event options (rightCensoringTime=, maxEventNumber= > 1) and a
## negation after a sign or as an exponent

## R starts at 0 at t_0 = 0 and rises toward Kin/Kout before the first
## record (a dose at 24); starting at the first record would leave R at 0
kitVariant("ode-turnover", "ode-t0-before-data",
           "t_0 = 0 with the first data record at t = 24 (the system evolves from t_0, not from the first record)",
           tags=c("ode"),
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
               Kin_pop <- 10; Kout_pop <- 0.1; Imax_pop <- 0.8; IC50_pop <- 1
               omega_V ~ 0.04; omega_Cl ~ 0.09; omega_Kout ~ 0.04; omega_Imax ~ 0.25
               a <- 1; b <- 0.05
             })
             model({
               ka <- ka_pop
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(omega_Cl)
               Kin <- Kin_pop
               Kout <- Kout_pop * exp(omega_Kout)
               Imax <- expit(logit(Imax_pop) + omega_Imax)
               IC50 <- IC50_pop
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               d/dt(R) <- Kin * (1 - Imax * Cc / (Cc + IC50)) - Kout * R
               R ~ add(a) + prop(b) + combined1()
             })
           },
           ## the t = 0 row starts the truth's integration and is not written
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxOther(.id, 0, cmt=3),
                     mlxDose(.id, 24, amt=100, cmt=1),
                     mlxObs(.id, 24 + c(pkTimes(72), 96), cmt=3))
           },
           write=function(d) .kitMonolixRows(d[d$EVID != 2L, ])[, c("ID", "TIME", "AMT", "DV")],
           model=local({
             .m <- sub("R_0 = Kin/Kout\n", "R_0 = 0\n", .kitEnv$cases[["ode-turnover"]]$model, fixed=TRUE)
             if (!grepl("t_0 = 0\n", .m, fixed=TRUE) || !grepl("R_0 = 0\n", .m, fixed=TRUE)) {
               stop("ode-t0-before-data: the base model changed")
             }
             .m
           }))

## without t_0 Monolix starts each subject at its first dose or
## observation (rxode2 starts at 0); the truth resets to the initial
## conditions there (rxode2 ignores a reset that is a subject's first record)
kitVariant("ode-t0-before-data", "ode-no-t0-late-data",
           "no t_0 and the first data record at t = 24 (the system starts at the first dose or observation)",
           tags=c("ode"),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxOther(.id, 24, cmt=3),
                     mlxOther(.id, 24, cmt=3, evid=3L),
                     mlxDose(.id, 24, amt=100, cmt=1),
                     mlxObs(.id, 24 + c(pkTimes(72), 96), cmt=3))
           },
           write=function(d) .kitMonolixRows(d[!(d$EVID %in% c(2L, 3L)), ])[, c("ID", "TIME", "AMT", "DV")],
           model=local({
             .m <- sub("t_0 = 0\n", "", .kitEnv$cases[["ode-t0-before-data"]]$model, fixed=TRUE)
             if (grepl("t_0", .m, fixed=TRUE)) stop("ode-no-t0-late-data: the base model changed")
             .m
           }))

## rightCensoringTime= is a simulation setting; the data's end record
## censors the event
kitVariant("tte-weibull-exact", "tte-right-censoring-time",
           "rightCensoringTime=48 in the event definition (the end of the study is also in the data)",
           tags=c("tte", "discrete"),
           model=local({
             .m <- sub("maxEventNumber=1, hazard=h}", "maxEventNumber=1, rightCensoringTime=48, hazard=h}",
                       .kitEnv$cases[["tte-weibull-exact"]]$model, fixed=TRUE)
             if (!grepl("rightCensoringTime=48", .m, fixed=TRUE)) stop("tte-right-censoring-time: the base model changed")
             .m
           }))

## at most three events: a subject stops being at risk after its third
kitVariant("tte-repeated", "tte-repeated-max-events",
           "repeated exact events with maxEventNumber=3 (no record after a subject's third event)",
           tags=c("tte", "discrete"),
           postSim=function(d, s) .tteRows(d, s, cmtH=2L, end=100, maxEvents=3),
           model=local({
             .m <- sub("Event = {type=event, hazard=haz}", "Event = {type=event, maxEventNumber=3, hazard=haz}",
                       .kitEnv$cases[["tte-repeated"]]$model, fixed=TRUE)
             if (!grepl("maxEventNumber=3", .m, fixed=TRUE)) stop("tte-repeated-max-events: the base model changed")
             .m
           }))

## -~f is -1 or 0 and 2^~f is 2 or 1
kitVariant("pkmodel-oral-1cmt", "fun-not-signed",
           "a negation after a sign and as an exponent (-~f, 2^~f)",
           tags=c("function"),
           model=.oralFunModel("if t > 12
  f = 1
else
  f = 0
end
Y = Cc*(2 + 0.5*-~f)*2^~f"),
           sim=.oralErrTruth(quote(add(a) + prop(b) + combined1()),
                             list(quote(a <- 0.05), quote(b <- 0.1)),
                             out=quote(Y),
                             extra=list(quote(f <- (time > 12)),
                                        quote(Y <- Cc * (2 - 0.5 * (1 - f)) * 2^(1 - f)))),
           mlxtran=.mlxProject(.oralFunPars, pred="Y"))

## a transit dose (an evid 7 row in the imported data) starts the subject:
## a start at the first observation would reset after the dose
kitVariant("macro-oral-transit", "transit-late-dose",
           "transit oral() macro without t_0, the dose at t = 12 and no observation then",
           tags=c("pk", "macro", "transit"),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 12, amt=100, cmt=1),
                     mlxObs(.id, 12 + pkTimes(48), cmt=7))
           })
