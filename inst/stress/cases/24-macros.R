## PK macros no other case uses: empty() and reset() administrations, a
## transit oral() macro, zero-order depot(), elimination by clearance,
## peripheral(k1_2, k2_1)

## the oral one-compartment macros; `extra` macro lines after oral()
.macroOral <- function(extra="", elim="elimination(cmt=1, k=Cl/V)",
                       cmt="compartment(cmt=1, amount=Ac)", oral="oral(adm=1, cmt=1, ka)",
                       eq="Cc = Ac/V") {
  paste0("DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl}

PK:
", cmt, "
", oral, "
", extra, if (nzchar(extra)) "\n", elim, "

EQUATION:
", eq, "

OUTPUT:
output = Cc
")
}

## the truth's evid 5/3 rows become ADM=`adm` administrations (AMT=1, not
## used by Monolix)
.macroEventWrite <- function(adm) {
  function(d) {
    .ev <- d$EVID %in% c(3L, 5L)
    .w <- .kitMonolixRows(d)
    .w$ADM <- ifelse(.w$EVID == 1L, 1L, NA)
    .w$AMT[.ev] <- 1
    .w$ADM[.ev] <- adm
    .w[, c("ID", "TIME", "AMT", "ADM", "DV")]
  }
}

.macroEventContent <- paste0(.mlxContent, "\nADM = {use=administration}")

## the depot still holds a few percent of the dose when the central
## compartment is emptied
kitVariant("pkmodel-oral-1cmt", "macro-empty",
           "empty(adm=2, target=Ac): an ADM=2 line empties the central compartment, absorption continues",
           tags=c("pk", "macro", "dosing"),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxDose(.id, 2.5, amt=0, cmt=2, evid=5L),
                     mlxDose(.id, 30, amt=100, cmt=1),
                     mlxObs(.id, c(pkTimes(24), 31, 33, 36, 42, 48), cmt=2))
           },
           write=.macroEventWrite(2L),
           model=.macroOral("empty(adm=2, target=Ac)"),
           mlxtran=.mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3)),
                               content=.macroEventContent))

## the depot still holds drug at the reset, so emptying only the central
## compartment would differ
kitVariant("pkmodel-oral-1cmt", "macro-reset",
           "reset(adm=3): an ADM=3 line resets every compartment (depot too) with drug on board, then a new dose",
           tags=c("pk", "macro", "dosing"),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxDose(.id, 2.5, amt=0, cmt=1, evid=3L),
                     mlxDose(.id, 6, amt=100, cmt=1),
                     mlxObs(.id, c(0.25, 0.5, 1, 1.5, 2, 7, 8, 10, 12, 16, 24, 36, 48), cmt=2))
           },
           write=.macroEventWrite(3L),
           model=.macroOral("reset(adm=3)"),
           mlxtran=.mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3)),
                               content=.macroEventContent))

## the empty must not get the dose's lag (rxode2 would delay an evid=5
## event too); the truth lags ADM=1 only
kitVariant("pkmodel-oral-1cmt", "macro-empty-tlag",
           "iv(adm=1, cmt=1, Tlag) and empty(adm=2, target=Ac) on the same compartment",
           tags=c("pk", "macro", "dosing", "tlag"),
           sim=function() {
             ini({
               V_pop <- 30; Cl_pop <- 3; Tlag_pop <- 0.5
               omega_V ~ 0.04; omega_Cl ~ 0.09
               a <- 0.05; b <- 0.1
             })
             model({
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(omega_Cl)
               Tlag <- Tlag_pop
               d/dt(central) <- -Cl / V * central
               alag(central) <- Tlag * (ADM == 1)
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxDose(.id, 6, amt=0, cmt=1, adm=2L, evid=5L),
                     mlxDose(.id, 24, amt=100, cmt=1),
                     mlxObs(.id, c(0.25, 1, 2, 4, 5.75, 6.25, 6.5, 8, 23, 25, 26, 30, 36, 48), cmt=1))
           },
           write=.macroEventWrite(2L),
           model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {V, Cl, Tlag}

PK:
compartment(cmt=1, amount=Ac)
iv(adm=1, cmt=1, Tlag)
empty(adm=2, target=Ac)
elimination(cmt=1, k=Cl/V)

EQUATION:
Cc = Ac/V

OUTPUT:
output = Cc
",
           mlxtran=.mlxProject(list(V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3), Tlag=.mlxPar(0.5)),
                               content=.macroEventContent))

kitVariant("pkmodel-oral-transit", "macro-oral-transit",
           "oral(cmt=1, Mtt, Ktr, ka) transit absorption macro",
           tags=c("pk", "macro", "transit"),
           model=sub("input = {ka, V, Cl}", "input = {Mtt, Ktr, ka, V, Cl}",
                     .macroOral(oral="oral(cmt=1, Mtt, Ktr, ka)"), fixed=TRUE))

kitVariant("pkmodel-tk0", "macro-depot-tk0",
           "depot(target=Ac, Tk0) zero-order absorption macro",
           tags=c("pk", "macro"),
           model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {Tk0, V, Cl}

PK:
depot(target=Ac, Tk0)

EQUATION:
ddt_Ac = -Cl/V*Ac
Cc = Ac/V

OUTPUT:
output = Cc
")

kitVariant("pkmodel-oral-1cmt", "macro-elimination-cl",
           "elimination(cmt=1, Cl) with compartment(volume=V, concentration=Cc)",
           tags=c("pk", "macro"),
           model=sub("output = Cc", "output = Cout",
                     .macroOral(cmt="compartment(cmt=1, amount=Ac, volume=V, concentration=Cc)",
                                elim="elimination(cmt=1, Cl)", eq="Cout = Cc"), fixed=TRUE),
           sim=.oralErrTruth(quote(add(a) + prop(b) + combined1()),
                             list(quote(a <- 0.05), quote(b <- 0.1)),
                             out=quote(Cout), extra=list(quote(Cout <- Cc))),
           mlxtran=.mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3)),
                               pred="Cout"))

kitVariant("pkmodel-iv-2cmt", "macro-peripheral-underscore",
           "peripheral(k1_2=Q/V, k2_1=Q/V2) with the underscore rate names",
           tags=c("pk", "macro"),
           model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {V, Cl, Q, V2}

PK:
compartment(cmt=1, amount=Ac, volume=V, concentration=Cc)
iv(cmt=1)
peripheral(k1_2=Q/V, k2_1=Q/V2)
elimination(cmt=1, k=Cl/V)

OUTPUT:
output = Cc
")
