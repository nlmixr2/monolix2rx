## A space-delimited data set (single spaces, and columns aligned with
## runs of spaces), and steady-state and ADDL infusions into a compartment
## with a bioavailability (the import gives them as a duration)

kitVariant("pkmodel-oral-1cmt", "data-space-delimiter",
           "delimiter = space with single spaces",
           tags=c("data", "syntax"),
           write=.writeDelim(c("ID", "TIME", "AMT", "DV"), " "),
           mlxtran=.dataProject(delimiter="space"))

## left-justified columns padded to a common width: runs of spaces between
## the columns, none at the start or end of a line
kitVariant("pkmodel-oral-1cmt", "data-space-aligned",
           "delimiter = space with columns aligned by runs of spaces",
           tags=c("data", "syntax"),
           knownRun="Monolix 2024R1 refuses runs of spaces: \"Delimiter not detected\" (the import reads them)",
           write=function(d) {
             .l <- .writeDelim(c("ID", "TIME", "AMT", "DV"), "\t")(d)
             .f <- do.call(rbind, strsplit(.l, "\t", fixed=TRUE))
             .f <- apply(.f, 2, function(x) formatC(x, width=max(nchar(x)) + 2, flag="-"))
             trimws(apply(.f, 1, paste, collapse=""), "right")
           },
           mlxtran=.dataProject(delimiter="space"))

.ivpTruth <- function() {
  ini({
    p_pop <- 0.7; V_pop <- 30; Cl_pop <- 3
    omega_V ~ 0.04; omega_Cl ~ 0.09
    a <- 0.05; b <- 0.1
  })
  model({
    p <- p_pop
    V <- V_pop * exp(omega_V)
    Cl <- Cl_pop * exp(omega_Cl)
    d/dt(central) <- -Cl / V * central
    f(central) <- p
    Cc <- central / V
    Cc ~ add(a) + prop(b) + combined1()
  })
}

.ivpModel <- "DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {p, V, Cl}

EQUATION:
Cc = pkmodel(p, V, Cl)

OUTPUT:
output = Cc
"

.ivpPar <- list(p=.mlxPar(0.7, dist="logitNormal"), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3))

## the truth infuses over its DUR; the file gives the RATE (a dose without
## a duration is a bolus, RATE=.)
.ivpWrite <- function(columns) {
  function(d) {
    .w <- .kitMonolixRows(d)
    .w$RATE <- ifelse(is.na(.w$AMT) | .w$DUR == 0, NA, .w$AMT / .w$DUR)
    .w[, columns]
  }
}

kitVariant("dose-ss-infusion", "dose-ss-infusion-p",
           "steady-state RATE infusion into pkmodel(p, V, Cl): p scales the rate, the duration AMT/RATE is kept",
           tags=c("dosing", "ss", "infusion"),
           sim=.ivpTruth,
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1, ss=1L, ii=12, DUR=2),
                     mlxDose(.id, 24, amt=100, cmt=1),
                     mlxObs(.id, c(0.5, 1.5, 2, 3, 6, 11.5, 12.5, 14, 18, 23.5, 25, 30, 36, 48), cmt=1))
           },
           write=.ivpWrite(c("ID", "TIME", "AMT", "RATE", "SS", "II", "DV")),
           model=.ivpModel,
           mlxtran=.mlxProject(.ivpPar, content=paste0(.mlxContent, "
RATE = {use=rate}
SS = {use=steadystate, nbdoses=7}
II = {use=interdoseinterval}")))

kitVariant("dose-addl-infusion", "dose-addl-infusion-p",
           "RATE infusions repeated with ADDL/II into pkmodel(p, V, Cl)",
           sim=.ivpTruth,
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1, ii=12, addl=3, DUR=2.5),
                     mlxObs(.id, c(0.5, 1, 2, 2.5, 3, 4, 6, 12, 36.5, 38, 39, 42, 48, 60), cmt=1))
           },
           write=.ivpWrite(c("ID", "TIME", "AMT", "RATE", "II", "ADDL", "DV")),
           model=.ivpModel,
           mlxtran=.mlxProject(.ivpPar, content=paste0(.mlxContent, "
RATE = {use=rate}
II = {use=interdoseinterval}
ADDL = {use=additionaldose}")))
