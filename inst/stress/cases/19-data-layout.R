## Data set layouts: column names, subject order, shared lines, negative
## times, subjects without doses

kitVariant("pkmodel-oral-1cmt", "data-col-names",
           "data columns not named ID/TIME/AMT/DV (SUBJ, HOUR, DOSE, Y)",
           tags=c("data"),
           write=function(d) {
             .w <- .kitMonolixRows(d)[, c("ID", "TIME", "AMT", "DV")]
             names(.w) <- c("SUBJ", "HOUR", "DOSE", "Y")
             .w
           },
           mlxtran=.dataProject(content="SUBJ = {use=identifier}
HOUR = {use=time}
DOSE = {use=amount}
Y = {use=observation, name=CONC, type=continuous}"))

## Monolix keeps the order of the file; rxode2 sorts by id
kitVariant("pkmodel-oral-1cmt", "data-id-unsorted",
           "subjects in descending ID order in the data file",
           tags=c("data"),
           write=function(d) {
             .w <- .kitMonolixRows(d)[, c("ID", "TIME", "AMT", "DV")]
             .w[order(-.w$ID, seq_len(nrow(.w))), ]
           })

## move each observation onto the dose line at its time
.shareDoseObs <- function(d) {
  .w <- .kitMonolixRows(d)[, c("ID", "TIME", "AMT", "DV", "EVID")]
  .dose <- which(.w$EVID == 1L)
  .obs <- which(.w$EVID == 0L)
  .m <- match(paste(.w$ID[.dose], .w$TIME[.dose]), paste(.w$ID[.obs], .w$TIME[.obs]))
  .w$DV[.dose[!is.na(.m)]] <- .w$DV[.obs[.m[!is.na(.m)]]]
  .w <- .w[-.obs[.m[!is.na(.m)]], c("ID", "TIME", "AMT", "DV")]
  if (!any(!is.na(.w$AMT) & !is.na(.w$DV))) stop("no shared dose/observation line")
  .w
}

## a trough sample on the dose line; for an oral dose the order of the two
## does not change the prediction
kitVariant("pkmodel-oral-1cmt", "data-dose-obs-line",
           "a dose and an observation on the same line (AMT and DV both set)",
           tags=c("data", "dosing"),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, c(0, 12, 24), amt=100, cmt=1),
                     mlxObs(.id, c(1, 2, 4, 8, 12, 14, 24, 26, 30, 36, 48), cmt=2))
           },
           write=.shareDoseObs,
           dryData=function(m, sim) {
             .d <- m$monolixData
             .t <- .d$time %in% c(12, 24)
             .n <- c(dose=sum(.t & !is.na(.d$amt) & is.na(.d$dv)),
                     obs=sum(.t & is.na(.d$amt) & !is.na(.d$dv)))
             .want <- 2 * length(unique(.d$id))
             if (any(.n != .want)) paste("the shared lines gave", .n[["dose"]], "doses and",
                                         .n[["obs"]], "observations at 12 and 24")
           })

## an IV bolus, where the order matters: Monolix and rxode2 both give the
## dose before the observation
kitVariant("pkmodel-iv-2cmt", "data-dose-obs-line-iv",
           "IV bolus and an observation on the same line",
           tags=c("data", "dosing"),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxObs(.id, c(0.5, 2, 6, 12, 14, 24, 26, 36, 48), cmt=1),
                     mlxDose(.id, c(0, 12, 24), amt=100, cmt=1))
           },
           write=.shareDoseObs)

kitVariant("pkmodel-oral-1cmt", "data-time-negative",
           "pre-dose observations at negative times (the dose at 0)",
           tags=c("data"),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxObs(.id, c(-2, -1, pkTimes(48)), cmt=2))
           })

## placebo subjects: the response stays at its baseline Kin/Kout
kitVariant("ode-turnover", "data-placebo",
           "subjects without any dose (placebo) next to dosed subjects",
           tags=c("data", "pd"),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id[.id %% 2 == 1], 0, amt=100, cmt=1),
                     mlxObs(.id, c(pkTimes(72), 96, 120), cmt=3))
           })

kitVariant("pkmodel-oral-1cmt", "out-name-conc",
           "prediction not named Cc (Conc = pkmodel(...), output = Conc)",
           tags=c("output"),
           sim=.oralErrTruth(quote(add(a) + prop(b) + combined1()),
                             list(quote(a <- 0.05), quote(b <- 0.1)),
                             out=quote(Conc), extra=list(quote(Conc <- Cc))),
           model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl}

EQUATION:
Conc = pkmodel(ka, V, Cl)

OUTPUT:
output = Conc
",
           mlxtran=.mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3)),
                               pred="Conc"))
