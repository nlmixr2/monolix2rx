## A bioavailability on infusions, ADDL repeats of an infusion and of a
## lagged oral dose, censoring on the second endpoint only, sparse IDs

## Monolix 2024R1 keeps the duration AMT/RATE and scales the rate by p
## (the truth infuses by duration; the file gives the RATE)
kitVariant("macro-iv-tlag-p", "macro-iv-p-rate",
           "iv(cmt=1, Tlag, p) with RATE infusions: p scales the rate, the duration AMT/RATE is kept",
           tags=c("pk", "macro", "infusion"),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, c(0, 12), amt=100, cmt=1, DUR=2.5),
                     mlxObs(.id, c(0.5, 1, 2, 2.5, 3, 3.5, 4, 6, 12.5, 13, 14, 15, 16, 20, 24), cmt=1))
           },
           write=function(d) {
             d <- .kitMonolixRows(d)
             d$RATE <- ifelse(is.na(d$AMT), NA, d$AMT / d$DUR)
             d[, c("ID", "TIME", "AMT", "RATE", "DV")]
           },
           mlxtran=.mlxProject(list(Tlag=.mlxPar(0.75, 0.3), p=.mlxPar(0.8, dist="logitNormal"),
                                    V=.mlxPar(10, 0.2), Cl=.mlxPar(2, 0.3)),
                               content=paste0(.mlxContent, "
RATE = {use=rate}")))

## with a duration rxode2 also keeps the duration and scales the rate
kitVariant("macro-iv-p-rate", "macro-iv-p-tinf",
           "iv(cmt=1, Tlag, p) with infusions given by their duration (use=infusiontime)",
           tags=c("pk", "macro", "infusion"),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, c(0, 12), amt=100, cmt=1, DUR=2.5, TINF=2.5),
                     mlxObs(.id, c(0.5, 1, 2, 2.5, 3, 3.5, 4, 6, 12.5, 13, 14, 15, 16, 20, 24), cmt=1))
           },
           write=function(d) .kitMonolixRows(d)[, c("ID", "TIME", "AMT", "TINF", "DV")],
           mlxtran=.kitSub("macro-iv-p-rate", "mlxtran", "RATE = {use=rate}", "TINF = {use=infusiontime}"))

kitVariant("dose-infusion-rate", "dose-addl-infusion",
           "one RATE infusion record repeated with ADDL/II",
           tags=c("dosing", "infusion"),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, rate=40, cmt=1, ii=12, addl=3),
                     mlxObs(.id, c(0.5, 1, 2, 2.5, 3, 4, 6, 12, 36.5, 38, 39, 42, 48, 60), cmt=1))
           },
           columns=c("ID", "TIME", "AMT", "RATE", "II", "ADDL", "DV"),
           mlxtran=.mlxProject(.ivPar, content=paste0(.mlxContent, "
RATE = {use=rate}
II = {use=interdoseinterval}
ADDL = {use=additionaldose}")))

kitVariant("pkmodel-oral-tlag", "dose-addl-tlag",
           "pkmodel(Tlag, ka, V, Cl) with doses repeated by ADDL/II",
           tags=c("dosing", "pkmodel"),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1, ii=12, addl=3),
                     mlxObs(.id, c(pkTimes(12), 36.25, 36.5, 37, 38, 40, 48, 60, 72), cmt=2))
           },
           columns=c("ID", "TIME", "AMT", "II", "ADDL", "DV"),
           mlxtran=.mlxProject(list(Tlag=.mlxPar(0.5, 0.2), ka=.mlxPar(1.2, 0.3),
                                    V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3)),
                               content=paste0(.mlxContent, "
II = {use=interdoseinterval}
ADDL = {use=additionaldose}")))

## the metabolite (YTYPE 2) is censored below 0.3; the parent never is
kitVariant("two-endpoints-parent-metabolite", "data-cens-second-endpoint",
           "CENS on the second YTYPE endpoint only, with CENS=0 on every parent row",
           tags=c("data", "cens", "endpoints"),
           postSim=function(d, s) {
             .m <- match(d$ROWID, s$ROWID)
             .obs <- d$EVID == 0 & d$MDV == 0 & !is.na(.m)
             d$DV[.obs] <- signif(s$sim[.m[.obs]], 6)
             d$CENS <- as.integer(.obs & d$DVID == 2L & d$DV < 0.3)
             d$DV[d$CENS == 1L] <- 0.3
             d
           },
           dryData=function(m, sim) {
             .t <- sim$data[sim$data$EVID == 0 & sim$data$MDV == 0, ]
             if (!any(.t$CENS == 1L)) return("the simulation has no censored metabolite")
             .d <- m$monolixData
             .d <- .d[!is.na(.d$dv), ]
             .k <- function(id, time, cmt) paste(id, time, cmt)
             .m <- match(.k(.t$ID, .t$TIME, paste0("y", .t$DVID)), .k(.d$id, .d$time, .d$cmt))
             if (anyNA(.m)) return("observations missing")
             .bad <- c(cens=!identical(as.integer(.d$cens[.m]), .t$CENS),
                       dv=!isTRUE(all.equal(.d$dv[.m], .t$DV)))
             if (any(.bad)) paste("differs from the data:", paste(names(.bad)[.bad], collapse=", "))
           },
           write=function(d) {
             d <- .kitMonolixRows(d)
             d$YTYPE <- ifelse(is.na(d$DV), NA, d$DVID)
             d$CENS[is.na(d$DV)] <- NA
             d[, c("ID", "TIME", "AMT", "DV", "YTYPE", "CENS")]
           },
           mlxtran=.kitSub("two-endpoints-parent-metabolite", "mlxtran", "YTYPE = {use=observationtype}",
                           "YTYPE = {use=observationtype}\nCENS = {use=censored}"))

## numeric IDs with gaps and more digits than the subject count
kitVariant("pkmodel-oral-1cmt", "data-id-sparse",
           "numeric subject IDs with gaps (7, 114, 221, ...)",
           tags=c("data"),
           data=function(nSub) {
             .id <- 107L * seq_len(nSub) - 100L
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxObs(.id, pkTimes(48), cmt=2))
           })
