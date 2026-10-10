## Data records: observation types other than 1/2, replicate samples, a
## subject without observations

## the parent/metabolite project with YTYPE values `ytype` (parent first)
## and the [CONTENT] `yname` list
.pmVariant <- function(name, covers, ytype, yname, pred=c(y1="Cc", y2="Cm"),
                       err=c("combined1(a1, b1)", "combined1(a2, b2)")) {
  kitVariant("two-endpoints-parent-metabolite", name, covers,
             tags=c("endpoints", "data"),
             write=function(d) {
               d <- .kitMonolixRows(d)
               d$YTYPE <- ifelse(is.na(d$DV), NA, ytype[d$DVID])
               d[, c("ID", "TIME", "AMT", "DV", "YTYPE")]
             },
             mlxtran=.mlxProject(
               list(ka=.mlxPar(1.2), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3),
                    Vm=.mlxPar(20), Clm=.mlxPar(2, 0.3), fm=.mlxPar(0.6)),
               pred=pred, err=err, errPar=c(a1=0.05, b1=0.1, a2=0.02, b2=0.15),
               content=paste0("ID = {use=identifier}
TIME = {use=time}
AMT = {use=amount}
DV = {use=observation, name={y1, y2}, yname={", paste0("'", yname, "'", collapse=", "),
"}, type={continuous, continuous}}
YTYPE = {use=observationtype}")))
}

.pmVariant("data-ytype-string", "observation types given as strings (YTYPE PARENT/METAB)",
           ytype=c("PARENT", "METAB"), yname=c("PARENT", "METAB"))

.pmVariant("data-ytype-codes", "observation types that are not 1 and 2 (YTYPE 5 parent, 2 metabolite)",
           ytype=c(5L, 2L), yname=c("5", "2"))

## y1 is the metabolite: YTYPE 2 is listed first in yname
.pmVariant("data-ytype-swapped", "yname listing YTYPE 2 first, so y1 (the first observation) is the metabolite",
           ytype=c(1L, 2L), yname=c("2", "1"), pred=c(y1="Cm", y2="Cc"),
           err=c("combined1(a2, b2)", "combined1(a1, b1)"))

kitVariant("pkmodel-oral-1cmt", "data-replicate-obs",
           "replicate samples (two or three observations at the same time)",
           tags=c("data"),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxObs(.id, pkTimes(48), cmt=2),
                     mlxObs(.id, c(2, 8, 8), cmt=2))
           })

## whether Monolix keeps such a subject is to confirm in run mode
kitVariant("pkmodel-oral-1cmt", "data-no-obs-subject",
           "a subject with a dose and no observations",
           tags=c("data"),
           data=function(nSub) {
             .id <- seq_len(nSub)
             .obs <- .id[.id != 2L]
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxObs(.obs, pkTimes(48), cmt=2))
           })
