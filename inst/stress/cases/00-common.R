## Shared builders for the case files (loaded first)

## One [INDIVIDUAL] parameter: Monolix `sd` is the square root of the
## rxode2 omega; NULL sd is no-variability.  `extra` is appended to the
## definition (like ", covariate=lw70, coefficient=beta_Cl_lw70").
.mlxPar <- function(pop, sd=NULL, dist="logNormal", extra="") {
  list(pop=pop, sd=sd, dist=dist, extra=extra)
}

.mlxContent <- "ID = {use=identifier}
TIME = {use=time}
AMT = {use=amount}
DV = {use=observation, name=CONC, type=continuous}"

## A one-endpoint (CONC) project.  `par`: named list of .mlxPar();
## `err`/`errPar`: the error model and its parameter values; `params`:
## other population parameters (covariate coefficients ...);
## `indInput`: extra [INDIVIDUAL] inputs (covariates); `covariate`: a
## [COVARIATE] section; `indExtra`: extra [INDIVIDUAL] DEFINITION lines.
.mlxProject <- function(par, err="combined1(a, b)", errPar=c(a=0.05, b=0.1),
                        params=NULL, content=.mlxContent, indInput=NULL,
                        covariate=NULL, indExtra=NULL) {
  .nm <- names(par)
  .in <- unlist(lapply(.nm, function(n) {
    c(paste0(n, "_pop"), if (!is.null(par[[n]]$sd)) paste0("omega_", n))
  }))
  .def <- vapply(.nm, function(n) {
    .p <- par[[n]]
    paste0(n, " = {distribution=", .p$dist, ", typical=", n, "_pop, ",
           if (is.null(.p$sd)) "no-variability" else paste0("sd=omega_", n),
           .p$extra, "}")
  }, character(1))
  .val <- function(n, v) paste0(n, " = {value=", v, ", method=MLE}")
  .pv <- c(vapply(.nm, function(n) .val(paste0(n, "_pop"), par[[n]]$pop), ""),
           unlist(lapply(.nm, function(n) {
             if (!is.null(par[[n]]$sd)) .val(paste0("omega_", n), par[[n]]$sd)
           })),
           if (length(params)) .val(names(params), params),
           .val(names(errPar), errPar))
  paste0("<DATAFILE>

[FILEINFO]
file = '{{DATA}}'
delimiter = comma
header = {{{HEADER}}}

[CONTENT]
", content, "

<MODEL>
", if (!is.null(covariate)) paste0("\n", covariate, "\n"), "
[INDIVIDUAL]
input = {", paste(c(.in, names(params), indInput), collapse=", "), "}

DEFINITION:
", paste(c(.def, indExtra), collapse="\n"), "

[LONGITUDINAL]
input = {", paste(names(errPar), collapse=", "), "}

file = '{{MODEL}}'

DEFINITION:
CONC = {distribution=normal, prediction=Cc, errorModel=", err, "}

<FIT>
data = CONC
model = CONC

<PARAMETER>
", paste(.pv, collapse="\n"), "

<MONOLIX>

{{TASKS}}

{{SETTINGS}}
")
}
