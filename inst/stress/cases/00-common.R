## Shared builders for the case files (loaded first)

## One [INDIVIDUAL] parameter: Monolix `sd` is the square root of the
## rxode2 omega; NULL sd is no-variability.  `extra` is appended to the
## definition (like ", covariate=lw70, coefficient=beta_Cl_lw70"); `iov`
## is the inter-occasion sd (gamma_<name>, level id*occ).
.mlxPar <- function(pop, sd=NULL, dist="logNormal", extra="", iov=NULL) {
  list(pop=pop, sd=sd, dist=dist, extra=extra, iov=iov)
}

.mlxContent <- "ID = {use=identifier}
TIME = {use=time}
AMT = {use=amount}
DV = {use=observation, name=CONC, type=continuous}"

## A one-endpoint (CONC) project.  `par`: named list of .mlxPar();
## `err`/`errPar`: the error model and its parameter values; `params`:
## other population parameters (covariate coefficients ...);
## `indInput`: extra [INDIVIDUAL] inputs (covariates); `covariate`: a
## [COVARIATE] section; `indExtra`: extra [INDIVIDUAL] DEFINITION lines;
## `pred`: the model output observed; `covParams`: population parameters
## used only in [COVARIATE] (latent class probabilities); `indDecl`:
## [INDIVIDUAL] declarations (Monolix repeats categorical covariates there);
## `obsDist`/`obsExtra`: the observation distribution and extra fields
## (like ", min=0, max=1"); `delimiter`; `file`: the [FILEINFO] file value.
## Several endpoints: named `pred` (observation name = model output) and
## `err` vectors.
.mlxProject <- function(par, err="combined1(a, b)", errPar=c(a=0.05, b=0.1),
                        params=NULL, content=.mlxContent, indInput=NULL,
                        covariate=NULL, indExtra=NULL, pred="Cc",
                        covParams=NULL, indDecl=NULL, obsDist="normal",
                        obsExtra="", delimiter="comma", file="'{{DATA}}'") {
  .nm <- names(par)
  .in <- unlist(lapply(.nm, function(n) {
    c(paste0(n, "_pop"), if (!is.null(par[[n]]$sd)) paste0("omega_", n),
      if (!is.null(par[[n]]$iov)) paste0("gamma_", n))
  }))
  .def <- vapply(.nm, function(n) {
    .p <- par[[n]]
    .var <- if (!is.null(.p$iov) && !is.null(.p$sd)) {
      paste0("varlevel={id, id*occ}, sd={omega_", n, ", gamma_", n, "}")
    } else if (!is.null(.p$iov)) {
      paste0("varlevel=id*occ, sd=gamma_", n)
    } else if (is.null(.p$sd)) "no-variability" else paste0("sd=omega_", n)
    paste0(n, " = {distribution=", .p$dist, ", typical=", n, "_pop, ", .var, .p$extra, "}")
  }, character(1))
  .val <- function(n, v) paste0(n, " = {value=", v, ", method=MLE}")
  ## several endpoints: `pred`/`err` vectors, named by observation name
  .obs <- if (length(pred) > 1L) names(pred) else "CONC"
  .fit <- if (length(.obs) > 1L) paste0("{", paste(.obs, collapse=", "), "}") else .obs
  .pv <- c(vapply(.nm, function(n) .val(paste0(n, "_pop"), par[[n]]$pop), ""),
           unlist(lapply(.nm, function(n) {
             c(if (!is.null(par[[n]]$sd)) .val(paste0("omega_", n), par[[n]]$sd),
               if (!is.null(par[[n]]$iov)) .val(paste0("gamma_", n), par[[n]]$iov))
           })),
           if (length(params)) .val(names(params), params),
           if (length(covParams)) .val(names(covParams), covParams),
           .val(names(errPar), errPar))
  paste0("<DATAFILE>

[FILEINFO]
file = ", file, "
delimiter = ", delimiter, "
header = {{{HEADER}}}

[CONTENT]
", content, "

<MODEL>
", if (!is.null(covariate)) paste0("\n", covariate, "\n"), "
[INDIVIDUAL]
input = {", paste(c(.in, names(params), indInput), collapse=", "), "}
", if (length(indDecl)) paste0("\n", paste(indDecl, collapse="\n"), "\n"), "
DEFINITION:
", paste(c(.def, indExtra), collapse="\n"), "

[LONGITUDINAL]
input = {", paste(names(errPar), collapse=", "), "}

file = '{{MODEL}}'

DEFINITION:
", paste0(.obs, " = {distribution=", obsDist, obsExtra, ", prediction=", pred,
          ", errorModel=", err, "}", collapse="\n"), "

<FIT>
data = ", .fit, "
model = ", .fit, "

<PARAMETER>
", paste(.pv, collapse="\n"), "

<MONOLIX>

{{TASKS}}

{{SETTINGS}}
")
}
