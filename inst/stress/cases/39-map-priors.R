## Bayesian priors (MAP): Monolix 2024R1 writes the prior in a [POPULATION]
## DEFINITION: block with literal numbers and the population parameter with
## method=MAP.  The import reads Monolix's final (MAP) estimates and writes
## the prior as an ini() prior() on the imported theta.

## add `prior` lines to [POPULATION] and make `pars` MAP
.mapProject <- function(base, prior, pars) {
  .p <- .kitSub(base, "mlxtran", "<MODEL>\n",
                paste0("<MODEL>\n\n[POPULATION]\nDEFINITION:\n", paste(prior, collapse="\n"), "\n"))
  for (.n in pars) {
    .p <- sub(paste0("(", .n, " = \\{value=[^,]+), method=MLE\\}"), "\\1, method=MAP}", .p)
    if (!grepl(paste0(.n, " = \\{value=[^,]+, method=MAP\\}"), .p)) {
      stop("no MLE value line for ", .n, call.=FALSE)
    }
  }
  .p
}

kitVariant("pkmodel-oral-1cmt", "param-map-lognormal",
           "a log-normal MAP prior on ka_pop ([POPULATION] DEFINITION:, method=MAP)",
           tags=c("params", "prior"),
           mlxtran=.mapProject("pkmodel-oral-1cmt",
                               "ka_pop = {distribution=logNormal, typical=1.2, sd=0.25}", "ka_pop"))

## the prior has the distribution of its parameter (V is normal here)
kitVariant("param-normal-novar", "param-map-normal",
           "a normal MAP prior on V_pop of a normally distributed V",
           tags=c("params", "prior"),
           mlxtran=.mapProject("param-normal-novar",
                               "V_pop = {distribution=normal, typical=30, sd=5}", "V_pop"))
