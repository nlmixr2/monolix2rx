## Other spellings of the same project: lowercase names, options in
## another order, exponent notation, empty fields, Windows line endings

## Monolix writes logNormal/logitNormal; that it reads the lowercase
## spelling is to confirm in run mode
kitVariant("pkmodel-oral-1cmt", "param-dist-lowercase",
           "lowercase distribution names (lognormal, logitnormal) in [INDIVIDUAL] and lognormal in [LONGITUDINAL]",
           tags=c("params", "syntax"),
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3; fr_pop <- 0.7
               omega_ka ~ 0.09; omega_V ~ 0.04; omega_Cl ~ 0.09; omega_fr ~ 0.25
               a <- 0.2
             })
             model({
               ka <- ka_pop * exp(omega_ka)
               V <- V_pop + omega_V
               Cl <- Cl_pop * exp(omega_Cl)
               fr <- expit(logit(fr_pop) + omega_fr)
               d/dt(depot) <- -ka * depot
               f(depot) <- fr
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ lnorm(a)
             })
           },
           model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl, fr}

EQUATION:
Cc = pkmodel(ka, V, Cl, p=fr)

OUTPUT:
output = Cc
",
           mlxtran=.mlxProject(list(ka=.mlxPar(1.2, 0.3, dist="lognormal"),
                                    V=.mlxPar(30, 0.2, dist="normal"),
                                    Cl=.mlxPar(3, 0.3, dist="lognormal"),
                                    fr=.mlxPar(0.7, 0.5, dist="logitnormal")),
                               err="constant(a)", errPar=c(a=0.2), obsDist="lognormal"))

## Monolix writes distribution= first; a hand-written project may not
kitVariant("cov-wt-lw70", "param-option-order",
           "[INDIVIDUAL] DEFINITION: options in another order (distribution= last, covariate= first)",
           tags=c("params", "syntax", "covariate"),
           mlxtran=local({
             .m <- .kitEnv$cases[["cov-wt-lw70"]]$mlxtran
             .m <- sub("ka = {distribution=logNormal, typical=ka_pop, sd=omega_ka}",
                       "ka = {typical=ka_pop, sd=omega_ka, distribution=logNormal}", .m, fixed=TRUE)
             .m <- sub("V = {distribution=logNormal, typical=V_pop, sd=omega_V, covariate=lw70, coefficient=beta_V_lw70}",
                       "V = {covariate=lw70, coefficient=beta_V_lw70, sd=omega_V, distribution=logNormal, typical=V_pop}",
                       .m, fixed=TRUE)
             if (!grepl("distribution=logNormal}", .m, fixed=TRUE) ||
                   !grepl("V = {covariate=", .m, fixed=TRUE)) {
               stop("param-option-order: the cov-wt-lw70 definitions changed")
             }
             .m
           }))

kitVariant("pkmodel-oral-1cmt", "syntax-exponent",
           "numbers in exponent notation (1.2e+00, 3E1, 3e0, 5.0E-02) in <PARAMETER> and the model file",
           tags=c("syntax"),
           model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl}

EQUATION:
Cmg = pkmodel(ka, V, Cl)
Cc = 1e-3*Cmg*1.0E+3

OUTPUT:
output = Cc
",
           mlxtran=.mlxProject(list(ka=.mlxPar("1.2e+00", "3e-1"), V=.mlxPar("3E1", "2.0E-1"),
                                    Cl=.mlxPar("3e0", "0.3")),
                               errPar=c(a="5.0E-02", b="1e-1")))

## an empty field as missing, like "." (to confirm in run mode)
kitVariant("pkmodel-oral-1cmt", "data-empty-missing",
           "missing AMT and DV written as empty fields instead of '.'",
           tags=c("data"),
           write=function(d) {
             .w <- .kitMonolixRows(d)[, c("ID", "TIME", "AMT", "DV")]
             .w[] <- lapply(.w, function(x) {
               .r <- trimws(formatC(x, digits=15, format="fg"))
               .r[is.na(x)] <- ""
               .r
             })
             c(paste(names(.w), collapse=","), do.call(paste, c(.w, sep=",")))
           })

## a project saved on Windows; readLines() drops the CR of the project and
## model file, so the import side mostly checks the data reader
kitVariant("pkmodel-oral-1cmt", "syntax-crlf",
           "Windows line endings (CRLF) in the project, the model file and the data",
           tags=c("syntax", "data"),
           crlf=TRUE,
           write=function(d) {
             .w <- .kitMonolixRows(d)[, c("ID", "TIME", "AMT", "DV")]
             .w[] <- lapply(.w, function(x) {
               .r <- trimws(formatC(x, digits=15, format="fg"))
               .r[is.na(x)] <- "."
               .r
             })
             paste0(c(paste(names(.w), collapse=","), do.call(paste, c(.w, sep=","))), "\r")
           })
