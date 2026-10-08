## Data set reading: delimiters, file= forms, ignored columns, string IDs

## write= for another delimiter; `fun` edits the written data.frame
.writeDelim <- function(columns, sep, fun=identity) {
  function(d) {
    .w <- fun(.kitMonolixRows(d))[, columns, drop=FALSE]
    .w[] <- lapply(.w, function(x) {
      .r <- if (is.numeric(x)) trimws(formatC(x, digits=15, format="fg")) else as.character(x)
      .r[is.na(x)] <- "."
      .r
    })
    c(paste(names(.w), collapse=sep), do.call(paste, c(.w, sep=sep)))
  }
}

.dataProject <- function(...) {
  .mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3)), ...)
}

kitVariant("pkmodel-oral-1cmt", "data-semicolon-ignore",
           "semicolon delimiter, an ignored text column and a dose-only AMT column with '.'",
           tags=c("data"),
           write=.writeDelim(c("ID", "TIME", "AMT", "DV", "NOTE"), ";", function(w) {
             w$NOTE <- ifelse(is.na(w$AMT), "obs", "dose")
             w
           }),
           mlxtran=.dataProject(delimiter="semicolon",
                                content=paste0(.mlxContent, "\nNOTE = {use=ignore}")))

## the Monolix 2024 file={path=} form and a data set in a subdirectory
kitVariant("pkmodel-oral-1cmt", "data-tab-subdir-path",
           "tab-delimited data in a subdirectory, given as file={path='data/pk.txt'}",
           tags=c("data", "mlx2024"),
           dataFile="data/pk.txt",
           write=.writeDelim(c("ID", "TIME", "AMT", "DV"), "\t"),
           mlxtran=.dataProject(delimiter="tab", file="{path='{{DATA}}'}"))

## the truth uses the same character IDs; the file lists the subjects in
## reverse order
kitVariant("pkmodel-oral-1cmt", "data-string-id",
           "character subject identifiers (S-001 ...) listed out of order",
           tags=c("data"),
           data=function(nSub) {
             .id <- sprintf("S-%03d", seq_len(nSub))
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxObs(.id, pkTimes(48), cmt=2))
           },
           write=.writeDelim(c("ID", "TIME", "AMT", "DV"), ",", function(w) {
             w[order(-match(w$ID, unique(w$ID)), seq_len(nrow(w))), ]
           }),
           mlxtran=.dataProject())

## observations flagged MDV=1 carry a value that must be ignored
kitVariant("pkmodel-oral-1cmt", "data-mdv",
           "MDV column (use=missingdependentvariable) with flagged observations",
           tags=c("data", "mdv"),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxObs(.id, pkTimes(48), cmt=2),
                     mlxObs(.id, c(5, 30), cmt=2, mdv=1L))
           },
           write=.writeDelim(c("ID", "TIME", "AMT", "DV", "MDV"), ",", function(w) {
             w$DV[w$MDV == 1L] <- 999
             w
           }),
           mlxtran=.dataProject(content=paste0(.mlxContent, "\nMDV = {use=missingdependentvariable}")))

## below LOQ: CENS=1, DV=LOQ; interval censored with LIMIT=0
kitVariant("pkmodel-oral-1cmt", "data-cens-limit",
           "left-censored observations (use=censored) with a LIMIT column (use=limit)",
           tags=c("data", "cens"),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxObs(.id, c(pkTimes(48), 72, 96), cmt=2))
           },
           columns=c("ID", "TIME", "AMT", "DV", "CENS", "LIMIT"),
           postSim=function(d, s) {
             .m <- match(d$ROWID, s$ROWID)
             .obs <- d$EVID == 0 & d$MDV == 0 & !is.na(.m)
             d$DV[.obs] <- signif(s$sim[.m[.obs]], 6)
             d$CENS <- as.integer(.obs & d$DV < 0.1)
             d$DV[d$CENS == 1L] <- 0.1
             d$LIMIT <- ifelse(d$CENS == 1L, 0, NA_real_)
             d
           },
           dryData=function(m, sim) {
             .t <- sim$data[sim$data$EVID == 0 & sim$data$MDV == 0, ]
             .d <- m$monolixData
             .d <- .d[!is.na(.d$dv), ]
             .m <- match(paste(.t$ID, .t$TIME), paste(.d$id, .d$time))
             if (anyNA(.m)) return("observations missing")
             .bad <- c(cens=!identical(as.integer(.d$cens[.m]), .t$CENS),
                       limit=!isTRUE(all.equal(.d$limit[.m], .t$LIMIT)),
                       dv=!isTRUE(all.equal(.d$dv[.m], .t$DV)))
             if (any(.bad)) paste("differs from the data:", paste(names(.bad)[.bad], collapse=", "))
           },
           mlxtran=.dataProject(content=paste0(.mlxContent, "
CENS = {use=censored}
LIMIT = {use=limit}")))

## two time-varying regressors; the data columns (REGA, REGB) and the
## model regressors (crcl, alb) differ in name, Monolix matches them by order
.regTruth <- function() {
  ini({
    ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
    omega_ka ~ 0.09; omega_V ~ 0.04; omega_Cl ~ 0.09
    a <- 0.05; b <- 0.1
  })
  model({
    ka <- ka_pop * exp(omega_ka)
    V <- V_pop * exp(omega_V)
    Cl <- Cl_pop * exp(omega_Cl)
    Cli <- Cl * (CLCR / 100)^0.75 * (ALB / 4)^0.5
    d/dt(depot) <- -ka * depot
    d/dt(central) <- ka * depot - Cli / V * central
    Cc <- central / V
    Cc ~ add(a) + prop(b) + combined1()
  })
}

.regData <- function(nSub) {
  .id <- seq_len(nSub)
  .d <- mlxBind(mlxDose(.id, c(0, 24), amt=100, cmt=1),
                mlxObs(.id, c(pkTimes(24), 25, 26, 28, 32, 36, 48), cmt=2))
  ## declining renal function with a step at 24 h; albumin rising
  .base <- 60 + 10 * (.d$ID %% 5)
  .d$CLCR <- signif(.base - 0.4 * .d$TIME - ifelse(.d$TIME >= 24, 15, 0), 6)
  .d$ALB <- signif(3 + 0.02 * .d$TIME + 0.1 * (.d$ID %% 3), 6)
  .d
}

kitCase(
  name="data-regressor",
  covers="two time-varying regressors (use=regressor) on Cl, data columns REGA/REGB matched by order to model regressors crcl/alb",
  tags=c("data", "regressor"),
  sim=.regTruth,
  data=.regData,
  write=.writeDelim(c("ID", "TIME", "AMT", "DV", "REGA", "REGB"), ",", function(w) {
    w$REGA <- w$CLCR
    w$REGB <- w$ALB
    w
  }),
  model="DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl, crcl, alb}
crcl = {use=regressor}
alb = {use=regressor}

PK:
depot(target=Ad)

EQUATION:
Cli = Cl*(crcl/100)^0.75*(alb/4)^0.5
ddt_Ad = -ka*Ad
ddt_Ac = ka*Ad - Cli/V*Ac
Cc = Ac/V

OUTPUT:
output = Cc
",
  mlxtran=.dataProject(content=paste0(.mlxContent, "\nREGA = {use=regressor}\nREGB = {use=regressor}")))

## string categories with a reference that is not alphabetically first;
## the truth uses a 0/1 column, the file the strings
kitVariant("pkmodel-oral-1cmt", "data-cat-string",
           "categorical covariate with string categories (SEX F/M, reference M) on Cl",
           tags=c("data", "covariate", "categorical"),
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3; beta_Cl_SEX_F <- -0.35
               omega_ka ~ 0.09; omega_V ~ 0.04; omega_Cl ~ 0.09
               a <- 0.05; b <- 0.1
             })
             model({
               ka <- ka_pop * exp(omega_ka)
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(beta_Cl_SEX_F * FEMALE + omega_Cl)
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               Cc ~ add(a) + prop(b) + combined1()
             })
           },
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxObs(.id, pkTimes(48), cmt=2),
                     cov=mlxCov(nSub, FEMALE=function(n) rep_len(c(0L, 1L, 1L), n)))
           },
           write=.writeDelim(c("ID", "TIME", "AMT", "DV", "SEX"), ",", function(w) {
             w$SEX <- ifelse(w$FEMALE == 1L, "F", "M")
             w
           }),
           mlxtran=.mlxProject(
             list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2),
                  Cl=.mlxPar(3, 0.3, extra=", covariate=SEX, coefficient={beta_Cl_SEX_F, 0}")),
             params=c(beta_Cl_SEX_F=-0.35), indInput="SEX",
             content=paste0(.mlxContent, "\nSEX = {use=covariate, type=categorical}"),
             covariate="[COVARIATE]
input = SEX

SEX = {type=categorical, categories={'F', 'M'}}",
             indDecl="SEX = {type=categorical, categories={'F', 'M'}}"))

## lines flagged by an ignoredline column (here named MDV) are dropped,
## doses as well as observations
kitVariant("pkmodel-oral-1cmt", "data-ignoredline",
           "MDV = {use=ignoredline}: flagged dose and observation lines are ignored",
           tags=c("data", "mdv"),
           write=.writeDelim(c("ID", "TIME", "AMT", "DV", "MDV"), ",", function(w) {
             w$MDV <- 0L
             .x <- w[!duplicated(w$ID), ]
             .x$TIME <- 6
             .x$AMT <- 1000
             .x$DV <- NA
             .x$MDV <- 1L
             .o <- .x
             .o$TIME <- 7
             .o$AMT <- NA
             .o$DV <- 999
             .w <- rbind(w, .x, .o)
             .w[order(match(.w$ID, unique(w$ID)), .w$TIME), ]
           }),
           mlxtran=.dataProject(content=paste0(.mlxContent, "\nMDV = {use=ignoredline}")))
