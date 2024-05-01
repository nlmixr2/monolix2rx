
<!-- README.md is generated from README.Rmd. Please edit that file -->

# monolix2rx <img src="man/figures/logo.png" align="right" height="138" alt="" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/nlmixr2/monolix2rx/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nlmixr2/monolix2rx/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/nlmixr2/monolix2rx/branch/main/graph/badge.svg)](https://app.codecov.io/gh/nlmixr2/monolix2rx?branch=main)
[![CRAN
version](http://www.r-pkg.org/badges/version/monolix2rx)](https://cran.r-project.org/package=monolix2rx)
[![CRAN total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/monolix2rx)](https://cran.r-project.org/package=monolix2rx)
[![CRAN total
downloads](https://cranlogs.r-pkg.org/badges/monolix2rx)](https://cran.r-project.org/package=monolix2rx)
[![CodeFactor](https://www.codefactor.io/repository/github/nlmixr2/monolix2rx/badge)](https://www.codefactor.io/repository/github/nlmixr2/monolix2rx)
![r-universe](https://nlmixr2.r-universe.dev/badges/monolix2rx)
<!-- badges: end -->

The goal of monolix2rx is to convert `Monolix` to `rxode2` to use for
simulation and sharing the model in an open-source framework.

## Installation

You can install the development version of monolix2rx from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nlmixr2/monolix2rx")
```

## Example

If you are trying to convert a Monolix to a rxode2 model you simply need
the path to the `mlxtran` file. For example, the classic demo of
theophylline is included in `monolix2rx` and can be imported below:

``` r
library(monolix2rx)
#> Loading required namespace: rxode2
# First load in the model; in this case the theo model
# This is modified from the Monolix demos by saving the model
# file as a text file (hence you can access without model library).
# Additionally some of the file paths were shortened so they could
# be included with monolix2rx

pkgTheo <- system.file("theo", package="monolix2rx")
mlxtranFile <- file.path(pkgTheo, "theophylline_project.mlxtran")

rx <- monolix2rx(mlxtranFile)
#> ℹ updating model values to final parameter estimates
#> ℹ done
#> ℹ reading run info (# obs, doses, Monolix Version, etc) from summary.txt
#> ℹ done
#> ℹ reading covariance from FisherInformation/covarianceEstimatesLin.txt
#> ℹ done
#> ℹ imported monolix and translated to rxode2 compatible data ($monolixData)
#> ℹ imported monolix ETAS (_SAEM) imported to rxode2 compatible data ($etaData)
#> ℹ imported monolix pred/ipred data to compare ($predIpredData)
#> ℹ solving ipred problem
#> ℹ done
#> ℹ solving pred problem
#> ℹ done

rx
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>      ka_pop       V_pop      Cl_pop           a           b 
#>  0.42699448 -0.78635157 -3.21457598  0.43327956  0.05425953 
#> 
#> Omega ($omega): 
#>           omega_ka    omega_V   omega_Cl
#> omega_ka 0.4503145 0.00000000 0.00000000
#> omega_V  0.0000000 0.01594701 0.00000000
#> omega_Cl 0.0000000 0.00000000 0.07323701
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2          central
#>  ── μ-referencing ($muRefTable): ──  
#>    theta      eta level
#> 1 ka_pop omega_ka    id
#> 2  V_pop  omega_V    id
#> 3 Cl_pop omega_Cl    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     description <- "The administration is extravascular with a first order absorption (rate constant ka).\nThe PK model has one compartment (volume V) and a linear elimination (clearance Cl).\nThis has been modified so that it will run without the model library"
#>     thetaMat <- lotri({
#>         ka_pop + V_pop + Cl_pop ~ c(0.09785, 0.00082606, 0.00041937, 
#>             -4.2833e-05, -6.7957e-06, 1.1318e-05)
#>         a + b ~ c(0.015333, -0.0026458, 0.00056232)
#>     })
#>     validation <- c("ipred relative difference compared to Monolix ipred: 0.04%; 95% percentile: (0%,0.52%); rtol=0.00038", 
#>         "ipred absolute difference compared to Monolix ipred: 95% percentile: (0.000362, 0.00848); atol=0.00254", 
#>         "pred relative difference compared to Monolix pred: 0%; 95% percentile: (0%,0%); rtol=6.6e-07", 
#>         "pred absolute difference compared to Monolix pred: 95% percentile: (1.6e-07, 1.27e-05); atol=3.66e-06", 
#>         "iwres relative difference compared to Monolix iwres: 0%; 95% percentile: (0.06%,32.22%); rtol=0.0153", 
#>         "iwres absolute difference compared to Monolix pred: 95% percentile: (0.000403, 0.0138); atol=0.00305")
#>     ini({
#>         ka_pop <- 0.426994483535611
#>         V_pop <- -0.786351566327091
#>         Cl_pop <- -3.21457597916301
#>         a <- c(0, 0.433279557549051)
#>         b <- c(0, 0.0542595276206251)
#>         omega_ka ~ 0.450314511978718
#>         omega_V ~ 0.0159470121255372
#>         omega_Cl ~ 0.0732370098834837
#>     })
#>     model({
#>         cmt(depot)
#>         cmt(central)
#>         ka <- exp(ka_pop + omega_ka)
#>         V <- exp(V_pop + omega_V)
#>         Cl <- exp(Cl_pop + omega_Cl)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(central) <- +ka * depot - Cl/V * central
#>         Cc <- central/V
#>         CONC <- Cc
#>         CONC ~ add(a) + prop(b) + combined1()
#>     })
#> }

# If you are only interseted in the parsing you can use `mlxtran`

mlx <- mlxtran(mlxtranFile)
#> ℹ reading run info (# obs, doses, Monolix Version, etc) from summary.txt
#> ℹ done
#> ℹ reading covariance from FisherInformation/covarianceEstimatesLin.txt
#> ℹ done

mlx
#> DESCRIPTION:
#> The administration is extravascular with a first order absorption (rate constant ka).
#> The PK model has one compartment (volume V) and a linear elimination (clearance Cl).
#> This has been modified so that it will run without the model library
#> 
#> <DATAFILE>
#> [FILEINFO]
#> ; parsed: $DATAFILE$FILEINFO$FILEINFO
#> file = 'data/theophylline_data.txt'
#> delimiter = tab
#> header = {ID, AMT, TIME, CONC, WEIGHT, SEX}
#> 
#> [CONTENT]
#> ; parsed: $DATAFILE$CONTENT$CONTENT
#> ID = {use=identifier}
#> TIME = {use=time}
#> AMT = {use=amount}
#> CONC = {use=observation, name=CONC, type=continuous}
#> WEIGHT = {use=covariate, type=continuous}
#> SEX = {use=covariate, type=categorical}
#> 
#> <MODEL>
#> [INDIVIDUAL]
#> ; parsed: $MODEL$INDIVIDUAL$INDIVIDUAL
#> input = {ka_pop, omega_ka, V_pop, omega_V, Cl_pop, omega_Cl}
#> 
#> DEFINITION:
#> ; parsed: $MODEL$INDIVIDUAL$DEFINITION
#> ka = {distribution=lognormal, typical=ka_pop, sd=omega_ka}
#> V = {distribution=lognormal, typical=V_pop, sd=omega_V}
#> Cl = {distribution=lognormal, typical=Cl_pop, sd=omega_Cl}
#> 
#> [LONGITUDINAL]
#> ; parsed: $MODEL$LONGITUDINAL$LONGITUDINAL
#> input = {a, b, ka, V, Cl}
#> file = 'oral1_1cpt_kaVCl.txt'
#> 
#> DEFINITION:
#> ; parsed: $MODEL$LONGITUDINAL$DEFINITION
#> CONC = {distribution=normal, prediction=Cc, errorModel=combined1(a, b)}
#> 
#> EQUATION:
#> 
#> ; PK model definition
#> Cc = pkmodel(ka, V, Cl)
#> 
#> OUTPUT:
#> ; parsed: $MODEL$LONGITUDINAL$OUTPUT
#> output = Cc
#> 
#> <FIT>
#> ; parsed: $FIT$FIT
#> data = {CONC}
#> model = {CONC}
#> 
#> <PARAMETER>
#> ; parsed: $PARAMETER$PARAMETER
#> Cl_pop = {value=0.1, method=MLE}
#> V_pop = {value=0.5, method=MLE}
#> a = {value=1, method=MLE}
#> b = {value=0.3, method=MLE}
#> ka_pop = {value=1, method=MLE}
#> omega_Cl = {value=1, method=MLE}
#> omega_V = {value=1, method=MLE}
#> omega_ka = {value=1, method=MLE}
#> 
#> <MONOLIX>
#> [TASKS]
#> ; parsed: $MONOLIX$TASKS$TASKS
#> populationParameters()
#> individualParameters(method = {conditionalMean, conditionalMode})
#> fim(method = Linearization)
#> logLikelihood(method = Linearization)
#> plotResult(method = {indfits, obspred, vpc, residualsscatter, residualsdistribution, parameterdistribution, covariatemodeldiagnosis, randomeffects, covariancemodeldiagnosis, saemresults})
#> 
#> [SETTINGS]
#> GLOBAL:
#> ; parsed: $MONOLIX$SETTINGS$GLOBAL
#> exportpath = 'tp'
#> 
#> ; unparsed sections:
#> ;  $MODEL$LONGITUDINAL$EQUATION

# this can be converted to a list
head(as.list(mlx))
#> $mlxtran
#> [1] ""
#> 
#> $DATAFILE
#> $DATAFILE$DATAFILE
#> list()
#> 
#> $DATAFILE$FILEINFO
#> $DATAFILE$FILEINFO$FILEINFO
#> $DATAFILE$FILEINFO$FILEINFO$file
#> [1] "data/theophylline_data.txt"
#> 
#> $DATAFILE$FILEINFO$FILEINFO$header
#> [1] "ID"     "AMT"    "TIME"   "CONC"   "WEIGHT" "SEX"   
#> 
#> $DATAFILE$FILEINFO$FILEINFO$delimiter
#> [1] "tab"
#> 
#> 
#> 
#> $DATAFILE$CONTENT
#> $DATAFILE$CONTENT$CONTENT
#> $DATAFILE$CONTENT$CONTENT$use1
#>               identifier                     time          eventidentifier 
#>                     "ID"                   "TIME"                       NA 
#>                   amount        interdoseinterval                 censored 
#>                    "AMT"                       NA                       NA 
#>                    limit          observationtype           administration 
#>                       NA                       NA                       NA 
#>              steadystate              observation                 occasion 
#>                       NA                   "CONC"                       NA 
#>                     rate           additionaldose missingdependentvariable 
#>                       NA                       NA                       NA 
#> 
#> $DATAFILE$CONTENT$CONTENT$cont
#> [1] "WEIGHT"
#> 
#> $DATAFILE$CONTENT$CONTENT$cat
#> $DATAFILE$CONTENT$CONTENT$cat$SEX
#> $DATAFILE$CONTENT$CONTENT$cat$SEX$cat
#> character(0)
#> 
#> $DATAFILE$CONTENT$CONTENT$cat$SEX$quote
#> logical(0)
#> 
#> 
#> 
#> $DATAFILE$CONTENT$CONTENT$reg
#> character(0)
#> 
#> $DATAFILE$CONTENT$CONTENT$nbdoses
#> [1] 7
#> 
#> $DATAFILE$CONTENT$CONTENT$yname
#> character(0)
#> 
#> $DATAFILE$CONTENT$CONTENT$ynameQuote
#> logical(0)
#> 
#> $DATAFILE$CONTENT$CONTENT$ytype
#> character(0)
#> 
#> $DATAFILE$CONTENT$CONTENT$ytypeQuote
#> logical(0)
#> 
#> $DATAFILE$CONTENT$CONTENT$name
#> [1] "CONC"
#> 
#> $DATAFILE$CONTENT$CONTENT$type
#> [1] "continuous"
#> 
#> 
#> 
#> 
#> $MODEL
#> $MODEL$MODEL
#> list()
#> 
#> $MODEL$INDIVIDUAL
#> $MODEL$INDIVIDUAL$INDIVIDUAL
#> $MODEL$INDIVIDUAL$INDIVIDUAL$input
#> [1] "ka_pop"   "omega_ka" "V_pop"    "omega_V"  "Cl_pop"   "omega_Cl"
#> 
#> $MODEL$INDIVIDUAL$INDIVIDUAL$cat
#> NULL
#> 
#> $MODEL$INDIVIDUAL$INDIVIDUAL$reg
#> character(0)
#> 
#> $MODEL$INDIVIDUAL$INDIVIDUAL$file
#> character(0)
#> 
#> 
#> $MODEL$INDIVIDUAL$DEFINITION
#> $MODEL$INDIVIDUAL$DEFINITION$vars
#> $MODEL$INDIVIDUAL$DEFINITION$vars$ka
#> $MODEL$INDIVIDUAL$DEFINITION$vars$ka$distribution
#> [1] "lognormal"
#> 
#> $MODEL$INDIVIDUAL$DEFINITION$vars$ka$typical
#> [1] "ka_pop"
#> 
#> $MODEL$INDIVIDUAL$DEFINITION$vars$ka$sd
#> [1] "omega_ka"
#> 
#> 
#> $MODEL$INDIVIDUAL$DEFINITION$vars$V
#> $MODEL$INDIVIDUAL$DEFINITION$vars$V$distribution
#> [1] "lognormal"
#> 
#> $MODEL$INDIVIDUAL$DEFINITION$vars$V$typical
#> [1] "V_pop"
#> 
#> $MODEL$INDIVIDUAL$DEFINITION$vars$V$sd
#> [1] "omega_V"
#> 
#> 
#> $MODEL$INDIVIDUAL$DEFINITION$vars$Cl
#> $MODEL$INDIVIDUAL$DEFINITION$vars$Cl$distribution
#> [1] "lognormal"
#> 
#> $MODEL$INDIVIDUAL$DEFINITION$vars$Cl$typical
#> [1] "Cl_pop"
#> 
#> $MODEL$INDIVIDUAL$DEFINITION$vars$Cl$sd
#> [1] "omega_Cl"
#> 
#> 
#> 
#> $MODEL$INDIVIDUAL$DEFINITION$fixed
#> numeric(0)
#> 
#> $MODEL$INDIVIDUAL$DEFINITION$cor
#> [1] level v1    v2    est  
#> <0 rows> (or 0-length row.names)
#> 
#> $MODEL$INDIVIDUAL$DEFINITION$est
#>      type     name fixed level
#> 1 typical   ka_pop FALSE   pop
#> 2      sd omega_ka FALSE    id
#> 3 typical    V_pop FALSE   pop
#> 4      sd  omega_V FALSE    id
#> 5 typical   Cl_pop FALSE   pop
#> 6      sd omega_Cl FALSE    id
#> 
#> $MODEL$INDIVIDUAL$DEFINITION$rx
#> [1] "ka <- exp(ka_pop + omega_ka)" "V <- exp(V_pop + omega_V)"   
#> [3] "Cl <- exp(Cl_pop + omega_Cl)"
#> 
#> 
#> 
#> $MODEL$LONGITUDINAL
#> $MODEL$LONGITUDINAL$LONGITUDINAL
#> $MODEL$LONGITUDINAL$LONGITUDINAL$input
#> [1] "a"  "b"  "ka" "V"  "Cl"
#> 
#> $MODEL$LONGITUDINAL$LONGITUDINAL$cat
#> NULL
#> 
#> $MODEL$LONGITUDINAL$LONGITUDINAL$reg
#> character(0)
#> 
#> $MODEL$LONGITUDINAL$LONGITUDINAL$file
#> [1] "oral1_1cpt_kaVCl.txt"
#> 
#> 
#> $MODEL$LONGITUDINAL$DEFINITION
#> $MODEL$LONGITUDINAL$DEFINITION$endpoint
#> $MODEL$LONGITUDINAL$DEFINITION$endpoint[[1]]
#> $MODEL$LONGITUDINAL$DEFINITION$endpoint[[1]]$var
#> [1] "CONC"
#> 
#> $MODEL$LONGITUDINAL$DEFINITION$endpoint[[1]]$dist
#> [1] "normal"
#> 
#> $MODEL$LONGITUDINAL$DEFINITION$endpoint[[1]]$pred
#> [1] "Cc"
#> 
#> $MODEL$LONGITUDINAL$DEFINITION$endpoint[[1]]$err
#> $MODEL$LONGITUDINAL$DEFINITION$endpoint[[1]]$err$errName
#> [1] "combined1"
#> 
#> $MODEL$LONGITUDINAL$DEFINITION$endpoint[[1]]$err$typical
#> [1] "a" "b"
#> 
#> 
#> $MODEL$LONGITUDINAL$DEFINITION$endpoint[[1]]$autocor
#> character(0)
#> 
#> 
#> 
#> $MODEL$LONGITUDINAL$DEFINITION$fixed
#> numeric(0)
#> 
#> 
#> $MODEL$LONGITUDINAL$EQUATION
#> $MODEL$LONGITUDINAL$EQUATION[[1]]
#> [1] "; PK model definition\nCc = pkmodel(ka, V, Cl)"
#> 
#> 
#> $MODEL$LONGITUDINAL$OUTPUT
#> $MODEL$LONGITUDINAL$OUTPUT$output
#> [1] "Cc"
#> 
#> $MODEL$LONGITUDINAL$OUTPUT$table
#> character(0)
#> 
#> 
#> 
#> 
#> $FIT
#> $FIT$FIT
#>   data dataQuote model modelQuote
#> 1 CONC     FALSE  CONC      FALSE
#> 
#> 
#> $PARAMETER
#> $PARAMETER$PARAMETER
#>       name value method
#> 1   Cl_pop   0.1    MLE
#> 2    V_pop   0.5    MLE
#> 3        a   1.0    MLE
#> 4        b   0.3    MLE
#> 5   ka_pop   1.0    MLE
#> 6 omega_Cl   1.0    MLE
#> 7  omega_V   1.0    MLE
#> 8 omega_ka   1.0    MLE
#> 
#> 
#> $MONOLIX
#> $MONOLIX$MONOLIX
#> list()
#> 
#> $MONOLIX$TASKS
#> $MONOLIX$TASKS$TASKS
#> $MONOLIX$TASKS$TASKS$populationParameters
#> list()
#> 
#> $MONOLIX$TASKS$TASKS$individualParameters
#> $MONOLIX$TASKS$TASKS$individualParameters$method
#> $MONOLIX$TASKS$TASKS$individualParameters$method[[1]]
#> [1] "conditionalMean"
#> 
#> $MONOLIX$TASKS$TASKS$individualParameters$method[[2]]
#> [1] "conditionalMode"
#> 
#> 
#> 
#> $MONOLIX$TASKS$TASKS$fim
#> $MONOLIX$TASKS$TASKS$fim$method
#> [1] "Linearization"
#> 
#> 
#> $MONOLIX$TASKS$TASKS$logLikelihood
#> $MONOLIX$TASKS$TASKS$logLikelihood$method
#> [1] "Linearization"
#> 
#> 
#> $MONOLIX$TASKS$TASKS$plotResult
#> $MONOLIX$TASKS$TASKS$plotResult$method
#> $MONOLIX$TASKS$TASKS$plotResult$method[[1]]
#> [1] "indfits"
#> 
#> $MONOLIX$TASKS$TASKS$plotResult$method[[2]]
#> [1] "obspred"
#> 
#> $MONOLIX$TASKS$TASKS$plotResult$method[[3]]
#> [1] "vpc"
#> 
#> $MONOLIX$TASKS$TASKS$plotResult$method[[4]]
#> [1] "residualsscatter"
#> 
#> $MONOLIX$TASKS$TASKS$plotResult$method[[5]]
#> [1] "residualsdistribution"
#> 
#> $MONOLIX$TASKS$TASKS$plotResult$method[[6]]
#> [1] "parameterdistribution"
#> 
#> $MONOLIX$TASKS$TASKS$plotResult$method[[7]]
#> [1] "covariatemodeldiagnosis"
#> 
#> $MONOLIX$TASKS$TASKS$plotResult$method[[8]]
#> [1] "randomeffects"
#> 
#> $MONOLIX$TASKS$TASKS$plotResult$method[[9]]
#> [1] "covariancemodeldiagnosis"
#> 
#> $MONOLIX$TASKS$TASKS$plotResult$method[[10]]
#> [1] "saemresults"
#> 
#> 
#> 
#> 
#> 
#> $MONOLIX$SETTINGS
#> $MONOLIX$SETTINGS$SETTINGS
#> $MONOLIX$SETTINGS$SETTINGS[[1]]
#> [1] ""
#> 
#> 
#> $MONOLIX$SETTINGS$GLOBAL
#> $MONOLIX$SETTINGS$GLOBAL$exportpath
#> [1] "tp"
```

## Translating models from the Monolix model library

For models using Monolix’s model library, the models may not be
accessible as text files in all versions of Monolix. In the `mlxtran`
files you may see something like:

    lib:bolus_1cpt_TlagVCl.txt

For older versions of Monolix, the model libraries are a group of text
files. You can find it by looking for a file in the Monolix library like
`bolus_1cpt_TlagVCl.txt`. In this case it would be in
`pk/bolus_1cpt_TlagVCl.txt`. The parent directory would be the model
library. If you have access to these files (even if they are from an old
version of Monolix) you can make `monolix2rx` aware of the model library
by using:

``` r
# If the model library was located in ~/src/monolix/library
# Then you would set the model library up as follows:
options(monolix2rx.library="~/src/monolix/library/")
```

In Unix, this can be a symbolic link to whatever model library you would
like to use.

You can check to see if it works by trying to translate the model file
to `rxode2`:

``` r
monolix2rx("lib:bolus_1cpt_TlagVCl.txt")
#> ℹ cannot find individual parameter estimates
#>  ── rxode2-based free-form 1-cmt ODE model ────────────────────────────────────── 
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1          central
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     description <- "The administration is via a bolus with a lag time (Tlag).\nThe PK model has one compartment (volume V) and a linear elimination (clearance Cl)."
#>     model({
#>         cmt(central)
#>         d/dt(central) <- -Cl/V * central
#>         alag(central) <- Tlag
#>         Cc <- central/V
#>     })
#> }
```

If you computer is setup correctly (like above) you will see the
translated model. Note since it isn’t a `mlxtran` file the relationship
between population parameters, between subject variability etc and
initial parameter estimates are not in the model.

If the model library is not setup correctly you will see or cannot be
found in an old model library you get:

``` r
try(monolix2rx("lib:notThere.txt"))
#> Warning in .mlxtranLib(file): while options('monolix2rx.library') is set, could not find model file 'lib:notThere.txt'
#> please save the model to translate
#> Error : could not find the model file
```

In newer versions of Monolix, the model library was turned into a binary
database that is accessed by the GUI. To me there are advantages of
this:

  - A binary database would be much faster in loading models

  - With a model library, you don’t have to put common model files all
    over the place (saving space on your system)

  - It would make their hard work on the excellent model library harder
    to take and put into another system (They have at least 31,558
    models).
    
      - Note I believe that would be dishonest in general and think a
        model library should only be imported from a open-source library
        or it should be created on its own (as we are doing in
        `nlmixr2lib`).
    
      - Please do not request direct translations of models from Monolix
        to our library `nlmixr2lib`; these requests will be rejected.

If you want the model library as text files, you may be able to reach
out to Lixoft and ask if they will provide them to you.

My biggest concern with this a approach is submitting Monolix models
from the model library to regulatory bodies. For the regulators to be
able to truly see the models they have to have a working copy Monolix.
If they do not, the model is a black box.

For that reason, I believe best practice when submitting to a regulatory
body is to make the model available by making some change to the model
and saving it to a final location. That way the regulators can see the
model.

This approach also allows the model to be translated to `rxode2`.

# Note on testing

The tests in this package include testing the `Monolix` demo files, the
`Monolix` library files (if available), and Monolix validation suite.

Since these are a part of Monolix itself, they are not included in this
package. You can setup `monolix2rx` to run tests on all of these files
as well by setting up some options:

``` r
# setup monolix library (and will test that the parsing and translation are as expected)
options(monolix2rx.library="~/src/monolix/library/")
# setup monolix demos to be tested
options(monolix2rx.demo="~/src/monolix/demos/")
```
