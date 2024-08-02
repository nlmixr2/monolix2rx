
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
#> using C compiler: ‘gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
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
#>     dfObs <- 120
#>     dfSub <- 12
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
mlx <- as.list(mlx)

mlx$DATAFILE$FILEINFO$FILEINFO
#> $file
#> [1] "data/theophylline_data.txt"
#> 
#> $header
#> [1] "ID"     "AMT"    "TIME"   "CONC"   "WEIGHT" "SEX"   
#> 
#> $delimiter
#> [1] "tab"
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
#> using C compiler: ‘gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
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
database that is accessed by the GUI and `lixoftConnectors`. If you have
`lixoftConnectors` on your system and it can successfully load the model
with `lixoftConnectors::getLibraryModelContent()` then `monolix2rx` will
also load the model correctly (and will use this version over the text
files when both are setup)

This means you will need to import models into `rxode2` you need to:

  - For a model built from the model library you will need:
    
      - have a path to the text file Monolix Library and setup the
        `monolix2rx.library` with
        `options(monolix2rx.library="~/src/monolix/library/")`
    
      - have `lixoftConnectors` installed and connected to a newer (and
        licensed) version of Monolix that can get the model library
        content by `lixoftConnectors::getLibraryModelContent()`
    
      - or without these options, you will need to save the model to a
        text file outside of the model library so you can import the
        model.

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
