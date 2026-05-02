# Read and parse mlxtran lines

Read and parse mlxtran lines

## Usage

``` r
mlxtran(file, equation = FALSE, update = FALSE)
```

## Arguments

- file:

  mlxtran file to process

- equation:

  parse the equation block to rxode2 (some models cannot be translated)

- update:

  when true, try to update the parameter block to the final parameter
  estimates

## Value

mlxtran object

## Author

Matthew L. Fidler

## Examples

``` r
# First load in the model; in this case the theo model
# This is modified from the Monolix demos by saving the model
# File as a text file (hence you can access without model library)
# setup.
#
# This example is also included in the monolix2rx package, so
# you refer to the location with `system.file()`:

pkgTheo <- system.file("theo", package="monolix2rx")

mlx <- mlxtran(file.path(pkgTheo, "theophylline_project.mlxtran"))
#> ℹ integrated model file 'oral1_1cpt_kaVCl.txt' into mlxtran object
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
```
