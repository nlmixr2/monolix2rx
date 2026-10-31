# Translate a monolix file to rxode2

Translate a monolix file to rxode2

## Usage

``` r
monolix2rx(
  mlxtran,
  update = TRUE,
  thetaMatType = c("sa", "lin"),
  sd = 1,
  cor = 1e-05,
  theta = 0.5,
  ci = 0.95,
  sigdig = 3,
  envir = parent.frame(),
  dirn = NULL
)
```

## Arguments

- mlxtran:

  file name for mlxtran to translate to rxode2

- update:

  is a boolean that represents if the final parameter estimates should
  be used for the translation (when present)

- thetaMatType:

  This lists the preferred source for `thetaMat` covariance matrix. By
  default it is `sa` for simulated annealing, though you could use `lin`
  for linearized covariance calculation. If only one is present, then
  use whatever is present

- sd:

  Default standard deviation for between subject
  variability/inter-occasion variability that are missing.

- cor:

  Default correlation for missing correlations estimate

- theta:

  default population estimate

- ci:

  confidence interval for validation, by default 0.95

- sigdig:

  number of significant digits for validation, by default 3

- envir:

  represents the environment used for evaluating the corresponding
  rxode2 function

- dirn:

  directory of the Monolix project, by default it is the current working
  directory

## Value

rxode2 model

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

rx <- monolix2rx(file.path(pkgTheo, "theophylline_project.mlxtran"))
#> ℹ integrated model file 'oral1_1cpt_kaVCl.txt' into mlxtran object
#> ℹ updating model values to final parameter estimates from tp/populationParameters.txt
#> ℹ done
#> ℹ reading run info (# obs, doses, Monolix Version, etc) from summary.txt
#> ℹ done
#> ℹ reading covariance from FisherInformation/covarianceEstimatesLin.txt
#> ℹ done
#> ℹ imported monolix and translated to rxode2 compatible data ($monolixData)
#> ℹ imported monolix ETAS (_mode) imported to rxode2 compatible data ($etaData)
#> ℹ imported monolix ETAS (_SAEM) to $etaSaemData
#> ℹ imported monolix pred/ipred data to compare ($predIpredData)
#>  
#>  
#> ℹ solving ipred problem
#> ℹ done
#> ℹ solving pred problem
#> ℹ done

pkgCov <- system.file("cov", package="monolix2rx")

rx <- monolix2rx(file.path(pkgCov, "warfarin_covariate3_project.mlxtran"))
#> ℹ integrated model file 'oral1_1cpt_TlagkaVCl.txt' into mlxtran object
#> ℹ updating model values to final parameter estimates from w3/populationParameters.txt
#> ℹ done
#> ℹ reading run info (# obs, doses, Monolix Version, etc) from summary.txt
#> ℹ done
#> ℹ reading covariance from FisherInformation/covarianceEstimatesSA.txt
#> ℹ done
#> ℹ imported monolix and translated to rxode2 compatible data ($monolixData)
#> ℹ imported monolix ETAS (_mode) imported to rxode2 compatible data ($etaData)
#> ℹ imported monolix ETAS (_SAEM) to $etaSaemData
#> ℹ imported monolix pred/ipred data to compare ($predIpredData)
#>  
#>  
#> ℹ solving ipred problem
#> ℹ done
#> ℹ solving pred problem
#> ℹ done

rx
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>       Tlag_pop         ka_pop          V_pop         Cl_pop  beta_V_tSex_F 
#>    -0.25949800     0.35610590     2.13606937    -2.00665359    -0.38227857 
#> beta_Cl_tSex_F              a              b 
#>    -0.09383651     0.24818991     0.05086658 
#> 
#> Omega ($omega): 
#>            omega_Tlag  omega_ka    omega_V   omega_Cl
#> omega_Tlag  0.3836648 0.0000000 0.00000000 0.00000000
#> omega_ka    0.0000000 0.9857194 0.00000000 0.00000000
#> omega_V     0.0000000 0.0000000 0.02782834 0.00000000
#> omega_Cl    0.0000000 0.0000000 0.00000000 0.08142194
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2          central
#>  ── μ-referencing ($muRefTable): ──  
#>      theta        eta level                   covariates
#> 1 Tlag_pop omega_Tlag    id                             
#> 2   ka_pop   omega_ka    id                             
#> 3    V_pop    omega_V    id  (tSex == "F")*beta_V_tSex_F
#> 4   Cl_pop   omega_Cl    id (tSex == "F")*beta_Cl_tSex_F
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     description <- "The administration is extravascular with a first order absorption (rate constant ka) and a lag time (Tlag).\nThe PK model has one compartment (volume V) and a linear elimination (clearance Cl)."
#>     dfObs <- 479
#>     dfSub <- 32
#>     thetaMat <- lotri({
#>         Tlag_pop ~ c(Tlag_pop = 0.116829)
#>         ka_pop ~ c(Tlag_pop = 0.00381408, ka_pop = 0.0956804)
#>         V_pop ~ c(Tlag_pop = -3.00146e-06, ka_pop = -0.000764125, 
#>             V_pop = 0.00124869)
#>         beta_V_tSex_F ~ c(Tlag_pop = 0.00087222, ka_pop = 0.00102328, 
#>             V_pop = -0.00125133, beta_V_tSex_F = 0.00724034)
#>         Cl_pop ~ c(Tlag_pop = 0.000321585, ka_pop = -0.00103337, 
#>             V_pop = 1.44514e-05, beta_V_tSex_F = -1.88035e-05, 
#>             Cl_pop = 0.00314118)
#>         beta_Cl_tSex_F ~ c(Tlag_pop = 6.45183e-05, ka_pop = 0.000819193, 
#>             V_pop = -1.50053e-05, beta_V_tSex_F = -1.29385e-06, 
#>             Cl_pop = -0.00313677, beta_Cl_tSex_F = 0.0199246)
#>         omega_Tlag ~ c(Tlag_pop = -0.0869622, ka_pop = 0.00399853, 
#>             V_pop = 0.000353655, beta_V_tSex_F = -0.00119581, 
#>             Cl_pop = 1.51396e-05, beta_Cl_tSex_F = -0.00034514, 
#>             omega_Tlag = 0.116943)
#>         omega_ka ~ c(Tlag_pop = -0.0172905, ka_pop = 0.0120718, 
#>             V_pop = 0.000202894, beta_V_tSex_F = -0.000442385, 
#>             Cl_pop = 0.000550191, beta_Cl_tSex_F = -0.000725679, 
#>             omega_Tlag = 0.0139898, omega_ka = 0.0694802)
#>         omega_V ~ c(Tlag_pop = -0.000199902, ka_pop = 0.000210585, 
#>             V_pop = 5.5227e-06, beta_V_tSex_F = -1.72411e-06, 
#>             Cl_pop = -1.98536e-05, beta_Cl_tSex_F = 2.11319e-05, 
#>             omega_Tlag = 0.000296489, omega_ka = -0.000253742, 
#>             omega_V = 0.000656055)
#>         omega_Cl ~ c(Tlag_pop = -0.00027062, ka_pop = 0.000142875, 
#>             V_pop = -5.0202e-06, beta_V_tSex_F = 1.01086e-05, 
#>             Cl_pop = -3.18012e-06, beta_Cl_tSex_F = -3.1184e-05, 
#>             omega_Tlag = 0.000190543, omega_ka = -0.000195656, 
#>             omega_V = 3.1147e-06, omega_Cl = 0.00133407)
#>         a ~ c(Tlag_pop = 0.000596188, ka_pop = -0.000281646, 
#>             V_pop = -4.22388e-05, beta_V_tSex_F = -5.67417e-07, 
#>             Cl_pop = 3.6694e-05, beta_Cl_tSex_F = 5.18592e-05, 
#>             omega_Tlag = -0.000767474, omega_ka = -0.000402704, 
#>             omega_V = -3.56563e-05, omega_Cl = 5.77815e-05, a = 0.00146135)
#>         b ~ c(Tlag_pop = -5.73116e-05, ka_pop = 4.72163e-05, 
#>             V_pop = 1.20773e-05, beta_V_tSex_F = -4.84525e-06, 
#>             Cl_pop = -5.83399e-06, beta_Cl_tSex_F = -4.64896e-06, 
#>             omega_Tlag = 5.67197e-05, omega_ka = 4.06959e-05, 
#>             omega_V = -4.39519e-06, omega_Cl = -1.31481e-05, 
#>             a = -0.000214637, b = 5.66332e-05)
#>     })
#>     validation <- c("ipred relative difference compared to Monolix ipred: 0%; 95% percentile: (0%,0%); rtol=1.21e-06", 
#>         "ipred absolute difference compared to Monolix ipred: 95% percentile: (7.19e-08, 4.41e-05); atol=5.15e-06", 
#>         "pred relative difference compared to Monolix pred: 0%; 95% percentile: (0%,0%); rtol=1.1e-06", 
#>         "pred absolute difference compared to Monolix pred: 95% percentile: (1.87e-08, 4.92e-05); atol=6.07e-06", 
#>         "iwres relative difference compared to Monolix iwres: 0%; 95% percentile: (0%,0.05%); rtol=1.32e-05", 
#>         "iwres absolute difference compared to Monolix iwres: 95% percentile: (6.19e-08, 2.84e-05); atol=6.02e-06")
#>     ini({
#>         Tlag_pop <- -0.259498000083172
#>         ka_pop <- 0.356105897545116
#>         V_pop <- 2.1360693683596
#>         Cl_pop <- -2.00665359474148
#>         beta_V_tSex_F <- -0.382278567256413
#>         beta_Cl_tSex_F <- -0.0938365059053137
#>         a <- c(0, 0.24818990819019)
#>         b <- c(0, 0.0508665778176092)
#>         omega_Tlag ~ 0.383664766381593
#>         omega_ka ~ 0.985719433448741
#>         omega_V ~ 0.0278283386496708
#>         omega_Cl ~ 0.0814219400052839
#>     })
#>     model({
#>         cmt(depot)
#>         cmt(central)
#>         if (sex == 0) {
#>             tSex <- "F"
#>         }
#>         else if (sex == 1) {
#>             tSex <- "M"
#>         }
#>         else {
#>             tSex <- "M"
#>         }
#>         Tlag <- exp(Tlag_pop + omega_Tlag)
#>         ka <- exp(ka_pop + omega_ka)
#>         V <- exp(V_pop + beta_V_tSex_F * (tSex == "F") + omega_V)
#>         Cl <- exp(Cl_pop + beta_Cl_tSex_F * (tSex == "F") + omega_Cl)
#>         d/dt(depot) <- -ka * depot
#>         alag(depot) <- Tlag
#>         d/dt(central) <- +ka * depot - Cl/V * central
#>         Cc <- central/V
#>         concentration <- Cc
#>         concentration ~ add(a) + prop(b) + combined1()
#>     })
#> }

# If the mlxtran lines are edited it can't detect the directory so
# give it with `dirn`:

lines <- readLines(file.path(pkgTheo, "theophylline_project.mlxtran"))

rx <- monolix2rx(lines, dirn=pkgTheo)
#> ℹ integrated model file 'oral1_1cpt_kaVCl.txt' into mlxtran object
#> ℹ updating model values to final parameter estimates from tp/populationParameters.txt
#> ℹ done
#> ℹ reading run info (# obs, doses, Monolix Version, etc) from summary.txt
#> ℹ done
#> ℹ reading covariance from FisherInformation/covarianceEstimatesLin.txt
#> ℹ done
#> ℹ imported monolix and translated to rxode2 compatible data ($monolixData)
#> ℹ imported monolix ETAS (_mode) imported to rxode2 compatible data ($etaData)
#> ℹ imported monolix ETAS (_SAEM) to $etaSaemData
#> ℹ imported monolix pred/ipred data to compare ($predIpredData)
#>  
#>  
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
#>         ka_pop ~ 0.0416560546073138
#>         V_pop ~ c(0.00118325511651798, 0.00202122030181813)
#>         Cl_pop ~ c(-0.000695680378916356, -0.000371376957496329, 
#>             0.00701317875319478)
#>         omega_ka ~ c(omega_ka = 0.022259)
#>         omega_V ~ c(omega_ka = -7.6443e-05, omega_V = 0.0014578)
#>         omega_Cl ~ c(omega_ka = 3.062e-06, omega_V = -1.2912e-05, 
#>             omega_Cl = 0.0039578)
#>         a ~ c(omega_ka = -0.0001227, omega_V = -6.5914e-05, omega_Cl = -0.00041194, 
#>             a = 0.015333)
#>         b ~ c(omega_ka = -1.3886e-05, omega_V = -3.1105e-05, 
#>             omega_Cl = 5.2805e-05, a = -0.0026458, b = 0.00056232)
#>     })
#>     validation <- c("ipred relative difference compared to Monolix ipred: 0%; 95% percentile: (0%,0%); rtol=6.68e-07", 
#>         "ipred absolute difference compared to Monolix ipred: 95% percentile: (1.8e-07, 1.38e-05); atol=3.41e-06", 
#>         "pred relative difference compared to Monolix pred: 0%; 95% percentile: (0%,0%); rtol=6.78e-07", 
#>         "pred absolute difference compared to Monolix pred: 95% percentile: (1.6e-07, 1.27e-05); atol=3.66e-06", 
#>         "iwres relative difference compared to Monolix iwres: 0%; 95% percentile: (0%,0.03%); rtol=6.93e-06", 
#>         "iwres absolute difference compared to Monolix iwres: 95% percentile: (7.96e-08, 1.85e-05); atol=2.71e-06")
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
```
