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
  envir = parent.frame()
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
#> ℹ updating model values to final parameter estimates
#> ℹ done
#> ℹ reading run info (# obs, doses, Monolix Version, etc) from summary.txt
#> ℹ done
#> ℹ reading covariance from FisherInformation/covarianceEstimatesLin.txt
#> ℹ done
#> Warning: NAs introduced by coercion
#> ℹ imported monolix and translated to rxode2 compatible data ($monolixData)
#> ℹ imported monolix ETAS (_SAEM) imported to rxode2 compatible data ($etaData)
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
#> ℹ updating model values to final parameter estimates
#> ℹ done
#> ℹ reading run info (# obs, doses, Monolix Version, etc) from summary.txt
#> ℹ done
#> ℹ reading covariance from FisherInformation/covarianceEstimatesSA.txt
#> ℹ done
#> ℹ imported monolix and translated to rxode2 compatible data ($monolixData)
#> ℹ imported monolix ETAS (_SAEM) imported to rxode2 compatible data ($etaData)
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
#>         Tlag_pop ~ c(Tlag_pop = 0.0695270111760315)
#>         ka_pop ~ c(Tlag_pop = 0.00420093609313868, ka_pop = 0.195044017895198)
#>         V_pop ~ c(Tlag_pop = -1.96027567180685e-05, ka_pop = -0.00923639238851991, 
#>             V_pop = 0.0894995626339569)
#>         beta_V_tSex_F ~ c(Tlag_pop = 0.000672864307227817, ka_pop = 0.00146099695978716, 
#>             V_pop = -0.0105938787089785, beta_V_tSex_F = 0.00724034)
#>         Cl_pop ~ c(Tlag_pop = 3.33517518294536e-05, ka_pop = -0.000198349948605509, 
#>             V_pop = 1.64480523051151e-05, beta_V_tSex_F = -2.52790128781238e-06, 
#>             Cl_pop = 5.67721912406063e-05)
#>         beta_Cl_tSex_F ~ c(Tlag_pop = 4.97719167561125e-05, ka_pop = 0.00116960996255074, 
#>             V_pop = -0.000127036295934593, beta_V_tSex_F = -1.29385e-06, 
#>             Cl_pop = -0.000421700477175592, beta_Cl_tSex_F = 0.0199246)
#>         omega_Tlag ~ c(Tlag_pop = -0.0670860109353223, ka_pop = 0.00570893614027221, 
#>             V_pop = 0.00299407684209903, beta_V_tSex_F = -0.00119581, 
#>             Cl_pop = 2.03533460988456e-06, beta_Cl_tSex_F = -0.00034514, 
#>             omega_Tlag = 0.116943)
#>         omega_ka ~ c(Tlag_pop = -0.0133385617208073, ka_pop = 0.0172356179141179, 
#>             V_pop = 0.00171771988746332, beta_V_tSex_F = -0.000442385, 
#>             Cl_pop = 7.3966470999696e-05, beta_Cl_tSex_F = -0.000725679, 
#>             omega_Tlag = 0.0139898, omega_ka = 0.0694802)
#>         omega_V ~ c(Tlag_pop = -0.000154212149163577, ka_pop = 0.000300664573505569, 
#>             V_pop = 4.67557030887738e-05, beta_V_tSex_F = -1.72411e-06, 
#>             Cl_pop = -2.66907442804329e-06, beta_Cl_tSex_F = 2.11319e-05, 
#>             omega_Tlag = 0.000296489, omega_ka = -0.000253742, 
#>             omega_V = 0.000656055)
#>         omega_Cl ~ c(Tlag_pop = -0.00020876675474306, ka_pop = 0.000203991029463676, 
#>             V_pop = -4.25014903301396e-05, beta_V_tSex_F = 1.01086e-05, 
#>             Cl_pop = -4.27528356071897e-07, beta_Cl_tSex_F = -3.1184e-05, 
#>             omega_Tlag = 0.000190543, omega_ka = -0.000195656, 
#>             omega_V = 3.1147e-06, omega_Cl = 0.00133407)
#>         a ~ c(Tlag_pop = 0.000459922525965396, ka_pop = -0.000402122537073151, 
#>             V_pop = -0.00035759769526248, beta_V_tSex_F = -5.67417e-07, 
#>             Cl_pop = 4.93306085861609e-06, beta_Cl_tSex_F = 5.18592e-05, 
#>             omega_Tlag = -0.000767474, omega_ka = -0.000402704, 
#>             omega_V = -3.56563e-05, omega_Cl = 5.77815e-05, a = 0.00146135)
#>         b ~ c(Tlag_pop = -4.42123891106805e-05, ka_pop = 6.74134848256571e-05, 
#>             V_pop = 0.000102247569651447, beta_V_tSex_F = -4.84525e-06, 
#>             Cl_pop = -7.84308816660971e-07, beta_Cl_tSex_F = -4.64896e-06, 
#>             omega_Tlag = 5.67197e-05, omega_ka = 4.06959e-05, 
#>             omega_V = -4.39519e-06, omega_Cl = -1.31481e-05, 
#>             a = -0.000214637, b = 5.66332e-05)
#>     })
#>     validation <- c("ipred relative difference compared to Monolix ipred: 0.39%; 95% percentile: (0.02%,3.18%); rtol=0.00394", 
#>         "ipred absolute difference compared to Monolix ipred: 95% percentile: (8.49e-05, 0.166); atol=0.0175", 
#>         "pred relative difference compared to Monolix pred: 0%; 95% percentile: (0%,0%); rtol=1.21e-06", 
#>         "pred absolute difference compared to Monolix pred: 95% percentile: (1.87e-08, 4.92e-05); atol=6.07e-06", 
#>         "iwres relative difference compared to Monolix iwres: 0%; 95% percentile: (0.24%,204.01%); rtol=0.0794", 
#>         "iwres absolute difference compared to Monolix pred: 95% percentile: (0.0026, 0.33); atol=0.0362")
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
```
