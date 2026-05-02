# Get equation block from a Monolix model txt file

Get equation block from a Monolix model txt file

## Usage

``` r
mlxTxt(file, retFile = FALSE)
```

## Arguments

- file:

  string representing the model text file. Can be lib:fileName.txt if
  library setup/available

- retFile:

  boolean that tells `mlxTxt()` to return the file name instead of error
  if the file does not exist

## Value

parsed equation or file name

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

mod <- mlxTxt(file.path(pkgTheo, "oral1_1cpt_kaVCl.txt"))

mod
#> DESCRIPTION:
#> The administration is extravascular with a first order absorption (rate constant ka).
#> The PK model has one compartment (volume V) and a linear elimination (clearance Cl).
#> This has been modified so that it will run without the model library
#> 
#> <MODEL>
#> [LONGITUDINAL]
#> ; parsed: $MODEL$LONGITUDINAL$LONGITUDINAL
#> input = {ka, V, Cl}
#> 
#> EQUATION:
#> ; parsed: $MODEL$LONGITUDINAL$EQUATION
#> ; PK model definition
#> Cc = pkmodel(ka, V, Cl)
#> 
#> OUTPUT:
#> ; parsed: $MODEL$LONGITUDINAL$OUTPUT
#> output = Cc
```
