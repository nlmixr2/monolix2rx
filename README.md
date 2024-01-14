
<!-- README.md is generated from README.Rmd. Please edit that file -->

# monolix2rx

<!-- badges: start -->

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

This is a basic example which shows you how to solve a common problem:

``` r
library(monolix2rx)
## basic example code
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
#> Loading required namespace: rxode2
#>  ── rxode2-based free-form 1-cmt ODE model ────────────────────────────────────── 
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1          central
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     description <- "The administration is via a bolus with a lag time (Tlag).\nThe PK model has one compartment (volume V) and a linear elimination (clearance Cl)."
#>     model({
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
        to our tool `nlmixr2lib`; these requests will be rejected.

If you want the model library as text files, you may be able to reach
out to lixoft and ask if they will provide them to you.

My biggest concern with this a monolith model library is approach is
submitting Monolix models from the model library to regulatory bodies
including models with the model library. For the regulators to be able
to truly see the models they have to have a working copy Monolix. If
they do not, the model they are using is a black box. I believe that
many regions this is true.

For that reason, I believe best practice when submitting to a regulatory
body is to make the model available by making some change to the model
and saving it to a final location. That way the regulators can see the
model.

This approach also allows the model to be translated to rxode2.

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
