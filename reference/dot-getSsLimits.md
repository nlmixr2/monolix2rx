# rxode2 steady-state limits for Monolix's number of steady-state doses

With `ssAtol`/`ssRtol` of 100 rxode2 stops at `minSS` doses, so `maxSS`
can be raised to rxode2's floor without changing the solve; `minSS`
below rxode2's floor of 5 cannot be reproduced.

## Usage

``` r
.getSsLimits(x)
```

## Arguments

- x:

  monolix2rx object, its parsed project or its rxUi

## Value

named integer vector `minSS`, `maxSS`

## Author

Matthew L. Fidler
