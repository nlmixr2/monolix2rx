# monolix2rx 0.0.7

* Range-check the input length in all 13 `trans_*` parser entry-points
  before narrowing it for `dparse()`'s `int` buffer length.  A buffer of
  `INT_MAX` bytes or more now raises a clean R error instead of handing
  the parser a truncated (possibly negative) length.  This is defensive:
  every current caller passes an R string, which R already caps at
  `INT_MAX` bytes.

# monolix2rx 0.0.6

* Updated to add types for rstudio completion

# monolix2rx 0.0.5

* Updated for new solving option in rxode2 4.0 (and depend on the packages)

* Bug fixes for importing models from `lixoftConnectors`.

# monolix2rx 0.0.4

* Added `ignoreline` support #22

# monolix2rx 0.0.3

* For initial conditions starting with `rxCov_` don't add to ini

# monolix2rx 0.0.2

* Remove `rxode2parse` `LinkingTo`

* Add urls for website

* Remove sentence about the residual specification not always being
  captured.  Right now for 'Monolix' it always is.

# monolix2rx 0.0.1

* Initial CRAN submission.
