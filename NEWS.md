# monolix2rx 0.0.7

* Switch all 13 `trans_*` parser entry-points to use the new
  `udparse(curP, gBuf, (unsigned int)strlen(gBuf))` API
  (dparser >= 1.3.2) instead of the previous
  `dparse(curP, gBuf, (int)strlen(gBuf))`.  The unsigned-int parameter
  removes the silent truncation that the `(int)` cast caused on inputs
  near or above `INT_MAX` bytes; no per-call-site length guard is
  needed in monolix2rx itself.

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
