# monolix2rx 0.0.7

* Document known `(int)strlen(gBuf)` cast in all 13 `trans_*` parser
  entry-points.  Inputs at or above `INT_MAX` bytes cause silent length
  truncation in the `dparse()` call.  A long-term fix will switch each
  call site to `udparse()` once dparser-R ships that symbol to CRAN.

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
