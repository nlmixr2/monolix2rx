# monolix2rx 0.0.7

* Fix `int col` overflow in `getLine` (`src/parseSyntaxErrors.h`).  When
  reporting a syntax error, `getLine` walks the source string to locate
  the offending line.  The column accumulator was a signed `int` that
  could wrap on lines wider than `INT_MAX` bytes.  After the wrap,
  `R_Calloc(col + 1, char)` received a tiny (or negative) size, and the
  subsequent `memcpy(buf, src + i, col)` then wrote past the
  allocation.  The fix uses `size_t` for the accumulator and adds
  explicit bounds checks before the cast back to `int`.

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
