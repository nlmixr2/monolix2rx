# monolix2rx 0.0.7

* Fix implicit `ptrdiff_t` to `int` truncation in `rc_dup_str`
  (`src/shared.c`).  When the parser passes a string segment longer than
  `INT_MAX` bytes (or a NUL-terminated string of that length), the
  pointer difference / `strlen` result was silently cast to `int`,
  truncating the length to a wrong (often negative) value.  The new
  guard rejects such inputs with an informative R error.  Also adds a
  thread-safety comment documenting that the parser globals are
  intentionally not mutex-protected (R is single-threaded).

* Fix `int col` overflow in `getLine` (`src/parseSyntaxErrors.h`).  When
  reporting a syntax error, `getLine` walks the source string to locate
  the offending line.  The column accumulator was a signed `int` that
  could wrap on lines wider than `INT_MAX` bytes.  After the wrap,
  `R_Calloc(col + 1, char)` received a tiny (or negative) size, and the
  subsequent `memcpy(buf, src + i, col)` then wrote past the
  allocation.  The fix uses `size_t` for the accumulator and adds
  explicit bounds checks before the cast back to `int`.

* Document known `(int)strlen(gBuf)` cast in all 13 `trans_*` parser
  entry-points.  Inputs at or above `INT_MAX` bytes cause silent length
  truncation in the `dparse()` call.  A long-term fix will switch each
  call site to `udparse()` once dparser-R ships that symbol to CRAN.

# monolix2rx 0.0.6

* Updated to add types for rstudio completion

- Defensive `drop = FALSE` on the imported `thetaMat` covariance subset so a single surviving parameter is not collapsed to a scalar.

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
