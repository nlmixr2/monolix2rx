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

* Add integer overflow guards in the C-level string buffer
  (`src/sbuf.c`).  `sAppendN`, `sAppend`, and `addLine` previously
  computed the new allocation size as `sbb->o + 2 + n + SBUF_MXBUF`
  (or analogous expression).  When the user-controlled `n` was large
  enough this expression overflowed `int` to a negative value, which
  `R_Realloc` then converted to a huge unsigned size and crashed.  The
  guard converts this into a clean R error.

* Add regression tests covering the C parser memory-safety guards above.

# monolix2rx 0.0.6

* Updated to add types for rstudio completion

- Defensive `drop = FALSE` on the imported `thetaMat` covariance subset so a single surviving parameter is not collapsed to a scalar.

- Parameters whose off-diagonal covariances are `NaN`/`NA`/`Inf` are now also dropped from the imported `thetaMat` (previously only the diagonal was checked for `NaN`/`NA`, so non-finite covariances could silently propagate into simulations).

- When every parameter is dropped from the imported `thetaMat`, the covariance information is now ignored with a warning instead of storing a `0x0` matrix that would break `rxSolve()` simulations; `rxSolve()` also warns when `nStud > 1` is requested but no `thetaMat` is available, so uncertainty is never silently omitted.

- Fixed `rxSolve()` fallbacks that read `dfObs`/`thetaMat` from the wrong location when the values were stored on the model instead of its `meta` environment.

- `rxSolve()` now actually uses the Monolix-style `maxSS` it reports (number of steady-state doses plus one); previously the computed value was ignored and the literal default `10000L` was passed to the solver.  The guard also checked `missing(maxSS)` twice where it meant `minSS`, so a user-specified `minSS` no longer gets silently overwritten.  Note this can change steady-state simulation results: like Monolix itself, a fixed number of doses is now simulated, so slowly accumulating drugs reproduce Monolix's (possibly pre-steady-state) concentrations instead of being dosed to full steady state; pass `maxSS`/`minSS` explicitly to override.

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
