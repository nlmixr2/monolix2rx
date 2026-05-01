# monolix2rx 0.0.7

* Fix implicit `ptrdiff_t` to `int` truncation in `rc_dup_str`
  (`src/shared.c`).  When the parser passes a string segment longer than
  `INT_MAX` bytes (or a NUL-terminated string of that length), the
  pointer difference / `strlen` result was silently cast to `int`,
  truncating the length to a wrong (often negative) value.  The new
  guard rejects such inputs with an informative R error.  Also adds a
  thread-safety comment documenting that the parser globals are
  intentionally not mutex-protected (R is single-threaded).

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
