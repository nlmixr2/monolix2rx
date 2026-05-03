# monolix2rx 0.0.7

* Fixed implicit `ptrdiff_t` to `int` truncation in `rc_dup_str` (`src/shared.c`);
  pointer differences are now range-checked before conversion to `int`.

* Fixed potential integer overflow in all 13 `trans_*` parser entry-points: the
  `strlen(gBuf)` result is now checked against `INT_MAX` before being cast to
  `int` for the `dparse()` call.

* Fixed signed integer overflow in `sbuf` size arithmetic (`src/sbuf.c`):
  `sAppendN`, `sAppend`, and `addLine` now guard against overflow before
  computing reallocation sizes.

* Fixed `int col` overflow in `getLine` (`src/parseSyntaxErrors.h`): the column
  accumulator is now `size_t` with an explicit bounds check before use.

* Added thread-safety comment to `src/shared.c` documenting that the global
  parser state is intentionally not mutex-protected, consistent with R's
  single-threaded execution model.

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
