# monolix2rx 0.0.7

* Add integer overflow guards in the C-level string buffer
  (`src/sbuf.c`).  `sAppendN`, `sAppend`, and `addLine` previously
  computed the new allocation size as `sbb->o + 2 + n + SBUF_MXBUF`
  (or analogous expression).  When the user-controlled `n` was large
  enough this expression overflowed `int` to a negative value, which
  `R_Realloc` then converted to a huge unsigned size and crashed.  The
  guard converts this into a clean R error.

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
