# monolix2rx 0.0.7

* Added a Monolix stress kit (`inst/stress`, like the nonmem2rx and
  babelmixr2 kits): it simulates each case with rxode2, writes the Monolix
  project, runs Monolix and checks the `monolix2rx()` import; a translate
  mode and a mock Monolix check it without Monolix.

* Dropped the re-exports (`rxode2()`, `rxode()`, `RxODE()`, `ini()`,
  `model()`, `model<-`, `rxRename()`, `rxSolve()`, `rxUiGet()`, `logit()`,
  `expit()`, `lotri()`, `autoplot()` and `%>%`).  Load `nlmixr2` (or
  `rxode2`/`magrittr`) to get them; this also works around a roxygen2 8.1.0
  re-export failure (issue #47, r-lib/roxygen2#1915).

* `monolix2rx()`, `mlxtran()` and `mlxTxt()` now have `dirn` to giving
  the actual directory of the Monolix project, in case the project
  moved for some reason. This makes it possible to translate project files
  outside the project directory (#44)

* The project directory in mlxtran is now absolute.

* Import `Monolix` 2024 Excel (`.xls`, `.xlsx`; first sheet) and SAS
  (`.sas7bdat`, `.xpt`) data sets, using `readxl`/`haven` when they are
  installed (issue #10).
* The data set regressor columns (`use=regressor` in `[CONTENT]`) are now
  matched to the model regressors (`use=regressor` in `[LONGITUDINAL]`) by
  their column order in the data set, as Monolix does, and renamed to the
  model names on import.  A mismatched regressor count or a missing
  regressor column is now an error (issue #2).

* Imported `amt`/`time`/`dv` character columns now match `na.strings`
  literally (ignoring surrounding whitespace); the default `"."` was a
  regular expression wildcard, so any single-character value was silently
  turned into `NA`.  Genuine `NA`, blank and `"NaN"` values no longer keep
  such a column as character (issue #56).

* Imported data sets now cast the continuous covariates to double;
  previously the categorical covariates were cast instead, turning
  character categories into `NA`.

* Support the `Monolix` 2024 file specification `file={path='data.csv'}` in
  addition to the older `file='data.csv'`; the two are equivalent.  This
  applies to the data file in `<DATAFILE> [FILEINFO]` (and
  `<DATA_FORMATTING> [FILEINFO]`) as well as the model file in
  `<MODEL> [LONGITUDINAL]` (issue #43).

* Categorical covariate transformations (`[COVARIATE] DEFINITION:`
  `transform=`/`categories=`/`reference=`) without a `reference` now fall
  back to the first category instead of assigning an empty label; the
  transformation translation is now tested end-to-end (issue #6).

* Range-check the input length in all 13 `trans_*` parser entry-points
  before narrowing it for `dparse()`'s `int` buffer length.  A buffer of
  `INT_MAX` bytes or more now raises a clean R error instead of handing
  the parser a truncated (possibly negative) length.  This is defensive:
  every current caller passes an R string, which R already caps at
  `INT_MAX` bytes.

* Removed the dparser ambiguities in the `EQUATION:`/`PK:`,
  `[LONGITUDINAL] DEFINITION:` and `file=` grammars, and made the equation
  statement list plain left recursive.  Parsing long equation blocks is now
  linear instead of quadratic; `if`/`else` blocks with function calls parse
  up to ~80x faster.  Setting `MONOLIX2RX_STRICT_AMBIGUITY` (as the tests
  do) makes any remaining grammar ambiguity an error (issue #51).

* Fixed implicit `ptrdiff_t` to `int` truncation in `rc_dup_str` (`src/shared.c`);
  pointer differences are now range-checked before conversion to `int`.

* Fixed `int` index/column overflow in `getLine` (`src/parseSyntaxErrors.h`):
  both are now `size_t` with explicit bounds checks before use.  The line
  buffer is now `R_alloc()`'d so it is reclaimed if an error longjmps out of
  the syntax-error highlighter.

* A syntax error reported on an empty line no longer writes a NUL byte into
  the error report, which silently cut off the rest of the report.

* `rc_dup_str()` now gives each duplicated string its own allocation.  It used
  to append into one growing buffer, so a reallocation left pointers from
  earlier calls (held across calls, e.g. both operands of a logical operator)
  pointing at freed memory.

* A syntax error's R error message now includes the highlighted source line
  and caret.  They were only printed to the console because the header
  written to the error report first suppressed them.

* Added thread-safety comment to `src/shared.c` documenting that the global
  parser state is intentionally not mutex-protected, consistent with R's
  single-threaded execution model.

* Latent categorical covariates (between-subject mixtures,
  `lcat = {type=categorical, categories={1, 2}, P(lcat=1)=plcat1}` in
  `[COVARIATE] DEFINITION:`) are translated to rxode2's `mix()`
  (`lcat <- mix(1, plcat1, 2)`, rxode2 >= 5.1.8) with the class
  probabilities in `ini()`.  Monolix's class per subject is not read yet,
  so these models are not validated.

* Fixed categorical covariate effects with numeric categories:
  `coefficient={0, beta}` for categories `{1, 2}` was translated as
  `beta*lcat` instead of `beta*(lcat == 2)`.  Coefficients now follow the
  declared categories by position.

* `delay(x, T)` in `EQUATION:` is translated to rxode2's `delay()` (needs
  rxode2 >= 5.1.7); models with a delay are solved with `dop853`.

* `pkmodel()` accepts the peripheral clearances and volumes `Q2`, `V2`,
  `Q3` and `V3` (as `k12 = Q2/V`, `k21 = Q2/V2`, ...).

* Fixed the order of `EQUATION:` lines written before `pkmodel()` or a PK
  macro: they were translated after the macro's ODEs, so a rate computed
  there (`k12 = Q/V`) was used before it was defined.  Lines between
  macros in a `PK:` block are no longer dropped.

* Fixed the inter-occasion variability level name: `varlevel={id, id*occ}`
  gave `gamma ~ ... | occ2`, but the data's occasion column is `occ`
  (`id*occ*occ` is now `occ2`).

* Fixed `.getNbdoses()` and `.getStiff()`, which did not recognize the
  parsed project and always returned 7 and `FALSE`: the validation and
  `rxSolve()` now use the project's `nbdoses=` and `odeType=`.  The new
  `.getSsLimits()` gives the matching `minSS`/`maxSS`, raised to rxode2's
  floor (5 and 7) when `nbdoses` is smaller; babelmixr2, which calls
  `.getNbdoses()` directly, needs it for `nbdoses` below 6.

* Fixed `predRtol` (and the pred line of the validation), which was
  relative to Monolix's `ipred` instead of its `pred`; the iwres line of
  the validation reported the pred median.

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
