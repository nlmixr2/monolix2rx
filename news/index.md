# Changelog

## monolix2rx 0.0.7

- Range-check the input length in all 13 `trans_*` parser entry-points
  before narrowing it for `dparse()`’s `int` buffer length. A buffer of
  `INT_MAX` bytes or more now raises a clean R error instead of handing
  the parser a truncated (possibly negative) length. This is defensive:
  every current caller passes an R string, which R already caps at
  `INT_MAX` bytes.

## monolix2rx 0.0.6

CRAN release: 2025-08-29

- Updated to add types for rstudio completion

- Defensive `drop = FALSE` on the imported `thetaMat` covariance subset
  so a single surviving parameter is not collapsed to a scalar.

- Parameters whose off-diagonal covariances are `NaN`/`NA`/`Inf` are now
  also dropped from the imported `thetaMat` (previously only the
  diagonal was checked for `NaN`/`NA`, so non-finite covariances could
  silently propagate into simulations).

- When every parameter is dropped from the imported `thetaMat`, the
  covariance information is now ignored with a warning instead of
  storing a `0x0` matrix that would break
  [`rxSolve()`](https://nlmixr2.github.io/rxode2/reference/rxSolve.html)
  simulations;
  [`rxSolve()`](https://nlmixr2.github.io/rxode2/reference/rxSolve.html)
  also warns when `nStud > 1` is requested but no `thetaMat` is
  available, so uncertainty is never silently omitted.

- Fixed
  [`rxSolve()`](https://nlmixr2.github.io/rxode2/reference/rxSolve.html)
  fallbacks that read `dfObs`/`thetaMat` from the wrong location when
  the values were stored on the model instead of its `meta` environment.

- [`rxSolve()`](https://nlmixr2.github.io/rxode2/reference/rxSolve.html)
  now actually uses the Monolix-style `maxSS` it reports (number of
  steady-state doses plus one); previously the computed value was
  ignored and the literal default `10000L` was passed to the solver. The
  guard also checked `missing(maxSS)` twice where it meant `minSS`, so a
  user-specified `minSS` no longer gets silently overwritten. Note this
  can change steady-state simulation results: like Monolix itself, a
  fixed number of doses is now simulated, so slowly accumulating drugs
  reproduce Monolix’s (possibly pre-steady-state) concentrations instead
  of being dosed to full steady state; pass `maxSS`/`minSS` explicitly
  to override.

## monolix2rx 0.0.5

CRAN release: 2025-07-15

- Updated for new solving option in rxode2 4.0 (and depend on the
  packages)

- Bug fixes for importing models from `lixoftConnectors`.

## monolix2rx 0.0.4

CRAN release: 2024-11-28

- Added `ignoreline` support
  [\#22](https://github.com/nlmixr2/monolix2rx/issues/22)

## monolix2rx 0.0.3

CRAN release: 2024-10-24

- For initial conditions starting with `rxCov_` don’t add to ini

## monolix2rx 0.0.2

CRAN release: 2024-09-20

- Remove `rxode2parse` `LinkingTo`

- Add urls for website

- Remove sentence about the residual specification not always being
  captured. Right now for ‘Monolix’ it always is.

## monolix2rx 0.0.1

CRAN release: 2024-09-20

- Initial CRAN submission.
