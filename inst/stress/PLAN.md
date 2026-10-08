# monolix2rx Monolix stress kit -- plan

Modeled on the nonmem2rx kit (`nonmem2rx/inst/stress`) and the babelmixr2
kit (`babelmixr2/inst/stress`).  For each case the kit:

1. **simulates** a Monolix-style dataset with an rxode2 "truth" model,
   using Monolix-faithful solving options (the ones `.validateModel()` in
   `R/validate.R` uses: `nbSSDoses`, LOCF covariates);
2. writes the **Monolix project** (`run.mlxtran`, `model.txt` or a `lib:`
   reference, `data.csv`) with initial estimates equal to the true values;
3. **runs Monolix** (lixoftConnectors in a child process, or a command);
4. **imports** with `monolix2rx()` and grades it: monolix2rx's own
   `ipredRtol`/`predRtol`/`iwresAtol`, plus the imported `ini()`,
   omegas, error parameters, `thetaMat` and `dfSub`/`dfObs`.

## Modes

- **translate** (no Monolix): import the project before any results exist
  (the readers return `NULL` and estimates fall back to `<PARAMETER>`),
  solve at the true values with zero random effects and compare PRED to
  the rxode2 truth; compare the omegas and residual-error parameters.
  Runs in `tests/testthat/test-stress.R`.
- **run**: run Monolix on every project, then import with validation.
- **import/replay**: re-import a returned zip without Monolix.

## Layout

```
inst/stress/
  PLAN.md             this file
  README.md           how to run the kit
  stress.R            source(): loads monolix2rx + kit + cases
  run-stress.R        Rscript front end
  R/stress-kit.R      stressCheck() stressList() stressKit() stressReplay() stressBundle()
  R/main.R            runKit()
  R/case.R            kitCase()/kitVariant() registry
  R/data.R            mlxDose() mlxObs() mlxOther() mlxBind() mlxCov() builders
  R/sim.R             rxode2 truth simulation (Monolix-faithful options), data writing
  R/mlxtran.R         project/model templating
  R/monolix.R         Monolix discovery, version, execution, error capture
  R/import.R          monolix2rx import, translate checks, metrics
  R/run.R             per-case pipeline; PASS/FAIL/ERROR/XFAIL/XPASS/SKIP
  R/report.R          summary.md / summary.csv
  bin/run-lixoft.R    child process: lixoftConnectors load/run/resave
  mock/fake-monolix.R plumbing self-test: writes Monolix output from the truth
  cases/NN-*.R
tests/testthat/test-stress.R   translate mode on a sample + the mock run; skip_on_cran
```

## Design decisions

1. **Hand-written mlxtran templates**, not babelmixr2-generated projects:
   the point is the spellings Monolix users write (`pkmodel()`, macros,
   `lib:`, `file={path=}`, `[CONTENT]` forms ...).
2. **Two imports per run**: the hand-written `run.mlxtran`, and
   `run-resaved.mlxtran` written by `lixoftConnectors::saveProject()` after
   the run (Monolix's canonical rewrite; catches issue #43-style spellings).
3. **Monolix runs in a child process** (`bin/run-lixoft.R` via `Rscript`):
   `runScenario()` blocks and cannot be timed out in process,
   lixoftConnectors' global state breaks forking, and a child gives one
   command-template interface (`{mlxtran}`) shared by the Monolix CLI and
   the mock.  Failure capture follows babelmixr2's `.lixoftCapture()`
   (`loadProject()` returns `FALSE` and prints `[ERROR]`).
4. **Estimation presets**: `est="full"` (SAEM, conditional mode/mean, FIM,
   log-likelihood) and `est="fixed"` (every population parameter
   `method=FIXED`; individual parameters and predictions only).
5. **Version gating**: the Monolix version (from
   `getLixoftConnectorsState()`) gates cases with `minMonolix=` (SKIP on
   older Monolix, like the nonmem2rx `nm75` tag).
6. **Pass criteria**: translate PRED and IPRED at the truth's simulated
   etas (so the eta -> parameter transforms are checked) <= 0.01 %, omega/error parameters
   <= 1e-6 relative; run ipred/pred median <= 1 %, 95th percentile <= 5 %,
   `monolixNotMatched` empty, `thetaMat` positive definite when FIM ran,
   `dfSub`/`dfObs` equal to the data.  `tol=`, `known=` (always) and
   `knownRun=` (only with Monolix output) per case.
7. **Rows are matched by (id, time, repeat index)**: monolix2rx drops data
   columns not declared in `[CONTENT]`, so a ROWID column cannot survive.

## Cases

| file | theme |
|---|---|
| `01-pk.R` | `lib:` models; `pkmodel()` parameterizations (`k`/`Cl`, `k12`/`Q`, `Tlag`, `p`, `Tk0`, `Ktr`/`Mtt`, `Vm`/`Km`); explicit macros (`compartment`, `oral`, `iv`, `depot(target, adm=)`, `peripheral`, `transfer`, `effect`, `elimination`, `empty`/`reset`) |
| `02-ode.R` | `ddt_` systems: turnover with `_0`, Michaelis-Menten, `t` in equations, `t0`, `odeType=stiff`, if/else, math functions |
| `03-dosing.R` | ADDL/II, SS (`nbSSDoses`), `infusionrate` and `infusiontime`, several ADM routes, EVID 3/4 washout, MDV, ties, first dose not at 0 |
| `04-data.R` | delimiters, ignored columns and lines, string IDs, MDV, CENS/LIMIT (imported values checked), two regressors matched by order, string categories, 2024 `file={path=}`, data in a subdirectory |
| `05-params.R` | logNormal/normal/logitNormal/probitNormal, covariate effects with transformed covariates, correlation blocks, `method=FIXED`, no-variability parameters, `[INDIVIDUAL]` vs `[POPULATION]` |
| `06-error.R` | constant, proportional, combined1/2, `c` variants, logNormal/logitNormal observations, two endpoints |
| `07-tasks.R` | FIM linearization vs SA, conditional mean vs mode, custom `exportpath`, `nbSSDoses`, `odeType` |
| `08-dde.R` | delay differential equations (below) |
| `09-mixture.R` | BSMM and WSMM mixtures (below) |
| `10-iov.R` | inter-occasion variability (below) |
| `11-special.R` | discrete/categorical endpoints, parent/metabolite, resaved project re-import |

### Delay differential equations (`08-dde.R`, tag `dde`)

monolix2rx translates `delay()` to rxode2's `delay(state, T)` (rxode2 >=
5.1.7), solved with `dop853`.  The history is the initial condition in
Monolix, but in rxode2 5.1.8 only for a literal `x(0)` (see the findings).

| case | covers |
|---|---|
| `dde-hutchinson` | delayed logistic growth, no doses: `x_0` history and `t0` |
| `dde-delayed-effect` | PK + indirect response driven by `delay(Cc, tau)`, ETA on `tau` |
| `dde-two-delays` | two different delays on two states |
| `dde-delay-of-depot` | delay of a state that receives doses |
| `dde-delay-expr` | delay given by an expression |
| `dde-multidose` | ADDL/II through a delayed system |
| `dde-ss` | steady state with a delay (records what Monolix and rxode2 each do) |
| `dde-stiff` | `odeType=stiff` with a delay |
| `dde-pkmodel-mixed` | `pkmodel()`/macros feeding a delayed `ddt_` state |

Importer work: emit `delay(x, T)`; `.getMethod()` must use a dense method
(`dop853`, or `ros4` when stiff -- `liblsoda` cannot record the history);
a clear error on rxode2 < 5.1.7 (cases SKIP there).  Run ipred tolerance
starts at 2 %.  To confirm on Monolix: delays varying in time/by
individual, SS with delays, an expression as the delay
(`dde-delay-expr`), `delay()` of a macro compartment amount
(`dde-pkmodel-mixed`), and `odeType=stiff` with a delay (`dde-stiff`).

### Mixtures (`09-mixture.R`, tag `mixture`)

Monolix's own definitions (the Monolix methods document, "Mixture of
models"): WSMM is the weighted prediction `f = p1*f1 + p2*f2` with the
usual error model (not a likelihood mixture); BSMM puts each subject in
one group with probability `pk`.  So:

- `wsmm(f1, p1, f2, p2)` -> `(p1)*(f1) + (p2)*(f2)`; `p` may vary by subject.
- `bsmm(f1, p1, f2, p2)` -> `mix(f1, p1_pop, f2)`: rxode2 `mix()` needs
  population probabilities, so `p1` must be a literal or an individual
  parameter without variability or covariates; its typical value becomes
  a natural-scale `ini()` parameter (`p1_pop <- 0.3`, unbounded:
  nlmixr2 estimates `mix()` probabilities on the mlogit scale) and the
  model line `p1 <- p1_pop`.  A probability with an eta is refused, a last
  probability other than `1 - p1 - ...` warns, and `mix()` needs an eta
  somewhere in the model.  A population parameter used directly as a
  probability (no `[INDIVIDUAL]` definition) is not supported yet.
- Latent covariates (`P(lcat=1)=plcat1`) -> `lcat <- mix(1, plcat1, 2)`.
- Mixture models are not validated until Monolix's class per subject is
  read (rxode2 would draw the classes at random).

| case | Monolix form | translate mode |
|---|---|---|
| `bsmm-latent-cov-cl` | latent categorical covariate on `Cl` | PASS |
| `bsmm-structural` | `bsmm(C1, p1, C2, 1-p1)` | PASS |
| `bsmm-3groups` | `bsmm(..., 1-p1-p2)`, three groups | PASS |
| `wsmm-two-pred` | `wsmm(f1, p1, f2, 1-p1)`, `p1` with an eta | PASS |
| `bsmm-p-iiv` | `bsmm()` probability with an eta | XFAIL (refused) |

The true class is a hidden data column (`POP`) removed by `write=`, given
to the import as `mixest`.  Run mode: IPRED needs Monolix's class per
subject (`IndividualParameters/`, to locate on a real run).

### Inter-occasion variability (`10-iov.R`, tag `iov`)

Parsed today (`varlevel={id, id*occ}`, `correlation={level=id*occ,...}`);
three things to check first:

1. Fixed: `.def2iniRenameOcc()` named `id*occ` `occ2` while the data
   column is `occ`.
2. Only one occasion column is mapped (`.use1Rx`), so nested occasions
   have nowhere to go.
3. Monolix writes per-(subject, occasion) individual parameters; the
   validation solve expects one row per subject.

| case | covers |
|---|---|
| `iov-cl-basic` | `use=occasion`; `Cl` with `varlevel={id, id*occ}`; occasion 2 opens with an `EVID=4` washout |
| `iov-only` | IOV without BSV (`varlevel=id*occ`) |
| `iov-ka-v-unequal` | IOV on `ka`/`V`; occasions 2/5/7 of unequal length, drug on board between occasions |
| `iov-correlation` | `correlation={level=id*occ, r(ka, Cl)}` |
| `iov-ka-f-multi` | IOV on `Tlag` (with BSV), `ka` and logitNormal `p` |
| `iov-ss` | SS restarting at each occasion |
| `iov-time-varying-cov` | covariate (`lw70`) changing between occasions |
| `iov-nested` | `OCC1`/`OCC2`, `varlevel={id, id*occ1, id*occ1*occ2}` (XFAIL) |
| `iov-dde`, `iov-mixture` | combinations |

Translate checks add: each omega level vs its truth matrix; the `ini()`
occasion variable exists in the imported data.  Run checks add: ipred with
per-occasion individual parameters (`knownRun=` until fixed); one eta row
per subject-occasion; `dfSub` counts subjects.

## Phases

0. **Skeleton**: kit infrastructure, Monolix data builders and templating,
   `bin/run-lixoft.R`, `mock/fake-monolix.R`, one smoke case end to end in
   translate and mock-run modes, `test-stress.R`.
1. **Translate mode, about 10 core cases** plus the XFAIL smoke set
   (`dde-hutchinson`, `dde-delayed-effect`, `bsmm-latent-cov-cl`,
   `iov-cl-basic`); fix importer bugs found (normal PRs).
2. **Mock run and replay** plumbing proven without a license.
3. **Real Monolix run** on a licensed machine: calibrate tolerances,
   record `known=` diagnoses, confirm the resaved import, freeze the mock's
   per-occasion and mixture output layouts.
   - 2.5 **Importer PRs**, each turning cases XFAIL -> PASS: `delay()`;
     latent BSMM -> `mix()`; structural `bsmm()` -> `mix()`; `wsmm()` ->
     weighted prediction; IOV fixes (name match, per-occasion validation, several
     occasion columns).  Additive only to babelmixr2-facing fields.
4. **Full case list**; record results per Monolix version.

## Importer findings (translate mode)

Each one is a `known=` case until its fix lands (phase 2.5):

- `pkmodel()` does not accept `Q2`/`V2`/`Q3`/`V3` (`inst/equation.g`
  `pkpars0`); `.pkmodel2macro()` would emit
  `peripheral(k12=Q2/V, k21=Q2/V2)` (`pkmodel-iv-2cmt`).
- The `pkmodel()` ODEs are written before the `EQUATION:` lines that
  precede `pkmodel()`, so `k12 = Q/V; Cc = pkmodel(V, Cl, k12, k21)` uses
  `k12` before it is defined (`pkmodel-iv-2cmt-k`).
- `.getNbdoses()` (`R/validate.R`) tests for class `mlxtran`, but the
  parsed project is `monolix2rxMlxtran`, so validation and `rxSolve()`
  always use 7 steady-state doses (`dose-ss`, `nbdoses=10`).

Notes for later cases:

- Fixed: the imported data routed doses by the Monolix compartment
  number.  Without an ADM column nothing was routed (`data$adm` partially
  matched `admd`).  `Tk0` doses had no `rate=-2` and transit doses no
  `evid=7` (`macro-oral-iv-2cmt`, `pkmodel-tk0`, `pkmodel-oral-transit`);
  observation rows with an ADM value could become doses
  (`pkmodel-tk0-adm-rows`); MDV=1 rows were read as doses (`data-mdv`);
  `depot(target=Ac, ka)` lost its depot ODE (`macro-depot-target`).
- `compartment(cmt=1, amount=Ac)` without a volume also emits
  `Cc <- Ac/1`; it is harmless while the model defines `Cc` after it.
- `dose-late-ties`: the data lists the dose before the tied observation;
  which one Monolix applies first is to confirm in run mode.
- rxode2's `transit()` only follows the last dose (Savic); Monolix's
  transit with overlapping doses is to confirm in run mode.

- A stiff project with `delay()` needs `ros4`, not `liblsoda` (what
  `.getMethod()` and babelmixr2 pick for `odeType=stiff`).
- rxode2's `minSS=n` is not the same as `n` explicit doses, and
  `linCmt()` solves steady state analytically (ignores `minSS`): a tighter
  `dose-ss` tolerance or a `linCmt()` SS case will show both.

- rxode2 5.1.8 `delay()` uses `x(0)` as the history only when it is a
  literal constant; a computed or parameter `x(0)` gives a history of 0
  (`dde-hutchinson`; monolix2rx writes `x_0 <- 10; x(0) <- x_0`).  To fix
  in rxode2 (nlmixr2/rxode2#1441).

- rxode2 5.1.8 steady state with `delay()`: the undelayed states reach
  steady state, but a state driven by the delay starts at its initial
  condition and the delay history is 0 (`dde-ss`, `knownRun=`;
  nlmixr2/rxode2#1447).  An
  ignored `SS` column was still read by rxode2 (fixed: ignored columns
  named like rxode2 event columns are dropped).

- Delay models are solved with `dop853` even for `odeType=stiff`
  (rxode2's own default is `dop853+ros4`); `.getDelay()` is not exported
  for babelmixr2's control.

## Truth gaps to close with the importer work

- BSMM (run mode): validation needs Monolix's estimated class per
  subject as `mixest`; where Monolix writes it is to confirm.
- A project without `<MONOLIX> [SETTINGS] exportpath` stops the import
  (`R/parameterUpdate.R`), seen while writing the latent-covariate test.

- `use=ignoredline`: the import drops each flagged line (doses too) and
  the flag column (`data-ignoredline`); that Monolix ignores the whole
  line is to confirm in run mode.

- Regressors: the import matches the data columns to the model's
  `X = {use=regressor}` lines in their order; whether Monolix follows
  those lines or the `input = {...}` order (they agree in
  `data-regressor`) is to confirm.

- `knownRun=` turns any run-mode failure into XFAIL, including a Monolix
  run that did not finish; it should match the expected reason.

- `thetaMat` is the transformed-scale covariance.  To confirm on a real
  Monolix 2021+ run: latent class probabilities (natural in `ini()`, with no
  `[INDIVIDUAL]` Jacobian) and `mean=` parameters may be stored on another
  scale in the covariance files.

## Risks

- Real rxode2/Monolix differences (dose/observation ties, regressor
  interpolation, SS with lag) need `knownRun=` marks, not importer fixes.
- SAEM moves the estimates; run validation compares at Monolix's own final
  estimates, so drift is harmless.  `est="fixed"` gives a deterministic
  baseline (whether Monolix accepts an all-fixed project: to confirm).
- Monolix cannot run in CI: CI covers translate mode and the mock.
