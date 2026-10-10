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
| `02-ode.R` | `ddt_` systems: turnover with `_0`, Michaelis-Menten, `t` in equations, `t0`, `odeType=stiff`, if/else, math functions; `amtDose`/`tDose` (also in `depot(p=)`), logical operators and nested if, less common functions and `a^b^c`, an initial condition with an eta |
| `03-dosing.R` | ADDL/II, SS (`nbSSDoses`), `infusionrate` and `infusiontime`, several ADM routes, EVID 3/4 washout, MDV, ties, first dose not at 0 |
| `04-data.R` | delimiters, ignored columns and lines, string IDs, MDV, CENS/LIMIT (imported values checked), two regressors matched by order, string categories, 2024 `file={path=}`, data in a subdirectory |
| `05-params.R` | logNormal/normal/logitNormal/probitNormal, covariate effects with transformed covariates, correlation blocks, `method=FIXED`, no-variability parameters, `[INDIVIDUAL]` vs `[POPULATION]` |
| `06-error.R` | constant, proportional, combined1/2, `c` variants, logNormal/logitNormal observations, two endpoints |
| `07-tasks.R` | no `exportpath`, `exportpath` in a subdirectory, FIM/likelihood by linearization, conditional mode only, population parameters only (mostly run mode; `nbSSDoses` is in `03-dosing.R`, `odeType` in `02-ode.R`) |
| `08-dde.R` | delay differential equations (below) |
| `09-mixture.R` | BSMM and WSMM mixtures (below) |
| `10-iov.R` | inter-occasion variability (below) |
| `11-special.R` | parent/metabolite (YTYPE); count (Poisson, zero-inflated) and categorical (cumulative logit, P(Y=c), binary with PK) observations; a continuous and a discrete observation in one project |
| `12-tte.R` | time-to-event (`type=event`): single exact event (Weibull), interval censored, repeated events, hazard driven by `pkmodel()`, observation starting after the first dose, concentrations and events in one project |
| `13-covariates.R` | categorical transform grouping categories (`transform=`), several covariates on one parameter, an untransformed covariate, covariates on normal and logitNormal parameters, categories mixing numbers and strings |
| `14-pk-macros.R` | `elimination(Vm, Km)`, `oral(Tk0, Tlag, p)`, two `peripheral()` macros (three compartments), `effect()`, `iv(Tlag, p)` |
| `15-individual.R` | logitNormal with `min`/`max`, `var=` instead of `sd=`, a negative correlation in a three-way block, two correlation blocks listed out of order, a covariate on a correlated parameter, a regressor as an ODE input changing on regressor-only lines |
| `16-dosing.R` | steady-state infusion, SS with ADDL, an `EVID=3` reset with drug on board, an infusion through `iv(Tlag)`, SS with an absorption lag, left and right censoring without `LIMIT` |
| `17-error-output.R` | error parameters not named `a`/`b`, a fixed error parameter, a logitNormal observation on (0, 100), `OUTPUT: table=`, an observed Imax effect |
| `18-syntax.R` | `;` comments, a lowercase `method=fixed`, a fixed omega, `method=BAYES` with a `[POPULATION]` prior, a parameter named `F`, variables and states named like rxode2 keywords (also in `PK:` and a macro argument), `X_0` for a state with an underscore and for a plain variable |
| `19-data-layout.R` | data columns not named ID/TIME/AMT/DV, subjects in descending ID order, a dose and an observation on one line, observations at negative times, placebo subjects, a prediction not named `Cc` |
| `20-covariates.R` | a categorical covariate on two parameters, an occasion column without inter-occasion variability, `min()` in a `[COVARIATE] EQUATION:`, a covariate computed from two covariates through an intermediate |
| `21-individual.R` | `if`/`elseif`/`else` in a `[COVARIATE] EQUATION:`, covariates on parameters without random effects, a model with no random effects |
| `22-spelling.R` | lowercase distribution names, `[INDIVIDUAL] DEFINITION:` options in another order, numbers in exponent notation, empty fields for missing data, Windows (CRLF) line endings |
| `23-data-records.R` | observation types given as strings, as codes other than 1/2 and listed in another order in `yname`; replicate samples; a subject without observations |
| `24-macros.R` | `empty()` and `reset()` administrations (also next to a dose lag), a transit `oral()` macro, zero-order `depot(Tk0)`, `elimination(Cl)` with a volume, `peripheral(k1_2, k2_1)` |
| `25-error-models.R` | an error parameter shared by two endpoints, two observations of the same prediction, an additive error fixed at 0, autocorrelated residuals |
| `26-functions.R` | `rem(t, 24)` in a circadian input, `sinh`/`cosh`/`tanh`/`atan2`/`asin`/`ceil`, the negations `~a` and `!a`, an explicit `odeType = nonStiff`, `mean=` instead of `typical=` |
| `27-covariate-data.R` | covariates missing on dose lines (continuous and categorical) or given on the first line only, covariate columns no parameter uses, a regressor given only where it changes |
| `28-start-events.R` | `t_0 = 0` before the first record and no `t_0` with a late first record (also a late transit dose), `rightCensoringTime=`, repeated events with `maxEventNumber=3`, a negation after a sign or as an exponent |
| `29-start-rem.R` | a non-zero `t_0` before the first record, the `t0` spelling with a comment, `rem()` of a negative covariate |

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
2. Fixed: only one occasion column was mapped (`.use1Rx`).  Nested
   occasions now become `occ`, `occ2`, ... (the k-th numbers the
   combinations of the first k columns).  To confirm on Monolix: the
   level names it writes (`id*occ1*occ2` or `id*occ*occ`; both are read
   by depth) and that the first occasion column is the outer level.
3. Monolix writes per-(subject, occasion) individual parameters; the
   validation solve expects one row per subject.

| case | covers |
|---|---|
| `iov-cl-basic` | `use=occasion`; `Cl` with `varlevel={id, id*occ}`; occasion 2 opens with an `EVID=4` washout |
| `iov-only` | IOV without BSV (`varlevel=id*occ`) |
| `iov-ka-v-unequal` | IOV on `ka`/`V`; occasions 2/5/6/7 of unequal length (6 without observations), drug on board between occasions |
| `iov-correlation` | `correlation={level=id*occ, r(ka, Cl)}` |
| `iov-ka-f-multi` | IOV on `Tlag` (with BSV), `ka` and logitNormal `p` |
| `iov-ss` | SS at each occasion with drug still on board at the second (reset vs. added dose: to confirm in run mode) |
| `iov-time-varying-cov` | covariate (`lw70`) changing between occasions |
| `iov-nested` | `OCC1`/`OCC2`, `varlevel={id, id*occ1, id*occ1*occ2}` |
| `iov-dde` | IOV on `Cl` in a `delay()` model, delayed history across the occasion change |
| `iov-mixture` | `bsmm()` with IOV on `V` over two dosing occasions |

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

- rxode2 before 5.1.8 `delay()` uses `x(0)` as the history only when it
  is a literal constant; a computed or parameter `x(0)` gives a history
  of 0 (`dde-hutchinson`; monolix2rx writes `x_0 <- 10; x(0) <- x_0`).
  Fixed in rxode2 5.1.8 (nlmixr2/rxode2#1441); `known=` only before it.

- rxode2 5.1.8 steady state with `delay()`: the undelayed states reach
  steady state, but a state driven by the delay starts at its initial
  condition and the delay history is 0 (`dde-ss`, `knownRun=`;
  nlmixr2/rxode2#1447).  An
  ignored `SS` column was still read by rxode2 (fixed: ignored columns
  named like rxode2 event columns are dropped).

- Delay models are solved with `dop853` even for `odeType=stiff`
  (rxode2's own default is `dop853+ros4`); `.getDelay()` is not exported
  for babelmixr2's control.

- Discrete observations (`type=count`, `type=categorical`) stopped the
  translation; they become rxode2 `pois()`, `ll()` or the named ordinal
  `c()`.  PRED of a random draw is not comparable, so these cases check
  the per-observation log-likelihood at the true etas (`dryLik=TRUE`).
  To confirm in run mode: that Monolix writes no `predictions.txt` for a
  discrete-only project (the kit accepts its absence) and the
  zero-inflated `if k > 0` syntax.  Markov dependence is refused.
  A continuous and a discrete observation (`disc-mixed-continuous`): the
  discrete endpoint gave `NA <- Cc` (fixed), and the continuous one was not
  validated (fixed: predictions are read for continuous endpoints only).
  Whether Monolix then writes `predictions.txt` or `predictions_y1.txt` is
  to confirm; the import reads either.

- `07-tasks.R` under the mock only reruns the base case (the mock ignores
  [TASKS] and writes no FisherInformation).  With Monolix,
  `tasks-pop-only` may write no `predictions*.txt` (the kit would call
  that unfinished) and `tasks-fim-lin` should give `covarianceEstimatesLin`;
  to confirm.
  nlmixr2est 7.1.0 aborts R (rxode2 `rxFixRes()` subscript out of
  bounds) when fitting a named ordinal `c(p0=0, 1)`: the category values
  become fixed `rx.Y.ordinal*` thetas that no model line names.  Plain
  rxode2 reproduces it (nlmixr2/rxode2#1450).  Simulation is fine.

- `amtDose` became a parameter `dose` (rxode2's last dose amount is
  `dose()`), and `amtDose`/`tDose` in `PK:` macro arguments were copied
  untranslated (fixed; `ode-dose-keywords`, `pk-depot-p-amtdose`).  Other
  Monolix syntax in macro arguments (`~=`, `t`, `invlogit()`...,
  `a^b^c`) is now translated too, and `inftDose` there is an error.  In
  `f()` rxode2's `dose()` is the dose being given.  Not covered: before
  the first dose rxode2 gives NA (`dose0()`/`tlast0()` give 0), and an
  observation tied with a dose; Monolix's values there are to confirm.
- `a^b^c` was written unchanged and rxode2 does not parse it (fixed:
  `a^(b^c)`; `ode-math-functions-2`).

- Time-to-event observations (`type=event`) stopped the translation.
  They become a cumulative hazard state and `ll()`: each record gives
  the likelihood from the hazard since the previous record of the
  endpoint (exact, interval censored or no event); the first record
  starts the observation.  rxode2 `lag()` counts every record (doses,
  other endpoints), so the previous record's hazard is carried by a
  `lag0()` recurrence keyed on `CMT`; the endpoint's compartment number
  comes from a first parse, and a single event endpoint's observation
  rows get its `cmt` in the imported data.  The truth instead resets its
  hazard with an EVID=5 replace just after each record.  A model with
  only `DEFINITION:` (`hazard=1/Te`) did not import its data (fixed).
  Rows with a missing DV are not event records.  An EVID=3/4 reset clears
  the cumulative hazard state, so the hazard then counts from the reset
  (it went negative before; fixed).  An event on a subject's first record
  counts the hazard from time 0 (it was dropped; fixed).  A reset is
  found by `cumhaz` being exactly 0 after a positive value.  rxode2 applies
  a reset before a record at the same time, which then loses its interval
  (every state is reset, so the hazard before it cannot be kept).  A
  steady-state dose is not a reset: it moves `cumhaz` to its steady-state
  value, so the next record's interval is wrong (no case; Monolix's
  behavior is to confirm).
  To confirm in run mode: that Monolix integrates from the first record
  (`tte-late-start`), from time 0 without a start record, and across a
  washout (no case yet); that it writes no predictions for an event-only
  project; and the per-record likelihood split (only the sum is in
  Monolix's output).
  rxode2 5.1.8 `lag(time)` aborts R (fixed in rxode2 main, #1434); the
  translation does not use it.  Fitting with `lag0()` recurrences in
  nlmixr2 is not checked.

- `13-covariates.R`: a categorical transform (`[COVARIATE] DEFINITION:`
  `transform=`) becomes string assignments (`tRACE <- "A"`) compared with
  `tRACE == "B"`.  rxode2 before 5.1.8 numbers the literals in comparisons apart
  from the assigned ones, so a comparison is only right when both appear
  in the same order: `tRACE <- "A"; if (RACE == 2) tRACE <- "B";
  isB <- (tRACE == "B")` gives isB = 1 for RACE 1.  The phenobarbital and
  warfarin projects (`inst/cov`) agree by that luck; `cov-transform-group`
  (reference first) does not (`known=` before 5.1.8, where it is fixed;
  nlmixr2/rxode2#1456).
  Categories mixing numbers and strings (`{'U', '1', '3'}`) compared the
  numbers unquoted with the character data column, so a coefficient went to
  another category (fixed; `cov-cat-mixed`).  The quoted numbers then
  only match a character column: data passed to the imported model with a
  numeric RACE column would compare by level number instead (the Monolix
  data set is always character there, since it holds 'U').  Numeric labels
  of a transform are compared unquoted (`tRACE == 2`), which rxode2 reads
  as the level number: right when the labels are 1, 2, ... in their
  assigned order (`cov-transform-numeric`), wrong otherwise
  (`{'0'={1}, '1'={2}}`).  Quoting them is to revisit with rxode2 5.1.8 (#1456 fixed).
- `15-individual.R`: the random effects (`eta_Cl_SAEM`) were renamed
  `omega_Cl`, not the parameter's `sd=`/`var=` name, so `var=omega2_Cl`
  failed validation (fixed; `param-var`).  The mock wrote the column from
  the eta name; it now strips `omega2_` too.  Inter-occasion levels keep
  the old naming.
- `16-dosing.R`: the validation left censored observations out of the
  rxode2 solve only, so Monolix's predictions for them were reported as
  not matched (fixed; `data-cens-limit` and `data-cens-both` in the mock
  run).  Whether Monolix places lagged steady-state doses like rxode2
  (`dose-ss-tlag`) is to confirm in run mode.
- `17-error-output.R`: the kit's likelihood check (`dryLik`) needs a
  non-normal endpoint (rxode2's `.getQuotedDistributionAndLlikArgs()`
  only handles the generalized likelihoods), so
  `err-logitnormal-percent` checks the imported bounds (`predDf`
  `trLow`/`trHi`) directly.  Whether Monolix accepts an input parameter
  (`Cl`) in `OUTPUT: table=` (`out-table`) is to confirm in run mode.
- `18-syntax.R`: a lowercase `method=fixed` was imported as estimated
  (fixed; `param-fixed-lowercase`).  Variables named like rxode2
  keywords (`rate`, `dur`, `time`, `ii`, ...) were refused by rxode2;
  the `[LONGITUDINAL]` text now renames them `mlx_<name>` before parsing,
  except a macro keyword argument (`cmt=`); as a model input or output
  they are an error (`name-rxode2-keywords`, `name-rxode2-keywords-pk`);
  only `monolix2rx()` checks that, so `mlxtran(equation=TRUE)` alone
  gives an `rx` that does not define such a prediction.
  `X_0` was an initial condition only up to the first underscore, so
  `A_c_0` had none, and `E_0` without a state `E` became `E(0)`, which
  rxode2 refuses (fixed; `ode-init-underscore`, `ode-init-not-state`).
  The `[POPULATION]` prior spelling in `param-bayes` and whether Monolix
  accepts these names (`F`, `rate`, `time`, `ii`) are to confirm in run
  mode.
- `19-data-layout.R`: a line with both a dose and an observation lost
  the observation (fixed; `data-dose-obs-line`, `data-dose-obs-line-iv`).
  Monolix's data format documentation: a line may hold both, the dose is
  given before the observation is made (rxode2 does the same whatever the
  row order), and with an EVID column the observation of an EVID=1/3/4
  line is ignored, so those lines are not split.  A censored shared line
  keeps its censoring on the observation only.
- `20-covariates.R`: a `[COVARIATE] EQUATION:` covariate defined from an
  earlier one was inlined without the earlier one (fixed;
  `cov-equation-bmi`).  `mlxtranGetMutate()` (not used by the import)
  wrote `min()`/`max()` as column aggregates (fixed; unit test only, the
  model gets the elementwise `min()`).  Whether a new occasion resets
  the system without EVID=3/4 is to confirm in run mode
  (`data-occ-no-iov` leaves a week between them, so it does not depend
  on it).
- `21-individual.R`: `if`/`else` in a `[COVARIATE] EQUATION:` left the
  covariate undefined (the inlining assumed assignments only); the
  `if`/`else` and the assignments using it now start the `model()` block,
  and the others stay inlined so they remain mu-referenced covariates
  (fixed; `cov-equation-ifelse`).
  Without random effects the imported `$etaData` (only `id`) became a
  vector, so the validation was skipped (fixed; `param-no-iiv` in the
  mock run); without the file the ids now come from the data.  Whether Monolix runs such a model, and writes an `id`-only
  random effects file, is to confirm in run mode.
- `22-spelling.R`: an `[INDIVIDUAL] DEFINITION:` line had to start with
  `distribution=` (fixed; `param-option-order`); Monolix writes it first,
  so whether Monolix accepts another order is to confirm in run mode,
  as are lowercase distribution names (`param-dist-lowercase`) and empty
  fields as missing values (`data-empty-missing`).
- `23-data-records.R`: the import maps each observation type through its
  position in `yname` (string types, codes 5/2, and `yname={'2', '1'}`
  all pass).  The mock wrote predictions for one continuous endpoint
  only; it now writes `predictions_<observation>.txt` for each, so the
  two-endpoint cases run under the mock.  Whether Monolix keeps a subject
  without observations (`data-no-obs-subject`) is to confirm in run mode.
  The full mock run showed that character subject ids stopped the
  validation (`rxSolve()` refused the individual parameters' character
  `id`, and assigns them by position in the data's subject order); fixed
  (`data-string-id`, now in the CI mock test with `data-ytype-swapped`).
- `24-macros.R`: `empty()` and `reset()` were parsed but their
  administrations were given as ordinary doses (fixed; `macro-empty`,
  `macro-reset`), and with a `Tlag`/`p`/`Tk0` on a `cmt=` macro the
  translation stopped; the lag now applies to the dose's ADM only, since
  rxode2 also delays an `evid=5` event (fixed; `macro-empty-tlag`).
  Only the `PK:` block is read for them (in `EQUATION:` a warning), and
  like any administration the line needs a nonzero amount; an `EVID=4`
  or `ADDL` on such a line is dropped.  rxode2's `evid=3` resets to the
  initial conditions (whether Monolix resets to them or to 0 is to
  confirm).  To confirm in run mode: that Monolix ignores that
  amount, and the order of an empty/reset and an observation at the same
  time (the cases avoid ties).
- `25-error-models.R`: an error parameter shared by two endpoints stopped
  the import (fixed; `err-shared-param`): rxode2 refuses it in two
  endpoints, so the later ones use an alias.  `autoCorrCoef=` is not
  translated (rxode2 has no autocorrelated residuals); it now warns
  (`err-autocorr`; the predictions do not change).  Whether Monolix's
  IWRES accounts for the autocorrelation is to confirm in run mode, as
  are one error parameter in two observation models and a fixed 0
  additive error.  The
  mock maps two observations of one prediction by position
  (`err-same-pred`).
- `26-functions.R`: `rem(a, b)` (two arguments in Monolix's function
  list; the grammar had one and the walker refused it), `sinh()` and the
  negations `~a`/`!a` were syntax errors (fixed; `fun-rem-circadian`,
  `fun-hyperbolic`, `fun-not-operator`).  `rem()` is rxode2's `%%` (C
  `fmod`, the sign of the dividend, like Monolix's), also in a PK macro
  argument; a negation there is an error (R parses `~a + b` as
  `~(a + b)`).  In a `[COVARIATE] EQUATION:` the data side computes it
  with R's floored `%%`, which differs for a negative covariate (no case).
  A negation after a sign or as an exponent (`-~b`, `b^~c`) is still a
  syntax error.  `pkmodel()` or a PK macro followed by an `if` wrote the
  macro argument punctuation before the `if` (fixed).  `mean=` is the mean of the transformed parameter, so the
  values in `param-mean-keyword` are on the log scale; whether a Monolix
  project (not only Simulx) accepts `mean=` is to confirm in run mode.
- `27-covariate-data.R`: rxode2 filled a covariate missing on some lines
  (it passed) but warned that the column was missing for the subject; the
  import now fills a subject's missing covariate values with its value
  (per occasion; a subject with several values is left to rxode2); the
  cases check that the imported data has none missing.  To
  confirm in run mode: that Monolix accepts missing covariate values on
  dose lines and on all but the first line, and that it carries a sparse
  regressor forward (`reg-sparse`).
- `28-start-events.R`: rxode2 integrates from time 0, which is Monolix's
  start with `t_0 = 0` (`ode-t0-before-data` passed as is).  Without
  `t_0`, Monolix starts each subject at its first dose or observation
  (Monolix documentation), so a response with production (`R_0 = 0`) and
  data from t = 24 differed (fixed; `ode-no-t0-late-data`): the imported
  data resets the subject there.  rxode2 ignores a reset (`evid=3`) that is
  a subject's first record, so an `evid=2` row comes first (the truth does
  the same).  The start is the first administration (transit `evid=7`
  and `empty()`/`reset()` lines too; `transit-late-dose`) or
  observation; the start rows take that record's regressors and
  covariates and are not censored.  To confirm in run mode: the
  per-subject start without `t_0` (Lixoft: "the first time value
  encountered for each individual"), and whether a regressor-only line
  starts it (the import assumes not).  A project with an event endpoint
  is left as before: its hazard counts from time 0 when the first record
  is an event (an earlier decision), which this start would contradict;
  which one Monolix does is to confirm.
- `29-start-rem.R`: a non-zero `t_0` was ignored with a warning, so the
  system started at 0 (fixed; `ode-t0-nonzero`): the imported data starts
  every subject whose records begin at or after `t_0` there; a `t_0` that
  is not a number still starts at 0.  `t_0 = 0 ; comment` warned that it
  was non-zero (fixed).  The model inlines covariate equations, so
  `rem()` of a negative covariate was right there (`cov-rem-negative`);
  `mlxtranGetMutate()` (not used by the import) used R's floored `%%`
  (fixed; unit test).  To confirm in run mode: that Monolix starts at a
  `t_0` before the first record, and what it does with records before a
  non-zero `t_0` (the import leaves such a subject starting at 0; no
  case).  A time-to-event project ignores `t_0` too.
  `rightCensoringTime=` (a simulation setting) and `maxEventNumber=3`
  needed no change.  `-~f` and `2^~f` were syntax errors (fixed;
  `fun-not-signed`).
- `reg-ode-input`: whether Monolix's ODE solver restarts at regressor-only
  lines (a small numerical difference) is to confirm in run mode.

## Truth gaps to close with the importer work

- BSMM (run mode): validation needs Monolix's estimated class per
  subject as `mixest`; where Monolix writes it is to confirm.
- A project without `<MONOLIX> [SETTINGS] exportpath` stopped the import
  (`R/parameterUpdate.R`); the results are now read from the directory
  named like the project, Monolix's default (`tasks-no-exportpath`).  To
  confirm in run mode: that default, and what `saveProject()` writes
  without an exportpath (the mock adds `exportpath = 'run'`).

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
