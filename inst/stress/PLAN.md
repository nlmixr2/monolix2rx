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
| `04-data.R` | delimiters, `header=` forms, ignored columns/lines, string IDs, `observationtype`/YTYPE, CENS/LIMIT, regressors, categorical covariates, 2024 `file={path=}`, data in a subdirectory |
| `05-params.R` | logNormal/normal/logitNormal/probitNormal, covariate effects with transformed covariates, correlation blocks, `method=FIXED`, no-variability parameters, `[INDIVIDUAL]` vs `[POPULATION]` |
| `06-error.R` | constant, proportional, combined1/2, `c` variants, logNormal/logitNormal observations, two endpoints |
| `07-tasks.R` | FIM linearization vs SA, conditional mean vs mode, custom `exportpath`, `nbSSDoses`, `odeType` |
| `08-dde.R` | delay differential equations (below) |
| `09-mixture.R` | BSMM and WSMM mixtures (below) |
| `10-iov.R` | inter-occasion variability (below) |
| `11-special.R` | discrete/categorical endpoints, parent/metabolite, resaved project re-import |

### Delay differential equations (`08-dde.R`, tag `dde`)

monolix2rx parses `delay()` but refuses it (`src/equation.c:240`).
rxode2 >= 5.1.7 has `delay(state, T)` with Monolix semantics (constant
initial-condition history).

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
individual, SS with delays.

### Mixtures (`09-mixture.R`, tag `mixture`)

monolix2rx refuses `bsmm()`/`wsmm()` (`src/equation.c:250-259`).

| case | Monolix form | target |
|---|---|---|
| `bsmm-latent-cov-cl` | latent categorical covariate (`P(lcat=1)=plcat1`) as a covariate effect on `Cl` | `mix(cl1, p1, cl2)` |
| `bsmm-latent-3class` | three classes | `mix(a, p1, b, p2, c)` |
| `bsmm-latent-iiv` | latent class on a parameter that also has an eta | `mix()` + eta |
| `bsmm-structural` | `bsmm(M1, p1, M2, 1-p1)` in `EQUATION:` | `mix()` on the predictions |
| `wsmm-two-pred` | `wsmm(f1, p, f2, 1-p)` | `ll()` + simulation path |
| `wsmm-p-iiv` | logitNormal `p` with an eta | as above |
| `wsmm-3comp` | three components | as above |
| `wsmm-combined-err` | combined1 error (component SDs differ) | as above |
| `wsmm-bsmm` | same simulation, wsmm and bsmm variants | |

The true class is a hidden data column (`POP`) removed by `write=`.

BSMM validation: IPRED needs Monolix's assigned class for each subject (read from
`IndividualParameters/`); PRED starts `tol=list(pred=NA)` until the real
run shows which class `popPred` uses.

WSMM translation target (nlmixr2 generalized likelihood):

```r
g1 <- sqrt(a^2 + (b*f1)^2); g2 <- sqrt(a^2 + (b*f2)^2)
ll(CONC) ~ log(p1*dnorm(DV, f1, g1) + (1 - p1)*dnorm(DV, f2, g2))
ipred <- p1*f1 + (1 - p1)*f2              # Monolix's reported prediction: to confirm
cmpSim <- rxbinom(1, p1)
sim <- ifelse(cmpSim == 1, f1, f2) + ifelse(cmpSim == 1, g1, g2)*rxnorm()
```

WSMM translate checks: per-observation log-likelihood vs an independent
mixture likelihood (1e-6); `ipred` vs the truth's mixture mean; a large
simulation reproduces the component proportion (binomial CI) and each
component's mean/SD; optional `fit` tag: an nlmixr2 focei fit recovers
`p`.  The component is drawn per observation in `postSim=`.

### Inter-occasion variability (`10-iov.R`, tag `iov`)

Parsed today (`varlevel={id, id*occ}`, `correlation={level=id*occ,...}`);
three things to check first:

1. `.def2iniRenameOcc()` (`R/def2ini.R:247`) appears to name `id*occ`
   `occ2` while the data column is `occ`.
2. Only one occasion column is mapped (`.use1Rx`), so nested occasions
   have nowhere to go.
3. Monolix writes per-(subject, occasion) individual parameters; the
   validation solve expects one row per subject.

| case | covers |
|---|---|
| `iov-cl-basic` | `use=occasion`; `Cl` with `varlevel={id, id*occ}` |
| `iov-ka-f-multi` | IOV on `ka`, `Tlag`, `p` with different distributions |
| `iov-only` | IOV without BSV (`varlevel=id*occ`) |
| `iov-correlation` | `correlation={level=id*occ, r(ka, Cl)}` plus id-level correlation |
| `iov-nested` | `OCC1`/`OCC2`, `varlevel={id, id*occ1, id*occ1*occ2}` (XFAIL) |
| `iov-unequal-occ` | unequal occasion counts, values not starting at 1, an occasion without observations |
| `iov-no-washout` | occasion change with drug on board, no reset |
| `iov-washout` | occasion change on an `EVID=4` record |
| `iov-ss` | SS restarting at each occasion |
| `iov-time-varying-cov` | covariate changing between occasions |
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
     latent BSMM -> `mix()`; structural `bsmm()`; `wsmm()` -> `ll()` +
     simulation; IOV fixes (name match, per-occasion validation, several
     occasion columns).  Additive only to babelmixr2-facing fields.
4. **Full case list**; record results per Monolix version.

## Risks

- Real rxode2/Monolix differences (dose/observation ties, regressor
  interpolation, SS with lag) need `knownRun=` marks, not importer fixes.
- SAEM moves the estimates; run validation compares at Monolix's own final
  estimates, so drift is harmless.  `est="fixed"` gives a deterministic
  baseline (whether Monolix accepts an all-fixed project: to confirm).
- Monolix cannot run in CI: CI covers translate mode and the mock.
