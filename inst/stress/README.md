# monolix2rx Monolix stress kit

The stress kit checks how monolix2rx imports Monolix runs, case by case.
For each case it:

1. **simulates** a Monolix-style dataset with an rxode2 model (the
   "truth"), using the same Monolix-faithful solving options that
   monolix2rx validates with;
2. writes the **data file, model file and project** (`run.mlxtran`), whose
   initial estimates equal the true values;
3. **runs Monolix**; and
4. **imports** the run with `monolix2rx()` and checks it: Monolix's own
   predictions against rxode2's, and the project Monolix resaved.

It has two modes:

- **translate**: steps 1, 2 and 4 without Monolix. The project is imported
  before any results exist, the translated model is solved at the true
  values (with the random effects set to zero, and at the simulated etas)
  and compared with the rxode2 truth, and the imported omegas and residual-error parameters are
  compared with the truth. No Monolix is needed.
- **run**: also runs Monolix and validates the import against Monolix's
  output. Use this mode on a machine that has Monolix.

The plan, the case list still to write and the importer work it drives
are in [PLAN.md](PLAN.md).

## Quick start: the kit for a Monolix machine

Everything runs from the R console, for example in RStudio; no terminal
is needed.

1. In a fresh R session (in RStudio: Session -> Restart R), load the
   monolix2rx version to test and the kit:

   ```r
   devtools::load_all("path/to/monolix2rx")
   source(system.file("stress", "stress.R", package = "monolix2rx"))
   ```

2. Check the versions and that Monolix is found:

   ```r
   stressCheck()
   ```

   Monolix is run through `lixoftConnectors` (in a separate R process per
   case) when it is installed, or with a command template given by
   `options(monolix2rx.monolix=)`, `options(babelmixr2.monolix=)` or
   `monolix=`; `{mlxtran}` in the template is replaced by the project file.

3. Run the kit:

   ```r
   res <- stressKit()
   res <- stressKit(cases = "pkmodel")      # a few cases first
   res <- stressKit(est = "fixed")          # population parameters fixed: faster
   res[res$status %in% c("FAIL", "ERROR", "XPASS"), c("case", "status", "note")]
   ```

   `stressKit()` zips the output folder
   (`monolix2rx-stress-<date>-<time>.zip`; `attr(res, "zip")`).

4. Send the zip file back.

`stressKit()` arguments: `monolix=`, `modes=` (`"translate"` and/or
`"run"`), `cases=` (a regular expression), `tags=`, `est=` (`"full"` or
`"fixed"`), `nSub=`, `jobs=` (cases at once, in background R sessions;
safe in RStudio and on Windows), `timeout=`, `out=`, `bundle=`.
`stressList()` lists the cases.

### Without Monolix

```r
res <- stressKit(modes = "translate", bundle = FALSE)
```

### Back home: replaying a returned zip

```r
devtools::load_all("path/to/monolix2rx")
source(system.file("stress", "stress.R", package = "monolix2rx"))
res <- stressReplay("monolix2rx-stress-20261007-101500.zip")
```

## Running it with Rscript

```sh
STRESS=inst/stress/run-stress.R   # in a monolix2rx checkout
Rscript "$STRESS" --check
Rscript "$STRESS" --list
Rscript "$STRESS" --mode=translate
Rscript "$STRESS" --kit
Rscript "$STRESS" --replay=monolix2rx-stress-20261007-101500.zip
```

The exit status is 1 when any case fails.

## Output

The output directory has `results.csv` (one row per case), `summary.md`,
`sessionInfo.txt` and one directory per case with `run.mlxtran`,
`model.txt`, `data.csv`, the simulation (`sim.rds`), Monolix's results
(`run/`), `run-resaved.mlxtran`, `monolix.log` and the import logs.

`status` is `PASS`, `FAIL`, `ERROR` (the kit itself failed), `XFAIL` (a
known issue; the diagnosis is in `note`), `XPASS` (a known issue that now
passes) or `SKIP` (the case needs a newer Monolix).  The checks:

- `dryMaxRel`: largest % difference between the translated PRED and the
  truth (passes at 0.01 %); `dryIpredMaxRel`: the same for IPRED at the
  truth's simulated etas, which checks how the etas enter the parameters;
  `dryOmegaDiff`/`dryErrDiff`: largest relative
  difference of the imported omega/residual parameters (passes at 1e-6)
- `ipredRtol`/`predRtol`: median % difference between Monolix and rxode2
  (passes at 1 %; computed by the kit from monolix2rx's compared rows,
  monolix2rx's own values are `pkgIpredRtol`/`pkgPredRtol`); `ipredQ95`/`predQ95`: 95th percentiles (5 %)
- `nNotMatched`: Monolix predictions rxode2 did not reproduce (must be 0);
  `dfSub`/`dfObs` must equal the data; `thetaMatPd`: the covariance is
  positive definite; `resaved`: the resaved project imports

Per-case thresholds are set with `tol=list(dry=, mat=, ipred=, pred=,
ipredQ95=, predQ95=, iwres=, validate=)`.

## Self-test without Monolix

`mock/fake-monolix.R` stands in for Monolix so the run/import plumbing can
be checked without a license (it writes Monolix-format results from the
rxode2 truth, including the true random effects):

```r
stressKit(monolix = paste("Rscript", system.file("stress", "mock", "fake-monolix.R",
                                                  package = "monolix2rx"), "{mlxtran}"),
          bundle = FALSE, out = tempfile("stress-mock"))
```

This is **not** Monolix and proves nothing about Monolix's behavior.

## Writing a case

See the field list at the top of `R/case.R` and `cases/01-pk.R`.  Rules of
thumb:

- Write `<PARAMETER>` values **equal to** the `sim` values (Monolix `sd=`
  omegas are the square roots of the rxode2 variances).
- Name the `sim` etas like the Monolix omega parameters (`omega_Cl`) and
  the residual parameters like the Monolix ones (`a`, `b`).
- Avoid records that tie in TIME unless ties are the point of the case.
- `kitVariant(base, name, covers, ...)` reuses a case with some fields
  overridden; `known=`/`knownRun=` record an understood failure.
