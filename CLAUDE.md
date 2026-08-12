# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when
working with code in this repository.

## Overview

**monolix2rx** reads a *finished* 'Monolix' run -- the `.mlxtran` project
file, the model text file it points at, its data set, and its results
directory -- and returns an `rxode2`/`nlmixr2` model (an `rxUi`) with the
initial estimates, `thetaMat` covariance, `dfSub`/`dfObs` and solving
options filled in.  When the data and results are available it also solves
the translated model and compares `pred`/`ipred`/`iwres` against Monolix's
own tables, recording the agreement in the model's `meta` environment (it
prints as `validation`; see `R/validate.R`).

The package is mostly R plus C: each block of the `.mlxtran` language has
its own dparser grammar in `inst/*.g`, compiled to a parse table header in
`src/*.g.d_parser.h`, driven by a hand-written tree walker in `src/*.c` that
calls back into R.  `src/util.cpp` is the only C++ (Rcpp glue for those
callbacks and for `lixoftConnectors`).

`babelmixr2` is a reverse dependency: it converts these objects into full
`nlmixr2` fits, so the exported object shape (the `monolix2rxMlxtran` list
layout, the `rxUi` `$meta` entries, `mlxtran()`/`monolix2rx()` signatures)
is an interface other packages read.  A released reverse dependency cannot
be patched retroactively -- do not rename or repurpose an existing field as
an incidental part of another change; add to it instead, and ask first.

## Build and Development Commands

### Install/Build
```r
devtools::load_all()   # compiles src/ as needed
devtools::install()
# Or from the shell: R CMD INSTALL .
```

### Document AND regenerate the parser tables
```r
devtools::document()
```

`R/monolix2rx.R` carries `#' @eval .monolix2rxBuildGram()`, so
`devtools::document()` is what regenerates the generated files:

- every `inst/*.g` -> `src/*.g.d_parser.h` (via `dparser::mkdparse()` plus a
  `file.rename()` of the `.c` to `.h`),
- `R/rxSolve.R` (`.monolix2rxBuildRxSolve()`) -- rebuilt from the
  **installed** `rxode2`'s `rxSolve()` formals, so it changes whenever
  rxode2 changes, independently of anything you edited,
- `R/rxUiGetGen.R` (`.monolix2rxRxUiGetMethods()`).

So always `git diff` after documenting and revert churn you did not intend:
a newer roxygen2 rewrites `NAMESPACE`/`DESCRIPTION`
(`RoxygenNote`/`Config/roxygen2/version`), the unrelated `*.g.d_parser.h`
files pick up a new absolute path in their `#line` directive, and a
roxygen run that dies partway (e.g. `object 'rxSolve' not found` when the
installed rxode2 cannot be loaded) can leave a truncated `NAMESPACE`.

For a **grammar-only** change the same work without the rest is:

```r
dparser::mkdparse("inst/mlxtranFileinfo.g", "src/", grammar_ident="mlxtranFileinfo")
file.rename("src/mlxtranFileinfo.g.d_parser.c", "src/mlxtranFileinfo.g.d_parser.h")
pkgbuild::compile_dll()
```

The committed headers all carry `#line 7 "/home/matt/src/monolix2rx/src/..."`;
normalize a regenerated header to that path rather than committing your own
checkout/worktree path.

### Tests
```r
devtools::test()
testthat::test_local(filter="fileinfo")   # one file, no "test-"/".R"
testthat::snapshot_accept()               # after an intended print() change
```

`tests/testthat/test-demos.R` walks `getOption("monolix2rx.demo")` and parses
every `.mlxtran` under it, asserting nothing is left unparsed.  The option is
unset by default, so that file silently tests nothing -- point it at a Monolix
demo tree when touching the parsers.

### R CMD check
```r
devtools::check()
```

## Architecture

### Import pipeline

1. **Sectioning** (`R/mlxtran.R`).  `mlxtran()` reads the lines and
   `.mlxtranParseItem()` only splits them into `<SECTION>` / `[SUBSECTION]` /
   `SUBSUB:` buckets in `.mlxEnv$lst` (text is transliterated to ASCII with
   `stringi` first).  No block syntax is understood at this stage.
2. **Per-block parsing** (`.mlxtranFinalize()`).  Each bucket's raw text is
   handed to the parser for that block, which `.Call()`s the matching
   `trans_*` C entry point.  Before that, `.mlxtran()` reads the
   `<MODEL> [LONGITUDINAL] file=` model text file and folds its blocks into
   the same section list (`mlxTxt()`), so the equations arrive in the parsed
   object as if they had been written inline.
3. **Walkers and R callbacks.**  Each `trans_*` runs `dparse()` and then
   `wprint_parsetree_*()`, which recurses over the parse tree and dispatches
   on the dparser **node name as a string** (`"filename_t1"`, `"identifier"`,
   ...), calling into R through the `monolix2rxSingle(value, ".rFun")` family
   (`src/util.h` macros, `src/util.cpp` glue).  Those R functions accumulate
   into the `.monolix2rx` environment; the `.xxxIni()`/`.xxx()` pair in R
   resets it, parses, and collects the result into a classed list.
4. **Translation** (`R/monolix2rx.R`).  The parsed object is turned into
   `ini({})` + `model({})` text -- `[INDIVIDUAL] DEFINITION:` via
   `R/def2ini.R` and `R/indDef.R`, `PK:` macros via `R/pk2rx.R` /
   `R/pkmodel2macro.R`, `EQUATION:` via `R/equation.R`, error models via
   `R/singleEndpoint.R`, covariate transforms via
   `R/mlxtranTransformGetRxCode.R` -- then parsed as R code and validated
   against the Monolix results (`R/validate.R`, `R/ipredImport.R`,
   `R/dataImport.R`).

> [!IMPORTANT]
> The walkers key on node names, so **adding a production to a grammar can
> silently feed new text to an existing handler.**  In `mlxtranFileinfo.c`
> every `identifier` node becomes a data-set header column; in `mlxtranInd.c`
> it can become an `input=` name.  When adding syntax either use literal
> terminals (a quoted `'path'` in the grammar is not an `identifier`) or give
> the construct its own node name and stop the recursion for it.  A parse that
> "works" is not enough -- assert the *other* fields of the parsed object in a
> test, because the failure mode is a spurious extra header/input, not an
> error.

### Block -> grammar -> walker -> R

| mlxtran block | grammar (`inst/`) | walker (`src/`) | R (`R/`) |
|---|---|---|---|
| `<DATAFILE>`/`<DATA_FORMATTING> [FILEINFO]` | `mlxtranFileinfo.g` | `mlxtranFileinfo.c` | `fileinfo.R` |
| `[CONTENT]` | `mlxtranContent.g` | `mlxtranContent.c` | `content.R` |
| `<DATAFILE> [SETTINGS]` | `dataSettings.g` | `dataSettings.c` | `dataSettings.R` |
| `[COVARIATE]`/`[INDIVIDUAL]`/`[LONGITUDINAL]` headers (`input=`, `file=`, categories, regressors) | `mlxtranInd.g` | `mlxtranInd.c` | `ind.R`, `long.R` |
| `[INDIVIDUAL] DEFINITION:`, `[POPULATION] DEFINITION:` | `mlxtranIndDefinition.g` | `mlxtranIndDefinition.c` | `indDef.R`, `popDef.R` |
| `[LONGITUDINAL] DEFINITION:`, `[COVARIATE] DEFINITION:` | `longDef.g` | `longDef.c` | `longDef.R` |
| `[LONGITUDINAL] EQUATION:`/`PK:`, `[COVARIATE] EQUATION:` | `equation.g` | `equation.c` | `equation.R`, `pk.R`, `covEq.R` |
| `[LONGITUDINAL] OUTPUT:` | `longOutput.g` | `longOutput.c` | `longOut.R` |
| `<FIT>` | `mlxtranFit.g` | `mlxtranFit.c` | `fit.R` |
| `<PARAMETER>` | `mlxtranParameter.g` | `mlxtranParameter.c` | `parameter.R` |
| `<MONOLIX> [TASKS]` | `mlxtranTask.g` | `mlxtranTask.c` | `task.R` |
| `<MONOLIX> [SETTINGS]` | `mlxtranOp.g` | `mlxtranOp.c` | `mlxtranOp.R` |
| `summary.txt` DATASET INFORMATION | `summaryData.g` | `summaryData.c` | `summaryData.R` |

### Round-trip invariant

Every parsed block is a classed list with `as.character()` (re-emitting
mlxtran text), `print()` (which uses it, and is snapshot-tested) and
`as.list()`.  When the grammar learns a new spelling of something, keep
`as.character()` emitting the **canonical/older** form -- the accepted input
set widens, the output stays stable so the snapshots keep their meaning.

### Adding or changing a grammar

Each `src/<name>.c` `#define`s the shared parser symbols (`curP`, `gBuf`,
`_pn`, `freeP`, `parseFree`, ...) to per-grammar names before including
`parseSyntaxErrors.h`, sets `record` to the block name for error messages,
and calls `dparse()` with the length from `monolix2rxParseLen()` (`src/util.h`).
A brand-new grammar therefore needs: `inst/<name>.g`, a `mkdparse()` stanza in
`R/buildParser.R`, `src/<name>.c` following that pattern, its `_monolix2rx_trans_*`
prototype in `src/util.h`, an entry in the `callMethods[]` table in
`src/init.c`, and its `<name>_parseFree()` added to `monolix2rx_full_parseFree()`
in `src/mem.c` (otherwise the parser leaks between calls).

(`inst/mlxtranPk.g` is dormant -- its `mkdparse()` call is commented out in
`R/buildParser.R` and `PK:` blocks go through `equation.g`.)

`src/init.c` is the **manual** `.Call` registration table with HARDCODED
argument counts (e.g. `{"_monolix2rx_trans_individual", ..., 2}`).  Changing an
entry point's arity without editing `init.c` does not fail to compile -- it
surfaces at runtime as `Incorrect number of arguments (N), expecting M`.

### Generated files (do not edit manually)

- `src/*.g.d_parser.h` -- from `inst/*.g` via dparser
- `R/rxSolve.R` -- from `.monolix2rxBuildRxSolve()` in `R/buildParser.R`
- `R/rxUiGetGen.R` -- from `.monolix2rxRxUiGetMethods()` in `R/buildParser.R`
- Regenerate all of them with `devtools::document()`.

### Important files

| File | Purpose |
|------|---------|
| `R/mlxtran.R` | `mlxtran()`; sectioning and `.mlxtranFinalize()` dispatch |
| `R/monolix2rx.R` | `monolix2rx()`; assembles the `rxUi` |
| `R/buildParser.R` | Build-time generation (grammars, `rxSolve.R`, `rxUiGetGen.R`) |
| `R/validate.R` | Solve-and-compare against the Monolix results |
| `R/dataImport.R`, `R/ipredImport.R`, `R/etaImport.R` | Read the Monolix data set / prediction / eta tables |
| `R/def2ini.R`, `R/mlxtranJac.R` | Distribution definitions -> `ini({})`, covariance transforms |
| `R/pk2rx.R`, `R/pkmodel2macro.R` | `PK:` macros -> rxode2 |
| `R/mlxtranLib.R`, `R/lixoftConnectors.R` | `lib:` model files, optional `lixoftConnectors` |
| `src/util.h` | Callback macros, `trans_*` prototypes, `monolix2rxParseLen()` |
| `src/util.cpp` | Rcpp glue calling the `.rFun` callbacks |
| `src/mem.c` | Whole-package parser init/free |
| `src/init.c` | Manual `.Call` registration table |

### Testing

- testthat edition 3; snapshots in `tests/testthat/_snaps`.
- Parser tests call the internal `.xxx()` on a text fragment and assert the
  parsed fields (see `tests/testthat/test-fileinfo.R`).
- End-to-end tests use the shipped projects: `system.file("theo", package="monolix2rx")`
  and `system.file("cov", package="monolix2rx")`.  Copy one to `tempdir()` when a
  test needs to modify it.
- Tests that need a real Monolix install (`lixoftConnectors`) or the demo tree
  skip themselves; keep it that way.

## R Code Style

- **Exported functions**: `camelCase` (e.g. `monolix2rx`, `mlxtran`,
  `monolixEndpoints`)
- **Internal/non-exported functions**: `.camelCase` with a leading dot (e.g.
  `.fileinfo`, `.mlxtranFinalize`)
- **Local variables inside functions**: `.camelCase` with a leading dot (e.g.
  `.ret`, `.lines`, `.mlxtran`) -- this is pervasive here and deliberate; it
  keeps locals from colliding with model/data variable names.
- **R callbacks invoked from C** are internal and dotted, and their names are
  spelled out in `src/util.h` macros -- renaming one means editing both sides.
- **S3 methods**: `generic.class` (e.g. `as.character.monolix2rxFileinfo`,
  `rxSolve.monolix2rx`)
- Avoid `snake_case` for new names.  Pharmacometric parameter names keep their
  conventional capitalization (`.Tlag`, `.Cl`, `.Vm`) even though the linter
  objects.
- American English spelling; ASCII only.

### Linting

`.lintr` matches rxode2's configuration (camelCase `object_name_linter` with
the internal-dot and S3 regexes, 120-character lines, `cyclocomp` 50;
`infix_spaces_linter`/`commas_linter` disabled).  Run
`lintr::lint_package()`.  There is a pre-existing backlog -- mostly
domain-capitalized PK names, Monolix-style `*_pop` names in tests, and
indentation -- so judge a change by whether it *adds* violations, and do not
mass-rename to satisfy the linter.

### C/C++ Conventions

- The parser `.c` files are C and follow the dparser walker pattern described
  above; do not add Rcpp to them (`src/util.cpp` is the only C++ file).
- `#define _(String) (String)` and the per-grammar `#define`s must come before
  including `parseSyntaxErrors.h`.
- Pass the parse buffer length through `monolix2rxParseLen()` rather than
  `strlen()` directly.
- Node values come from `rc_dup_str()`; the quote-stripping idiom in the
  walkers (`v++; v[strlen(v)-1] = 0;`) assumes the token really is quoted --
  only use it on a terminal whose regex guarantees both quotes.
- C++ (`src/util.cpp`) uses `#define USE_FC_LEN_T` / `#define STRICT_R_HEADERS`
  and Rcpp's `BEGIN_RCPP`/`END_RCPP` around anything that calls back into R.

## Documentation and Comment Style

- Keep comments and documentation terse.  One line is usually enough; explain
  the non-obvious *why*, not what the code plainly does.  No worked examples or
  numeric anecdotes in comments.
- Roxygen: short descriptions (a sentence or two), but keep every `@param`,
  `@return`, `@author`, `@export`/`@noRd`, and `@examples` tag.  Internal
  helpers are documented with `@noRd`.
- `NEWS.md` is organized per version (`# monolix2rx X.Y.Z`) as past-tense
  bullets: user-facing changes first, then bug fixes.  Group a long list of
  fixes by subsystem in `### <category>` subsections, and add to an existing
  entry/section rather than creating a near-duplicate.  Reference the issue
  number when there is one.  No multi-paragraph root-cause narration.
- ASCII only, everywhere in the repo (CRAN requirement, and `.mlxtran` input is
  transliterated to ASCII anyway): `--` for em-dashes, `->` for arrows,
  straight quotes, `...` for ellipses.

## Monolix Version Notes

Monolix keeps extending the `.mlxtran` language, and a project saved by a newer
Monolix must still import here.  Accept the new spelling *in addition to* the
old one, keep `as.character()` on the old one, and note the Monolix version in
the grammar comment.  Known widenings:

- 2024: `file={path='data.csv'}` is equivalent to `file='data.csv'`, both for
  the `[FILEINFO]` data set and the `[LONGITUDINAL]` model file (issue #43).
- A model file may be a library reference (`file='lib:oral1_1cpt_kaVCl.txt'`),
  resolved by `R/mlxtranLib.R` or, when the Monolix install is available, by
  `lixoftConnectors` (`R/lixoftConnectors.R`).
