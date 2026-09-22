test_that(".parameterUpdate keeps FIXED/unmatched parameter values instead of NA", {
  # Reproduce the bug: when populationParameters.txt only contains MLE
  # parameters (Monolix omits FIXED ones), the old code replaced every
  # unmatched parameter with NA_real_.  The fixed code keeps the original
  # value.
  .pkgTheo <- system.file("theo", package = "monolix2rx")
  skip_if_not(dir.exists(.pkgTheo))

  .tmp <- withr::local_tempdir()
  file.copy(list.files(.pkgTheo, full.names = TRUE), .tmp, recursive = TRUE)

  # Build a populationParameters.txt that intentionally omits some parameters
  # (mimicking what Monolix writes when some are FIXED).
  .mlx <- withr::with_dir(.tmp,
    suppressMessages(mlxtran("theophylline_project.mlxtran")))
  .par <- .mlx$PARAMETER$PARAMETER
  # keep only ka_pop and V_pop as the "MLE-estimated" subset
  .subset <- .par[.par$name %in% c("ka_pop", "V_pop"), ]
  .popParFile <- file.path(.tmp, "tp", "populationParameters.txt")
  write.csv(
    data.frame(parameter = .subset$name, value = .subset$value * 10),
    .popParFile, row.names = FALSE, quote = FALSE
  )

  .upd <- withr::with_dir(.tmp,
    suppressMessages(.parameterUpdate(.mlx)))

  # Parameters present in the file must be updated
  expect_equal(
    .upd$PARAMETER$PARAMETER$value[.upd$PARAMETER$PARAMETER$name == "ka_pop"],
    .par$value[.par$name == "ka_pop"] * 10
  )
  expect_equal(
    .upd$PARAMETER$PARAMETER$value[.upd$PARAMETER$PARAMETER$name == "V_pop"],
    .par$value[.par$name == "V_pop"] * 10
  )

  # Parameters absent from the file must keep their original values (not NA)
  .missing <- .par$name[!.par$name %in% c("ka_pop", "V_pop")]
  for (.n in .missing) {
    .orig <- .par$value[.par$name == .n]
    .got  <- .upd$PARAMETER$PARAMETER$value[.upd$PARAMETER$PARAMETER$name == .n]
    expect_false(is.na(.got),
      label = paste0("value for '", .n, "' must not be NA after update"))
    expect_equal(.got, .orig,
      label = paste0("value for '", .n, "' must be unchanged"))
  }
})

test_that(".monolixDataLoad returns NULL gracefully when FILEINFO$file is absent", {
  # Reproduce the crash: if [FILEINFO] is commented-out (or missing), the
  # parsed object has no $file entry, so file.exists(NULL) returns logical(0)
  # and `if (.try)` throws "argument is of length zero".
  .pkgTheo <- system.file("theo", package = "monolix2rx")
  skip_if_not(dir.exists(.pkgTheo))

  .tmp <- withr::local_tempdir()
  file.copy(list.files(.pkgTheo, full.names = TRUE), .tmp, recursive = TRUE)

  # Read the project normally, then surgically remove the file entry to
  # simulate a project whose [FILEINFO] block is entirely commented out.
  .mlx <- withr::with_dir(.tmp,
    suppressMessages(mlxtran("theophylline_project.mlxtran")))
  .mlx$DATAFILE$FILEINFO$FILEINFO$file <- character(0)

  # Must not throw "argument is of length zero"; must return NULL silently.
  expect_null(withr::with_dir(.tmp,
    suppressMessages(.monolixDataLoad(.mlx))))
})

# ---------------------------------------------------------------------------
# .parameterUpdateFromSummary -- one test per summary.txt in the package,
# verifying that every shipped file parses without error and that the known
# parameter values (taken from the matching populationParameters.txt where
# available, or read directly from the file) are recovered correctly.
# ---------------------------------------------------------------------------

test_that(".parameterUpdateFromSummary parses theo/tp summary.txt (Monolix 5.1.1 pre-2020 heading)", {
  .f <- system.file("theo", "tp", "summary.txt", package = "monolix2rx")
  skip_if_not(file.exists(.f))
  .res <- .parameterUpdateFromSummary(.f)
  expect_s3_class(.res, "data.frame")
  expect_named(.res, c("parameter", "value"))
  expect_true(nrow(.res) > 0L)
  expect_false(any(is.na(.res$value)))
  # spot-check against populationParameters.txt values
  .pop <- read.csv(system.file("theo", "tp", "populationParameters.txt",
                               package = "monolix2rx"))
  for (.n in .pop$parameter) {
    .sv <- .res$value[.res$parameter == .n]
    .pv <- .pop$value[.pop$parameter == .n]
    expect_true(length(.sv) == 1L, label = paste0("parameter '", .n, "' found in summary"))
    # summary.txt rounds to ~3-4 sig figs; allow 1 % relative tolerance
    expect_equal(.sv, .pv, tolerance = 0.01,
                 label = paste0("summary value for '", .n, "'"))
  }
})

test_that(".parameterUpdateFromSummary parses theo/pm summary.txt (Monolix 2023R1)", {
  .f <- system.file("theo", "pm", "summary.txt", package = "monolix2rx")
  skip_if_not(file.exists(.f))
  .res <- .parameterUpdateFromSummary(.f)
  expect_s3_class(.res, "data.frame")
  expect_named(.res, c("parameter", "value"))
  expect_true(nrow(.res) > 0L)
  expect_false(any(is.na(.res$value)))
  # spot-check a few known parameters
  .pop <- read.csv(system.file("theo", "pm", "populationParameters.txt",
                               package = "monolix2rx"))
  for (.n in .pop$parameter) {
    .sv <- .res$value[.res$parameter == .n]
    .pv <- .pop$value[.pop$parameter == .n]
    expect_true(length(.sv) == 1L, label = paste0("parameter '", .n, "' found in summary"))
    expect_equal(.sv, .pv, tolerance = 0.01,
                 label = paste0("summary value for '", .n, "'"))
  }
})

test_that(".parameterUpdateFromSummary parses cov/w1 summary.txt (2023R1, SA FIM)", {
  .f <- system.file("cov", "w1", "summary.txt", package = "monolix2rx")
  skip_if_not(file.exists(.f))
  .res <- .parameterUpdateFromSummary(.f)
  expect_s3_class(.res, "data.frame")
  expect_named(.res, c("parameter", "value"))
  expect_false(any(is.na(.res$value)))
  .pop <- read.csv(system.file("cov", "w1", "populationParameters.txt",
                               package = "monolix2rx"))
  for (.n in .pop$parameter) {
    .sv <- .res$value[.res$parameter == .n]
    expect_true(length(.sv) == 1L, label = paste0("'", .n, "' found in w1 summary"))
    expect_equal(.sv, .pop$value[.pop$parameter == .n], tolerance = 0.01,
                 label = paste0("w1 summary value for '", .n, "'"))
  }
})

test_that(".parameterUpdateFromSummary parses cov/w2 summary.txt (2023R1, Lin FIM)", {
  .f <- system.file("cov", "w2", "summary.txt", package = "monolix2rx")
  skip_if_not(file.exists(.f))
  .res <- .parameterUpdateFromSummary(.f)
  expect_s3_class(.res, "data.frame")
  expect_named(.res, c("parameter", "value"))
  expect_false(any(is.na(.res$value)))
  .pop <- read.csv(system.file("cov", "w2", "populationParameters.txt",
                               package = "monolix2rx"))
  for (.n in .pop$parameter) {
    .sv <- .res$value[.res$parameter == .n]
    expect_true(length(.sv) == 1L, label = paste0("'", .n, "' found in w2 summary"))
    expect_equal(.sv, .pop$value[.pop$parameter == .n], tolerance = 0.01,
                 label = paste0("w2 summary value for '", .n, "'"))
  }
})

test_that(".parameterUpdateFromSummary parses cov/w3 summary.txt (2023R1, covariate model)", {
  .f <- system.file("cov", "w3", "summary.txt", package = "monolix2rx")
  skip_if_not(file.exists(.f))
  .res <- .parameterUpdateFromSummary(.f)
  expect_s3_class(.res, "data.frame")
  expect_named(.res, c("parameter", "value"))
  expect_false(any(is.na(.res$value)))
  .pop <- read.csv(system.file("cov", "w3", "populationParameters.txt",
                               package = "monolix2rx"))
  for (.n in .pop$parameter) {
    .sv <- .res$value[.res$parameter == .n]
    expect_true(length(.sv) == 1L, label = paste0("'", .n, "' found in w3 summary"))
    expect_equal(.sv, .pop$value[.pop$parameter == .n], tolerance = 0.01,
                 label = paste0("w3 summary value for '", .n, "'"))
  }
})

test_that(".parameterUpdateFromSummary parses cov/pb summary.txt (2023R1, APGAR categories)", {
  .f <- system.file("cov", "pb", "summary.txt", package = "monolix2rx")
  skip_if_not(file.exists(.f))
  .res <- .parameterUpdateFromSummary(.f)
  expect_s3_class(.res, "data.frame")
  expect_named(.res, c("parameter", "value"))
  expect_false(any(is.na(.res$value)))
  .pop <- read.csv(system.file("cov", "pb", "populationParameters.txt",
                               package = "monolix2rx"))
  for (.n in .pop$parameter) {
    .sv <- .res$value[.res$parameter == .n]
    expect_true(length(.sv) == 1L, label = paste0("'", .n, "' found in pb summary"))
    expect_equal(.sv, .pop$value[.pop$parameter == .n], tolerance = 0.01,
                 label = paste0("pb summary value for '", .n, "'"))
  }
})

test_that(".parameterUpdate falls back to summary.txt with a warning when populationParameters.txt is absent", {
  .pkgTheo <- system.file("theo", package = "monolix2rx")
  skip_if_not(dir.exists(.pkgTheo))

  .tmp <- withr::local_tempdir()
  file.copy(list.files(.pkgTheo, full.names = TRUE), .tmp, recursive = TRUE)

  # Remove populationParameters.txt to force the summary.txt fallback
  file.remove(file.path(.tmp, "tp", "populationParameters.txt"))

  .mlx <- withr::with_dir(.tmp,
    suppressMessages(mlxtran("theophylline_project.mlxtran", update = FALSE)))

  # Must warn about rounding and still update from summary.txt
  expect_warning(
    .upd <- withr::with_dir(.tmp, suppressMessages(.parameterUpdate(.mlx))),
    "populationParameters.txt not found"
  )
  expect_warning(
    withr::with_dir(.tmp, suppressMessages(.parameterUpdate(.mlx))),
    "summary.txt"
  )

  # Values should be close (summary.txt is rounded) to the known finals
  .known <- read.csv(
    system.file("theo", "tp", "populationParameters.txt", package = "monolix2rx"))
  for (.n in .known$parameter) {
    .got <- .upd$PARAMETER$PARAMETER$value[.upd$PARAMETER$PARAMETER$name == .n]
    expect_true(length(.got) == 1L, label = paste0("'", .n, "' updated from summary"))
    expect_false(is.na(.got), label = paste0("'", .n, "' not NA after summary fallback"))
    expect_equal(.got, .known$value[.known$parameter == .n], tolerance = 0.01,
                 label = paste0("summary fallback value for '", .n, "'"))
  }
})

test_that(".parameterUpdateFromSummary returns NULL for a file with no population parameters section", {
  .tmp <- withr::local_tempdir()
  .f <- file.path(.tmp, "empty_summary.txt")
  writeLines(c("DATASET INFORMATION", "Number of individuals: 10"), .f)
  expect_null(.parameterUpdateFromSummary(.f))
})

test_that(".parameterUpdateFromSummary handles scientific notation, negative values, and nan/NA/NaN value tokens", {
  .tmp <- withr::local_tempdir()
  .f <- file.path(.tmp, "sci_summary.txt")
  writeLines(c(
    "ESTIMATION OF THE POPULATION PARAMETERS ________________________________________",
    "Fixed Effects ----------------------------     se_sa    rse(%)",
    "Kmax_pop   :            0.556    0.0302      5.44",
    "IC50_pop   :         1.25e+05   4.6e+03      3.68",
    "Q_pop      :            1e-06",
    "beta_neg   :           -0.195     0.123",
    "bad_nan    :              nan     0.123",
    "bad_NA     :               NA     0.456",
    "bad_NaN    :              NaN     0.789",
    " _______________________________________________________________________________"
  ), .f)
  # nan/NA/NaN as the value token must be returned as NA_real_ (not dropped,
  # not causing a warning -- it is a legitimate "no estimate" result)
  .res <- .parameterUpdateFromSummary(.f)
  # warned parameters are present with NA value
  expect_true("bad_nan" %in% .res$parameter)
  expect_true("bad_NA"  %in% .res$parameter)
  expect_true("bad_NaN" %in% .res$parameter)
  expect_true(is.na(.res$value[.res$parameter == "bad_nan"]))
  expect_true(is.na(.res$value[.res$parameter == "bad_NA"]))
  expect_true(is.na(.res$value[.res$parameter == "bad_NaN"]))
  # valid parameters are still present and correct
  expect_equal(.res$value[.res$parameter == "Kmax_pop"], 0.556)
  expect_equal(.res$value[.res$parameter == "IC50_pop"], 1.25e5)
  expect_equal(.res$value[.res$parameter == "Q_pop"],    1e-6)
  expect_equal(.res$value[.res$parameter == "beta_neg"], -0.195)
  # the nan/NA/NaN rows are the only NAs in the result
  expect_equal(sum(is.na(.res$value)), 3L)
  expect_false(any(is.nan(.res$value)))
})

# ---------------------------------------------------------------------------
# External summary.txt files from ../eci830_exploratory (skipped when the
# repo is not present).  No populationParameters.txt exists for any of these
# so spot-checks use values read directly from the summary.txt itself.
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# External summary.txt files (skipped when the sibling repo is not present).
# The helper resolves paths relative to the package source root so neither
# the repo name nor any model paths are embedded here.
# ---------------------------------------------------------------------------

.externalSummary <- function(...) {
  .pkgRoot <- dirname(dirname(normalizePath(testthat::test_path(),
                                            mustWork = FALSE)))
  normalizePath(file.path(dirname(.pkgRoot), ...), winslash = "/",
                mustWork = FALSE)
}

test_that(".parameterUpdateFromSummary works on external summary.txt files (2024R1, MLE + FIXED mix)", {
  # Locate every summary.txt one level up from the package source root;
  # skip the whole test when none are found so CI stays clean.
  .root <- dirname(dirname(normalizePath(testthat::test_path(),
                                         mustWork = FALSE)))
  .files <- list.files(dirname(.root), pattern = "^summary\\.txt$",
                       recursive = TRUE, full.names = TRUE)
  # Only keep files outside the package itself
  .files <- .files[!startsWith(normalizePath(.files, mustWork = FALSE),
                                normalizePath(.root, mustWork = FALSE))]
  skip_if(length(.files) == 0L, "no external summary.txt files found")

  for (.f in .files) {
    .res <- .parameterUpdateFromSummary(.f)
    if (is.null(.res)) next
    .lbl <- basename(dirname(.f))
    expect_true(is.data.frame(.res),
                label = paste0("data.frame from ", .lbl))
    expect_equal(names(.res), c("parameter", "value"),
                 label = paste0("columns from ", .lbl))
    expect_true(nrow(.res) > 0L,
                label = paste0("rows from ", .lbl))
    expect_true(is.numeric(.res$value),
                label = paste0("numeric values from ", .lbl))
    expect_false(any(is.nan(.res$value)),
                 label = paste0("no NaN from ", .lbl))
  }
})
