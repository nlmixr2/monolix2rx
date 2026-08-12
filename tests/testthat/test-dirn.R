test_that("mlxtran() finds the project files when 'dirn' is given (issue #44)", {
  .theo <- system.file("theo", package = "monolix2rx")
  skip_if_not(dir.exists(.theo))

  .lines <- readLines(file.path(.theo, "theophylline_project.mlxtran"))

  withr::with_dir(tempdir(), {
    # without 'dirn' the mlxtran lines are resolved against the working
    # directory, so the model text file cannot be found
    expect_error(monolix2rx(.lines, update = FALSE),
                 "does not exist")
    # the error names the directory searched and points at 'dirn='
    expect_error(monolix2rx(.lines, update = FALSE),
                 normalizePath(tempdir(), winslash = "/"), fixed = TRUE)
    expect_error(monolix2rx(.lines, update = FALSE), "dirn=", fixed = TRUE)
    expect_error(mlxTxt("oral1_1cpt_kaVCl.txt"), "dirn=", fixed = TRUE)

    .mlx <- mlxtran(.lines, dirn = .theo)
    expect_equal(normalizePath(attr(.mlx, "dirn"), winslash = "/"),
                 normalizePath(.theo, winslash = "/"))
    expect_true(!is.null(.mlx$MODEL$LONGITUDINAL$EQUATION))

    .ui <- monolix2rx(.lines, dirn = .theo, update = FALSE)
    expect_true(inherits(.ui, "monolix2rx"))
  })
})

test_that("'dirn' resolves a relative mlxtran/txt file name", {
  .theo <- system.file("theo", package = "monolix2rx")
  skip_if_not(dir.exists(.theo))

  withr::with_dir(tempdir(), {
    .mlx <- mlxtran("theophylline_project.mlxtran", dirn = .theo)
    expect_true(inherits(.mlx, "monolix2rxMlxtran"))

    .txt <- mlxTxt("oral1_1cpt_kaVCl.txt", dirn = .theo)
    expect_true(inherits(.txt, "monolix2rxMlxtran"))

    # an absolute file name still wins over 'dirn'
    .mlx <- mlxtran(file.path(.theo, "theophylline_project.mlxtran"),
                    dirn = tempdir())
    expect_equal(normalizePath(attr(.mlx, "dirn"), winslash = "/"),
                 normalizePath(.theo, winslash = "/"))
  })
})

test_that("'dirn' never captures an absolute or 'lib:' file name", {
  # file.path("/proj", "/tmp/m.txt") is "/proj//tmp/m.txt", so a
  # same-named file under 'dirn' could silently win over the absolute
  # name the user asked for
  .d <- file.path(tempdir(), "dirn-abs")
  dir.create(file.path(.d, "tmp"), recursive = TRUE, showWarnings = FALSE)
  writeLines("decoy", file.path(.d, "tmp", "decoy.txt"))
  .real <- file.path(tempdir(), "tmp-real")
  dir.create(.real, recursive = TRUE, showWarnings = FALSE)

  expect_equal(.monolixInDirn("/tmp/decoy.txt", .d), "/tmp/decoy.txt")
  expect_equal(.monolixInDirn("~/decoy.txt", .d), "~/decoy.txt")
  expect_equal(.monolixInDirn("c:/tmp/decoy.txt", .d), "c:/tmp/decoy.txt")
  expect_equal(.monolixInDirn("\\\\srv\\decoy.txt", .d), "\\\\srv\\decoy.txt")
  expect_equal(.monolixInDirn("lib:decoy.txt", .d), "lib:decoy.txt")
  # a relative name that exists under 'dirn' is resolved there
  expect_equal(.monolixInDirn(file.path("tmp", "decoy.txt"), .d),
               file.path(.d, "tmp", "decoy.txt"))
  # a relative name that does not exist under 'dirn' is not allowed to
  # fall back to a same-named file in the working directory
  expect_equal(.monolixInDirn("nope.txt", .d), file.path(.d, "nope.txt"))
  expect_equal(.monolixInDirn("nope.txt", NULL), "nope.txt")

  withr::with_dir(.real, {
    writeLines("decoy", "decoy.txt")
    expect_error(mlxTxt("decoy.txt", dirn = .d), "decoy.txt")
    expect_error(mlxtran("decoy.mlxtran", dirn = .d))
  })
})

test_that("re-reading a parsed mlxtran object reads its results directory", {
  .theo <- system.file("theo", package = "monolix2rx")
  skip_if_not(dir.exists(.theo))

  .mlx <- mlxtran(file.path(.theo, "theophylline_project.mlxtran"))
  expect_true(inherits(attr(.mlx, "covLinUntransformed"), "matrix"))

  # `.mlxtranCov()`/`.mlxtranSumary()` read `exportpath` relative to the
  # working directory; updating from elsewhere used to silently drop the
  # covariance and the run information
  attr(.mlx, "covLinUntransformed") <- NULL
  withr::with_dir(tempdir(), {
    .upd <- mlxtran(.mlx, update = TRUE)
    expect_true(inherits(attr(.upd, "covLinUntransformed"), "matrix"))
    # summary.txt is read from the project too
    expect_equal(attr(.upd, "version"), attr(.mlx, "version"))
  })
})

test_that("the stored project directory survives a change of working directory", {
  .theo <- system.file("theo", package = "monolix2rx")
  skip_if_not(dir.exists(.theo))

  .mlx <- withr::with_dir(dirname(.theo),
                          mlxtran(file.path(basename(.theo),
                                            "theophylline_project.mlxtran")))
  # the directory is stored absolute, so it still points at the project
  # after the working directory moves elsewhere
  withr::with_dir(tempdir(), {
    expect_equal(normalizePath(.monolixGetPwd(.mlx), winslash = "/"),
                 normalizePath(.theo, winslash = "/"))
  })
})

test_that("'dirn' is checked", {
  expect_error(mlxtran("foo.mlxtran", dirn = file.path(tempdir(), "does-not-exist")),
               "dirn")
  expect_error(mlxtran("foo.mlxtran", dirn = c("a", "b")),
               "dirn")
  expect_error(mlxtran("foo.mlxtran", dirn = NA_character_), "dirn")
  expect_error(mlxtran("foo.mlxtran", dirn = 1L), "dirn")
  expect_error(mlxTxt("foo.txt", dirn = 1L), "dirn")
  expect_error(monolix2rx("foo.txt", dirn = 1L), "dirn")
  expect_null(.monolixDirn(NULL))
  expect_equal(.monolixDirn(tempdir()),
               normalizePath(tempdir(), winslash = "/", mustWork = FALSE))
})

test_that(".monolixInDirn() only resolves a single file name", {
  .d <- tempdir()
  # anything that is not one non-missing string is passed through so the
  # caller reports it, rather than being turned into a path here
  expect_equal(.monolixInDirn(c("a.txt", "b.txt"), .d), c("a.txt", "b.txt"))
  expect_equal(.monolixInDirn(character(0), .d), character(0))
  expect_equal(.monolixInDirn(NA_character_, .d), NA_character_)
  expect_equal(.monolixInDirn(1L, .d), 1L)
})

test_that(".monolixIsAbsPath() classifies unix and windows names", {
  expect_true(.monolixIsAbsPath("/tmp/m.txt"))
  expect_true(.monolixIsAbsPath("~/m.txt"))
  expect_true(.monolixIsAbsPath("c:/proj/m.txt"))
  expect_true(.monolixIsAbsPath("C:\\proj\\m.txt"))
  expect_true(.monolixIsAbsPath("\\\\srv\\share\\m.txt"))
  expect_false(.monolixIsAbsPath("m.txt"))
  expect_false(.monolixIsAbsPath("sub/m.txt"))
  expect_false(.monolixIsAbsPath("./m.txt"))
  expect_false(.monolixIsAbsPath("../m.txt"))
  # a drive-relative name is not absolute
  expect_false(.monolixIsAbsPath("c:m.txt"))
})

test_that("'dirn' overrides the directory of an already parsed object", {
  .theo <- system.file("theo", package = "monolix2rx")
  skip_if_not(dir.exists(.theo))

  .mlx <- mlxtran(file.path(.theo, "theophylline_project.mlxtran"))
  .d <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)

  .moved <- mlxtran(.mlx, dirn = .d)
  expect_equal(attr(.moved, "dirn"), .d)
  expect_equal(normalizePath(.monolixGetPwd(.moved), winslash = "/"), .d)
  # the original is untouched
  expect_equal(normalizePath(attr(.mlx, "dirn"), winslash = "/"),
               normalizePath(.theo, winslash = "/"))
})

test_that("a model text file is routed through mlxTxt() with 'dirn'", {
  .theo <- system.file("theo", package = "monolix2rx")
  skip_if_not(dir.exists(.theo))

  withr::with_dir(tempdir(), {
    # mlxtran() hands a .txt file to mlxTxt()
    .mlx <- mlxtran(file.path(.theo, "oral1_1cpt_kaVCl.txt"))
    expect_true(inherits(.mlx, "monolix2rxMlxtran"))
    expect_equal(normalizePath(attr(.mlx, "dirn"), winslash = "/"),
                 normalizePath(.theo, winslash = "/"))

    .mlx <- mlxtran("oral1_1cpt_kaVCl.txt", dirn = .theo)
    expect_equal(normalizePath(attr(.mlx, "dirn"), winslash = "/"),
                 normalizePath(.theo, winslash = "/"))

    # monolix2rx() takes the same route for a .txt model
    .ui <- monolix2rx("oral1_1cpt_kaVCl.txt", dirn = .theo)
    expect_true(inherits(.ui, "monolix2rx"))
  })
})

test_that("mlxTxt() keeps 'dirn' for a model read from the model library", {
  .lines <- readLines(file.path(system.file("theo", package = "monolix2rx"),
                                "oral1_1cpt_kaVCl.txt"))
  .d <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)

  # a `lib:` model has no directory of its own, so the project directory
  # given by the caller has to survive
  testthat::local_mocked_bindings(
    monolix2rxlixoftConnectors = function() TRUE,
    monolix2rxGetLibraryModelContent = function(filename) {
      paste(.lines, collapse = "\n")
    })
  .monolix2rx$lixoftConnectors <- TRUE
  on.exit(.monolix2rx$lixoftConnectors <- NA, add = TRUE)

  .mlx <- mlxTxt("lib:oral1_1cpt_kaVCl.txt", dirn = .d)
  expect_true(inherits(.mlx, "monolix2rxMlxtran"))
  expect_equal(attr(.mlx, "dirn"), .d)

  # without 'dirn' there is no directory to record
  .mlx <- mlxTxt("lib:oral1_1cpt_kaVCl.txt")
  expect_null(attr(.mlx, "dirn"))
})

test_that("mlxTxt() takes 'dirn' when it is handed model lines", {
  .theo <- system.file("theo", package = "monolix2rx")
  skip_if_not(dir.exists(.theo))

  .lines <- readLines(file.path(.theo, "oral1_1cpt_kaVCl.txt"))
  .d <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)

  .mlx <- mlxTxt(.lines, dirn = .d)
  expect_true(inherits(.mlx, "monolix2rxMlxtran"))
  expect_equal(attr(.mlx, "dirn"), .d)

  # with no 'dirn' the lines are taken to describe the working directory
  withr::with_dir(.theo, {
    .mlx <- mlxTxt(.lines)
    expect_equal(normalizePath(attr(.mlx, "dirn"), winslash = "/"),
                 normalizePath(.theo, winslash = "/"))
  })
})

test_that("mlxTxt() only consults the model library for a 'lib:' name", {
  .theo <- system.file("theo", package = "monolix2rx")
  skip_if_not(dir.exists(.theo))

  .d <- file.path(tempdir(), "short-name")
  dir.create(.d, showWarnings = FALSE)
  # a name too short to carry a "lib:" prefix skips the library lookup
  file.copy(file.path(.theo, "oral1_1cpt_kaVCl.txt"), file.path(.d, "m.t"),
            overwrite = TRUE)

  testthat::local_mocked_bindings(
    monolix2rxlixoftConnectors = function() TRUE,
    monolix2rxGetLibraryModelContent = function(filename) {
      stop("the model library must not be consulted here")
    })

  .mlx <- mlxTxt("m.t", dirn = .d)
  expect_true(inherits(.mlx, "monolix2rxMlxtran"))
  expect_equal(normalizePath(attr(.mlx, "dirn"), winslash = "/"),
               normalizePath(.d, winslash = "/"))
})

test_that("mlxTxt() initializes lixoftConnectors on the first 'lib:' model", {
  .lines <- readLines(file.path(system.file("theo", package = "monolix2rx"),
                                "oral1_1cpt_kaVCl.txt"))
  .d <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)

  testthat::local_mocked_bindings(
    monolix2rxlixoftConnectors = function() TRUE,
    monolix2rxInitializeLixoftConnectors = function(software = "monolix",
                                                    force = TRUE) TRUE,
    monolix2rxGetLibraryModelContent = function(filename) {
      paste(.lines, collapse = "\n")
    })
  .monolix2rx$lixoftConnectors <- NA
  on.exit(.monolix2rx$lixoftConnectors <- NA, add = TRUE)

  .mlx <- mlxTxt("lib:oral1_1cpt_kaVCl.txt", dirn = .d)
  expect_true(inherits(.mlx, "monolix2rxMlxtran"))
  expect_true(.monolix2rx$lixoftConnectors)
})

test_that("mlxTxt() warns when lixoftConnectors cannot be initialized", {
  .d <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)

  testthat::local_mocked_bindings(
    monolix2rxlixoftConnectors = function() TRUE,
    monolix2rxInitializeLixoftConnectors = function(software = "monolix",
                                                    force = TRUE) {
      stop("lixoftConnectors is not installed")
    },
    monolix2rxGetLibraryModelContent = function(filename) {
      stop("unreachable without a connector")
    })
  .monolix2rx$lixoftConnectors <- NA
  on.exit(.monolix2rx$lixoftConnectors <- NA, add = TRUE)

  # collect every warning so the unrelated model library warning from
  # .mlxtranLib() does not escape the test
  .warn <- character(0)
  withCallingHandlers(
    try(mlxTxt("lib:oral1_1cpt_kaVCl.txt", dirn = .d), silent = TRUE),
    warning = function(w) {
      .warn <<- c(.warn, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  expect_true(any(grepl("cannot be initialized", .warn)))
  expect_false(.monolix2rx$lixoftConnectors)
})

test_that("mlxTxt() falls back when the model library cannot be read", {
  .d <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
  testthat::local_mocked_bindings(
    monolix2rxlixoftConnectors = function() TRUE,
    monolix2rxGetLibraryModelContent = function(filename) {
      stop("no such library model")
    })
  .monolix2rx$lixoftConnectors <- TRUE
  on.exit(.monolix2rx$lixoftConnectors <- NA, add = TRUE)

  # .mlxtranLib() warns about the model library when it cannot expand the
  # name; the fallback behavior is what is under test here
  expect_error(suppressWarnings(mlxTxt("lib:not-a-model.txt", dirn = .d)),
               "dirn=", fixed = TRUE)
  expect_equal(suppressWarnings(mlxTxt("lib:not-a-model.txt", retFile = TRUE)),
               "lib:not-a-model.txt")
})
