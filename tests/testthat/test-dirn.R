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
  # a relative name that does not exist under 'dirn' is left alone
  expect_equal(.monolixInDirn("nope.txt", .d), "nope.txt")
  expect_equal(.monolixInDirn("nope.txt", NULL), "nope.txt")
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
})
