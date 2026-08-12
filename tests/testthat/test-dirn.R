test_that("mlxtran() finds the project files when 'dirn' is given (issue #44)", {
  .theo <- system.file("theo", package = "monolix2rx")
  skip_if_not(dir.exists(.theo))

  .lines <- readLines(file.path(.theo, "theophylline_project.mlxtran"))

  withr::with_dir(tempdir(), {
    # without 'dirn' the mlxtran lines are resolved against the working
    # directory, so the model text file cannot be found
    expect_error(monolix2rx(.lines, update = FALSE),
                 "does not exist")

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
