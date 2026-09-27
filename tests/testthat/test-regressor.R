.regMlxtran <- function(content, long) {
  list(
    DATAFILE = list(CONTENT = list(CONTENT = .content(content))),
    MODEL = list(LONGITUDINAL = list(LONGITUDINAL = .longitudinal(long)))
  )
}

test_that("regressors are matched to the model by order, not name (#2)", {
  .m <- .regMlxtran(
    "ID = {use=identifier}
TIME = {use=time}
DV = {use=observation, name=y1, type=continuous}
CONC = {use=regressor}
E0X = {use=regressor}",
    "input = {ka, E0, Cc}
Cc = {use=regressor}
E0 = {use=regressor}"
  )
  .d <- data.frame(ID = 1, TIME = 0, DV = 1, CONC = 2, E0X = 3, WT = 70)
  .r <- .dataRenameFromMlxtran(.d, .m)
  expect_equal(names(.r), c("id", "time", "dv", "Cc", "E0", "WT"))
  expect_equal(.r$Cc, 2)
  expect_equal(.r$E0, 3)

  # names swapped between the data and the model
  .m <- .regMlxtran(
    "ID = {use=identifier}
E0 = {use=regressor}
Cc = {use=regressor}",
    "input = {Cc, E0}
Cc = {use=regressor}
E0 = {use=regressor}"
  )
  .r <- .dataRenameRegressors(data.frame(ID = 1, E0 = 2, Cc = 3), .m)
  expect_equal(names(.r), c("ID", "Cc", "E0"))
  expect_equal(.r$Cc, 2)
  expect_equal(.r$E0, 3)
})

test_that("regressors follow the data set column order (#2)", {
  .m <- .regMlxtran(
    "ID = {use=identifier}
E0X = {use=regressor}
CONC = {use=regressor}",
    "input = {Cc, E0}
Cc = {use=regressor}
E0 = {use=regressor}"
  )
  .r <- .dataRenameFromMlxtran(data.frame(ID = 1, CONC = 2, E0X = 3), .m)
  expect_equal(names(.r), c("id", "Cc", "E0"))
  expect_equal(.r$Cc, 2)
  expect_equal(.r$E0, 3)
})

test_that("a regressor may reuse a translated single-use column name (#2)", {
  .m <- .regMlxtran(
    "ID_COL = {use=identifier}
REG_COL = {use=regressor}",
    "input = {ID_COL}
ID_COL = {use=regressor}"
  )
  .r <- .dataRenameFromMlxtran(data.frame(ID_COL = 1, REG_COL = 2), .m)
  expect_equal(.r, data.frame(id = 1, ID_COL = 2))
})

test_that("a data regressor column named like a translated column (#2)", {
  .m <- .regMlxtran(
    "ID = {use=identifier}
TIME_COL = {use=time}
time = {use=regressor}",
    "input = {Cc}
Cc = {use=regressor}"
  )
  .r <- .dataRenameFromMlxtran(data.frame(ID = 1, TIME_COL = 0, time = 2), .m)
  expect_equal(.r, data.frame(id = 1, time = 0, Cc = 2))
  expect_error(
    .dataRenameFromMlxtran(data.frame(ID = 1, TIME_COL = 0), .m),
    "missing from the data set"
  )
})

test_that("regressor count and name checks (#2)", {
  .d <- data.frame(ID = 1, CONC = 2, E0X = 3)
  .m <- .regMlxtran(
    "ID = {use=identifier}
CONC = {use=regressor}
E0X = {use=regressor}",
    "input = {Cc}
Cc = {use=regressor}"
  )
  expect_error(.dataRenameRegressors(.d, .m), "number of regressors")

  .m <- .regMlxtran(
    "ID = {use=identifier}
CONC = {use=regressor}",
    "input = {ka}"
  )
  expect_error(.dataRenameRegressors(.d, .m), "number of regressors")

  .m <- .regMlxtran(
    "ID = {use=identifier}
CONCX = {use=regressor}",
    "input = {Cc}
Cc = {use=regressor}"
  )
  expect_error(.dataRenameRegressors(.d, .m), "missing from the data set")

  # a used column that already has the model regressor name
  .m <- .regMlxtran(
    "ID = {use=identifier}
Cc = {use=covariate, type=continuous}
CONC = {use=regressor}",
    "input = {Cc2, Cc}
Cc = {use=regressor}"
  )
  expect_error(
    .dataRenameRegressors(data.frame(ID = 1, Cc = 1, CONC = 2), .m),
    "non-regressor data column"
  )

  # a model regressor named like a translated single-use column
  .m <- .regMlxtran(
    "ID = {use=identifier}
TIME = {use=time}
CONC = {use=regressor}",
    "input = {time}
time = {use=regressor}"
  )
  expect_error(
    .dataRenameRegressors(data.frame(ID = 1, TIME = 0, CONC = 2), .m),
    "translated data column"
  )

  .m <- .regMlxtran(
    "ID = {use=identifier}
CONC = {use=regressor}",
    "input = {cmt}
cmt = {use=regressor}"
  )
  expect_error(
    .dataRenameRegressors(data.frame(ID = 1, CONC = 2), .m),
    "translated data column"
  )

  .m <- .regMlxtran(
    "ID = {use=identifier}
CONC = {use=regressor}",
    "input = {TIME}
TIME = {use=regressor}"
  )
  expect_error(
    .dataRenameRegressors(data.frame(ID = 1, CONC = 2), .m),
    "translated data column"
  )

  .m <- .regMlxtran(
    "ID = {use=identifier}
CONC = {use=regressor}",
    "input = {rxMDvid}
rxMDvid = {use=regressor}"
  )
  expect_error(
    .dataRenameRegressors(data.frame(ID = 1, CONC = 2), .m),
    "translated data column"
  )

  # reserved even when the data has no time column
  .m <- .regMlxtran(
    "ID = {use=identifier}
CONC = {use=regressor}",
    "input = {time}
time = {use=regressor}"
  )
  expect_error(
    .dataRenameRegressors(data.frame(ID = 1, CONC = 2), .m),
    "translated data column"
  )

  # an unused column with the model regressor name is dropped
  .m <- .regMlxtran(
    "ID = {use=identifier}
CONC = {use=regressor}",
    "input = {Cc}
Cc = {use=regressor}"
  )
  .r <- .dataRenameRegressors(data.frame(ID = 1, Cc = 1, CONC = 2), .m)
  expect_equal(.r, data.frame(ID = 1, Cc = 2))

  # no regressors; data unchanged
  .m <- .regMlxtran("ID = {use=identifier}", "input = {ka}")
  expect_equal(.dataRenameRegressors(.d, .m), .d)
})

test_that("imported data uses the model regressor names (#2)", {
  skip_on_cran()
  .dir <- file.path(tempfile(), "theo")
  dir.create(.dir, recursive = TRUE)
  on.exit(unlink(dirname(.dir), recursive = TRUE))
  file.copy(
    system.file("theo", package = "monolix2rx"),
    dirname(.dir),
    recursive = TRUE
  )
  .f <- file.path(.dir, "theophylline_project.mlxtran")
  .l <- readLines(.f)
  .l <- sub(
    "WEIGHT = {use=covariate, type=continuous}",
    "WEIGHT = {use=regressor}",
    .l,
    fixed = TRUE
  )
  writeLines(.l, .f)
  .f <- file.path(.dir, "oral1_1cpt_kaVCl.txt")
  .l <- readLines(.f)
  .l <- sub(
    "input = {ka, V, Cl}",
    "input = {ka, V, Cl, WT}\nWT = {use=regressor}",
    .l,
    fixed = TRUE
  )
  .l <- sub(
    "Cc = pkmodel(ka, V, Cl)",
    "Cc = pkmodel(ka, V, Cl)\nwtReg = WT",
    .l,
    fixed = TRUE
  )
  writeLines(.l, .f)
  .rx <- suppressMessages(suppressWarnings(monolix2rx(file.path(
    .dir,
    "theophylline_project.mlxtran"
  ))))
  expect_true(inherits(.rx, "rxUi"))
  .d <- .rx$monolixData
  expect_true("WT" %in% names(.d))
  expect_false("WEIGHT" %in% names(.d))
  expect_true("WT" %in% .rx$allCovs)
})

test_that("continuous covariates, not categorical ones, are cast to double", {
  .m <- .regMlxtran(
    "ID = {use=identifier}
SEX = {use=covariate, type=categorical}
WT = {use=covariate, type=continuous}",
    "input = {ka}"
  )
  .r <- .dataRenameFromMlxtran(data.frame(ID = 1, SEX = "M", WT = 70L), .m)
  expect_equal(.r$SEX, "M")
  expect_type(.r$WT, "double")
})
