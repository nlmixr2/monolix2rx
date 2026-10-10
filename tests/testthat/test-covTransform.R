test_that("categorical covariate transforms translate to rxode2 (#6)", {
  .tr <- function(txt) {
    mlxtranTransformGetRxCode(list(
      MODEL = list(
        COVARIATE = list(COVARIATE = list(), DEFINITION = .longDef(txt))
      )
    ))
  }

  expect_equal(
    .tr(
      "tSex = {transform = 'sex', categories = {'F' = {'0'}, 'M' = {'1'}}, reference = 'M'}"
    ),
    "if (sex == 0) {\n  tSex <- 'F'\n} else if (sex == 1) {\n  tSex <- 'M'\n} else {\n  tSex <- 'M'\n}"
  )

  expect_equal(
    .tr(
      paste0(
        "tAPGAR = {transform = APGAR, categories = {High = {10, 8, 9}, ",
        "Low = {1, 2, 3}, Med = {4, 5, 6, 7}}, reference = Med}"
      )
    ),
    paste0(
      "if (APGAR == 10 || APGAR == 8 || APGAR == 9) {\n  tAPGAR <- 'High'\n}",
      " else if (APGAR == 1 || APGAR == 2 || APGAR == 3) {\n  tAPGAR <- 'Low'\n}",
      " else if (APGAR == 4 || APGAR == 5 || APGAR == 6 || APGAR == 7) {\n  tAPGAR <- 'Med'\n}",
      " else {\n  tAPGAR <- 'Med'\n}"
    )
  )

  # no reference: fall back to the first category instead of an empty label
  expect_equal(
    .tr("tSex = {transform = sex, categories = {F = {0}, M = {1}}}"),
    "if (sex == 0) {\n  tSex <- 'F'\n} else if (sex == 1) {\n  tSex <- 'M'\n} else {\n  tSex <- 'F'\n}"
  )

  expect_equal(
    .tr(
      "tSex = {transform = sex, categories = {F = {0}, M = {1}}, reference = ''}"
    ),
    "if (sex == 0) {\n  tSex <- 'F'\n} else if (sex == 1) {\n  tSex <- 'M'\n} else {\n  tSex <- 'F'\n}"
  )

  # a label with a single quote is double-quoted
  expect_equal(
    .tr(
      "tS = {transform = S, categories = {\"A's\" = {0}, B = {1}}, reference = B}"
    ),
    "if (S == 0) {\n  tS <- \"A's\"\n} else if (S == 1) {\n  tS <- 'B'\n} else {\n  tS <- 'B'\n}"
  )

  expect_error(
    mlxtranTransformGetRxCode(list(
      MODEL = list(
        COVARIATE = list(
          COVARIATE = list(),
          DEFINITION = list(
            transform = list(
              tSex = list(
                transform = character(0),
                catLabel = "F",
                catValue = list("0"),
                reference = "F"
              )
            )
          )
        )
      )
    )),
    "transform="
  )
  expect_error(
    .tr("tS = {transform = '', categories = {F = {0}}}"),
    "transform="
  )
  expect_error(.tr("tSex = {transform = sex, reference = M}"), "categories=")

  # any character value quotes every value of the transform
  expect_equal(
    .tr(
      "tS = {transform = S, categories = {A = {x, 1}, B = 2}, reference = B}"
    ),
    "if (S == 'x' || S == '1') {\n  tS <- 'A'\n} else if (S == '2') {\n  tS <- 'B'\n} else {\n  tS <- 'B'\n}"
  )

  # several transforms
  .two <- .tr(
    paste0(
      "tA = {transform = A, categories = {L = 0, H = 1}, reference = L}\n",
      "tB = {transform = B, categories = {N = 0, Y = 1}, reference = N}"
    )
  )
  expect_length(.two, 1L)
  expect_true(grepl("tA <- 'H'", .two, fixed = TRUE))
  expect_true(grepl("tB <- 'Y'", .two, fixed = TRUE))

  expect_null(mlxtranTransformGetRxCode(list(MODEL = list())))
})

test_that("covariate transforms are imported and validated (#6)", {
  skip_on_cran()
  .cov <- system.file("cov", package = "monolix2rx")
  for (.f in c(
    "phenobarbital_project.mlxtran",
    "warfarin_covariate3_project.mlxtran"
  )) {
    .rx <- suppressMessages(monolix2rx(file.path(.cov, .f)))
    expect_true(inherits(.rx, "rxUi"))
    .model <- paste(deparse(.rx$lstExpr), collapse = "\n")
    expect_true(grepl(
      if (grepl("pheno", .f)) "tAPGAR <- \"Med\"" else "tSex <- \"M\"",
      .model,
      fixed = TRUE
    ))
    expect_false(is.null(.rx$ipredCompare))
    expect_true(.rx$ipredAtol < 0.5)
    expect_true(.rx$predAtol < 0.01)
  }
})

test_that("a covariate transform without a reference imports (#6)", {
  skip_on_cran()
  .dir <- file.path(tempfile(), "cov")
  dir.create(.dir, recursive = TRUE)
  on.exit(unlink(dirname(.dir), recursive = TRUE))
  file.copy(
    list.files(system.file("cov", package = "monolix2rx"), full.names = TRUE),
    .dir,
    recursive = TRUE
  )
  .f <- file.path(.dir, "warfarin_covariate3_project.mlxtran")
  .l <- readLines(.f)
  .l <- sub("'M' = {'1'}  },", "'M' = {'1'}  }", .l, fixed = TRUE)
  .l <- .l[!grepl("reference = 'M'", .l, fixed = TRUE)]
  writeLines(.l, .f)
  .rx <- suppressMessages(monolix2rx(.f))
  .model <- paste(deparse(.rx$lstExpr), collapse = "\n")
  expect_true(grepl("else {\n    tSex <- \"F\"", .model, fixed = TRUE))
  expect_true(.rx$predAtol < 0.01)
})

test_that("a covariate equation can use an earlier one, and min() is per row", {
  .m <- list(MODEL=list(COVARIATE=list(EQUATION=.covEq("BMI = WT/(HT/100)^2\nlBMI = log(min(BMI, 40)/25)"))))
  .l <- .mlxtranChangeEquationInfoToParsedList(.m)
  expect_equal(deparse1(.l$lBMI), "log(min(WT/(HT/100)^2, 40)/25)")
  .d <- eval(str2lang(paste("data.frame(WT=c(50, 150), HT=180) |>", mlxtranGetMutate(.m))))
  expect_equal(.d$lBMI, log(pmin(c(50, 150) / 1.8^2, 40) / 25))
  # a name assigned again
  .m <- list(MODEL=list(COVARIATE=list(EQUATION=.covEq("lw = log(WT)\nlw = lw - 1\ny = 2*lw"))))
  .l <- .mlxtranChangeEquationInfoToParsedList(.m)
  expect_equal(vapply(.l, deparse1, ""), c(lw="log(WT) - 1", y="2 * (log(WT) - 1)"))
})

test_that("if/else covariate equations start the model", {
  .m <- list(MODEL=list(COVARIATE=list(EQUATION=.covEq(
    "lw = log(WT/70)\nif lw > 0\n  hWT = 1\nelse\n  hWT = 0\nend\nhw2 = 2*hWT"))))
  .r <- .mlxtranChangeVal(quote(model({Cl <- exp(Cl_pop + b * hw2 + c * lw)})), .m)
  # only the if/else and what uses it; lw stays inlined (a covariate of Cl)
  expect_equal(deparse1(.r[[2]][[2]]), "if (log(WT/70) > 0) {     hWT <- 1 } else {     hWT <- 0 }")
  expect_equal(vapply(as.list(.r[[2]])[-(1:2)], deparse1, ""),
               c("hw2 <- 2 * hWT", "Cl <- exp(Cl_pop + b * hw2 + c * log(WT/70))"))
})
