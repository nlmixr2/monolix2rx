test_that("Monolix 2024 Excel/SAS data sets import (#10)", {
  skip_if_not_installed("haven")
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  .d <- withr::local_tempdir()
  file.copy(list.files(system.file("theo", package="monolix2rx"), full.names=TRUE),
            .d, recursive=TRUE)
  .f <- file.path(.d, "theophylline_project.mlxtran")
  .txt <- .monolixDataLoad(mlxtran(.f))

  .dat <- utils::read.table(file.path(.d, "data", "theophylline_data.txt"),
                            header=TRUE, na.strings=".")
  writexl::write_xlsx(.dat, file.path(.d, "data", "theo.xlsx"))
  writexl::write_xlsx(.dat, file.path(.d, "data", "theo_nohead.xlsx"), col_names=FALSE)
  .lower <- .dat
  names(.lower) <- tolower(names(.lower))
  writexl::write_xlsx(.lower, file.path(.d, "data", "theo_lower.xlsx"))
  haven::write_xpt(.dat, file.path(.d, "data", "theo.xpt"))
  .files <- c("theo.xlsx", "theo_nohead.xlsx", "theo.xpt")
  # write_sas() is deprecated in haven, but still the only sas7bdat writer
  if (exists("write_sas", asNamespace("haven"))) {
    suppressWarnings(haven::write_sas(.dat, file.path(.d, "data", "theo.sas7bdat")))
    .files <- c(.files, "theo.sas7bdat")
  }

  .proj <- function(file) {
    .l <- sub("theophylline_data.txt", file, readLines(.f), fixed=TRUE)
    .f2 <- file.path(.d, paste0(gsub("[.]", "_", file), ".mlxtran"))
    writeLines(.l, .f2)
    .f2
  }

  for (.file in .files) {
    .mlx <- mlxtran(.proj(.file))
    expect_equal(.mlx$DATAFILE$FILEINFO$FILEINFO$file, paste0("data/", .file))
    expect_equal(.mlx$DATAFILE$FILEINFO$FILEINFO$header,
                 c("ID", "AMT", "TIME", "CONC", "WEIGHT", "SEX"))
    expect_equal(.monolixDataLoad(.mlx), .txt, ignore_attr=TRUE)
  }

  expect_warning(.lowerData <- .monolixDataLoad(mlxtran(.proj("theo_lower.xlsx"))),
                 "header does not match")
  expect_equal(.lowerData, .txt, ignore_attr=TRUE)

  .rx <- suppressWarnings(monolix2rx(.proj("theo.xlsx")))
  expect_true(inherits(.rx, "rxUi"))
  expect_true(is.data.frame(.rx$monolixData))
})

test_that("Excel header and extension edge cases (#10)", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  .xls <- readxl::readxl_example("datasets.xls")
  .xlsDat <- .monolixDataLoadBinary(.xls, "xls", names(readxl::read_excel(.xls)))
  expect_equal(dim(.xlsDat), c(32L, 11L))
  expect_true(is.numeric(.xlsDat[[1]]))

  # a numeric column name that matches the mlxtran header is still a header
  .d <- withr::local_tempdir()
  .f <- file.path(.d, "num.xlsx")
  writexl::write_xlsx(data.frame(ID=1:2, `24`=c(3, 4), check.names=FALSE), .f)
  .num <- .monolixDataLoadBinary(.f, "xlsx", c("ID", "24"))
  expect_equal(.num, data.frame(ID=c(1, 2), `24`=c(3, 4), check.names=FALSE))

  expect_warning(.num <- .monolixDataLoadBinary(.f, "xlsx", c("id", "24")),
                 "header does not match")
  expect_equal(nrow(.num), 2L)

  # text NA markers and dates are handled like read.table()
  skip_if_not_installed("haven")
  .x <- file.path(.d, "na.xpt")
  haven::write_xpt(data.frame(ID=c(1, 2), DV=c(".", "3"), COV=c("NA", "a"),
                              DT=as.Date(c("2024-01-01", "2024-01-02"))), .x)
  .na <- .monolixDataLoadBinary(.x, "xpt", c("ID", "DV", "COV", "DT"))
  expect_equal(.na$DV, c(NA, 3))
  expect_equal(.na$COV, c(NA, "a"))
  expect_equal(.na$DT, c("2024-01-01", "2024-01-02"))

  # text formats (and extension-less files) are left to read.table
  expect_null(.monolixDataLoadBinary(.f, "csv", "ID"))
  expect_null(.monolixDataLoadBinary(.f, character(0), "ID"))
})
