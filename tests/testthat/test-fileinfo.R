test_that("fileinfo", {

  .fi <- .fileinfo("file='pk.turnover.emax3-monolix.csv'
  delimiter = comma
  header = {ID, TIME, EVID, AMT, DV, ADM, YTYPE, nlmixrRowNums}")

  expect_snapshot(print(.fi))
  expect_error(as.list(.fi), NA)

  expect_equal(.fi$file, "pk.turnover.emax3-monolix.csv")
  expect_equal(.fi$header, c("ID", "TIME", "EVID", "AMT", "DV", "ADM", "YTYPE", "nlmixrRowNums"))
  expect_equal(.fi$delimiter, "comma")

})

test_that("fileinfo supports the Monolix 2024 file={path=} syntax (#43)", {

  .old <- .fileinfo("file='../data/data.csv'
  delimiter = comma
  header = {ID, TIME, DV, AMT}")

  # file={path='...'} is equivalent to file='...'
  .fi <- .fileinfo("file={path='../data/data.csv'}
  delimiter = comma
  header = {ID, TIME, DV, AMT}")

  expect_equal(as.list(.fi), as.list(.old))
  expect_equal(.fi$file, "../data/data.csv")
  expect_equal(.fi$header, c("ID", "TIME", "DV", "AMT"))
  expect_equal(.fi$delimiter, "comma")

  # extra whitespace and double quotes also parse
  expect_equal(.fileinfo("file = { path = \"../data/data.csv\" }
  delimiter = comma
  header = {ID, TIME, DV, AMT}")$file,
  "../data/data.csv")

  # `path` is not mistaken for a header column
  expect_false("path" %in% .fi$header)

  # an unquoted file name still accepts braces the way it always has
  expect_equal(.fileinfo("file=data{1}.csv
  delimiter = comma
  header = {ID, TIME}")$file,
  "data{1}.csv")

})
