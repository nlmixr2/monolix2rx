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
