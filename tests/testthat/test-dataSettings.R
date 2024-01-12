test_that("<DATAFILE> [SETTINGS]", {

  tmp <- .dataSettings("dataType={'1'=plasma,'2'=plasma}")


  expect_equal("dataType = {'1'=plasma, '2'=plasma}",
               as.character(tmp))


  tmp2 <- .dataSettings("dataType={1='plasma','2'=plasma}")

  expect_equal(as.character(tmp2),
               "dataType = {1='plasma', '2'=plasma}")

  tmp2 <- .dataSettings("dataType = {'dv'=plasma}")

  expect_equal(as.character(tmp2),
               "dataType = {'dv'=plasma}")

  expect_snapshot(print(tmp2))

  expect_error(as.list(tmp2), NA)

})
