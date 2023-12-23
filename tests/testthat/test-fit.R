test_that("fit parsing", {

  tmp <- .fit("data={y1, y2}
model={rx_prd_cp, rx_prd_effect}")
  tmp2 <- data.frame(data = c("y1", "y2"), model = c("rx_prd_cp", "rx_prd_effect"))
  class(tmp2) <- c("monolix2rxFit", "data.frame")

  expect_equal(tmp, tmp2)


  tmp <- .fit("data=y1
model=rx_prd_effect")
  tmp2 <- data.frame(data = "y1", model = "rx_prd_effect")
  class(tmp2) <- c("monolix2rxFit", "data.frame")
  expect_equal(tmp2, tmp)

  tmp <- .fit("data={y1}
model={rx_prd_effect}")
  expect_equal(tmp2, tmp)


  expect_error(.fit("data=y1
model={rx_prd_cp, rx_prd_effect}"), "<FIT>")

})
