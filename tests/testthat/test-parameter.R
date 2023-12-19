test_that("parameters", {

  expect_error(.parameter("ktr_pop={value=1}"), NA)
  expect_error(.parameter("ktr_pop=2"), NA)
  expect_error(.parameter("ktr_pop={value=1,method=mle}"), NA)
  expect_error(.parameter("ktr_pop={value=1,method=fixed}"), NA)
  expect_error(.parameter("ktr_pop={value=1, method=MLE}"), NA)

  tmp <- .parameter("ktr_pop={value=1, method=MLE}
ka_pop={value=1, method=FIXED}
cl_pop={value=0.1, method=MLE}
v_pop={value=10, method=MLE}
prop__err={value=0.1, method=MLE}")

  tmp2 <- data.frame(name = c("ktr_pop", "ka_pop", "cl_pop", "v_pop", "prop__err"),
                     value = c(1, 1, 0.1, 10, 0.1),
                     method = c("MLE", "FIXED", "MLE", "MLE", "MLE"))
  class(tmp2) <- c("monolix2rxParameter", "data.frame")

  expect_equal(tmp, tmp2)

  expect_error(.parameter("ktr_pop: 20"))

})
