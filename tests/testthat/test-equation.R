test_that("equation tests", {

  .ret <- .equation("x_0 = V
ddt_x = -k*x")

  expect_equal(.ret$rx,
               c("x(0) <- V", "d/dt(x) <-  - k * x"))

  .ret <- .equation("if t<=10
   c = 1
elseif t<20
   c = 2
else
   c = 3
end
")

  expect_equal(.ret$rx,
               c("if (time <= 10) {",
                 "c <- 1",
                 "} else if (time < 20) {",
                 "c <- 2",
                 "} else {",
                 "c <- 3",
                 "}"))

  .ret <- .equation("if t<=10
   dx = 1
else
   dx = 2
end
ddt_x = dx")

  expect_equal(.ret$rx,
               c("if (time <= 10) {",
                 "dx <- 1",
                 "} else {",
                 "dx <- 2",
                 "}",
                 "d/dt(x) <- dx"))

  .ret <- .equation("; ode is considered as stiff
odeType = stiff
x_0 = V
ddt_x = -k*x")

  expect_equal(.ret$odeType, "stiff")

  .ret <- .equation("; ode is considered as stiff
odeType =  nonStiff
x_0 = V
ddt_x = -k*x")

  expect_equal(.ret$odeType, "nonStiff")

  .ret <- .equation("; ode is considered as stiff
x_0 = V
ddt_x = -k*x")

  expect_equal(.ret$odeType, "nonStiff")

})
