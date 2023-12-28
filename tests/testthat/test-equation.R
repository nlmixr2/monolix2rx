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

  .ret <- .equation("x_0 = a
dx_0 = b
ddt_x = dx
ddt_dx = -x-dx")

  expect_equal(.ret$rx,
               c("x(0) <- a",
                 "dx(0) <- b",
                 "d/dt(x) <- dx",
                 "d/dt(dx) <-  - x - dx"))

  .ret <- .equation("a=amtDose")

  expect_equal(.ret$rx, "a <- dose")

  .ret <- .equation("a=tDose")

  expect_equal(.ret$rx, "a <- tlast")

  .ret <- .equation("a=t+3")
  expect_equal(.ret$rx, "a <- time + 3")

  expect_error(.equation("a=inftDose"), "inftDose")

  expect_error(.equation("t0=0"), "t0")

  expect_error(.equation("t_0=0"), "t0")

  .ret <- .equation("a=invlogit(b)")
  expect_equal(.ret$rx, "a <- expit(b)")

  .ret <- .equation("a=norminv(b)")
  expect_equal(.ret$rx, "a <- qnorm(b)")

  .ret <- .equation("a=normcdf(b)")
  expect_equal(.ret$rx, "a <- pnorm(b)")

  .ret <- .equation("a=gammaln(b)")
  expect_equal(.ret$rx, "a <- lgamma(b)")

  .ret <- .equation("a=factln(b)")
  expect_equal(.ret$rx, "a <- lfactorial(b)")

  expect_error(.equation("ddt_x = ka*x-k*delay(x,tau)"), "delay")

  expect_error(.equation("ddt_x = ka*x-k*rem(tau)"), "rem")

})
