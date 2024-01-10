test_that("equation tests", {

  pk <- .pk("")

  .ret <- .equation("x_0 = V
ddt_x = -k*x", pk)

  expect_equal(.ret$rx,
               c("x(0) <- V", "d/dt(x) <-  - k * x"))

  .ret <- .equation("if t<=10
   c = 1
elseif t<20
   c = 2
else
   c = 3
end
", pk)

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
ddt_x = dx", pk)

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
ddt_x = -k*x", pk)

  expect_equal(.ret$odeType, "stiff")

  .ret <- .equation("; ode is considered as stiff
odeType =  nonStiff
x_0 = V
ddt_x = -k*x", pk)

  expect_equal(.ret$odeType, "nonStiff")

  .ret <- .equation("; ode is considered as stiff
x_0 = V
ddt_x = -k*x", pk)

  expect_equal(.ret$odeType, "nonStiff")

  .ret <- .equation("x_0 = a
dx_0 = b
ddt_x = dx
ddt_dx = -x-dx", pk)

  expect_equal(.ret$rx,
               c("x(0) <- a",
                 "dx(0) <- b",
                 "d/dt(x) <- dx",
                 "d/dt(dx) <-  - x - dx"))

  .ret <- .equation("a=amtDose", pk)

  expect_equal(.ret$rx, "a <- dose")

  .ret <- .equation("a=tDose", pk)

  expect_equal(.ret$rx, "a <- tlast")

  .ret <- .equation("a=t+3", pk)
  expect_equal(.ret$rx, "a <- time + 3")

  expect_error(.equation("a=inftDose", pk), "inftDose")

  expect_warning(.equation("t0=0", pk), NA)
  expect_warning(.equation("t0=10", pk))

  expect_warning(.equation("t_0=0", pk), NA)
  expect_warning(.equation("t_0=10", pk))

  .ret <- .equation("a=invlogit(b)", pk)
  expect_equal(.ret$rx, "a <- expit(b)")

  .ret <- .equation("a=norminv(b)", pk)
  expect_equal(.ret$rx, "a <- qnorm(b)")

  .ret <- .equation("a=normcdf(b)", pk)
  expect_equal(.ret$rx, "a <- pnorm(b)")

  .ret <- .equation("a=gammaln(b)", pk)
  expect_equal(.ret$rx, "a <- lgamma(b)")

  .ret <- .equation("a=factln(b)", pk)
  expect_equal(.ret$rx, "a <- lfactorial(b)")

  expect_error(.equation("ddt_x = ka*x-k*delay(x,tau)", pk), "delay")

  expect_error(.equation("ddt_x = ka*x-k*rem(tau)", pk), "rem")

})

test_that("mixing pk and equation", {

  tmp <- .equation("Cc = pkmodel(Tlag,ka,V,Cl)
  E_0 = Rin/kout
  ddt_E= Rin*(1-Cc/(Cc+IC50)) - kout*E")

  expect_equal(tmp$rx,
               c("d/dt(cmt1d) <-  - ka*cmt1d",
                 "alag(cmt1d) <- Tlag",
                 "d/dt(cmt1) <-  + ka*cmt1d - Cl/V*cmt1",
                 "Cc <- cmt1/V",
                 "E(0) <- Rin / kout",
                 "d/dt(E) <- Rin * (1 - Cc / (Cc + IC50)) - kout * E"))

})

test_that("wsmm mixture not supported", {
  expect_error(.equation("f = wsmm(f1, p, f2, 1-p)"),
               "wsmm")
})

test_that("bsmm", {
  expect_error(.equation("f = bsmm(f1, p, f2, 1-p)"),
               "bsmm")
})

test_that("bsmm", {
  expect_error(.equation("M = bsmm(M1,p1,M2,1-p1)"),
               "bsmm")
})
