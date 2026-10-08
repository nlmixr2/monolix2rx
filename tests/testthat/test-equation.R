test_that("equation tests", {

  pk <- .pk("")

  .ret <- .equation("x_0 = V
ddt_x = -k*x", pk)

  expect_equal(.ret$rx,
               c("x_0 <- V", "x(0) <- x_0", "d/dt(x) <-  - k * x"))

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
               c("x_0 <- a",
                 "x(0) <- x_0",
                 "dx_0 <- b",
                 "dx(0) <- dx_0",
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

  if (utils::packageVersion("rxode2") >= "5.1.7") {
    expect_equal(.equation("ddt_x = ka*x-k*delay(x,tau)", pk)$rx,
                 "d/dt(x) <- ka * x - k * delay(x, tau)")
  } else {
    expect_error(.equation("ddt_x = ka*x-k*delay(x,tau)", pk), "rxode2 >= 5.1.7")
  }

  expect_error(.equation("ddt_x = ka*x-k*rem(tau)", pk), "rem")

})

test_that("mixing pk and equation", {

  tmp <- .equation("Cc = pkmodel(Tlag,ka,V,Cl)
  E_0 = Rin/kout
  ddt_E= Rin*(1-Cc/(Cc+IC50)) - kout*E")

  expect_equal(tmp$rx,
               c("d/dt(depot) <-  - ka*depot",
                 "alag(depot) <- Tlag",
                 "d/dt(central) <-  + ka*depot - Cl/V*central",
                 "Cc <- central/V",
                 "E_0 <- Rin / kout",
                 "E(0) <- E_0",
                 "d/dt(E) <- Rin * (1 - Cc / (Cc + IC50)) - kout * E"))

})

test_that("longitudinal PK equations keep dependency order", {
  pk <- .pk("Cc = pkmodel(ka, V, Cl, k12, k21)")
  pk$preEq <- c("V <- V1", "k12 <- Q / V1", "k21 <- Q / V2")
  pk$postEq <- "Cu <- fup * max(0, Cc)"

  tmp <- .equation("K = Kmax * Cu
  ddt_TS = TS - K * TS", pk)

  .wV <- which(tmp$rx == "V <- V1")
  .wk12 <- which(tmp$rx == "k12 <- Q / V1")
  .wk21 <- which(tmp$rx == "k21 <- Q / V2")
  .wCu <- which(tmp$rx == "Cu <- fup * max(0, Cc)")
  .wK <- which(tmp$rx == "K <- Kmax * Cu")
  .wCentral <- grep("^d/dt\\(central\\) <-", tmp$rx)

  expect_true(length(.wCentral) == 1L)
  expect_true(.wV < .wCentral)
  expect_true(.wk12 < .wCentral)
  expect_true(.wk21 < .wCentral)
  expect_true(.wCu < .wK)
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

test_that("EQUATION: lines before pkmodel() stay before its ODEs", {
  .rx <- .equation("k12 = Q/V\nk21 = Q/V2\nCc = pkmodel(V, Cl, k12, k21)\nE = 2*Cc")$rx
  expect_equal(.rx[1:2], c("k12 <- Q / V", "k21 <- Q / V2"))
  expect_equal(.rx[length(.rx)], "E <- 2 * Cc")
  expect_true(any(grepl("^d/dt[(]central[)]", .rx[3:5])))
})

test_that("depot(target=) into a macro compartment", {
  .rx <- .equation("Cc = Ac/V", .pk("compartment(cmt=1, amount=Ac)\ndepot(target=Ac, ka, Tlag)\nelimination(cmt=1, k=Cl/V)"))$rx
  expect_equal(.rx[1:3], c("d/dt(Acd) <-  - ka*Acd", "alag(Acd) <- Tlag", "d/dt(Ac) <-  - Cl/V*Ac + ka*Acd"))
})
