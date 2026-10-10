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

  expect_equal(.ret$rx, "a <- dose()")

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

test_that("bsmm and wsmm are kept for the mixture rewrite", {
  expect_equal(.equation("M = bsmm(M1,p1,M2,1-p1)")$rx, "M <- bsmm(M1, p1, M2, 1 - p1)")
  expect_equal(.equation("f = wsmm(f1, p, f2, 1-p)")$rx, "f <- wsmm(f1, p, f2, 1 - p)")
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

test_that("no PK state leaks into the next equation block", {
  .equation("Cc = Ac/V", .pk("compartment(cmt=1, amount=Ac)\ndepot(target=Ac, Tk0)\nelimination(cmt=1, k)"))
  .rx <- .equation("ddt_Ac = -k*Ac\nCc = Ac/V", .pk("depot(target=Ac, ka)"))$rx
  expect_false(any(grepl("dur", .rx)))
  expect_equal(.covEq("lw = log(WT/70)")$dplyr, "lw = log(WT / 70)")
})

test_that("depot(target=) into a compartment without other flows", {
  .rx <- .equation("Cc = Ac/V", .pk("compartment(cmt=1, amount=Ac)\ndepot(target=Ac, ka)"))$rx
  expect_true("d/dt(Ac) <- 0 + ka*Acd" %in% .rx)
  expect_true("d/dt(Acd) <-  - ka*Acd" %in% .rx)
})

test_that("transfer() keeps the inflow of the receiving compartment", {
  .pk <- "compartment(cmt=1, amount=Ac)\ncompartment(cmt=2, amount=Ab)\ntransfer(from=1, to=2, kt)"
  expect_equal(.equation("Cb = Ab/V", .pk(paste0(.pk, "\nelimination(cmt=2, k)")))$rx[2],
               "d/dt(Ab) <-  + kt*Ac - k*Ab")
  expect_equal(.equation("Cb = Ab/V", .pk(.pk))$rx[2], "d/dt(Ab) <-  + kt*Ac")
})

test_that("chained powers are right associative for rxode2", {
  expect_equal(.equation("y = 2^x^2\nz = x^2 + y^3\nu = a^b^c^d\nw = 2^-x^2")$rx,
               c("y <- 2^(x^2)", "z <- x^2 + y^3", "u <- a^(b^(c^d))", "w <- 2^( - x^2)"))
})

test_that("dose keywords in PK macro arguments are translated", {
  .rx <- .equation("ddt_Ap = -k*Ap\nCc = Ap", "depot(adm=1, target=Ap, p=2/amtDose, Tlag=0.1*tDose)")$rx
  expect_true("f(Ap) <- 2/dose()" %in% .rx)
  expect_true("alag(Ap) <- 0.1*tlast" %in% .rx)
  .rx <- .equation("ddt_Ap = -k*Ap\nCc = Ap", "depot(adm=1, target=Ap, p=invlogit(a^b^c), Tlag=normcdf(t))")$rx
  expect_true("f(Ap) <- expit(a^(b^c))" %in% .rx)
  expect_true("alag(Ap) <- pnorm(time)" %in% .rx)
  .rx <- .equation("ddt_Ap = -k*Ap\nCc = Ap", "depot(adm=1, target=Ap, p=2^-x^2, Tlag=1e-3*t)")$rx
  expect_true("f(Ap) <- 2^(-x^2)" %in% .rx)
  expect_true("alag(Ap) <- 1e-3*time" %in% .rx)
  expect_error(.equation("ddt_Ap = -k*Ap\nCc = Ap", "depot(adm=1, target=Ap, Tlag=inftDose)"),
               "inftDose")
  expect_true("f(central) <- dose()/100" %in% .equation("", "Cc = pkmodel(V, Cl, p=amtDose/100)")$rx)
})

test_that("names rxode2 reserves are renamed in [LONGITUDINAL] equations", {
  .e <- .equation("rate = max(Cl/V, 0)\nddt_ii = -rate*ii\nii_0 = 1\nCc = ii/V*t",
                  "dur = 0.5\ndepot(target=ii, Tlag=dur)\ncompartment(cmt=1, amount=Ab)")
  expect_equal(sort(.e$rename), c("dur", "ii", "rate"))
  expect_equal(.e$monolix, "rate = max(Cl/V, 0)\nddt_ii = -rate*ii\nii_0 = 1\nCc = ii/V*t")
  expect_true(all(c("mlx_dur <- 0.5", "mlx_rate <- max(Cl / V, 0)", "d/dt(mlx_ii) <-  - mlx_rate * mlx_ii",
                    "alag(mlx_ii) <- mlx_dur", "mlx_ii(0) <- mlx_ii_0", "Cc <- mlx_ii / V * time") %in% .e$rx))
  # a macro keyword argument keeps its name
  expect_equal(c(.mlxRenameReserved("compartment(cmt=1, amount=cmt)")), "compartment(cmt=1, amount=mlx_cmt)")
  # x_0 alone is a legal name; TIME is refused in any case
  expect_equal(c(.mlxRenameReserved("y = rate_0 + ii_0")), "y = rate_0 + ii_0")
  expect_equal(c(.mlxRenameReserved("TIME = t - 2 ; time")), "mlx_TIME = t - 2 ; time")
  # print() keeps the Monolix names
  expect_equal(as.character(.pk("dur = 0.5\ndepot(target=Ad, Tlag=dur)", TRUE)),
               c("dur <- 0.5", "depot(adm = 1, target = Ad, Tlag = dur, p = 1)"))
  # [COVARIATE] equations write data columns and keep their names
  expect_equal(.covEq("rate = WT/70")$dplyr, "rate = WT / 70")
})

test_that("X_0 is an initial condition only for a state X", {
  expect_equal(.equation("A_gut_0 = 10\nddt_A_gut = -A_gut\nCc = A_gut")$rx,
               c("A_gut_0 <- 10", "A_gut(0) <- A_gut_0", "d/dt(A_gut) <-  - A_gut", "Cc <- A_gut"))
  expect_equal(.equation("E_0 = 10\nCc = E_0")$rx, c("E_0 <- 10", "Cc <- E_0"))
})

test_that("a reserved name as a model input is an error", {
  skip_on_cran()
  .dir <- file.path(tempfile(), "theo")
  dir.create(.dir, recursive = TRUE)
  on.exit(unlink(dirname(.dir), recursive = TRUE))
  file.copy(
    list.files(system.file("theo", package = "monolix2rx"), full.names = TRUE),
    .dir,
    recursive = TRUE
  )
  .f <- file.path(.dir, "theophylline_project.mlxtran")
  .l <- readLines(.f)
  .l <- gsub("\\bCl\\b", "rate", .l, perl = TRUE)
  .l <- gsub("oral1_1cpt_kaVCl.txt", "model.txt", .l, fixed = TRUE)
  writeLines(.l, .f)
  writeLines(c("[LONGITUDINAL]", "input = {ka, V, rate}", "", "PK:", "depot(target=Ac, ka)",
               "", "EQUATION:", "ddt_Ac = -rate/V*Ac", "Cc = Ac/V", "", "OUTPUT:", "output = Cc"),
             file.path(.dir, "model.txt"))
  expect_error(suppressMessages(monolix2rx(.f)), "rxode2 reserves 'rate'")
})
