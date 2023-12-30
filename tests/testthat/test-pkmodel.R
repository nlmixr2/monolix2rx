test_that("pkmodel()", {

  .ret <- .pk("Cc = pkmodel(V, Cl)")

  expect_equal(.pkmodel(.ret$pkmodel, .ret$Cc, .ret$Ce),
               c("d/dt(central) <-  - Cl/V*central",
                 "Cc <- central/V"))

  .ret <- .pk("Cc = pkmodel(V, Cl, ka)")

  expect_equal(.pkmodel(.ret$pkmodel, .ret$Cc, .ret$Ce),
               c("d/dt(depot) <- -ka*depot",
                 "d/dt(central) <- ka*depot - Cl/V*central",
                 "Cc <- central/V"))


  .ret <- .pk("{Cp, Ce} = pkmodel(V, Cl, ka, ke0)")

  expect_equal(.pkmodel(.ret$pkmodel, .ret$Cc, .ret$Ce),
               c("d/dt(depot) <- -ka*depot",
                 "d/dt(central) <- ka*depot - Cl/V*central",
                 "Cp <- central/V",
                 "d/dt(Ce) <- ke0*(Cp - Ce)"))

  .ret <- .pk("{Cp, Ce} = pkmodel(V, Cl, ka, ke0, Mtt, Ktr)")

  expect_equal(.pkmodel(.ret$pkmodel, .ret$Cc, .ret$Ce),
              c("d/dt(depot) <- -ka*depot + transit(Mtt*Ktr-1, Mtt, 1.0)",
                "d/dt(central) <- ka*depot - Cl/V*central",
                "Cp <- central/V",
                "d/dt(Ce) <- ke0*(Cp - Ce)"))

  .ret <- .pk("{Cp, Ce} = pkmodel(V, Cl, ka, ke0, Mtt, Ktr, p=f)")

  expect_equal(.pkmodel(.ret$pkmodel, .ret$Cc, .ret$Ce),
               c("d/dt(depot) <- -ka*depot + transit(Mtt*Ktr-1, Mtt, f)",
                 "d/dt(central) <- ka*depot - Cl/V*central",
                 "Cp <- central/V",
                 "d/dt(Ce) <- ke0*(Cp - Ce)"))


  .ret <- .pk("{Cp, Ce} = pkmodel(V, Cl, ka, ke0, Mtt, Ktr, p=f, Tlag)")

  expect_equal(.pkmodel(.ret$pkmodel, .ret$Cc, .ret$Ce),
               c("d/dt(depot) <- -ka*depot + transit(Mtt*Ktr-1, Mtt, f)",
                 "alag(depot) <- Tlag",
                 "d/dt(central) <- ka*depot - Cl/V*central",
                 "Cp <- central/V", "d/dt(Ce) <- ke0*(Cp - Ce)"))

  .ret <- .pk("{Cp, Ce} = pkmodel(V, Cl, ka, ke0, Mtt, Ktr, p=f, Tlag, k12=Q/V, k21=Q/V2)")

  expect_equal(.pkmodel(.ret$pkmodel, .ret$Cc, .ret$Ce),
               c("d/dt(depot) <- -ka*depot + transit(Mtt*Ktr-1, Mtt, f)",
                 "alag(depot) <- Tlag",
                 "d/dt(central) <- ka*depot - Q/V*central + Q/V2*periph - Cl/V*central",
                 "d/dt(periph) <- Q/V*central - Q/V2*periph",
                 "Cp <- central/V",
                 "d/dt(Ce) <- ke0*(Cp - Ce)"))

  .ret <- .pk("Cp = pkmodel(V, Cl, ka, p=f, Tlag, k12, k21, k13, k31)")

  expect_equal(.pkmodel(.ret$pkmodel, .ret$Cc, .ret$Ce),
               c("d/dt(depot) <- -ka*depot",
                 "f(depot) <- f",
                 "alag(depot) <- Tlag",
                 "d/dt(central) <- ka*depot - k12*central + k21*periph - k13*central + k31*periph2 - Cl/V*central",
                 "d/dt(periph) <- k12*central - k21*periph",
                 "d/dt(periph2) <- k13*central - k31*periph2",
                 "Cp <- central/V"))

  .ret <- .pk("Cp = pkmodel(V, Cl, p=f, Tlag, k12, k21, k13, k31, Tk0)")

  expect_equal(.pkmodel(.ret$pkmodel, .ret$Cc, .ret$Ce),
               c("d/dt(central) <-  - k12*central + k21*periph - k13*central + k31*periph2 - Cl/V*central",
                 "alag(central) <- Tlag",
                 "dur(central) <- Tk0",
                 "f(central) <- f",
                 "d/dt(periph) <- k12*central - k21*periph",
                 "d/dt(periph2) <- k13*central - k31*periph2",
                 "Cp <- central/V"))

  .ret <- .pk("Cp = pkmodel(V, Km, Vm, p=f, Tlag, k12, k21, k13, k31, Tk0)")

  expect_equal(.pkmodel(.ret$pkmodel, .ret$Cc, .ret$Ce),
               c("d/dt(central) <-  - k12*central + k21*periph - k13*central + k31*periph2 - (Vm*central/V)/(Km + central/V)",
                 "alag(central) <- Tlag",
                 "dur(central) <- Tk0",
                 "f(central) <- f",
                 "d/dt(periph) <- k12*central - k21*periph",
                 "d/dt(periph2) <- k13*central - k31*periph2",
                 "Cp <- central/V"))

  .ret <- .pk("; To define a compartment with ID 1, of volume V, an amount called Ac, and a concentration called Cc
compartment(cmt=1, amount=Ac, volume=V, concentration=Cc)")

  expect_equal(.pkmodel(.ret$pkmodel, .ret$Cc, .ret$Ce),
               character(0))

})
