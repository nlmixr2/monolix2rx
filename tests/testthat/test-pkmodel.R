test_that("pkmodel()", {

  .ret <- .pk("Cc = pkmodel(V, Cl)")

  expect_equal(.pkmodel2macro(.ret, TRUE),
              c("compartment(cmt=1, volume=V, concentration=Cc)",
                "iv(adm=1, cmt=1)",
                "elimination(cmt=1, Cl)"))

  expect_equal(.pk2rx(.ret)$pk,
               c("d/dt(central) <-  - Cl/V*central",
                 "Cc <- central/V"))

  .ret <- .pk("Cc = pkmodel(V, Cl, ka)")

  expect_equal(.pkmodel2macro(.ret, TRUE),
              c("compartment(cmt=1, volume=V, concentration=Cc)",
                "absorption(adm=1, ka, cmt=1)",
                "elimination(cmt=1, Cl)"))

  expect_equal(.pk2rx(.ret)$pk,
               c("d/dt(depot) <-  - ka*depot",
                 "d/dt(central) <-  + ka*depot - Cl/V*central",
                 "Cc <- central/V"))


  .ret <- .pk("{Cp, Ce} = pkmodel(V, Cl, ka, ke0)")

  expect_equal(.pkmodel2macro(.ret, TRUE),
               c("compartment(cmt=1, volume=V, concentration=Cp)",
                 "absorption(adm=1, ka, cmt=1)",
                 "elimination(cmt=1, Cl)",
                 "effect(cmt=1, ke0, concentration=Ce)"))

  expect_equal(.pk2rx(.ret)$pk,
               c("d/dt(depot) <-  - ka*depot",
                 "d/dt(central) <-  + ka*depot - Cl/V*central",
                 "Cp <- central/V",
                 "d/dt(Ce) <- ke0*(Cp - Ce)"))

  .ret <- .pk("{Cp, Ce} = pkmodel(V, Cl, ka, ke0, Mtt, Ktr)")

  expect_equal(.pkmodel2macro(.ret, TRUE),
               c("compartment(cmt=1, volume=V, concentration=Cp)",
                 "absorption(adm=1, ka, Ktr, Mtt, cmt=1)",
                 "elimination(cmt=1, Cl)",
                 "effect(cmt=1, ke0, concentration=Ce)"))

  expect_equal(.pk2rx(.ret)$pk,
               c("d/dt(depot) <-  - ka*depot + transit(Mtt*Ktr-1, Mtt, 1)",
                 "d/dt(central) <-  + ka*depot - Cl/V*central",
                 "Cp <- central/V",
                 "d/dt(Ce) <- ke0*(Cp - Ce)"))

  .ret <- .pk("{Cp, Ce} = pkmodel(V, Cl, ka, ke0, Mtt, Ktr, p=f)")

  expect_equal(.pkmodel2macro(.ret, TRUE),
               c("compartment(cmt=1, volume=V, concentration=Cp)",
                 "absorption(adm=1, p = f, ka, Ktr, Mtt, cmt=1)",
                 "elimination(cmt=1, Cl)",
                 "effect(cmt=1, ke0, concentration=Ce)"))

  expect_equal(.pk2rx(.ret)$pk,
               c("d/dt(depot) <-  - ka*depot + transit(Mtt*Ktr-1, Mtt, f)",
                 "d/dt(central) <-  + ka*depot - Cl/V*central",
                 "Cp <- central/V",
                 "d/dt(Ce) <- ke0*(Cp - Ce)"))

  .ret <- .pk("{Cp, Ce} = pkmodel(V, Cl, ka, ke0, Mtt, Ktr, p=f, Tlag)")

  expect_equal(.pkmodel2macro(.ret, TRUE),
               c("compartment(cmt=1, volume=V, concentration=Cp)",
                 "absorption(adm=1, Tlag, p = f, ka, Ktr, Mtt, cmt=1)",
                 "elimination(cmt=1, Cl)",
                 "effect(cmt=1, ke0, concentration=Ce)"))

  expect_equal(.pk2rx(.ret)$pk,
               c("d/dt(depot) <-  - ka*depot + transit(Mtt*Ktr-1, Mtt, f)",
                 "alag(depot) <- Tlag",
                 "d/dt(central) <-  + ka*depot - Cl/V*central",
                 "Cp <- central/V",
                 "d/dt(Ce) <- ke0*(Cp - Ce)"))


  .ret <- .pk("{Cp, Ce} = pkmodel(V, Cl, ka, ke0, Mtt, Ktr, p=f, Tlag, k12=Q/V, k21=Q/V2)")

  expect_equal(.pkmodel2macro(.ret, TRUE),
               c("compartment(cmt=1, volume=V, concentration=Cp)",
                 "absorption(adm=1, Tlag, p = f, ka, Ktr, Mtt, cmt=1)",
                 "peripheral(k12 = Q/V, k21 = Q/V2)",
                 "elimination(cmt=1, Cl)",
                 "effect(cmt=1, ke0, concentration=Ce)"))

  expect_equal(.pk2rx(.ret)$pk,
               c("d/dt(depot) <-  - ka*depot + transit(Mtt*Ktr-1, Mtt, f)",
                 "alag(depot) <- Tlag",
                 "d/dt(central) <-  - Q/V*central + Q/V2*cmt2 + ka*depot - Cl/V*central",
                 "Cp <- central/V",
                 "d/dt(Ce) <- ke0*(Cp - Ce)",
                 "d/dt(cmt2) <-  + Q/V*central - Q/V2*cmt2"))

  .ret <- .pk("Cp = pkmodel(V, Cl, ka, p=f, Tlag, k12, k21, k13, k31)")

  expect_equal(.pkmodel2macro(.ret, TRUE),
               c("compartment(cmt=1, volume=V, concentration=Cp)",
                 "absorption(adm=1, Tlag, p = f, ka, cmt=1)",
                 "peripheral(k12, k21)",
                 "peripheral(k13, k31)",
                 "elimination(cmt=1, Cl)"))

  expect_equal(.pk2rx(.ret)$pk,
               c("d/dt(depot) <-  - ka*depot",
                 "f(depot) <- f",
                 "alag(depot) <- Tlag",
                 "d/dt(central) <-  - k12*central + k21*cmt2 - k13*central + k31*cmt3 + ka*depot - Cl/V*central",
                 "Cp <- central/V",
                 "d/dt(cmt2) <-  + k12*central - k21*cmt2",
                 "d/dt(cmt3) <-  + k13*central - k31*cmt3"))

  .ret <- .pk("Cp = pkmodel(V, Cl, p=f, Tlag, k12, k21, k13, k31, Tk0)")

  expect_equal(.pkmodel2macro(.ret, TRUE),
               c("compartment(cmt=1, volume=V, concentration=Cp)",
                 "absorption(adm=1, Tlag, Tk0, p = f, Tk0, cmt=1)",
                 "peripheral(k12, k21)",
                 "peripheral(k13, k31)",
                 "elimination(cmt=1, Cl)"))

  expect_equal(.pk2rx(.ret)$pk,
               c("d/dt(central) <-  - k12*central + k21*cmt2 - k13*central + k31*cmt3 - Cl/V*central",
                 "dur(central) <- Tk0",
                 "f(central) <- f",
                 "alag(central) <- Tlag",
                 "Cp <- central/V",
                 "d/dt(cmt2) <-  + k12*central - k21*cmt2",
                 "d/dt(cmt3) <-  + k13*central - k31*cmt3"))

  .ret <- .pk("Cp = pkmodel(V, Km, Vm, p=f, Tlag, k12, k21, k13, k31, Tk0)")

  expect_equal(.pkmodel2macro(.ret, TRUE),
               c("compartment(cmt=1, volume=V, concentration=Cp)",
                 "absorption(adm=1, Tlag, Tk0, p = f, Tk0, cmt=1)",
                 "peripheral(k12, k21)",
                 "peripheral(k13, k31)",
                 "elimination(cmt=1, Vm, Km)"))

  expect_equal(.pk2rx(.ret)$pk,
               c("d/dt(central) <-  - k12*central + k21*cmt2 - k13*central + k31*cmt3 - (Vm*central/V)/(Km + central/V)",
                 "dur(central) <- Tk0",
                 "f(central) <- f",
                 "alag(central) <- Tlag",
                 "Cp <- central/V",
                 "d/dt(cmt2) <-  + k12*central - k21*cmt2",
                 "d/dt(cmt3) <-  + k13*central - k31*cmt3"))

  .ret <- .pk("; To define a compartment with ID 1, of volume V, an amount called Ac, and a concentration called Cc
compartment(cmt=1, amount=Ac, volume=V, concentration=Cc)")

  expect_equal(.pkmodel2macro(.ret, TRUE),
               c("compartment(cmt = 1, amount = Ac, volume = V, concentration = Cc)"))

  expect_equal(.pk2rx(.ret)$equation$endLines,
               "Cc <- Ac/V")

  .ret <- .pk("Cp = pkmodel(V, k=kel, p=f, Tlag, k12, k21, k13, k31, Tk0)")

  expect_equal(.pkmodel2macro(.ret, TRUE),
               c("compartment(cmt=1, volume=V, concentration=Cp)",
                 "absorption(adm=1, Tlag, Tk0, p = f, Tk0, cmt=1)",
                 "peripheral(k12, k21)",
                 "peripheral(k13, k31)",
                 "elimination(cmt=1, k = kel)"))

  expect_equal(.pk2rx(.ret)$pk,
               c("d/dt(central) <-  - k12*central + k21*cmt2 - k13*central + k31*cmt3 - kel*central",
                 "dur(central) <- Tk0",
                 "f(central) <- f",
                 "alag(central) <- Tlag",
                 "Cp <- central/V",
                 "d/dt(cmt2) <-  + k12*central - k21*cmt2",
                 "d/dt(cmt3) <-  + k13*central - k31*cmt3"))

})
