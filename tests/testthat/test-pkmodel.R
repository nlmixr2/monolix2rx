test_that("pkmodel()", {

  .ret <- .pk("Cc = pkmodel(V, Cl)")

  expect_equal(.pkmodel2macro(.ret, TRUE),
              c("compartment(cmt=1, volume=V, concentration=Cc)",
                "iv(adm=1, cmt=1)",
                "elimination(cmt=1, Cl)"))

  expect_equal(.pk2rx(.ret)$pk,
               c("d/dt(cmt1) <-  - Cl/V*cmt1",
                 "Cc <- cmt1/V"))

  .ret <- .pk("Cc = pkmodel(V, Cl, ka)")

  expect_equal(.pkmodel2macro(.ret, TRUE),
              c("compartment(cmt=1, volume=V, concentration=Cc)",
                "absorption(adm=1, ka, cmt=1)",
                "elimination(cmt=1, Cl)"))

  expect_equal(.pk2rx(.ret)$pk,
               c("d/dt(cmt1d) <-  - ka*cmt1d",
                 "d/dt(cmt1) <-  + ka*cmt1d - Cl/V*cmt1",
                 "Cc <- cmt1/V"))


  .ret <- .pk("{Cp, Ce} = pkmodel(V, Cl, ka, ke0)")

  expect_equal(.pkmodel2macro(.ret, TRUE),
               c("compartment(cmt=1, volume=V, concentration=Cp)",
                 "absorption(adm=1, ka, cmt=1)",
                 "elimination(cmt=1, Cl)",
                 "effect(cmt=1, ke0, concentration=Ce)"))

  expect_equal(.pk2rx(.ret)$pk,
               c("d/dt(cmt1d) <-  - ka*cmt1d",
                 "d/dt(cmt1) <-  + ka*cmt1d - Cl/V*cmt1",
                 "Cp <- cmt1/V",
                 "d/dt(Ce) <- ke0*(Cp - Ce)"))

  .ret <- .pk("{Cp, Ce} = pkmodel(V, Cl, ka, ke0, Mtt, Ktr)")

  expect_equal(.pkmodel2macro(.ret, TRUE),
               c("compartment(cmt=1, volume=V, concentration=Cp)",
                 "absorption(adm=1, ka, Ktr, Mtt, cmt=1)",
                 "elimination(cmt=1, Cl)",
                 "effect(cmt=1, ke0, concentration=Ce)"))

  expect_equal(.pk2rx(.ret)$pk,
               c("d/dt(cmt1d) <-  - ka*cmt1d + transit(Mtt*Ktr-1, Mtt, 1.0)",
                 "d/dt(cmt1) <-  + ka*cmt1d - Cl/V*cmt1",
                 "Cp <- cmt1/V",
                 "d/dt(Ce) <- ke0*(Cp - Ce)"))

  .ret <- .pk("{Cp, Ce} = pkmodel(V, Cl, ka, ke0, Mtt, Ktr, p=f)")

  expect_equal(.pkmodel2macro(.ret, TRUE),
               c("compartment(cmt=1, volume=V, concentration=Cp)",
                 "absorption(adm=1, p = f, ka, Ktr, Mtt, cmt=1)",
                 "elimination(cmt=1, Cl)",
                 "effect(cmt=1, ke0, concentration=Ce)"))

  expect_equal(.pk2rx(.ret)$pk,
               c("d/dt(cmt1d) <-  - ka*cmt1d + transit(Mtt*Ktr-1, Mtt, f)",
                 "d/dt(cmt1) <-  + ka*cmt1d - Cl/V*cmt1",
                 "Cp <- cmt1/V",
                 "d/dt(Ce) <- ke0*(Cp - Ce)"))

  .ret <- .pk("{Cp, Ce} = pkmodel(V, Cl, ka, ke0, Mtt, Ktr, p=f, Tlag)")

  expect_equal(.pkmodel2macro(.ret, TRUE),
               c("compartment(cmt=1, volume=V, concentration=Cp)",
                 "absorption(adm=1, Tlag, p = f, ka, Ktr, Mtt, cmt=1)",
                 "elimination(cmt=1, Cl)",
                 "effect(cmt=1, ke0, concentration=Ce)"))

  expect_equal(.pk2rx(.ret)$pk,
               c("d/dt(cmt1d) <-  - ka*cmt1d + transit(Mtt*Ktr-1, Mtt, f)",
                 "alag(cmt1d) <- Tlag",
                 "d/dt(cmt1) <-  + ka*cmt1d - Cl/V*cmt1",
                 "Cp <- cmt1/V",
                 "d/dt(Ce) <- ke0*(Cp - Ce)"))


  .ret <- .pk("{Cp, Ce} = pkmodel(V, Cl, ka, ke0, Mtt, Ktr, p=f, Tlag, k12=Q/V, k21=Q/V2)")

  expect_equal(.pkmodel2macro(.ret, TRUE),
               c("compartment(cmt=1, volume=V, concentration=Cp)",
                 "absorption(adm=1, Tlag, p = f, ka, Ktr, Mtt, cmt=1)",
                 "peripheral(k12 = Q/V, k21 = Q/V2)",
                 "elimination(cmt=1, Cl)",
                 "effect(cmt=1, ke0, concentration=Ce)"))

  expect_equal(.pk2rx(.ret)$pk,
               c("d/dt(cmt1d) <-  - ka*cmt1d + transit(Mtt*Ktr-1, Mtt, f)",
                 "alag(cmt1d) <- Tlag",
                 "d/dt(cmt1) <-  - Q/V*cmt1 + Q/V2*cmt2 + ka*cmt1d - Cl/V*cmt1",
                 "Cp <- cmt1/V",
                 "d/dt(Ce) <- ke0*(Cp - Ce)",
                 "d/dt(cmt2) <-  + Q/V*cmt1 - Q/V2*cmt2"))

  .ret <- .pk("Cp = pkmodel(V, Cl, ka, p=f, Tlag, k12, k21, k13, k31)")

  expect_equal(.pkmodel2macro(.ret, TRUE),
               c("compartment(cmt=1, volume=V, concentration=Cp)",
                 "absorption(adm=1, Tlag, p = f, ka, cmt=1)",
                 "peripheral(k12, k21)",
                 "peripheral(k13, k31)",
                 "elimination(cmt=1, Cl)"))

  expect_equal(.pk2rx(.ret)$pk,
               c("d/dt(cmt1d) <-  - ka*cmt1d",
                 "f(cmt1d) <- f",
                 "alag(cmt1d) <- Tlag",
                 "d/dt(cmt1) <-  - k12*cmt1 + k21*cmt2 - k13*cmt1 + k31*cmt3 + ka*cmt1d - Cl/V*cmt1",
                 "Cp <- cmt1/V",
                 "d/dt(cmt2) <-  + k12*cmt1 - k21*cmt2",
                 "d/dt(cmt3) <-  + k13*cmt1 - k31*cmt3"))

  .ret <- .pk("Cp = pkmodel(V, Cl, p=f, Tlag, k12, k21, k13, k31, Tk0)")

  expect_equal(.pkmodel2macro(.ret, TRUE),
               c("compartment(cmt=1, volume=V, concentration=Cp)",
                 "absorption(adm=1, Tlag, Tk0, p = f, Tk0, cmt=1)",
                 "peripheral(k12, k21)",
                 "peripheral(k13, k31)",
                 "elimination(cmt=1, Cl)"))

  expect_equal(.pk2rx(.ret)$pk,
               c("d/dt(cmt1) <-  - k12*cmt1 + k21*cmt2 - k13*cmt1 + k31*cmt3 - Cl/V*cmt1",
                 "dur(cmt1) <- Tk0",
                 "f(cmt1) <- f",
                 "alag(cmt1) <- Tlag",
                 "Cp <- cmt1/V",
                 "d/dt(cmt2) <-  + k12*cmt1 - k21*cmt2",
                 "d/dt(cmt3) <-  + k13*cmt1 - k31*cmt3"))

  .ret <- .pk("Cp = pkmodel(V, Km, Vm, p=f, Tlag, k12, k21, k13, k31, Tk0)")

  expect_equal(.pkmodel2macro(.ret, TRUE),
               c("compartment(cmt=1, volume=V, concentration=Cp)",
                 "absorption(adm=1, Tlag, Tk0, p = f, Tk0, cmt=1)",
                 "peripheral(k12, k21)",
                 "peripheral(k13, k31)",
                 "elimination(cmt=1, Vm, Km)"))

  expect_equal(.pk2rx(.ret)$pk,
               c("d/dt(cmt1) <-  - k12*cmt1 + k21*cmt2 - k13*cmt1 + k31*cmt3 - (Vm*cmt1/V)/(Km + cmt1/V)",
                 "dur(cmt1) <- Tk0",
                 "f(cmt1) <- f",
                 "alag(cmt1) <- Tlag",
                 "Cp <- cmt1/V",
                 "d/dt(cmt2) <-  + k12*cmt1 - k21*cmt2",
                 "d/dt(cmt3) <-  + k13*cmt1 - k31*cmt3"))

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
               c("d/dt(cmt1) <-  - k12*cmt1 + k21*cmt2 - k13*cmt1 + k31*cmt3 - kel*cmt1",
                 "dur(cmt1) <- Tk0",
                 "f(cmt1) <- f",
                 "alag(cmt1) <- Tlag",
                 "Cp <- cmt1/V",
                 "d/dt(cmt2) <-  + k12*cmt1 - k21*cmt2",
                 "d/dt(cmt3) <-  + k13*cmt1 - k31*cmt3"))

})
