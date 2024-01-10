test_that("pk", {

  emptyObj <- function() {
    .ret <- .pk("")
  }

  .ret <- .pk("Cc = pkmodel(V, Cl)")

  .ret2 <- emptyObj()
  .ret2$pkmodel["V"] <- ""
  .ret2$pkmodel["Cl"] <- ""
  .ret2$Cc <- "Cc"

  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               "Cc = pkmodel(V, Cl)")

  .ret <- .pk("Cc = pkmodel(Tlag, ka, V, k, k12, k21)")

  .ret2 <- emptyObj()
  .ret2$pkmodel["V"] <- ""
  .ret2$pkmodel["k"] <- ""
  .ret2$pkmodel["k12"] <- ""
  .ret2$pkmodel["k21"] <- ""
  .ret2$pkmodel["ka"] <- ""
  .ret2$pkmodel["Tlag"] <- ""
  .ret2$Cc <- "Cc"

  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               "Cc = pkmodel(V, ka, Tlag, k, k12, k21)")

  .ret <- .pk("Cc = pkmodel(Tlag, ka, V, k=Cl/V, k12=Q/V, k21=Q/V2)")

  .ret2 <- emptyObj()
  .ret2$pkmodel["V"] <- ""
  .ret2$pkmodel["k"] <- "Cl/V"
  .ret2$pkmodel["k12"] <- "Q/V"
  .ret2$pkmodel["k21"] <- "Q/V2"
  .ret2$pkmodel["ka"] <- ""
  .ret2$pkmodel["Tlag"] <- ""
  .ret2$Cc <- "Cc"

  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               "Cc = pkmodel(V, ka, Tlag, k = Cl/V, k12 = Q/V, k21 = Q/V2)")

  .ret <- .pk("Cc = pkmodel(Tk0, V, k, k12, k21, k13, k31)")

  .ret2 <- emptyObj()
  .ret2$pkmodel["V"] <- ""
  .ret2$pkmodel["k"] <- ""
  .ret2$pkmodel["k12"] <- ""
  .ret2$pkmodel["k21"] <- ""
  .ret2$pkmodel["k13"] <- ""
  .ret2$pkmodel["k31"] <- ""
  .ret2$pkmodel["Tk0"] <- ""
  .ret2$Cc <- "Cc"

  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               "Cc = pkmodel(V, Tk0, k, k12, k21, k13, k31)")


  .ret <- .pk("Cc = pkmodel(ka, Mtt, Ktr, V, Cl)")


  .ret2 <- emptyObj()
  .ret2$pkmodel["V"] <- ""
  .ret2$pkmodel["Cl"] <- ""
  .ret2$pkmodel["ka"] <- ""
  .ret2$pkmodel["Mtt"] <- ""
  .ret2$pkmodel["Ktr"] <- ""
  .ret2$Cc <- "Cc"

  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               "Cc = pkmodel(V, ka, Ktr, Mtt, Cl)")

  .ret <- .pk("{Cc, Ce} = pkmodel(ka, V, Cl, ke0)")

  .ret2 <- emptyObj()
  .ret2$pkmodel["ka"] <- ""
  .ret2$pkmodel["V"] <- ""
  .ret2$pkmodel["Cl"] <- ""
  .ret2$pkmodel["ke0"] <- ""
  .ret2$Cc <- "Cc"
  .ret2$Ce <- "Ce"

  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               "{Cc, Ce} = pkmodel(V, ka, Cl, ke0)")

  .ret <- .pk("{Cc, Ce} = pkmodel(Tlag, ka, p, V, Vm, Km, k12, k21, k13, k31, ke0)")

  .ret2 <- emptyObj()
  .ret2$pkmodel["Tlag"] <- ""
  .ret2$pkmodel["ka"] <- ""
  .ret2$pkmodel["p"] <- ""
  .ret2$pkmodel["V"] <- ""
  .ret2$pkmodel["Vm"] <- ""
  .ret2$pkmodel["Km"] <- ""
  .ret2$pkmodel["k12"] <- ""
  .ret2$pkmodel["k21"] <- ""
  .ret2$pkmodel["k13"] <- ""
  .ret2$pkmodel["k31"] <- ""
  .ret2$pkmodel["ke0"] <- ""
  .ret2$Cc <- "Cc"
  .ret2$Ce <- "Ce"

  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               "{Cc, Ce} = pkmodel(V, ka, Tlag, p, Vm, Km, k12, k21, k13, k31, ke0)")


  .ret <- .pk("; To define a compartment with ID 1, of volume V, an amount called Ac, and a concentration called Cc
compartment(cmt=1, amount=Ac, volume=V, concentration=Cc)")

  .ret2 <- emptyObj()
  .ret2$compartment <- data.frame(cmt = 1L, amount = "Ac", volume = "V", concentration = "Cc")

  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               "compartment(cmt = 1, amount = Ac, volume = V, concentration = Cc)")

  .ret <- .pk("compartment(cmt=1, amount=Ac, concentration=Cc, volume=V)
iv(cmt=1, type=1)
elimination(cmt=1, k)")

  .ret2 <- emptyObj()
  .ret2$compartment <- data.frame(cmt = 1L, amount = "Ac", volume = "V", concentration = "Cc")
  .ret2$iv <- data.frame(adm = 1L, admd=1L, cmt = 1L, Tlag = NA_character_, p = NA_character_)
  .ret2$elimination <- data.frame(cmt = 1L, V = NA_character_, k = "", Cl = NA_character_,
                                  Vm = NA_character_, Km = NA_character_)
  .ret2$admd <- data.frame(adm = 1L, admd = 1L, cmt = 1L, target = NA_character_,
                           depot = FALSE, dur = FALSE, f = FALSE, tlag = FALSE)

  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               c("compartment(cmt = 1, amount = Ac, volume = V, concentration = Cc)",
                 "iv(adm = 1, cmt = 1)",
                 "elimination(cmt = 1, k)"))


  .ret <- .pk("; definition of a peripheral compartment with cmt=2 with rates k12 and k21,
; an amount called Ap, a volume V2, and a concentration Cp
peripheral(k12, k21, amount=Ap, volume=V2, concentration=Cp)
; definition of a peripheral compartment with cmt=3, linked to compartment 1,
; with rates k13 and k31 a volume equals by default to 1
peripheral(k13, k31)
; with compartments numbers larger than 9
peripheral(k2_13, k13_2)
")

  .ret2 <- emptyObj()
  .ret2$peripheral <- data.frame(in.i = c(1L, 1L, 2L),
                                 in.j = c(2L, 3L, 13L),
                                 in.eq = c("", "", ""),
                                 out.i = c(2L, 3L, 13L),
                                 out.j = c(1L, 1L, 2L),
                                 out.eq = c("", "", ""),
                                 amount = c("Ap", NA, NA),
                                 volume = c("V2", NA, NA),
                                 concentration = c("Cp", NA, NA))

  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               c("peripheral(k12, k21, amount = Ap, volume = V2, concentration = Cp)",
                 "",
                 "peripheral(k13, k31)",
                 "",
                 "peripheral(k2_13, k13_2)"))

  .ret <- .pk("peripheral(k12=Q/V, k21=Q/V2)")

  .ret2 <- emptyObj()
  .ret2$peripheral <- data.frame(in.i = 1L, in.j = 2L, in.eq = "Q/V",
                                 out.i = 2L, out.j = 1L, out.eq = "Q/V2",
                                 amount = NA_character_, volume = NA_character_, concentration = NA_character_)

  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               "peripheral(k12 = Q/V, k21 = Q/V2)")

  .ret <- .pk("; Define an effect compartment linked to the base compartment 1,
; with a transfer rate ke0 to the effect compartment,
; with Ce as concentration's name
effect(cmt=1, ke0, concentration=Ce)
")

  .ret2 <- emptyObj()
  .ret2$effect <- data.frame(cmt = 1L, ke0 = "", concentration = "Ce")

  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               "effect(cmt = 1, ke0, concentration = Ce)")

  expect_error(.pk("elimination(cmt=1, k, volume=v2)"), 'volume')

  .ret <- .pk("compartment(cmt=1, amount=Ac, concentration=Cc, volume=V)
elimination(cmt=1, k)
iv(cmt=1)
effect(cmt=1, ke0, concentration=Ce)")

  .ret2 <- emptyObj()
  .ret2$compartment <- data.frame(cmt = 1L, amount = "Ac", volume = "V", concentration = "Cc")
  .ret2$elimination <- data.frame(cmt = 1L, V = NA_character_, k = "",
                                  Cl = NA_character_, Vm = NA_character_, Km = NA_character_)
  .ret2$iv <- data.frame(adm =1L, admd=1L, cmt = 1L, Tlag = NA_character_, p = NA_character_)
  .ret2$effect <- data.frame(cmt = 1L, ke0 = "", concentration = "Ce")
  .ret2$admd <- data.frame(adm = 1L, admd = 1L, cmt = 1L, target = NA_character_, depot = FALSE, dur = FALSE, f = FALSE, tlag = FALSE)

  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               c("compartment(cmt = 1, amount = Ac, volume = V, concentration = Cc)",
                 "iv(adm = 1, cmt = 1)",
                 "elimination(cmt = 1, k)",
                 "effect(cmt = 1, ke0, concentration = Ce)"))

  .ret <- .pk("transfer(from=1, to=2, kt)")

  .ret2 <- emptyObj()
  .ret2$transfer <- data.frame(from = 1L, to = 2L, kt = "")
  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               "transfer(from = 1, to = 2, kt)")

  .ret <- .pk("compartment(cmt=1, amount=Ac, concentration=Cc1, volume=V1)
compartment(cmt=2, amount=Ad, concentration=Cc2, volume=V2)
oral(cmt=1,ka=1)
transfer(from=1, to=2, kt)")

  .ret2 <- emptyObj()
  .ret2$compartment <- data.frame(cmt = 1:2, amount = c("Ac", "Ad"),
                                  volume = c("V1", "V2"), concentration = c("Cc1", "Cc2"))
  .ret2$oral <- data.frame(adm = 1L, admd=1L, cmt = 1L, Tlag = NA_character_,
                           p = NA_character_, Tk0 = NA_character_, ka = "1",
                           Ktr = NA_character_, Mtt = NA_character_)
  .ret2$transfer <- data.frame(from = 1L, to = 2L, kt = "")
  .ret2$admd <- data.frame(adm = 1L, admd = 1L, cmt = 1L, target = NA_character_,
                           depot = TRUE, dur = FALSE, f = FALSE, tlag = FALSE)
  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               c("compartment(cmt = 1, amount = Ac, volume = V1, concentration = Cc1)",
                 "oral(adm = 1, cmt = 1, ka = 1)",
                 "",
                 "compartment(cmt = 2, amount = Ad, volume = V2, concentration = Cc2)",
                 "transfer(from = 1, to = 2, kt)"))

  .ret <- .pk("depot(type=1, target=Ad, Tlag, p=F)")

  .ret2 <- emptyObj()
  .ret2$depot <- data.frame(adm = 1L, admd=1L, target = "Ad", Tlag = "",
                            p = "F", Tk0 = NA_character_, ka = NA_character_,
                            Ktr = NA_character_, Mtt = NA_character_)
  .ret2$admd <- data.frame(adm = 1L, admd = 1L, cmt = NA_integer_, target = "Ad",
                           depot = FALSE, dur = FALSE, f = TRUE, tlag = TRUE)

  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               "depot(adm = 1, target = Ad, Tlag, p = F)")

  .ret <- .pk("depot(type=2, target=Ac, Tk0)")

  .ret2 <- emptyObj()
  .ret2$depot <- data.frame(adm = 2L, admd=1L, target = "Ac", Tlag = NA_character_,
                            p = NA_character_, Tk0 = "", ka = NA_character_,
                            Ktr = NA_character_, Mtt = NA_character_)
  .ret2$admd <- data.frame(adm = 2L, admd = 1L, cmt = NA_integer_, target = "Ac", depot = FALSE, dur = TRUE, f = FALSE, tlag = FALSE)
  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               "depot(adm = 2, target = Ac, Tk0)")

  .ret <- .pk("depot(target=Ac, ka, Tlag=2.1, p=0.3)")

  .ret2 <- emptyObj()
  .ret2$depot <- data.frame(adm = 1L, admd=1L, target = "Ac", Tlag = "2.1",
                            p = "0.3", Tk0 = NA_character_, ka = "",
                            Ktr = NA_character_, Mtt = NA_character_)
  .ret2$admd <- data.frame(adm = 1L, admd = 1L, cmt = NA_integer_, target = "Ac",
                           depot = TRUE, dur = FALSE, f = TRUE, tlag = TRUE)
  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               "depot(adm = 1, target = Ac, Tlag = 2.1, p = 0.3, ka)")

  .ret <- .pk("; zero order absorption process for the doses of type 1, in compartment 1 with a delay Tlag of 1 and a duration Tk0
absorption(adm=1, cmt=1, Tlag=1, Tk0 = 2, p=1)")

  .ret2 <- emptyObj()
  .ret2$oral <- data.frame(adm = 1L, admd=1L, cmt = 1L, Tlag = "1", p = "1",
                           Tk0 = "2", ka = NA_character_, Ktr = NA_character_,
                           Mtt = NA_character_)
  .ret2$admd <- data.frame(adm = 1L, admd = 1L, cmt = 1L, target = NA_character_,
                           depot = FALSE, dur = TRUE, f = TRUE, tlag = TRUE)
  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               "oral(adm = 1, cmt = 1, Tlag = 1, p = 1, Tk0 = 2)")

  .ret <- .pk("; first order absorption process for the doses of type 1, in compartment 1 with a delay Tlag of 1 and a rate ka
absorption(type=1, cmt=1, Tlag=1, ka)")

  .ret2 <- emptyObj()
  .ret2$oral <- data.frame(adm = 1L, admd=1L, cmt = 1L, Tlag = "1",
                           p = NA_character_, Tk0 = NA_character_,
                           ka = "", Ktr = NA_character_, Mtt = NA_character_)
  .ret2$admd <- data.frame(adm = 1L, admd = 1L, cmt = 1L, target = NA_character_,
                           depot = TRUE, dur = FALSE, f = FALSE, tlag = TRUE)
  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               "oral(adm = 1, cmt = 1, Tlag = 1, ka)")

  .ret <- .pk("absorption(adm=1, cmt=1, Tk0, p=F1)
absorption(adm=1, cmt=1, ka, p=1-F1, Tlag=Tk0)
")

  .ret2 <- emptyObj()
  .ret2$oral <- data.frame(adm = c(1L, 1L), admd=c(1L, 2L), cmt = c(1L, 1L),
                           Tlag = c(NA, "Tk0"), p = c("F1", "1-F1"),
                           Tk0 = c("", NA), ka = c(NA, ""),
                           Ktr = c(NA_character_, NA_character_),
                           Mtt = c(NA_character_, NA_character_))
  .ret2$admd <- data.frame(adm = c(1L, 1L), admd = 1:2, cmt = c(1L, 1L),
                           target = c(NA_character_, NA_character_),
                           depot = c(FALSE, TRUE),
                           dur = c(TRUE, FALSE),
                           f = c(TRUE, TRUE), tlag = c(FALSE, TRUE))
  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               c("oral(adm = 1, cmt = 1, p = F1, Tk0)",
                 "oral(adm = 1, cmt = 1, Tlag = Tk0, p = 1-F1, ka)"))

  .ret <- .pk("absorption(adm=1, cmt=1, ka, p=F1)
absorption(adm=1, cmt=1, Tk0, p=1-F1)")

  .ret2 <- emptyObj()
  .ret2$oral <- data.frame(adm = c(1L, 1L), admd=1:2, cmt = c(1L, 1L),
                           Tlag = c(NA_character_, NA_character_), p = c("F1", "1-F1"),
                           Tk0 = c(NA, ""), ka = c("", NA),
                           Ktr = c(NA_character_, NA_character_), Mtt = c(NA_character_, NA_character_))
  .ret2$admd <- data.frame(adm = c(1L, 1L), admd = 1:2, cmt = c(1L, 1L),
                           target = c(NA_character_, NA_character_), depot = c(TRUE, FALSE),
                           dur = c(FALSE, TRUE), f = c(TRUE, TRUE), tlag = c(FALSE, FALSE))
  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               c("oral(adm = 1, cmt = 1, p = F1, ka)",
                 "oral(adm = 1, cmt = 1, p = 1-F1, Tk0)"))


  .ret <- .pk("compartment(cmt=1, amount=A1, concentration=Cc_zoa, volume=V)
absorption(cmt=1, adm=1, Tk0)
elimination(cmt=1, k=1)

compartment(cmt=2, amount=A2, concentration=Cc_foa, volume=V)
absorption(cmt=2, adm=1, Tlag, ka)
elimination(cmt=2, k=1)

compartment(cmt=3, amount=A3, concentration=Cc_foaT, volume=V)
absorption(cmt=3, adm=1, Tlag, ka, Ktr, Mtt)
elimination(cmt=3, k=1)
")

  .ret2 <- emptyObj()
  .ret2$compartment <- data.frame(cmt = 1:3, amount = c("A1", "A2", "A3"),
                                  volume = c("V", "V", "V"),
                                  concentration = c("Cc_zoa", "Cc_foa", "Cc_foaT"))
  .ret2$oral <- data.frame(adm = c(1L, 1L, 1L), admd=1:3, cmt = 1:3,
                           Tlag = c(NA, "", ""),
                           p = c(NA_character_, NA_character_, NA_character_),
                           Tk0 = c("", NA, NA), ka = c(NA, "", ""),
                           Ktr = c(NA, NA, ""), Mtt = c(NA, NA, ""))
  .ret2$elimination <- data.frame(cmt = 1:3,
                                  V = c(NA_character_, NA_character_, NA_character_),
                                  k = c("1", "1", "1"),
                                  Cl = c(NA_character_, NA_character_, NA_character_),
                                  Vm = c(NA_character_, NA_character_, NA_character_),
                                  Km = c(NA_character_, NA_character_, NA_character_))
  .ret2$admd <- data.frame(adm = c(1L, 1L, 1L),
                           admd = 1:3,
                           cmt = 1:3, target = c(NA_character_, NA_character_, NA_character_),
                           depot = c(FALSE, TRUE, TRUE), dur = c(TRUE, FALSE, FALSE),
                           f = c(FALSE, FALSE, FALSE),
                           tlag = c(FALSE, TRUE, TRUE))
  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               c("compartment(cmt = 1, amount = A1, volume = V, concentration = Cc_zoa)",
                 "oral(adm = 1, cmt = 1, Tk0)",
                 "elimination(cmt = 1, k = 1)",
                 "",
                 "compartment(cmt = 2, amount = A2, volume = V, concentration = Cc_foa)",
                 "oral(adm = 1, cmt = 2, Tlag, ka)",
                 "elimination(cmt = 2, k = 1)",
                 "",
                 "compartment(cmt = 3, amount = A3, volume = V, concentration = Cc_foaT)",
                 "oral(adm = 1, cmt = 3, Tlag, ka, Ktr, Mtt)", "elimination(cmt = 3, k = 1)"))

  .ret <- .pk("; intravenous bolus for the doses of type 1, in compartment 1 with a delay Tlag at 1
iv(adm=1, cmt=1, Tlag=1, p=1)")

  .ret2 <- emptyObj()
  .ret2$iv <- data.frame(adm = 1L, admd=1L, cmt = 1L, Tlag = "1", p = "1")
  .ret2$admd <- data.frame(adm = 1L, admd = 1L, cmt = 1L, target = NA_character_,
                           depot = FALSE, dur = FALSE, f = TRUE, tlag = TRUE)
  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               "iv(adm = 1, cmt = 1, Tlag = 1, p = 1)")

  .ret <- .pk("compartment(cmt=1, amount=A1, concentration=Cc_iv_bolus, volume=V)
iv(cmt=1, adm=1, Tlag)
elimination(cmt=1, k=1)

compartment(cmt=2, amount=A2, concentration=Cc_iv_inf, volume=V)
iv(cmt=2, adm=2, Tlag)
elimination(cmt=2, k=1)
")

  .ret2 <- emptyObj()
  .ret2$compartment <- data.frame(cmt = 1:2, amount = c("A1", "A2"),
                                  volume = c("V", "V"),
                                  concentration = c("Cc_iv_bolus", "Cc_iv_inf"))
  .ret2$iv <- data.frame(adm = 1:2, admd=c(1L, 1L), cmt = 1:2, Tlag = c("", ""),
                         p = c(NA_character_, NA_character_))
  .ret2$elimination <- data.frame(cmt = 1:2,
                                  V = c(NA_character_, NA_character_),
                                  k = c("1", "1"),
                                  Cl = c(NA_character_, NA_character_),
                                  Vm = c(NA_character_, NA_character_),
                                  Km = c(NA_character_, NA_character_))
  .ret2$admd <- data.frame(adm = 1:2, admd = c(1L, 1L), cmt = 1:2,
                           target = c(NA_character_, NA_character_), depot = c(FALSE, FALSE),
                           dur = c(FALSE, FALSE), f = c(FALSE, FALSE),
                           tlag = c(TRUE, TRUE))

  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               c("compartment(cmt = 1, amount = A1, volume = V, concentration = Cc_iv_bolus)",
                 "iv(adm = 1, cmt = 1, Tlag)",
                 "elimination(cmt = 1, k = 1)",
                 "",
                 "compartment(cmt = 2, amount = A2, volume = V, concentration = Cc_iv_inf)",
                 "iv(adm = 2, cmt = 2, Tlag)",
                 "elimination(cmt = 2, k = 1)"))

  .ret <- .pk("compartment(cmt=1, amount=A1, concentration=Cc_zoa, volume=V)
absorption(cmt=1, type=1, Tlag, Tk0, p=1)
elimination(cmt=1,k=.25)

compartment(cmt=2, amount=A2, concentration=Cc_foa, volume=V)
absorption(cmt=2, type=1, Tlag, ka, p=1)
elimination(cmt=2,k=.25)

compartment(cmt=3, amount=A3, concentration=Cc_mixed, volume=V)
absorption(cmt=3, type=1, Tlag, Tk0, p=F)
absorption(cmt=3, type=1, Tlag, ka, p=1-F)
elimination(cmt=3,k=.25)
")

  .ret2 <- emptyObj()
  .ret2$compartment <- data.frame(cmt = 1:3, amount = c("A1", "A2", "A3"),
                                  volume = c("V", "V", "V"),
                                  concentration = c("Cc_zoa", "Cc_foa", "Cc_mixed"))

  .ret2$oral <- data.frame(adm = c(1L, 1L, 1L, 1L),
                           admd=1:4,
                           cmt = c(1L, 2L, 3L, 3L),
                           Tlag = c("", "", "", ""),
                           p = c("1", "1", "F", "1-F"),
                           Tk0 = c("", NA, "", NA), ka = c(NA, "", NA, ""),
                           Ktr = c(NA_character_, NA_character_, NA_character_, NA_character_),
                           Mtt = c(NA_character_, NA_character_, NA_character_, NA_character_))

  .ret2$elimination <- data.frame(cmt = 1:3,
                                  V = c(NA_character_, NA_character_, NA_character_),
                                  k = c(".25", ".25", ".25"),
                                  Cl = c(NA_character_, NA_character_, NA_character_),
                                  Vm = c(NA_character_, NA_character_, NA_character_),
                                  Km = c(NA_character_, NA_character_, NA_character_))
  .ret2$admd <- data.frame(adm = c(1L, 1L, 1L, 1L), admd = 1:4,
                           cmt = c(1L, 2L, 3L, 3L),
                           target = c(NA_character_, NA_character_, NA_character_, NA_character_),
                           depot = c(FALSE, TRUE, FALSE, TRUE),
                           dur = c(TRUE, FALSE, TRUE, FALSE),
                           f = c(TRUE, TRUE, TRUE, TRUE),
                           tlag = c(TRUE, TRUE, TRUE, TRUE))
  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               c("compartment(cmt = 1, amount = A1, volume = V, concentration = Cc_zoa)",
                 "oral(adm = 1, cmt = 1, Tlag, p = 1, Tk0)",
                 "elimination(cmt = 1, k = .25)",
                 "",
                 "compartment(cmt = 2, amount = A2, volume = V, concentration = Cc_foa)",
                 "oral(adm = 1, cmt = 2, Tlag, p = 1, ka)",
                 "elimination(cmt = 2, k = .25)",
                 "",
                 "compartment(cmt = 3, amount = A3, volume = V, concentration = Cc_mixed)",
                 "oral(adm = 1, cmt = 3, Tlag, p = F, Tk0)", "oral(adm = 1, cmt = 3, Tlag, p = 1-F, ka)",
                 "elimination(cmt = 3, k = .25)" ))

  .ret <- .pk("empty(adm=1, target=Ap)")

  .ret2 <- emptyObj()
  .ret2$empty <- data.frame(adm = 1L, admd=1L, target = "Ap")
  .ret2$admd <- data.frame(adm = 1L, admd = 1L, cmt = NA_integer_,
                           target = "Ap", depot = FALSE, dur = FALSE,
                           f = FALSE, tlag = FALSE)

  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               "empty(adm = 1, target = Ap)")

  .ret <- .pk("depot(adm=1, target=Ap, p=-Ap/amtDose)")

  .ret2 <- emptyObj()
  .ret2$depot <- data.frame(adm = 1L, admd=1L, target = "Ap",
                            Tlag = NA_character_, p = "-Ap/amtDose",
                            Tk0 = NA_character_, ka = NA_character_,
                            Ktr = NA_character_, Mtt = NA_character_)
  .ret2$admd <- data.frame(adm = 1L, admd = 1L, cmt = NA_integer_, target = "Ap",
                     depot = FALSE, dur = FALSE, f = TRUE, tlag = FALSE)
  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               "depot(adm = 1, target = Ap, p = -Ap/amtDose)")

  .ret <- .pk("reset(adm=2, target=Ac)")

  .ret2 <- emptyObj()
  .ret2$reset <- data.frame(adm = 2L, admd=1L, target = "Ac")
  .ret2$admd <- data.frame(adm = 2L, admd = 1L, cmt = NA_integer_, target = "Ac", depot = FALSE, dur = FALSE, f = FALSE, tlag = FALSE)
  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               "reset(adm = 2, target = Ac)")

  .ret <- .pk("empty(adm=3, target=Ac)
empty(adm=3, target=Ap)")

  .ret2 <- emptyObj()
  .ret2$empty <- data.frame(adm = c(3L, 3L), admd=c(1L, 2L), target = c("Ac", "Ap"))
  .ret2$admd <- data.frame(adm = c(3L, 3L), admd = 1:2, cmt = c(NA_integer_, NA_integer_),
                           target = c("Ac", "Ap"), depot = c(FALSE, FALSE), dur = c(FALSE, FALSE),
                           f = c(FALSE, FALSE), tlag = c(FALSE, FALSE))
  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               c("empty(adm = 3, target = Ac)",
                 "empty(adm = 3, target = Ap)"))

  .ret <- .pk("reset(adm=3, target=all)")

  .ret2 <- emptyObj()
  .ret2$reset <- data.frame(adm = 3L, admd=1L, target = "all")
  .ret2$admd <- data.frame(adm = 3L, admd = 1L, cmt = NA_integer_,
                           target = "all", depot = FALSE, dur = FALSE,
                           f = FALSE, tlag = FALSE)
  expect_equal(.ret, .ret2)

  expect_equal(as.character(.ret),
               "reset(adm = 3, target = all)")

  expect_true(is.list(as.list(.ret)))

  expect_error(.pk("Cc = pkmodel(Tlag, ka, k, k12, k21)"))

  expect_error(.pk("{Cc, Ce} = pkmodel(Tlag, ka, p, V, Vm, Km, k12, k21, k13, k31, ke0, Tk0)"))

  expect_error(.pk("Cc = pkmodel(ka, Cl, V, Vm, Km)"))

  expect_error(.pk("{Cc, Ce} = pkmodel(Tlag, ka, p, V, Vm, Km, k12, k21, k13, k31)"))

  expect_error(.pk("Cc = pkmodel(Tlag, ka, p, V, Vm, Km, k12, k21, k13, k31, ke0)"))

  expect_error(.pk("peripheral(k2_13, k13_2, k14_4)"))
})
