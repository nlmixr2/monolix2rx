test_that(".pk2rxAmt", {

  pk <- .pk("compartment(cmt=1, amount=Ac, concentration=Cc, volume=V)
iv(cmt=1, type=1)
elimination(cmt=1, k)")

  env <- new.env(parent=emptyenv())
  env$name <- vector("list", 1)
  env$lhs <- vector("list", 1)
  env$rhs <- vector("list", 1)
  env$extra <- vector("list", 1)

  env$name[[1]] <- "test"

  expect_error(.pk2rxAmt(env, pk, 1, amount="me"))

  env$name <- vector("list", 1)

  expect_equal(.pk2rxAmt(env, pk, 1, amount="me"), "me")

})

test_that(".pk2rxCompartment", {

  pk <- .pk("compartment(cmt=1, amount=Ac, concentration=Cc, volume=V)
iv(cmt=1, type=1)
elimination(cmt=1, k)")

  pk$compartment <- rbind(pk$compartment, pk$compartment)

  env <- new.env(parent=emptyenv())
  env$name <- vector("list", 1)
  env$lhs <- vector("list", 1)
  env$rhs <- vector("list", 1)
  env$extra <- vector("list", 1)

  expect_error(.pk2rxCompartment(env, pk, 1))

})

test_that(".pk2rxPeriph with large constants", {

  .env <- new.env(parent=emptyenv())
  .env$endLines <- character(0)
  .env$depotPostfix <- "d"

  .env$name      <- vector("list", 20)
  .env$lhs       <- vector("list", 20)
  .env$rhs       <- vector("list", 20)
  .env$lhsDepot  <- vector("list", 20)
  .env$rhsDepot  <- vector("list", 20)
  .env$lhsEffect <- vector("list", 20)
  .env$rhsEffect <- vector("list", 20)
  .env$dur       <- vector("list", 20)
  .env$f         <- vector("list", 20)
  .env$tlag      <- vector("list", 20)
  .env$fDepot    <- vector("list", 20)
  .env$tlagDepot <- vector("list", 20)
  .env$extra     <- vector("list", 20)
  .env$conc      <- vector("list", 20)
  .env$cmtDefault <- "cmt"


  pk <- .pk("compartment(cmt=1, amount=Ac, concentration=Cc, volume=V)
iv(cmt=1, type=1)
elimination(cmt=1, k)
peripheral(k10_20, k20_10)
")

  .pk2rxPeriph(.env, pk, 20)

  expect_equal(.env$rhs[[10]],
               " - k10_20*cmt10 + k20_10*cmt20")

  expect_equal(.env$rhs[[20]],
               " + k10_20*cmt10 - k20_10*cmt20")

})


test_that(".pk2rxAdmVal()", {

  pk <- .pk("compartment(cmt=1, amount=depot)\ndepot(type=1, target=depot, Tlag=tl, p=f)")

  pk$depot$target <- NA_character_

  expect_error(.pk2rxAdmVal(pk, pk$depot, "f", "f"))

  pk <- .pk("compartment(cmt=1, amount=depot)\ndepot(type=1, target=depot, Tlag=tl, p=f)\ndepot(type=1, target=depot,Tlag=tl1, p=1-f)")

  expect_equal(.pk2rxAdmVal(pk, pk$depot[1, ], "f", "f"),
               "+(ADMD==1)*(f)")

  pk <- .pk("compartment(cmt=1, amount=depot)\ndepot(type=1, target=depot, Tlag=tl, p=f)\ndepot(type=2, target=depot,Tlag=tl1, p=1-f)")

  expect_equal(.pk2rxAdmVal(pk, pk$depot[1, ], "f", "f"),
               "+(ADM==1)*(f)")


  pk <- .pk("compartment(cmt=1, amount=depot)\ndepot(type=1, target=depot, Tlag=tl, p=f1)\ndepot(type=1, target=depot,Tlag=tl1, p=f2)\ndepot(type=2, target=depot,Tlag=tl1, p=1-f1-f2)\ndepot(type=2, target=depot,Tlag=tl1, p=w)")

  expect_equal(.pk2rxAdmVal(pk, pk$depot[1, ], "f", "f"),
               "+(ADM==1 && ADMD==1)*(f)")

  pk$admd <- rbind(pk$admd, pk$admd[1, ])

  expect_error(.pk2rxAdmVal(pk, pk$depot[1, ], "f", "f"))

})


test_that(".pk2rxGetVar() null case", {
  expect_equal(.pk2rxGetVar(list(test=NULL), "test"), NA_character_)
})


test_that(".pk2rxEffect() duplicate effects and missing concentrations", {

  pk <- .pk("; Define an effect compartment linked to the base compartment 1,
; with a transfer rate ke0 to the effect compartment,
; with Ce as concentration's name
effect(cmt=1, ke0, concentration=Ce)
effect(cmt=1, ke0=ke02, concentration=Ce2)
")

  env <- new.env(parent=emptyenv())
  env$name <- vector("list", 1)
  env$lhs <- vector("list", 1)
  env$rhs <- vector("list", 1)
  env$extra <- vector("list", 1)
  env$lhsEffect <- vector("list", 1)
  env$rhsEffect <- vector("list", 1)

  expect_error(.pk2rxEffect(env, pk, 1))

  pk <- .pk("compartment(cmt=1, amount=Ac)
effect(cmt=1, ke0, concentration=Ce)")

  env <- new.env(parent=emptyenv())
  env$name <- vector("list", 1)
  env$lhs <- vector("list", 1)
  env$rhs <- vector("list", 1)
  env$extra <- vector("list", 1)
  env$lhsEffect <- vector("list", 1)
  env$rhsEffect <- vector("list", 1)

  expect_error(.pk2rxEffect(env, pk, 1), "concentration")

})


test_that(".pk2rxIv()",{

  pk <- .pk("compartment(cmt=1, amount=Ac, concentration=Cc, volume=V)
elimination(cmt=1, k)
iv(cmt=1, Tlag=alag, p=f2)
effect(cmt=1, ke0, concentration=Ce)")

  env <- new.env(parent=emptyenv())
  env$name <- vector("list", 1)
  env$tlag <- vector("list", 1)
  env$f <- vector("list", 1)

  .pk2rxIv(env, pk, 1)

  expect_equal(env$tlag[[1]], "alag(Ac) <- alag")
  expect_equal(env$f[[1]], "f(Ac) <- f2")

})

test_that(".pk2rxElimination()", {

  pk <- .pk("elimination(cmt=1, k)")

  env <- new.env(parent=emptyenv())
  env$name <- vector("list", 1)
  env$rhs <- list("")

  # doesn't need a volume
  expect_error(.pk2rxElimination(env, pk, 1),  NA)

  pk <- .pk("elimination(cmt=1, Cl)")

  env <- new.env(parent=emptyenv())
  env$name <- vector("list", 1)
  env$rhs <- list("")

  # needs a volume
  expect_error(.pk2rxElimination(env, pk, 1), "type")

  pk <- .pk("elimination(cmt=1, Vm, Km)")

  env <- new.env(parent=emptyenv())
  env$name <- vector("list", 1)
  env$rhs <- list("")

  #needs a volume
  expect_error(.pk2rxElimination(env, pk, 1), "type")

  pk <- .pk("compartment(cmt=1, amount=Ac)\nelimination(cmt=1, Cl)")

  env <- new.env(parent=emptyenv())
  env$name <- vector("list", 1)
  env$rhs <- list("")

  #needs a volume
  # now uses default volume of
  #expect_error(.pk2rxElimination(env, pk, 1), "type")

  pk <- .pk("compartment(cmt=1, amount=Ac,volume=Vc)\nelimination(cmt=1, Cl)")

  env <- new.env(parent=emptyenv())
  env$name <- vector("list", 1)
  env$rhs <- list("")

  .pk2rxElimination(env, pk, 1)

  expect_equal(env$rhs[[1]], " - Cl/Vc*Ac")

  expect_error(.pk("elimination(cmt=1, k, volume=v2)"), 'volume')

})


test_that("pk2rxTransfer()", {

  .env <- new.env(parent=emptyenv())
  .env$endLines <- character(0)
  .env$depotPostfix <- "d"

  .env$name      <- vector("list", 2)
  .env$lhs       <- vector("list", 2)
  .env$rhs       <- vector("list", 2)
  .env$lhsDepot  <- vector("list", 2)
  .env$rhsDepot  <- vector("list", 2)
  .env$lhsEffect <- vector("list", 2)
  .env$rhsEffect <- vector("list", 2)
  .env$dur       <- vector("list", 2)
  .env$f         <- vector("list", 2)
  .env$tlag      <- vector("list", 2)
  .env$fDepot    <- vector("list", 2)
  .env$tlagDepot <- vector("list", 2)
  .env$extra     <- vector("list", 2)
  .env$conc      <- vector("list", 2)
  .env$cmtDefault <- "cmt"

  .env$rhs[[1]] <- ""
  .env$rhs[[2]] <- ""

  pk <- .pk("transfer(from=1, to=2, kt)")

  .pk2rxTransfer(.env, pk, 2)

  expect_equal(.env$rhs[[1]], " - kt*central")
  expect_equal(.env$rhs[[2]], " + kt*central")

})

test_that("pk2rxDepot() tests", {

  .env <- new.env(parent=emptyenv())
  .env$endLines <- character(0)
  .env$depotPostfix <- "d"
  .env$lhsDepot  <- list()
  .env$rhsDepot  <- list()
  .env$extraDepot<- list()
  .env$dur       <- list()
  .env$f         <- list()
  .env$tlag      <- list()
  .env$fDepot    <- list()
  .env$tlagDepot <- list()

  pk <- .pk("depot(target=Ac, ka, Mtt, Ktr, p, Tlag)
depot(target=Ac2, ka=ka2, p=p2, Tlag=tlag2)
depot(target=Ac3, p=p3, Tlag=tlag3)")

  .pk2rxDepot(.env, pk)

  expect_equal(as.list(.env),
               list(dur = list(), endLines = character(0),
                    extraDepot = list(Ac = " + ka*Acd",
                                      Ac2 = " + ka2*Ac2d"),
                    lhsDepot = list(Ac = "", Ac2 = ""),
                    tlag = list(Ac3 = "alag(Ac3) <- tlag3"),
                    f = list(Ac3 = "f(Ac3) <- p3"),
                    tlagDepot = list(Ac = "alag(Acd) <- Tlag",
                                     Ac2 = "alag(Ac2d) <- tlag2"),
                    fDepot = list(Ac2 = "f(Ac2d) <- p2"),
                    depotPostfix = "d",
                    rhsDepot = list(Ac = " - ka*Acd + transit(Mtt*Ktr-1, Mtt, p)",
                                    Ac2 = " - ka2*Ac2d")))

  .env <- new.env(parent=emptyenv())
  .env$endLines <- character(0)
  .env$depotPostfix <- "d"
  .env$lhsDepot  <- list()
  .env$rhsDepot  <- list()
  .env$extraDepot<- list()
  .env$dur       <- list()
  .env$f         <- list()
  .env$tlag      <- list()
  .env$fDepot    <- list()
  .env$tlagDepot <- list()


  pk <- .pk("depot(target=Ac, Tk0=dur1)")

  .pk2rxDepot(.env, pk)

  expect_equal(as.list(.env),
               list(dur = list(Ac = "dur(Ac) <- dur1"),
                    endLines = character(0),
                    extraDepot = list(),
                    lhsDepot = list(),
                    tlag = list(),
                    f = list(),
                    tlagDepot = list(),
                    fDepot = list(),
                    depotPostfix = "d",
                    rhsDepot = list()))

})

test_that("pkmodel translation", {
  .pkm <- .pk("{Cc,Ce} = pkmodel(Tlag, Tk0, k=Cl/Vol, V=Vol, ke0)")
  expect_error(.pk2rx(.pkm), NA)
})
