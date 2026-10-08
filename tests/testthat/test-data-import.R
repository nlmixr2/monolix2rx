
pkgTheo <- system.file("theo", package="monolix2rx")

test_that("single endpoint import", {

  rx <- monolix2rx(file.path(pkgTheo, "theophylline_project.mlxtran"))
  expect_true(inherits(rx, "rxUi"))
  if (requireNamespace("vdiffr", quietly=TRUE)) {
    vdiffr::expect_doppelganger("single-endpoint-theo",
                                plot(rx))
    vdiffr::expect_doppelganger("single-endpoint-theo-p1",
                                plot(rx, page=1))
    vdiffr::expect_doppelganger("single-endpoint-theo-pall",
                                plot(rx, page=TRUE))
  }
})

test_that("multiple endpoint import", {

  rx <- monolix2rx(file.path(pkgTheo, "parent_metabolite_project.mlxtran"))
  expect_true(inherits(rx, "rxUi"))
  if (requireNamespace("vdiffr", quietly=TRUE)) {
    vdiffr::expect_doppelganger("multiple-endpoint-theo",
                                plot(rx))
    vdiffr::expect_doppelganger("multiple-endpoint-theo-p1",
                                plot(rx, page=1))
    vdiffr::expect_doppelganger("multiple-endpoint-theo-pall",
                                plot(rx, page=TRUE))
  }
})

test_that("dose routing follows the administration table", {
  .admd <- function(adm, admd=1L, cmt=1L, rxCmt="depot", dur=FALSE, transit=FALSE) {
    data.frame(adm=adm, admd=admd, cmt=cmt, target=NA_character_, depot=FALSE,
               dur=dur, f=FALSE, tlag=FALSE, transit=transit, rxCmt=rxCmt)
  }
  .d <- data.frame(id=1L, time=c(0, 1, 48, 49), amt=c(100, NA, 50, NA),
                   dv=c(NA, 1, NA, 2))
  # no adm column: every dose is adm 1 (data$adm partially matches admd)
  .r <- .dataConvertAdm(.d, .admd(1L))
  expect_equal(.r$cmt, c("depot", NA, "depot", NA))
  # each adm to its own compartment, by name
  .d$adm <- c(1L, 0L, 2L, 0L)
  .r <- .dataConvertAdm(.d, rbind(.admd(1L), .admd(2L, cmt=1L, rxCmt="Ac")))
  expect_equal(.r$cmt, c("depot", NA, "Ac", NA))
  # Tk0: modeled duration
  .r <- .dataConvertAdm(.d, rbind(.admd(1L, rxCmt="central", dur=TRUE), .admd(2L, rxCmt="central")))
  expect_equal(.r$rate, c(-2, NA, NA, NA))
  # transit: evid 7 so the dose only drives transit()
  .r <- .dataConvertAdm(.d, .admd(1L, transit=TRUE))
  expect_equal(.r$evid, c(7L, 0L, 1L, 0L))
  # a second macro on the same adm copies the dose row
  .r <- .dataConvertAdm(.d[1:2, ], rbind(.admd(1L), .admd(1L, admd=2L, rxCmt="central", dur=TRUE)))
  expect_equal(.r$cmt, c("depot", NA, "central"))
  expect_equal(.r$rate, c(NA, NA, -2))
})

test_that("iv() on a second route doses its own compartment", {
  .pk <- .pk("compartment(cmt=1, amount=Ac)\noral(adm=1, cmt=1, ka)\niv(adm=2, cmt=1)\nperipheral(k12, k21)\nelimination(cmt=1, k)")
  .admd <- .equation("Cc = Ac/V", .pk)$admd
  expect_equal(.admd$rxCmt, c("Acd", "Ac"))
  .t <- .equation("Cc = pkmodel(Mtt, Ktr, ka, V, Cl)")$admd
  expect_true(.t$transit)
})

test_that("a second macro on one adm copies every dose", {
  .admd <- data.frame(adm=1L, admd=1:2, cmt=1L, target=NA_character_, depot=c(TRUE, FALSE),
                      dur=c(FALSE, TRUE), f=FALSE, tlag=FALSE, transit=FALSE,
                      rxCmt=c("depot", "central"))
  .d <- data.frame(id=1L, time=c(0, 1, 12, 13), amt=c(100, NA, 100, NA), dv=c(NA, 1, NA, 2))
  .r <- .dataConvertAdm(.d, .admd)
  expect_equal(nrow(.r), 6L)
  expect_equal(.r$cmt[5:6], c("central", "central"))
  expect_equal(.r$rate, c(NA, NA, NA, NA, -2, -2))
})

test_that("routing only touches dose rows and copies unmodified doses", {
  .admd <- function(admd=1L, rxCmt="depot", dur=FALSE, transit=FALSE) {
    data.frame(adm=1L, admd=admd, cmt=1L, target=NA_character_, depot=FALSE,
               dur=dur, f=FALSE, tlag=FALSE, transit=transit, rxCmt=rxCmt)
  }
  # transit first, Tk0 second: the Tk0 copy is a plain dose with rate -2
  .d <- data.frame(id=1L, time=c(0, 1), amt=c(100, NA), dv=c(NA, 1))
  .r <- .dataConvertAdm(.d, rbind(.admd(transit=TRUE), .admd(2L, "central", dur=TRUE)))
  expect_equal(.r$evid, c(7L, 0L, 1L))
  expect_equal(.r$rate, c(NA, NA, -2))
  # Tk0 first, first order second: the depot copy has no rate
  .r <- .dataConvertAdm(.d, rbind(.admd(rxCmt="central", dur=TRUE), .admd(2L, "depot")))
  expect_equal(.r$rate, c(-2, NA, NA))
  # AMT=0 observations without an ADM column are not doses
  .d0 <- data.frame(id=1L, time=c(0, 1, 2), amt=c(100, 0, 0), dv=c(NA, 1, 2))
  .r <- .dataConvertAdm(.d0, .admd(transit=TRUE))
  expect_equal(.r$evid, c(7L, 0L, 0L))
  expect_equal(.r$cmt, c("depot", NA, NA))
  # ADM=1 on every row: observations get neither a rate nor a compartment
  .d1 <- data.frame(id=1L, time=c(0, 1), amt=c(100, NA), adm=1L, dv=c(NA, 1))
  .r <- .dataConvertAdm(.d1, .admd(rxCmt="central", dur=TRUE))
  expect_equal(.r$rate, c(-2, NA))
  expect_equal(.r$cmt, c("central", NA))
  # a data infusion time wins over Tk0
  .r <- .dataConvertAdm(transform(.d, dur=c(3, NA)), .admd(rxCmt="central", dur=TRUE))
  expect_equal(.r$rate, c(NA_real_, NA))
  # EVID=4 transit dose: a reset, then the evid 7 dose
  .d4 <- data.frame(id=1L, time=c(0, 1, 6, 7), amt=c(100, NA, 100, NA),
                    evid=c(1L, 0L, 4L, 0L), dv=c(NA, 1, NA, 2))
  .r <- .dataConvertAdm(.d4, .admd(transit=TRUE))
  expect_equal(.r$evid, c(7L, 0L, 3L, 7L, 0L))
  expect_equal(.r$time, c(0, 1, 6, 6, 7))
  expect_null(.r$rxTransitReset)
})

test_that("MDV=1 rows are not doses", {
  .d <- data.frame(id=1L, time=c(0, 1, 2), amt=c(100, NA, NA), mdv=c(1L, 1L, 0L), dv=c(NA, 999, 1))
  expect_equal(.dataEvid(.d)$evid, c(1L, 2L, 0L))
})

test_that("a depot named by target= keeps its depot compartment", {
  .pk <- .pk("compartment(cmt=1, amount=Ac)\ndepot(target=Ac, ka)\nelimination(cmt=1, k)")
  expect_equal(.equation("Cc = Ac/V", .pk)$admd$rxCmt, "Acd")
})

test_that("transit reset rows drop addl/ii/ss and NA evid doses are routed", {
  .admd <- data.frame(adm=1L, admd=1L, cmt=1L, target=NA_character_, depot=TRUE,
                      dur=FALSE, f=FALSE, tlag=FALSE, transit=TRUE, rxCmt="depot")
  .d <- data.frame(id=1L, time=c(0, 1), amt=c(100, NA), evid=c(4L, 0L),
                   ii=c(12, 0), addl=c(2L, 0L), dv=c(NA, 1))
  .r <- .dataConvertAdm(.d, .admd)
  expect_equal(.r$evid, c(3L, 7L, 0L))
  expect_equal(.r$addl, c(0L, 2L, 0L))
  expect_equal(.r$ii, c(0, 12, 0))
  .d <- data.frame(id=1L, time=c(0, 1), amt=c(100, NA), evid=c(NA, 0L), dv=c(NA, 1))
  .r <- .dataConvertAdm(.d, .admd)
  expect_equal(.r$cmt, c("depot", NA))
  expect_equal(.r$evid, c(7L, 0L))
})

test_that("an EVID column with NA on a dose is filled", {
  .d <- data.frame(id=1L, time=c(0, 1), amt=c(100, NA), evid=c(NA, 0L), dv=c(NA, 1))
  expect_equal(.dataEvid(.d)$evid, c(1L, 0L))
})

test_that("ignored columns named like rxode2 event columns are dropped", {
  .c <- .content("ID = {use=identifier}\nTIME = {use=time}\nAMT = {use=amount}\nSS = {use=ignore}\nDose = {use=ignore}\nNOTE = {use=ignore}\nSEX = {use=covariate, type=categorical}\nDV = {use=observation, name=y, type=continuous}")
  .d <- data.frame(ID=1L, TIME=0, AMT=100, SS=1L, Dose=10, NOTE="a", II=24, SEX=1L, DV=NA)
  expect_message(.r <- .dataDropIgnoredEvent(.d, .c), "SS', 'Dose'")
  # undeclared columns are kept: [CONTENT] does not list every declared column
  expect_equal(names(.r), c("ID", "TIME", "AMT", "NOTE", "II", "SEX", "DV"))
  # an ignored column in lowercased user data is also dropped
  .d <- data.frame(id=1L, time=0, amt=100, ss=1L, dv=NA)
  expect_message(.r <- .dataDropIgnoredEvent(.d, .c), "'ss'")
  expect_equal(names(.r), c("id", "time", "amt", "dv"))
  # an ignoredline flag is not an ignored column
  .c <- .content("ID = {use=identifier}\nTIME = {use=time}\nMDV = {use=ignoredline}\nEVID = {use=ignore}\nDV = {use=observation, name=y, type=continuous}")
  expect_equal(.c$ignore, c("MDV", "EVID"))
  expect_equal(.c$ignoreLine, "MDV")
  expect_equal(.content(paste(as.character(.c), collapse="\n"))$ignoreLine, "MDV")
  .d <- data.frame(ID=1L, TIME=0, MDV=1L, EVID=1L, DV=NA)
  expect_equal(names(suppressMessages(.dataDropIgnoredEvent(.d, .c))), c("ID", "TIME", "MDV", "DV"))
  # other ignoredline flags named like event columns are dropped
  .c <- .content("ID = {use=identifier}\nTIME = {use=time}\nE = {use=eventidentifier}\nEVID = {use=ignoredline}\nDV = {use=observation, name=y, type=continuous}")
  .d <- data.frame(ID=1L, TIME=0, E=0L, EVID=1L, DV=1)
  expect_equal(names(suppressMessages(.dataDropIgnoredEvent(.d, .c))), c("ID", "TIME", "E", "DV"))
  # a used column is kept when an ignored one differs only by case
  .c <- .content("ID = {use=identifier}\nTIME = {use=time}\nAMT = {use=amount}\namt = {use=ignore}\nDV = {use=observation, name=y, type=continuous}")
  .d <- data.frame(ID=1L, TIME=0, AMT=100, amt=5, DV=NA)
  expect_equal(names(suppressMessages(.dataDropIgnoredEvent(.d, .c))), c("ID", "TIME", "AMT", "DV"))
  # nested occasion columns are kept
  .c <- .content("ID = {use=identifier}\nTIME = {use=time}\nOCC = {use=occasion}\nOCC2 = {use=occasion}\nDV = {use=observation, name=y, type=continuous}")
  .d <- data.frame(ID=1L, TIME=0, OCC=1L, OCC2=1L, DV=NA)
  expect_identical(.dataDropIgnoredEvent(.d, .c), .d)
})
