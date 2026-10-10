
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

test_that("empty() and reset() administrations are not doses", {
  .pk <- .pk("compartment(cmt=1, amount=Ac)\noral(adm=1, cmt=1, ka)\nempty(adm=2, target=Ac)\nreset(adm=3)\nelimination(cmt=1, k)")
  .admd <- .equation("Cc = Ac/V", .pk)$admd
  .d <- data.frame(id=1L, time=c(0, 1, 2, 3, 4), amt=c(100, NA, 1, 1, NA),
                   adm=c(1L, 0L, 2L, 3L, 0L), dv=c(NA, 1, NA, NA, 2))
  .r <- .dataConvertAdm(.d, .admd, .pk)
  expect_equal(.r$evid, c(NA, NA, 5L, 3L, NA))
  expect_equal(.r$amt, c(100, NA, 0, NA, NA))
  expect_equal(.r$cmt, c("Acd", NA, "Ac", NA, NA))
  # without the macros they are still routed as doses
  expect_equal(.dataConvertAdm(.d, .admd)$amt, .d$amt)
  # a dose and an empty on the same adm: the empty is a copy of the dose row
  .pk2 <- .pk("compartment(cmt=1, amount=Ac)\niv(adm=1, cmt=1)\nempty(adm=1, target=Ac)\nelimination(cmt=1, k)")
  .r <- .dataConvertAdm(.d[1:2, ], .equation("Cc = Ac/V", .pk2)$admd, .pk2)
  expect_equal(.r$evid, c(1L, 0L, 5L))
  expect_equal(.r$amt, c(100, NA, 0))
  expect_equal(.r$cmt, c("Ac", NA, "Ac"))
})

test_that("a dose lag next to empty()/reset() applies to the dose's adm only", {
  .rx <- function(txt) .equation("Cc = Ac/V", .pk(txt))$rx
  expect_true("alag(Acd) <- +(ADM==1)*(Tlag)" %in%
                .rx("compartment(cmt=1, amount=Ac)\noral(adm=1, cmt=1, ka, Tlag)\nreset(adm=2)\nelimination(cmt=1, k)"))
  expect_true("alag(Ac) <- +(ADM==1)*(Tlag)" %in%
                .rx("compartment(cmt=1, amount=Ac)\niv(adm=1, cmt=1, Tlag)\nempty(adm=2, target=Ac)\nelimination(cmt=1, k)"))
  expect_true("alag(Acd) <- Tlag" %in%
                .rx("compartment(cmt=1, amount=Ac)\noral(adm=1, cmt=1, ka, Tlag)\nelimination(cmt=1, k)"))
  expect_warning(.equation("compartment(cmt=1, amount=Ac)\niv(cmt=1)\nempty(adm=2, target=Ac)\nCc = Ac"),
                 "only translated in the PK: block")
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
  # its flagged lines (doses or observations) and the flag are dropped
  .d <- data.frame(ID=1L, TIME=c(0, 0, 1, 2), AMT=c(100, 50, NA, NA), MDV=c(0L, 1L, 1L, NA),
                   EVID=1L, DV=c(NA, NA, 9, 1))
  expect_message(.r <- .dataDropIgnoredLines(.d, .c), "2 line")
  expect_equal(.r$TIME, c(0, 2))
  expect_equal(names(.r), c("ID", "TIME", "AMT", "EVID", "DV"))
  expect_identical(.dataDropIgnoredLines(.d[1, ], .c), .d[1, names(.r)])
  # lowercased user data and logical flags
  .l <- stats::setNames(.d, tolower(names(.d)))
  .l$mdv <- c(FALSE, TRUE, TRUE, NA)
  expect_equal(suppressMessages(.dataDropIgnoredLines(.l, .c))$time, c(0, 2))
  # a used column is kept when an ignored one differs only by case
  .c <- .content("ID = {use=identifier}\nTIME = {use=time}\nAMT = {use=amount}\namt = {use=ignore}\nDV = {use=observation, name=y, type=continuous}")
  .d <- data.frame(ID=1L, TIME=0, AMT=100, amt=5, DV=NA)
  expect_equal(names(suppressMessages(.dataDropIgnoredEvent(.d, .c))), c("ID", "TIME", "AMT", "DV"))
  # nested occasion columns are kept
  .c <- .content("ID = {use=identifier}\nTIME = {use=time}\nOCC = {use=occasion}\nOCC2 = {use=occasion}\nDV = {use=observation, name=y, type=continuous}")
  .d <- data.frame(ID=1L, TIME=0, OCC=1L, OCC2=1L, DV=NA)
  expect_identical(.dataDropIgnoredEvent(.d, .c), .d)
})

test_that("nested occasion columns become occ and a combined occ2", {
  .c <- .content("ID = {use=identifier}\nTIME = {use=time}\nP1 = {use=occasion}\nP2 = {use=occasion}\nDV = {use=observation, name=y, type=continuous}")
  expect_equal(.c$occ, c("P1", "P2"))
  expect_equal(.content(paste(as.character(.c), collapse="\n"))$occ, c("P1", "P2"))
  .d <- data.frame(ID=1L, TIME=1:6, P1=c(1, 1, 2, 2, 2, 3), P2=c(1, 2, 1, 1, 2, 1), DV=1)
  .r <- .dataRenameOcc(.d, .c)
  expect_equal(names(.r), c("ID", "TIME", "occ", "occ2", "DV"))
  expect_equal(.r$occ, .d$P1)
  # the inner level is numbered per (P1, P2) combination
  expect_equal(.r$occ2, c(1, 2, 3, 3, 4, 5))
  # one occasion column is left to the use=occasion rename
  .c1 <- .content("ID = {use=identifier}\nTIME = {use=time}\nOCC = {use=occasion}")
  expect_equal(.c1$occ, "OCC")
  expect_identical(.dataRenameOcc(.d, .c1), .d)
  expect_error(.dataRenameOcc(.d[, -4], .c), "P2")
  # lowercased user data, missing occasions and clashing columns
  .l <- stats::setNames(.d, tolower(names(.d)))
  expect_equal(.dataRenameOcc(.l, .c)$occ2, c(1, 2, 3, 3, 4, 5))
  .n <- .d
  .n$P1[2] <- NA
  .n$P2[5] <- NA
  expect_equal(.dataRenameOcc(.n, .c)$occ2, c(1, NA, 2, 2, NA, 3))
  # an exact match wins over another column differing only by case
  .mx <- data.frame(ID=1L, p1=9, P1=c(1, 2), p2=c(1, 1))
  expect_equal(.dataRenameOcc(.mx, .c)$occ, c(1, 2))
  expect_error(.dataRenameOcc(data.frame(p1=1, p2=1), .content("P1 = {use=occasion}\np1 = {use=occasion}")),
               "same data column")
  # an unused column named like a translated one is dropped, a used one stops
  .x <- .d
  .x$OCC <- 0
  expect_message(.r <- .dataRenameOcc(.x, .c), "dropped unused")
  expect_equal(names(.r), c("ID", "TIME", "occ", "occ2", "DV"))
  .cx <- .content("ID = {use=identifier}\nTIME = {use=time}\nP1 = {use=occasion}\nP2 = {use=occasion}\nOCC2 = {use=covariate, type=continuous}")
  .x$OCC <- NULL
  .x$OCC2 <- 0
  expect_error(.dataRenameOcc(.x, .cx), "clash with the translated occasion")
  .m <- list(DATAFILE=list(CONTENT=list(CONTENT=.c)),
             MODEL=list(LONGITUDINAL=list(LONGITUDINAL=.longitudinal("input = {ka, occ2}\nocc2 = {use=regressor}"))))
  .m$DATAFILE$CONTENT$CONTENT$reg <- "R1"
  expect_error(.dataRenameRegressors(data.frame(ID=1, R1=2), .m), "clash with a translated data column")
  expect_equal(.def2iniRenameOcc(c("id", "id*occ1", "id*occ1*occ2")), c("id", "occ", "occ2"))
})

test_that("a line with a dose and an observation becomes both", {
  .d <- data.frame(id=1L, time=c(0, 12, 12, 24), amt=c(100, 100, NA, 100), dv=c(NA, 1.5, 2, NA),
                   mdv=c(0L, 0L, 0L, 1L), ii=c(0, 12, 0, 0))
  .d$dv[4] <- 3
  .r <- .dataSplitDoseObs(.d)
  expect_equal(.r$time, c(0, 12, 12, 12, 24))
  expect_equal(.r$amt, c(100, 100, NA, NA, 100))
  expect_equal(.r$dv, c(NA, NA, 1.5, 2, 3))
  expect_equal(.r$ii, c(0, 12, 0, 0, 0))
  expect_identical(.dataSplitDoseObs(.d[1, ]), .d[1, ])
  # with an event id the observation of a dose line is ignored
  .e <- data.frame(id=1L, time=c(0, 12, 24), amt=c(100, 100, NA), dv=c(NA, 0, 3), evid=c(1L, 4L, 0L))
  expect_identical(.dataSplitDoseObs(.e), .e)
  # the censoring stays with the observation
  .c <- data.frame(id=1L, time=c(0, 1), amt=c(100, NA), dv=c(0.1, 2), cens=c(1L, 0L), limit=c(0, NA))
  .r <- .dataSplitDoseObs(.c)
  expect_equal(.r$cens, c(0L, 1L, 0L))
  expect_equal(.r$limit, c(NA, 0, NA))
  expect_equal(.r$dv, c(NA, 0.1, 2))
})
