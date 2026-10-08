
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
