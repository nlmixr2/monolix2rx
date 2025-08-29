.loadDparser <- function() {
  .Call(`_monolix2rx_iniDparserPtr`,
        dparser::.dparsePtr(),
        PACKAGE = "monolix2rx")
}
.onLoad <- function(libname, pkgname) {
  .loadDparser()
  .Call(`_monolix2rx_r_parseIni`)
  ## if (requireNamespace("nlme", quietly=TRUE)) {
  ##   rxode2::.s3register("nlme::getData", "nonmem2rx")
  ## }
  ## rxode2::.s3register("ggplot2::autoplot", "nonmem2rx")
  ## rxode2::.s3register("base::plot", "nonmem2rx")
  .rxUiGetRegister()
  if (requireNamespace("rxode2", quietly=FALSE)) {
    rxode2::.s3register("rxode2::rxUiGet", "monolixModelIwres")
  }
}
.onAttach <- function(libname,pkgname) {
  .loadDparser()
  .Call(`_monolix2rx_r_parseIni`)
}
