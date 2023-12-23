.fitIni <- function() {
  .monolix2rx$fitDat <- character(0)
  .monolix2rx$modelDat <- character(0)
}

.fit <- function(text) {
  .fitIni()
  .Call(`_monolix2rx_trans_fit`, text)
  if (length(.monolix2rx$fitDat) != length(.monolix2rx$modelDat)) {
    stop("number of model and data endpoints in <FIT> need to be the same",
         call.=FALSE)
  }
  .ret <- data.frame(data=.monolix2rx$fitDat, model=.monolix2rx$modelDat)
  class(.ret) <- c("monolix2rxFit", class(.ret))
  .ret
}

.fitDatId <- function(datId) {
  .monolix2rx$fitDat <- c(.monolix2rx$fitDat, datId)
}

.fitModelId <- function(modelId) {
  .monolix2rx$modelDat <- c(.monolix2rx$modelDat, modelId)

}
