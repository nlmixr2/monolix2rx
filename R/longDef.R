.longDefIni <- function(full=FALSE) {
  .monolix2rx$varName  <- NA_character_
  .monolix2rx$dist     <- NA_character_
  .monolix2rx$pred     <- NA_character_
  .monolix2rx$min      <- -Inf
  .monolix2rx$max      <- Inf
}
# Parsing uses
# .setDist() in indDef.R
#
.longDef <- function(text) {
  .longDefIni(TRUE)
  .Call(`_monolix2rx_trans_longdef`,  text)
}
.addEndpoint <- function(var) {
  .monolix2rx$varName <- var
}

.addPrediction <- function(var) {
  .monolix2rx$pred <- var
}

.setCombined1 <- function(v1, v2) {

}

.setCombined2 <- function(v1, v2) {

}

.setCombined1c <- function(v1, v2, v3) {

}

.setCombined2c <- function(v1, v2, v3) {

}

.setConstant <- function(var) {

}

.setProp <- function(var) {

}
