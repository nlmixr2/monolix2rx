.mlxtranSumary <- function(mlxtran) {
  .exportPath <- mlxtran$MONOLIX$SETTINGS$GLOBAL$exportpath
  .monolix2rx$dfSub <- 0L
  .monolix2rx$dfObs <- 0L
  .monolix2rx$obsLst <- list()
  .monolix2rx$ndose <- 0L
  attr(mlxtran, "version") <- NULL
  attr(mlxtran, "dfSub") <- .monolix2rx$dfSub
  attr(mlxtran, "dfObs") <- .monolix2rx$dfObs
  attr(mlxtran, "obsLst") <- .monolix2rx$obsLst
  attr(mlxtran, "ndose") <- .monolix2rx$ndose
  if (!file.exists(.exportPath)) {
    return(mlxtran)
  }
  .summary <- file.path(.exportPath, "summary.txt")
  if (!file.exists(.summary)){
    return(mlxtran)
  }
  .lines <- readLines(.summary)
  .env <- new.env(parent=emptyenv())
  .env$foundVersion <- FALSE
  .env$foundDataInfo <- FALSE
  .env$version <- NULL
  .env$n <- 1L
  .env$diLine <- character(0)
  lapply(.lines,
         function(l) {
           if (!.env$foundVersion) {
             if (.env$n > 5L) {
               .env$foundVersion <- TRUE
             }
             if (grepl(".*[vV]ersion *: *[^ ]*.*", l)) {
               .env$version <- sub(".*[vV]ersion *: *([^ ]*).*", "\\1", l)
               .env$foundVersion <- TRUE
             }
           } else if (.env$foundDataInfo) {
             .nc <- nchar(l)
             if (.nc >= 6) {
               if (substr(l, 1, 6) == "Number") {
                 .env$diLine <- c(.env$diLine, l)
               }
             }

           } else {
             .nc <- nchar(l)
             if (.nc >= 19) {
               if (substr(l, 1, 19) == "DATASET INFORMATION") {
                 .env$foundDataInfo <- TRUE
               }
             }
           }
           .env$n <- .env$n + 1L
         })
  if (length(.env$diLine) > 0) {
    .Call(`_monolix2rx_trans_summaryData`, paste(.env$diLine, collapse="\n"))
  }
  attr(mlxtran, "version") <- .env$version
  attr(mlxtran, "dfSub") <- .monolix2rx$dfSub
  attr(mlxtran, "dfObs") <- .monolix2rx$dfObs
  attr(mlxtran, "obsLst") <- .monolix2rx$obsLst
  attr(mlxtran, "ndose") <- .monolix2rx$ndose
  mlxtran
}

.summaryDataNid <- function(v) {
  .monolix2rx$dfSub <- as.integer(v)
}

.summaryDataObs <- function(type, v) {
  .val <- as.integer(v)
  .monolix2rx$obsLst[[type]] <- .val
  .monolix2rx$dfObs <- .monolix2rx$dfObs + .val
}

.summaryDataDose <- function(v) {
  .monolix2rx$ndose <- as.integer(v)
}
