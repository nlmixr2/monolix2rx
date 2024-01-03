.parameterUpdate <- function(mlx) {
  .exportPath <- mlx$MONOLIX$SETTINGS$GLOBAL$exportpath
  .popPar <- file.path(.exportPath, "populationParameters.txt")
  if (file.exists(.popPar)) {
    .popPar <- read.csv(.popPar)
    mlx$PARAMETER$PARAMETER$value <-
      vapply(mlx$PARAMETER$PARAMETER$name,
             function(n) {
               .w <- which(.popPar$parameter == n)
               if (length(.w) == 1L) {
                 return(.popPar[.w, "value"])
               }
               return(NA_real_)
             }, numeric(1), USE.NAMES = FALSE)
  }
  mlx
}
