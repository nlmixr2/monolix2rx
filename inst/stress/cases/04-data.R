## Data set reading: delimiters, file= forms, ignored columns, string IDs

## write= for another delimiter; `fun` edits the written data.frame
.writeDelim <- function(columns, sep, fun=identity) {
  function(d) {
    .w <- fun(.kitMonolixRows(d))[, columns, drop=FALSE]
    .w[] <- lapply(.w, function(x) {
      .r <- if (is.numeric(x)) trimws(formatC(x, digits=15, format="fg")) else as.character(x)
      .r[is.na(x)] <- "."
      .r
    })
    c(paste(names(.w), collapse=sep), do.call(paste, c(.w, sep=sep)))
  }
}

.dataProject <- function(...) {
  .mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3)), ...)
}

kitVariant("pkmodel-oral-1cmt", "data-semicolon-ignore",
           "semicolon delimiter, an ignored text column and a dose-only AMT column with '.'",
           tags=c("data"),
           write=.writeDelim(c("ID", "TIME", "AMT", "DV", "NOTE"), ";", function(w) {
             w$NOTE <- ifelse(is.na(w$AMT), "obs", "dose")
             w
           }),
           mlxtran=.dataProject(delimiter="semicolon",
                                content=paste0(.mlxContent, "\nNOTE = {use=ignore}")))

## the Monolix 2024 file={path=} form and a data set in a subdirectory
kitVariant("pkmodel-oral-1cmt", "data-tab-subdir-path",
           "tab-delimited data in a subdirectory, given as file={path='data/pk.txt'}",
           tags=c("data", "mlx2024"),
           dataFile="data/pk.txt",
           write=.writeDelim(c("ID", "TIME", "AMT", "DV"), "\t"),
           mlxtran=.dataProject(delimiter="tab", file="{path='{{DATA}}'}"))

## the truth uses the same character IDs; the file lists the subjects in
## reverse order
kitVariant("pkmodel-oral-1cmt", "data-string-id",
           "character subject identifiers (S-001 ...) listed out of order",
           tags=c("data"),
           data=function(nSub) {
             .id <- sprintf("S-%03d", seq_len(nSub))
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxObs(.id, pkTimes(48), cmt=2))
           },
           write=.writeDelim(c("ID", "TIME", "AMT", "DV"), ",", function(w) {
             w[order(-match(w$ID, unique(w$ID)), seq_len(nrow(w))), ]
           }),
           mlxtran=.dataProject())

## observations flagged MDV=1 carry a value that must be ignored
kitVariant("pkmodel-oral-1cmt", "data-mdv",
           "MDV column (use=missingdependentvariable) with flagged observations",
           tags=c("data", "mdv"),
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxObs(.id, pkTimes(48), cmt=2),
                     mlxObs(.id, c(5, 30), cmt=2, mdv=1L))
           },
           write=.writeDelim(c("ID", "TIME", "AMT", "DV", "MDV"), ",", function(w) {
             w$DV[w$MDV == 1L] <- 999
             w
           }),
           mlxtran=.dataProject(content=paste0(.mlxContent, "\nMDV = {use=missingdependentvariable}")))
