## Data files as spreadsheets and R write them: a quoted header and quoted
## identifiers, zero-padded identifiers (one pair equal as numbers), and a
## delimiter at the end of each line

## write.csv() quotes the header and the character columns
kitVariant("data-string-id", "data-quoted-header",
           "a quoted header and quoted character identifiers (write.csv style)",
           tags=c("data", "syntax"),
           write=function(d) {
             .l <- .writeDelim(c("ID", "TIME", "AMT", "DV"), ",", function(w) {
               w[order(-match(w$ID, unique(w$ID)), seq_len(nrow(w))), ]
             })(d)
             .l[1] <- paste0("\"", strsplit(.l[1], ",", fixed=TRUE)[[1]], "\"", collapse=",")
             sub("^([^,]+),", "\"\\1\",", .l[-1]) |> (\(x) c(.l[1], x))()
           })

kitVariant("data-string-id", "data-id-zero-padded",
           "zero-padded identifiers (001, 002, ...)",
           tags=c("data"),
           data=function(nSub) {
             .id <- sprintf("%03d", seq_len(nSub))
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxObs(.id, pkTimes(48), cmt=2))
           },
           write=.writeDelim(c("ID", "TIME", "AMT", "DV"), ","))

## Monolix identifiers are strings: 1 and 01 are two subjects; every
## identifier looks numeric, so read.table() makes numbers of them
kitVariant("data-string-id", "data-id-numeric-collision",
           "identifiers 1 and 01 (equal as numbers) as two subjects",
           tags=c("data"),
           data=function(nSub) {
             .id <- c("1", "01", as.character(seq_len(nSub - 2L) + 1L))
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxObs(.id, pkTimes(48), cmt=2), sort=FALSE)
           },
           write=.writeDelim(c("ID", "TIME", "AMT", "DV"), ","))

## a spreadsheet export can end each line with the delimiter
kitVariant("pkmodel-oral-1cmt", "data-trailing-delimiter",
           "every line (the header too) ends with a comma",
           tags=c("data", "syntax"),
           write=function(d) paste0(.writeDelim(c("ID", "TIME", "AMT", "DV"), ",")(d), ","))

kitVariant("data-id-numeric-collision", "data-id-collision-quoted",
           "identifiers 1 and 01 with a quoted header (write.csv style)",
           write=function(d) {
             .l <- .writeDelim(c("ID", "TIME", "AMT", "DV"), ",")(d)
             .l[1] <- paste0("\"", strsplit(.l[1], ",", fixed=TRUE)[[1]], "\"", collapse=",")
             .l
           })
