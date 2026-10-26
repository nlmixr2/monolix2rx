## More data file text: a UTF-8 byte order mark (Excel's "CSV UTF-8"),
## blank lines, and spaces after the commas of the header

kitVariant("pkmodel-oral-1cmt", "data-utf8-bom",
           "a UTF-8 byte order mark before the header",
           tags=c("data", "syntax"),
           write=function(d) {
             .l <- .writeDelim(c("ID", "TIME", "AMT", "DV"), ",")(d)
             .l[1] <- paste0("\ufeff", .l[1])
             .l
           })

kitVariant("pkmodel-oral-1cmt", "data-blank-lines",
           "blank lines between subjects and at the end of the file",
           tags=c("data", "syntax"),
           write=function(d) {
             .w <- .kitMonolixRows(d)
             .l <- .writeDelim(c("ID", "TIME", "AMT", "DV"), ",")(d)
             .new <- c(FALSE, .w$ID[-1] != .w$ID[-nrow(.w)])
             c(.l[1], unlist(lapply(seq_along(.l[-1]), function(i) {
               if (.new[i]) c("", .l[i + 1]) else .l[i + 1]
             })), "", "")
           })

kitVariant("pkmodel-oral-1cmt", "data-header-spaces",
           "a header with a space after each comma (ID, TIME, AMT, DV)",
           tags=c("data", "syntax"),
           write=function(d) {
             .l <- .writeDelim(c("ID", "TIME", "AMT", "DV"), ",")(d)
             .l[1] <- gsub(",", ", ", .l[1], fixed=TRUE)
             .l
           })
