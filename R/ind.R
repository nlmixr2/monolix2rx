#' Initialize the ini block for parsing
#'
#' @param full Is this a full parameter reset (if `TRUE` reset more)
#' @return nothing; called for side effects
#' @noRd
#' @author Matthew L. Fidler
.indIni <- function(full=TRUE) {
  .monolix2rx$catLst <- character(0)
  .monolix2rx$catLstQ <- logical(0)
  .monolix2rx$catName <- NA_character_
  if (full) {
    .fileinfoIni()
    .monolix2rx$inpLst <- character(0)
    .monolix2rx$regLst <- character(0)
    .monolix2rx$catLst2 <- NULL
    .monolix2rx$ind <- NULL
  }
}
#' Process the monolix input sections (say what to help users find out where)
#'
#' @param inp input string
#' @param where is this being processed (string, default [INDIVIDUAL])
#' @return input statement
#' @noRd
#' @author Matthew L. Fidler
.ind <- function(inp, what="[INDIVIDUAL]") {
  .indIni(TRUE)
  .Call(`_monolix2rx_trans_individual`, inp, what)
  .indFinalize()
  .monolix2rx$ind
}
#' Add categorical item
#'
#' @param var categorical variable
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.indCatItem <- function(var) {
  .v1 <- substr(var, 1, 1)
  .needQuote <- FALSE
  if (.v1 == "'" || .v1 == '"') {
    .v2 <- substr(var, nchar(var), nchar(var))
    if (.v1 == .v2) {
      var <- substr(var, 2, nchar(var)-1)
      .needQuote <- TRUE
    }
  }
  .monolix2rx$catLst <- c(.monolix2rx$catLst, var)
  .monolix2rx$catLstQ <- c(.monolix2rx$catLstQ, .needQuote)
}
#' Add input variable
#'
#'
#' @param var variable to add to input list
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.indAdd <- function(var)  {
  .monolix2rx$inpLst <- c(.monolix2rx$inpLst, var)
}
#' Push categorical variable on the parsing information list
#'
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.indPushCat <- function() {
  if (!is.na(.monolix2rx$catName)) {
    .tmp <- setNames(list(list(cat=.monolix2rx$catLst, quote=.monolix2rx$catLstQ)), .monolix2rx$catName)
    .monolix2rx$catLst2 <- c(.monolix2rx$catLst2, .tmp)
    .indIni(full=FALSE)
  }
}
#' Input a categorical covariate definition
#'
#' @param var variable name
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.indCat <- function(var) {
  .indPushCat()
  .monolix2rx$catName <- var
}

#' Finialize individual section
#'
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.indFinalize <- function() {
  .indPushCat()
  .ind <- list(input=.monolix2rx$inpLst,
               cat=.monolix2rx$catLst2,
               reg=.monolix2rx$regLst,
               file=.monolix2rx$file)
  class(.ind) <- "monolix2rxInd"
  .indIni(full=TRUE)
  .monolix2rx$ind <- .ind
}
#' Add regressors to the variable list
#'
#' @param reg regressor
#' @return nothing, called for side effect
#' @noRd
#' @author Matthew L. Fidler
.indReg <- function(reg) {
  # For each regression variable defined in the regressor list of the
  # Monolix Mlxtran model, there must be a column in the data set
  # defined as a regressor column X. Regressors in the data set and in
  # the Monolix Mlxtran model are matched by order, not by name.

  # What does that mean?
  .monolix2rx$regLst <- c(.monolix2rx$regLst, reg)
}
#' Print the categorical variables in `$cat`
#'
#' @param x that contains $cat, a categorical covariate list
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.printCat <- function(x) {
  .cat <- x$cat
  if (length(.cat) > 0L) {
    lapply(names(.cat),
           function(n) {
             .c <- .cat[[n]]
             cat(n, " = {type=categorical, categories=", sep="")
             .q <- .c$quote
             .c <- .c$cat
             if (length(.q) > 1) {
               cat("{")
             }
             cat(paste(vapply(seq_along(.c),
                              function(i) {
                                if (.q[i]) {
                                  return(paste0("'", .c[i], "'"))
                                }
                                .c[i]
                              }, character(1), USE.NAMES = FALSE),
                       collapse=", "))
             if (length(.q) > 1) {
               cat("}")
             }
             cat("}\n")
           })
  }
}
#' Print regressor items
#'
#' @param x contains x$reg
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.printReg <- function(x) {
  if (length(x$reg) > 0L) {
    lapply(x$reg,
           function(n) {
             cat(paste0(n, " = {use = regressor}\n"))
           })
  }
}

#' @export
print.monolix2rxInd <- function(x, ...) {
  .inp <- x$input
  if (length(.inp) > 0L) {
    cat("input = {", paste(.inp, collapse=", "), "}\n", sep="")
  }
  .printReg(x)
  .printCat(x)
  .printFile(x)
  invisible(x)
}
