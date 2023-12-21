#' Initialize content block parsing
#'
#' @param full is this a full reset?
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.contentIni <- function(full=TRUE) {
  .indIni(full=full)
  if (full) {
    .monolix2rx$use1 <-
      c(identifier=NA_character_,
        time=NA_character_,
        eventidentifier=NA_character_,
        amount=NA_character_,
        interdoseinterval=NA_character_,
        censored=NA_character_,
        limit=NA_character_,
        observationtype=NA_character_,
        administration=NA_character_,
        steadystate=NA_character_,
        observation=NA_character_)
    .monolix2rx$contLst <- character(0)
    .monolix2rx$ssNbdoses <- 7L
    .monolix2rx$yname <- character(0)
    .monolix2rx$name <- character(0)
    .monolix2rx$type <- character(0)
  }
}
#' Parse [CONTENT] from mlxtran
#'
#' @param text the parsing string
#' @return monolix2rxContent list
#' @noRd
#' @author Matthew L. Fidler
.content <- function(text) {
  .contentIni(TRUE)
  .Call(`_monolix2rx_trans_content`, text)
  .lst <- list(use1=.monolix2rx$use1,
               cont=.monolix2rx$contLst,
               cat=.monolix2rx$catLst2,
               reg=.monolix2rx$regLst,
               nbdoses=.monolix2rx$ssNbdoses,
               yname=.monolix2rx$yname,
               name=.monolix2rx$name,
               type=.monolix2rx$type)
  if (length(.lst$yname) != length(.lst$name)) {
    stop("for 'observation' type the length of 'name' and 'yname' should match",
         call.=FALSE)
  }
  class(.lst) <- "monolix2rxContent"
  return(.lst)
}
#' Set the single use variables
#'
#' @param use1 use1 is the name of the single use variable
#' @param name name of the variable in the dataset
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.contSetUse1 <- function(use1, name) {
  .monolix2rx$use1[use1] <- name
}
#' Set the steady state number of doses
#'
#' @param val This represents the integer
#' @return nothing called for side effects
#' @noRd
#' @author Matthew L. Fidler
.contentNbdoses <- function(val) {
  .monolix2rx$ssNbdoses <- as.integer(val)
}
#' Add continuous covariates to the list
#'
#' @param val This represents the integer
#' @return nothing, called for side effets
#' @noRd
#' @author Matthew L. Fidler
.contentContCov <- function(val) {
  .monolix2rx$contLst <- c(.monolix2rx$contLst, val)
}
#' Content Yname
#'
#' @param val value of yname
#' @return nothing, called for side effecs
#' @noRd
#' @author Matthew L. Fidler
.contentYname <- function(var) {
   .v1 <- substr(var, 1, 1)
  if (.v1 == "'" || .v1 == '"') {
    .v2 <- substr(var, nchar(var), nchar(var))
    if (.v1 == .v2) {
      var <- substr(var, 2, nchar(var)-1)
    }
  }
  .monolix2rx$yname <- c(.monolix2rx$yname, var)
}
#' Content name
#'
#' @param val value of name
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.contentName <- function(val) {
  .monolix2rx$name <- c(.monolix2rx$name, val)
}
#' Content type
#'
#' @param val type string
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.contentType <- function(val) {
  .monolix2rx$type <- c(.monolix2rx$type, val)
}

#' @export
print.monolix2rxContent <- function(x, ...) {
  lapply(names(x$use1), function(n) {
    if (is.na(x$use1[n])) return(NULL)
    cat(x$use1[n], " = {use=", n, sep="")
    if (n == "observation") {
      .name <- x$name
      .yname <- x$yname
      .type <- x$type
      if (length(.name) == 1L) {
        cat(", name=", .name, sep="")
        cat(", yname='", .yname, "'", sep="")
        cat(", type=", .type, sep="")

      } else {
        cat(", name={", paste(.name, collapse=", "), "}", sep="")
        cat(", yname={'", paste(.yname, collapse="', '"), "'}", sep="")
        cat(", type={", paste(.type, collapse=", "), "}", sep="")
      }
    } else if (n == "steadystate") {
      cat(", nbdoses=", x$nbdoses, sep="")
    }
    cat("}\n", sep="")
  })
  .printReg(x)
  lapply(x$cont, function(n) {
    cat(n, " = {use=covariate, type=continuous}\n", sep="")
  })
  .printCat(x)
  invisible(x)
}
