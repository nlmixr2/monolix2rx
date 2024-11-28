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
        observation=NA_character_,
        occasion=NA_character_,
        rate=NA_character_,
        additionaldose=NA_character_,
        missingdependentvariable=NA_character_,
        infusiontime=NA_character_)
    .monolix2rx$contLst <- character(0)
    .monolix2rx$ssNbdoses <- 7L
    .monolix2rx$yname <- character(0)
    .monolix2rx$ynameQuote <- logical(0)
    .monolix2rx$ytype <- character(0)
    .monolix2rx$ytypeQuote <- logical(0)
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
  .indPushCat()
  .lst <- list(use1=.monolix2rx$use1,
               cont=.monolix2rx$contLst,
               cat=.monolix2rx$catLst2,
               reg=.monolix2rx$regLst,
               ignore=.monolix2rx$ignoreLst,
               nbdoses=.monolix2rx$ssNbdoses,
               yname=.monolix2rx$yname,
               ynameQuote=.monolix2rx$ynameQuote,
               ytype=.monolix2rx$ytype,
               ytypeQuote=.monolix2rx$ytypeQuote,
               name=.monolix2rx$name,
               type=.monolix2rx$type)
  if (length(.lst$yname) != length(.lst$name) &&
        length(.lst$yname) != 0 && length(.lst$name) != 0) {
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
  .indPushCat()
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
  .indPushCat()
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
  .quote <- FALSE
  if (.v1 == "'" || .v1 == '"') {
    .quote <- TRUE
    .v2 <- substr(var, nchar(var), nchar(var))
    if (.v1 == .v2) {
      var <- substr(var, 2, nchar(var)-1)
    }
  }
  .monolix2rx$yname <- c(.monolix2rx$yname, var)
  .monolix2rx$ynameQuote <- c(.monolix2rx$ynameQuote, .quote)
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
#' Push a ytype value for an observation
#'
#'
#' @param val value of the ytype
#' @param quote is the ytype quoted? (0 or 1)
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.contentYtype <- function(val, quote) {
  .monolix2rx$ytype <- c(.monolix2rx$ytype, val)
  .monolix2rx$ytypeQuote <- c(.monolix2rx$ytypeQuote, as.logical(quote))
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

.asCharacterSingleOrList <- function(name, what, quote=NULL, comma=", ", eq="=", bracket=FALSE) {
  if (!is.null(quote) && length(quote) == length(what)) {
    what <- vapply(seq_along(what),
                   function(i) {
                     if (quote[i]) {
                       return(paste0("'", what[i], "'"))
                     }
                     what[i]
                   }, character(1), USE.NAMES = FALSE)
  }
  if (length(what) == 0L) return("")
  if (length(what) == 1L && !bracket) {
    if (!is.na(what)) {
      return(paste0(comma, name, eq, what))
    } else {
      return("")
    }
  }
  paste0(comma, name, eq, "{", paste(what, collapse=", "), "}")
}

#' @export
as.character.monolix2rxContent <- function(x, ...) {
  .cur <- vapply(names(x$use1), function(n) {
    if (is.na(x$use1[n])) return(NA_character_)
    .ret <- paste0(x$use1[n], " = {use=", n)
    if (n == "observation") {
      .name <- x$name
      .yname <- x$yname
      .type <- x$type
      .ret <- paste0(.ret,
                     .asCharacterSingleOrList("name", .name),
                     .asCharacterSingleOrList("yname", .yname, x$ynameQuote),
                     .asCharacterSingleOrList("ytype", x$ytype, x$ytypeQuote),
                     .asCharacterSingleOrList("type", .type))
    } else if (n == "steadystate") {
      .ret <- paste0(.ret,
                     .asCharacterSingleOrList("nbdoses", x$nbdoses))
    }
    paste0(.ret, "}")
  }, character(1), USE.NAMES = FALSE)
  c(.cur[!is.na(.cur)],
    .asCharacterReg(x),
    .asCharacterIgnore(x),
    vapply(x$cont, function(n) {
      paste0(n, " = {use=covariate, type=continuous}")
    }, character(1), USE.NAMES = FALSE),
    .asCharacterCat(x))
}

#' @export
print.monolix2rxContent <- function(x, ...) {
  cat(paste(as.character.monolix2rxContent(x, ...),
            collapse="\n"), "\n", sep="")
  invisible(x)
}

#' @export
as.list.monolix2rxContent <- function(x, ...) {
  .x <- x
  class(.x) <- NULL
  return(.x)
}
