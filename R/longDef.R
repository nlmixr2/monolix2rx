#' Initialize the [LONGITUDNIAL] DEFINITION: section variables
#'
#'
#' @param full if this is full
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.longDefIni <- function(full=FALSE) {
  .monolix2rx$varName  <- NA_character_
  .monolix2rx$dist     <- NA_character_
  .monolix2rx$autocor <- character(0)
  .monolix2rx$pred     <- NA_character_
  .monolix2rx$min      <- -Inf
  .monolix2rx$max      <- Inf
  .monolix2rx$err      <- NULL
  .monolix2rx$eventType <- "exact" # monolix default
  .monolix2rx$maxEventNumber <- NA_integer_
  .monolix2rx$rightCensoringTime <- NA_real_
  .monolix2rx$intervalLength <- NA_real_
  .monolix2rx$categoriesInt <- integer(0)
  .monolix2rx$codeLine <- character(0)
  if (full) {
    .monolix2rx$defFixed <- numeric(0)
    .monolix2rx$longDef <- NULL
  }
}
#' Set the categories integer vector
#'
#' @param var string of the integer
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.setCategoriesInt <- function(var) {
  .i <- as.integer(var)
  if (length(.monolix2rx$categoriesInt) == 0) {
    .monolix2rx$categoriesInt <- .i
    return(invisible())
  }
  .old <- .monolix2rx$categoriesInt[length(.monolix2rx$categoriesInt)]
  if (.i <= .old) {
    stop("categories need to be in ascending order")
  }
  .monolix2rx$categoriesInt <- c(.monolix2rx$categoriesInt, .i)
}

#' Push a code line for error models that support them
#'
#' @param var code line
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.setCodeLine <- function(var) {
  .monolix2rx$codeLine <- c(.monolix2rx$codeLine, var)
}

#' Set the interval length
#'
#' @param var string of interval length
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.setIntervalLength <- function(var) {
  .monolix2rx$intervalLength <- as.numeric(var)
}
#' Set right censoring time
#'
#' @param var string for right censoring time
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.setRightCensoringTime <- function(var) {
  .monolix2rx$rightCensoringTime <- as.numeric(var)
}
#' Set event type
#'
#' @param var event type string
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.setEventType <- function(var) {
  .monolix2rx$eventType <- var
}
#' Set the maximum event number
#'
#' @param var text of integer maximum event number
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.setMaxEventNumber <- function(var) {
  .monolix2rx$maxEventNumber <- as.integer(var)
}
#' Parse the [LONGITUDNIAL] DEFINITION: section
#'
#' @param text text to parse
#' @return longDef definition
#' @noRd
#' @author Matthew L. Fidler
.longDef <- function(text) {
  .longDefIni(TRUE)
  .Call(`_monolix2rx_trans_longdef`,  text)
  .pushEndpoint()
  .ret <- list(endpoint=.monolix2rx$longDef,
               fixed=.monolix2rx$defFixed)
  class(.ret) <- "monolix2rxLongDef"
  .longDefIni(TRUE)
  .ret
}

#' Pushes the endpoint into the $longDef list
#'
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.pushEndpoint <- function() {
  if (!is.na(.monolix2rx$varName)) {
    if (.monolix2rx$dist == "event") {
      .err <- list(eventType=.monolix2rx$eventType,
                   maxEventNumber=.monolix2rx$maxEventNumber,
                   rightCensoringTime=.monolix2rx$rightCensoringTime,
                   intervalLength=.monolix2rx$intervalLength)
    } else if (.monolix2rx$dist == "categorical") {
      .err <- list(categories=.monolix2rx$categoriesInt,
                   code=.monolix2rx$codeLine)
    } else if (.monolix2rx$dist == "count") {
      .err <- list(code=.monolix2rx$codeLine)
    } else {
      .err <- .monolix2rx$err
    }
    .end <- list(var=.monolix2rx$varName,
                 dist=.monolix2rx$dist, # type in non cont.
                 pred=.monolix2rx$pred, # hazard in tte
                 err=.err,
                 autocor=.monolix2rx$autocor)
    .monolix2rx$longDef <- c(.monolix2rx$longDef, list(.end))
    .longDefIni(FALSE)
  }
}
#' Interfaces with the parser to add an endpoint
#'
#' @param var variable for endpoint addition
#' @return nothing, called for side effects
#' @author Matthew L. Fidler
#' @noRd
.addEndpoint <- function(var) {
  .pushEndpoint()
  .monolix2rx$varName <- var
}
#' Add prediction variable (can be hazard ratio in tte)
#'
#'
#' @param var prediction variable or hazard ratio
#' @return nothang, called for side effets
#' @noRd
#' @author Matthew L. Fidler
.addPrediction <- function(var) {
  .monolix2rx$pred <- var
}
#' Add autocorrelation
#'
#' @param cor correlation parameter
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.addAutocor <- function(cor) {
  .monolix2rx$autocor <- cor
}
#' This gets the values or the NA based on the supplied inputs
#'
#' @param lst list of values
#' @param errName error name, used for creating fixed parameter estimates
#' @return list with `$name` and `$val` for name and value respectively.
#' @noRd
#' @author Matthew L. Fidler
.getErrValue <- function(lst, errName) {
  .env <- new.env(parent=emptyenv())
  .env$name <- character(length(lst))
  .env$val <- numeric(length(lst))
  lapply(seq_along(lst),
         function(i) {
           .c <- lst[[i]]
           .v <- suppressWarnings(as.numeric(.c))
           if(is.na(.v)) {
             .env$name[i] <- .c
             .env$val[i] <- NA_real_
           } else {
             .env$name[i] <- paste0("rx_", .monolix2rx$varName, "_", errName, "_", i)
             .env$val[i] <- .v
             names(.v) <- .env$name[i]
             .monolix2rx$defFixed <- c(.monolix2rx$defFixed, .v)
           }
         })
  return(list(errName=errName,
              typical=.env$name))
}
#' Set the combined1() err
#'
#' @param v1 variable 1
#' @param v2 variable 2
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.setCombined1 <- function(v1, v2) {
  .monolix2rx$err <- .getErrValue(list(v1, v2), "combined1")
}
#' Set combined2 error
#'
#' @param v1 variable 1 error
#' @param v2 variable 2 error
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.setCombined2 <- function(v1, v2) {
  .monolix2rx$err <- .getErrValue(list(v1, v2), "combined2")
}
#' Set combined1c error
#'
#' @param v1 additive
#' @param v2 pow
#' @param v3 pow expononent
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.setCombined1c <- function(v1, v2, v3) {
  .monolix2rx$err <- .getErrValue(list(v1, v2, v3), "combined1c")
}
#' Set combinde2c error
#'
#' @param v1 additive1
#' @param v2 pow
#' @param v3 pow exponent
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.setCombined2c <- function(v1, v2, v3) {
  .monolix2rx$err <- .getErrValue(list(v1, v2, v3), "combined2c")
}
#' Set additive error (constant error)
#'
#' @param var error variable information
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.setConstant <- function(var) {
  .monolix2rx$err <- .getErrValue(list(var), "constant")
}
#' Set proportional error
#'
#' @param var proportional error variable
#' @return nothing, called for side effects
#' @export
#' @author Matthew L. Fidler
#' @noRd
.setProp <- function(var) {
  .monolix2rx$err <- .getErrValue(list(var), "proportional")
}

#' @export
as.character.monolix2rxLongDef <- function(x, ...) {
  vapply(seq_along(x$endpoint),
         function(i) {
           .lst <- x$endpoint[[i]]
           if (.lst$dist == "event") {
             .err <- .lst$err
             .ret <- paste0(.lst$var, " = {type=event",
                            .asCharacterSingleOrList("eventType", .err$eventType),
                            .asCharacterSingleOrList("maxEventNumber", .err$maxEventNumber),
                            .asCharacterSingleOrList("rightCensoringTime", .err$rightCensoringTime),
                            .asCharacterSingleOrList("intervalLength", .err$intervalLength))
             .ret <- paste0(.ret, ", hazard=", .lst$pred)
             .ret <- paste0(.ret, "}")
           } else if (.lst$dist == "categorical") {
             .err <- .lst$err
             .ret <- paste0(.lst$var, " = {type=categorical",
                            .asCharacterSingleOrList("categories", .err$categories),
                            ",\n", paste(.err$code, collapse="\n"), "}")
           } else if (.lst$dist == "count") {
             .err <- .lst$err
             .ret <- paste0(.lst$var, " = {type=count")
             .ret <- paste0(.ret,
                            ",\n", paste(.err$code, collapse="\n"), "}")
           } else  {
             .err <- .lst$err
             .v <- .varOrFixed(.err$typical, x$fixed)
             .ret <- paste0(.lst$var, " = {",
                            .asCharacterSingleOrList("distribution", .lst$dist, comma=""),
                            .asCharacterSingleOrList("prediction", .lst$pred),
                            ", errorModel = ",
                            .err$errName, "(", paste(.v, collapse=", "), ")",
                            .asCharacterSingleOrList("autoCorrCoef", .lst$autocor),
                            "}")
           }
         }, character(1),
         USE.NAMES=FALSE)
}

#' @export
print.monolix2rxLongDef <- function(x, ...) {
  ## name=.env$name,
  ## val=.env$val
  cat(paste(as.character.monolix2rxLongDef(x, ...), collapse="\n"), "\n", sep="")
  invisible(x)
}

#' @export
as.list.monolix2rxLongDef <- function(x, ...) {
  .x <- x
  class(.x) <- NULL
  .x
}
