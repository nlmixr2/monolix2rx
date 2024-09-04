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
  .monolix2rx$min      <- 0
  .monolix2rx$max      <- 1
  .monolix2rx$err      <- NULL
  .monolix2rx$eventType <- "exact" # monolix default
  .monolix2rx$maxEventNumber <- NA_integer_
  .monolix2rx$rightCensoringTime <- NA_real_
  .monolix2rx$intervalLength <- NA_real_
  .monolix2rx$categoriesInt <- integer(0)
  .monolix2rx$codeLine <- character(0)
  .monolix2rx$transformTo <- character(0)
  .monolix2rx$transformFrom <- character(0)
  .monolix2rx$transformQ <- character(0)
  .monolix2rx$transformCatB <- logical(0)
  .monolix2rx$transformCatLabel <- character(0)
  .monolix2rx$transformCatLabelQ <- logical(0)
  .monolix2rx$transformCatValue <- NULL
  .monolix2rx$transformCatValueQ <- NULL
  .monolix2rx$transformReference <- character(0)
  .monolix2rx$transformReferenceQ <- logical(0)
  if (full) {
    .monolix2rx$defFixed <- numeric(0)
    .monolix2rx$longDef <- NULL
    .monolix2rx$longDefTransform <- NULL
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
#' Push all code lines for error models that support them
#'
#' @param allCode -- all code lines
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.setAllCode <- function(allCode) {
  .monolix2rx$codeLine <- c(.monolix2rx$codeLine, strsplit(allCode, "\n")[[1]])
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
.longDef <- function(text, where="[LONGITUDINAL] DEFINITION:") {
  .longDefIni(TRUE)
  .Call(`_monolix2rx_trans_longdef`,  text, where)
  .pushEndpoint()
  .ret <- list(endpoint=.monolix2rx$longDef,
               fixed=.monolix2rx$defFixed)
  if (!is.null(.monolix2rx$longDefTransform)) {
    .ret$transform <- .monolix2rx$longDefTransform
  }
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
  .reset <- FALSE
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
    if (.monolix2rx$dist == "logitnormal") {
      .end <- c(.end,
                list(min=.monolix2rx$min,
                     max=.monolix2rx$max))
    }
    .monolix2rx$longDef <- c(.monolix2rx$longDef, list(.end))
    .reset <- TRUE
  }
  if (length(.monolix2rx$transformTo) == 1L && length(.monolix2rx$transformFrom) == 1L) {
    .pushTransform()
    .reset <- TRUE
  }
  if (.reset) .longDefIni(FALSE)
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
#' Set the minimum value of the distribution
#'
#' @param var number for the min
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.longDefSetMin <- function(var) {
  .monolix2rx$min <- as.numeric(var)
}
#' Set the maximum of the logit dist
#'
#' @param var maximum value
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.longDefSetMax <- function(var) {
  .monolix2rx$max <- as.numeric(var)
}
#' Push transform on the transformation list
#'
#' @return  nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.pushTransform <- function() {
  .val <- list(transform = .monolix2rx$transformFrom,
               transformQ=.monolix2rx$transformQ,
               catLabel=.monolix2rx$transformCatLabel,
               catLabelQ=.monolix2rx$transformCatLabelQ,
               catValue=.monolix2rx$transformCatValue,
               catValueQ=.monolix2rx$transformCatValueQ,
               catB=.monolix2rx$transformCatB,
               reference = .monolix2rx$transformReference,
               referenceQ = .monolix2rx$transformReferenceQ)
  .val <- list(.val)
  names(.val) <- .monolix2rx$transformTo
  .monolix2rx$longDefTransform <- c(.monolix2rx$longDefTransform, .val)
  .monolix2rx$transformTo <- character(0)
  .monolix2rx$transformFrom <- character(0)
  .monolix2rx$transformQ <- character(0)
  .monolix2rx$transformCatB <- logical(0)
  .monolix2rx$transformCatLabel <- character(0)
  .monolix2rx$transformCatLabelQ <- logical(0)
  .monolix2rx$transformCatValue <- NULL
  .monolix2rx$transformCatValueQ <- NULL
  .monolix2rx$transformReference <- character(0)
  .monolix2rx$transformReferenceQ <- logical(0)
}

.longDefSetTransformTo <- function(var) {
  if (length(.monolix2rx$transformTo) == 1L) {
    .pushTransform()
  }
  .monolix2rx$transformTo <- var
}
.longDefSetTransformFrom <- function(var, q) {
  .monolix2rx$transformFrom <- var
  .monolix2rx$transformQ <- as.logical(q)
}
.longDefSetTransformLabel <- function(var, q) {
  .monolix2rx$transformCatLabel <- c(.monolix2rx$transformCatLabel, var)
  .monolix2rx$transformCatLabelQ <- c(.monolix2rx$transformCatLabelQ, as.logical(q))
}
.longDefSetTransformValue <- function(var, q) {
  .monolix2rx$transformCatValue <- c(.monolix2rx$transformCatValue, list(var))
  .monolix2rx$transformCatValueQ <- c(.monolix2rx$transformCatValueQ, list(as.logical(q)))
}

.longDefSetTransformValueExtra <- function(var, q) {
  .v <- .monolix2rx$transformCatValue
 .v[[length(.v)]] <- c(.v[[length(.v)]], var)
  .monolix2rx$transformCatValue <- .v
  .v <- .monolix2rx$transformCatValueQ
  .v[[length(.v)]] <- c(.v[[length(.v)]], as.logical(q))
  .monolix2rx$transformCatValueQ <- .v
}

.longDefSetTransformB <- function(q) {
  .monolix2rx$transformCatB <- c(.monolix2rx$transformCatB, as.logical(q))
}

.longDefSetTransformB <- function(q) {
  .monolix2rx$transformCatB <- c(.monolix2rx$transformCatB, as.logical(q))
}
.longDefSetTransformRef <- function(var, q) {
  .monolix2rx$transformReference <- var
  .monolix2rx$transformReferenceQ <- as.logical(q)
}

#' @export
as.character.monolix2rxLongDef <- function(x, ...) {
  .ret <- vapply(seq_along(x$endpoint),
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
                                    ifelse(.lst$dist == "logitnormal",
                                           paste0(.asCharacterSingleOrList("min", .lst$min),
                                                  .asCharacterSingleOrList("max", .lst$max)),
                                           ""),
                                    ", errorModel=",
                                    .err$errName, "(", paste(.v, collapse=", "), ")",
                                    .asCharacterSingleOrList("autoCorrCoef", .lst$autocor),
                                    "}")
                   }
                 }, character(1),
                 USE.NAMES=FALSE)
  if (!is.null(x$transform)) {
    .ret <- c(.ret,
              vapply(names(x$transform),
                     function(n) {
                       .cur <- x$transform[[n]]
                       .transform <- .cur$transform
                       if (.cur$transformQ) .transform <- paste0("'", .transform, "'")
                       .ret <- paste0(n, " = {transform=", .transform)
                       if (length(.cur$catLabel) > 0L) {
                         .ret <- paste0(.ret, ", categories={")
                         .ret0 <- vapply(seq_along(.cur$catLabel),
                                         function(i){
                                           .lab <- .cur$catLabel[i]
                                           if (.cur$catLabelQ[i]) .lab <- paste0("'", .lab, "'")
                                           .asCharacterSingleOrList(.lab,
                                                                    .cur$catValue[[i]],
                                                                    .cur$catValueQ[[i]],comma="",
                                                                    bracket=.cur$catB[i])
                                         }, character(1), USE.NAMES = FALSE)
                         .ret <-paste0(.ret, paste(.ret0, collapse=", "), "}")
                       }
                       if (length(.cur$reference) == 1L) {
                         .reference <- .cur$reference
                         if (.cur$referenceQ) .reference <- paste0("'", .reference, "'")
                         .ret <- paste0(.ret, ", reference=", .reference)
                       }
                       paste0(.ret, "}")
                     },
                     character(1), USE.NAMES = FALSE))
  }
  .ret
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
#' Get monolix predictions from <MODEL> [LONGITUDINAL] DEFINITION:
#'
#'
#' @param x mlxtran/longitudinal monolix section
#' @return character vector of defined endpoints
#' @noRd
#' @author Matthew L. Fidler
.getMonolixPreds <- function(x) {
  if (inherits(x, "monolix2rxMlxtran")) {
    x <- x$MODEL$LONGITUDINAL$DEFINITION
  }
  if (!inherits(x, "monolix2rxLongDef")) return(character(0))
  x <- as.list(x)
  vapply(seq_along(x$endpoint),
         function(i) {
           .ret <- x$endpoint[[i]]
           if (!checkmate::testCharacter(.ret$pred, len=1)) return(NA_character_)
           .ret$pred
         }, character(1), USE.NAMES = TRUE)
}
