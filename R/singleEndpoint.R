#' Alias error parameters shared by several endpoints
#'
#' rxode2 refuses one error parameter in two endpoints, so a later
#' endpoint uses `rx_<par>_<endpoint> <- <par>` instead.
#'
#' @param endpoints parsed `[LONGITUDINAL] DEFINITION:` endpoints
#' @return endpoints, with the alias lines in the "alias" attribute
#' @noRd
#' @author Matthew L. Fidler
.endpointShareErr <- function(endpoints) {
  if (length(endpoints) == 0L) return(endpoints)
  .seen <- character(0)
  .alias <- character(0)
  for (.i in seq_along(endpoints)) {
    .t <- endpoints[[.i]]$err$typical
    if (is.null(.t)) next
    .par <- grepl("^[A-Za-z]", .t)
    .dup <- .par & .t %in% .seen
    .seen <- c(.seen, .t[.par])
    if (any(.dup)) {
      .new <- paste0("rx_", .t[.dup], "_", endpoints[[.i]]$var)
      .alias <- unique(c(.alias, paste0(.new, " <- ", .t[.dup])))
      endpoints[[.i]]$err$typical[.dup] <- .new
    }
  }
  attr(endpoints, "alias") <- .alias
  endpoints
}

#' Handle a single endpoint and convert to rxode2
#'
#' @param endpoint The endpoint to convert to syntax
#' @param cmt rxode2 compartment number of the endpoint (used by events)
#' @return rxode2 syntax for the monolix endpoint
#' @noRd
#' @author Matthew L. Fidler
.handleSingleEndpoint <- function(endpoint, cmt=0L) {
  # $MODEL$LONGITUDINAL$DEFINITION$endpoint[[i]]
  if (endpoint$dist == "event") {
    return(.handleEventEndpoint(endpoint, cmt))
  } else if (endpoint$dist == "categorical") {
    return(.handleCategoricalEndpoint(endpoint))
  } else if (endpoint$dist == "count") {
    return(.handleCountEndpoint(endpoint))
  } else if (endpoint$dist == "lognormal") {
    .add <- "lnorm"
  } else if (endpoint$dist == "normal") {
    .add <- "add"
  } else if (endpoint$dist == "logitnormal") {
    .add <- "logitNorm"
  } else if (endpoint$dist == "probitnormal") {
    .add <- "probitNorm"
  }
  .prd <- ""
  if (endpoint$var != endpoint$pred) {
    .prd <- paste0(endpoint$var, " <- ", endpoint$pred, "\n")
  }
  if (endpoint$err$errName == "constant") {
    return(paste0(.prd,
                  endpoint$var, " ~ ",
                  .add,
                  "(",
                  endpoint$err$typical[1],
                  ifelse(endpoint$dist == "logitnormal",
                         paste0(", ", endpoint$min, ", ", endpoint$max),
                         ""),
                  ")"))
  } else if (endpoint$err$errName == "proportional") {
    return(paste0(.prd,
                  endpoint$var, " ~ ",
                  ifelse(.add == "add", "", paste0(.add, "(NA) + ")),
                  "prop(",
                  endpoint$err$typical[1],
                  ")"))
  }
  if (endpoint$err$errName %in% c("combined1", "combined1c")) {
    .combined <- " + combined1()"
  } else if (endpoint$err$errName %in% c("combined2", "combined2c")) {
    .combined <- " + combined2()"
  }
  if (endpoint$err$errName %in% c("combined1", "combined2")) {
    .prop <- paste0(" + prop(", endpoint$err$typical[2], ")")
  } else if (endpoint$err$errName %in% c("combined1c", "combined2c")) {
    .prop <- paste0(" + pow(", endpoint$err$typical[2], ", ",
                    endpoint$err$typical[3], ")")
  }
  return(paste0(.prd,
                endpoint$var, " ~ ",
                .add, "(", endpoint$err$typical[1],
                ")", .prop, .combined))
}
