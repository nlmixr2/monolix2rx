#' Handle a single endpoint and convert to rxode2
#'
#' @param endpoint The endpoint to convert to syntax
#' @return rxode2 syntax for the monolix endpoint
#' @noRd
#' @author Matthew L. Fidler
.handleSingleEndpoint <- function(endpoint) {
  # $MODEL$LONGITUDINAL$DEFINITION$endpoint[[i]]
  if (endpoint$dist == "event") {
    stop("'event' endpoint not supported in translation yet",
         call.=FALSE)
  } else if (endpoint$dist == "categorical") {
    stop("'categorical' endpoint not supported in translation yet",
         call.=FALSE)
  } else if (endpoint$dist == "count") {
    stop("'count' endpoint not supported in translation yet",
         call.=FALSE)
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
