#' Split discrete definition code into statements
#'
#' @param code definition code lines
#' @return statements; they are separated by new lines or by commas
#'   outside parentheses
#' @noRd
#' @author Matthew L. Fidler
.discreteStatements <- function(code) {
  .lines <- unlist(strsplit(paste(code, collapse="\n"), "\n"))
  .ret <- unlist(lapply(.lines, function(l) {
    .ch <- strsplit(l, "")[[1]]
    .depth <- cumsum((.ch == "(") - (.ch == ")"))
    .cut <- which(.ch == "," & .depth == 0L)
    if (length(.cut) == 0L) return(l)
    substring(l, c(1L, .cut + 1L), c(.cut - 1L, nchar(l)))
  }))
  .ret <- trimws(.ret)
  .ret[.ret != ""]
}

#' Translate discrete definition statements to rxode2 lines
#'
#' @param stmt statements (the probability statements already rewritten
#'   as assignments)
#' @param var variable whose observed value replaces `k` (or NULL)
#' @return rxode2 lines
#' @noRd
#' @author Matthew L. Fidler
.discreteRx <- function(stmt, var=NULL) {
  .rx <- .equation(paste(stmt, collapse="\n"))$rx
  if (!is.null(var)) .rx <- gsub(paste0("\\b", var, "\\b"), "DV", .rx, perl=TRUE)
  .rx
}

#' Match a Poisson log-probability and return its rate
#'
#' @param e expression of DV
#' @param log is `e` the log probability
#' @return rate symbol or NULL
#' @noRd
#' @author Matthew L. Fidler
.discretePoisLambda <- function(e, log=TRUE) {
  .tmpl <- if (log) {
    list(quote(-.L + DV * log(.L) - lfactorial(DV)),
         quote(DV * log(.L) - .L - lfactorial(DV)),
         quote(-.L + DV * log(.L) - lgamma(DV + 1)),
         quote(DV * log(.L) - .L - lgamma(DV + 1)))
  } else {
    list(quote(exp(-.L) * .L^DV / factorial(DV)),
         quote(.L^DV * exp(-.L) / factorial(DV)))
  }
  .bind <- function(e, t, env) {
    if (identical(t, quote(.L))) {
      if (is.null(env$L)) {
        env$L <- e
        return(TRUE)
      }
      return(identical(env$L, e))
    }
    if (is.call(t)) {
      if (!is.call(e) || length(e) != length(t)) return(FALSE)
      for (.i in seq_along(t)) if (!.bind(e[[.i]], t[[.i]], env)) return(FALSE)
      return(TRUE)
    }
    identical(e, t)
  }
  .unparen <- function(x) {
    if (!is.call(x)) return(x)
    if (identical(x[[1]], quote(`(`))) return(.unparen(x[[2]]))
    as.call(lapply(as.list(x), .unparen))
  }
  e <- .unparen(e)
  for (.t in .tmpl) {
    .env <- new.env(parent=emptyenv())
    if (.bind(e, .t, .env) && is.name(.env$L)) return(.env$L)
  }
  NULL
}

#' Translate a count endpoint
#'
#' `log(P(Y=k)) = ...` or `P(Y=k) = ...` becomes `Y ~ pois(lambda)` when it
#' is the Poisson probability and `ll(Y) ~ Y_logp` otherwise.
#'
#' @param endpoint count endpoint
#' @return rxode2 lines
#' @noRd
#' @author Matthew L. Fidler
.handleCountEndpoint <- function(endpoint) {
  .var <- endpoint$var
  .s <- .discreteStatements(endpoint$err$code)
  .ns <- gsub("[[:space:]]", "", .s)
  .reLog <- paste0("^log[(]P[(]", .var, "=k[)][)]=(.*)$")
  .reP <- paste0("^P[(]", .var, "=k[)]=(.*)$")
  .isLog <- grepl(.reLog, .ns)
  .isP <- grepl(.reP, .ns)
  if (!any(.isLog | .isP) || (any(.isLog) && any(.isP))) {
    stop("count endpoint '", .var, "' needs log(P(", .var, "=k)) = or P(",
         .var, "=k) = (Markov dependence is not supported)", call.=FALSE)
  }
  .log <- any(.isLog)
  .lhs <- paste0(.var, ifelse(.log, "_logp", "_p"))
  .w <- which(.isLog | .isP)
  .s[.w] <- paste0(.lhs, " = ", sub(ifelse(.log, .reLog, .reP), "\\1", .ns[.w]))
  .rx <- .discreteRx(.s, "k")
  if (length(.s) == 1L) {
    .e <- str2lang(sub(paste0("^", .lhs, " <- "), "", .rx))
    .l <- .discretePoisLambda(.e, .log)
    if (!is.null(.l)) return(paste0(.var, " ~ pois(", deparse1(.l), ")"))
    .rx <- paste0(.lhs, " <- ", deparse1(.e))
  }
  # rxode2 reads a sum after ll() ~ as error terms, so use a variable
  paste(c(.rx,
          paste0("ll(", .var, ") ~ ", ifelse(.log, .lhs, paste0("log(", .lhs, ")")))),
        collapse="\n")
}

#' Translate a categorical endpoint
#'
#' `P(Y=c) = ...` for all but one category, or the cumulative `P(Y<=c)`
#' for all but the last, optionally under `logit()`/`probit()`/`log()`,
#' becomes rxode2's ordinal `Y ~ c(p0=0, p1=1, 2)`.
#'
#' @param endpoint categorical endpoint
#' @return rxode2 lines
#' @noRd
#' @author Matthew L. Fidler
.handleCategoricalEndpoint <- function(endpoint) {
  .var <- endpoint$var
  .cat <- endpoint$err$categories
  if (length(.cat) < 2L) {
    stop("categorical endpoint '", .var, "' needs at least two categories", call.=FALSE)
  }
  .s <- .discreteStatements(endpoint$err$code)
  .ns <- gsub("[[:space:]]", "", .s)
  .reF <- paste0("^(logit|probit|log)[(]P[(]", .var, "(<=|=)([0-9]+)[)][)]=(.*)$")
  .reP <- paste0("^()P[(]", .var, "(<=|=)([0-9]+)[)]=(.*)$")
  .w <- which(grepl(.reF, .ns) | grepl(.reP, .ns))
  .re <- ifelse(grepl(.reF, .ns[.w]), .reF, .reP)
  .fun <- mapply(sub, .re, "\\1", .ns[.w], USE.NAMES=FALSE)
  .op <- unique(mapply(sub, .re, "\\2", .ns[.w], USE.NAMES=FALSE))
  .c <- as.integer(mapply(sub, .re, "\\3", .ns[.w], USE.NAMES=FALSE))
  .rhs <- mapply(sub, .re, "\\4", .ns[.w], USE.NAMES=FALSE)
  # cumulative: all but the last category; P(Y=c): all but one, which
  # is the remainder
  .cum <- identical(.op, "<=")
  .rest <- if (.cum) .cat[length(.cat)] else setdiff(.cat, .c)
  .need <- setdiff(.cat, .rest)
  if (length(.op) != 1L || length(.rest) != 1L || !setequal(.c, .need)) {
    stop("categorical endpoint '", .var, "' needs P(", .var, "=c) for all but one of the categories ",
         paste(.cat, collapse=", "), " or P(", .var, "<=c) for all but the last",
         " (Markov dependence is not supported)", call.=FALSE)
  }
  .rhs <- ifelse(.fun == "logit", paste0("invlogit(", .rhs, ")"),
                 ifelse(.fun == "probit", paste0("normcdf(", .rhs, ")"),
                        ifelse(.fun == "log", paste0("exp(", .rhs, ")"), .rhs)))
  .p <- paste0(.var, "_p", .need)
  .s[.w] <- paste0(.var, ifelse(.cum, "_le", "_p"), .c, " = ", .rhs)
  .rx <- .discreteRx(.s)
  if (.cum) {
    .le <- paste0(.var, "_le", .need)
    .prev <- c("", if (length(.le) > 1L) paste0(" - ", .le[-length(.le)]))
    .rx <- c(.rx, paste0(.p, " <- ", .le, .prev))
  }
  paste(c(.rx,
          paste0(.var, " ~ c(", paste(c(paste0(.p, "=", .need), .rest),
                                      collapse=", "), ")")),
        collapse="\n")
}
