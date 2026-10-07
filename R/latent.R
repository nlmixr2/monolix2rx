#' Latent categorical covariates (between-subject mixtures)
#'
#' A `[COVARIATE] DEFINITION:` categorical covariate with class
#' probabilities (`P(lcat=1)=plcat1`) is not in the data; it becomes
#' rxode2's `mix()`, which draws the class per subject when simulating
#' and takes it from the data (`mixest`) when estimating.
#'
#' @param mlxtran parsed mlxtran object
#' @return list with `model` (the `mix()` lines) and `prob` (the
#'   probability parameters)
#' @noRd
#' @author Matthew L. Fidler
.latentMix <- function(mlxtran) {
  .model <- .prob <- character(0)
  for (.d in mlxtran$MODEL$COVARIATE$DEFINITION$endpoint) {
    if (!identical(.d$dist, "categorical")) next
    .code <- paste(.d$err$code, collapse="\n")
    if (!grepl("P[(]", .code)) next
    .cat <- as.character(.d$err$categories)
    .p <- .latentProb(.code, .d$var, .cat)
    .lit <- ifelse(is.na(suppressWarnings(as.numeric(.cat))),
                   paste0("'", .cat, "'"), .cat)
    .args <- c(rbind(.lit[-length(.lit)], .p), .lit[length(.lit)])
    .model <- c(.model, paste0(.d$var, " <- mix(", paste(.args, collapse=", "), ")"))
    .prob <- c(.prob, .p)
  }
  list(model=.model, prob=.prob)
}

#' Class probabilities of a latent covariate, in category order
#'
#' @param code the definition code (`P(lcat=1)=plcat1` lines)
#' @param var covariate name
#' @param cat categories
#' @return probability parameter names for all but the last category
#' @noRd
#' @author Matthew L. Fidler
.latentProb <- function(code, var, cat) {
  .code <- gsub("[[:space:]]", "", strsplit(code, "\n")[[1]])
  .re <- paste0("^P[(]", var, "=['\"]?([^)'\"]*)['\"]?[)]=(.*)$")
  .code <- .code[grepl(.re, .code)]
  .p <- stats::setNames(sub(.re, "\\2", .code), sub(.re, "\\1", .code))
  .need <- cat[-length(cat)]
  if (!all(.need %in% names(.p))) {
    stop("latent covariate '", var, "' needs P(", var, "=k) for the categories ",
         paste(.need, collapse=", "), call.=FALSE)
  }
  .p <- unname(.p[.need])
  if (!all(grepl("^[A-Za-z][A-Za-z0-9_.]*$", .p))) {
    stop("latent covariate '", var, "' probabilities must be parameters for mix()",
         call.=FALSE)
  }
  .p
}

#' Add the latent class probabilities to the ini block
#'
#' @param ini ini call from `.def2ini()`
#' @param prob probability parameter names
#' @param pars parsed `<PARAMETER>` section
#' @return ini call
#' @noRd
#' @author Matthew L. Fidler
.latentIni <- function(ini, prob, pars) {
  if (length(prob) == 0L) return(ini)
  .new <- lapply(prob, function(p) {
    .v <- .parsGetValue(pars, p)
    if (is.na(.v)) stop("latent class probability '", p, "' is not in <PARAMETER>", call.=FALSE)
    if (.parsGetFixed(pars, p)) {
      bquote(.(str2lang(p)) <- fixed(.(.v)))
    } else {
      bquote(.(str2lang(p)) <- .(.v))
    }
  })
  ini[[2]] <- as.call(c(as.list(ini[[2]]), .new))
  ini
}
