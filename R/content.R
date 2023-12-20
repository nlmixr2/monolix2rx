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
  }
}

.content <- function(text) {
  .contentIni(TRUE)
  .Call(`_monolix2rx_trans_content`, text)
}

.contSetUse1 <- function(use1, name) {
  .monolix2rx$use1[use1] <- name
}

.contentNbdoses <- function(val) {
  .monolix2rx$ssNbdoses <- as.integer(val)
}

.contentContCov <- function(val) {
  .monolix2rx$contLst <- c(.monolix2rx$contLst, val)
}
