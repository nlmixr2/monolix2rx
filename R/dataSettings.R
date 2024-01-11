#' Process <DATAFILE> [SETTINGS]
#'
#' @param text character vector of length one with text to process
#' @return datafile settings object
#' @noRd
#' @author Matthew L. Fidler
.dataSettings <- function(text) {
  .monolix2rx$dataSettingsLabel <- character(0)
  .monolix2rx$dataSettingsLabelQ <- logical(0)
  .monolix2rx$dataSettingsValue <- character(0)
  .monolix2rx$dataSettingsValueQ <- logical(0)
  .Call(`_monolix2rx_trans_data_settings`, text)
  .ret <- list(label=.monolix2rx$dataSettingsLabel,
               labelQ=.monolix2rx$dataSettingsLabelQ,
               value=.monolix2rx$dataSettingsValue,
               valueQ=.monolix2rx$dataSettingsValueQ)
  class(.ret) <- "monolix2rxDataSettings"
  .ret
}
#' Process a single label
#'
#' @param label character vector of label
#' @param q integer of if this is quoted
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.dataSettingsLabel <- function(label, q) {
  .monolix2rx$dataSettingsLabel <- c(.monolix2rx$dataSettingsLabel, label)
  .monolix2rx$dataSettingsLabelQ <- c(.monolix2rx$dataSettingsLabelQ, as.logical(q))
}
#' Process a single value
#'
#' @param label character vector of value
#' @param q integer representing if the value is quoted
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.dataSettingsValue <- function(label, q) {
  .monolix2rx$dataSettingsValue <- c(.monolix2rx$dataSettingsValue, label)
  .monolix2rx$dataSettingsValueQ <- c(.monolix2rx$dataSettingsValueQ, as.logical(q))
}

#' @export
as.character.monolix2rxDataSettings <- function(x, ...) {
  .inner <- paste(vapply(seq_along(x$label), function(i) {
    .label <- x$label[i]
    if (x$labelQ[i]) {
      .label <- paste0("'", .label, "'")
    }
    .value <- x$value[i]
    if (x$valueQ[i]) {
      .value <- paste0("'", .value, "'")
    }
    paste0(.label, "=", .value)
  }, character(1), USE.NAMES = FALSE), collapse=", ")
  paste0("dataType = {", .inner, "}")
}
#' @export
print.monolix2rxDataSettings <- function(x, ...) {
  cat(as.character.monolix2rxDataSettings(x, ...), "\n", sep="")
}
#' @export
as.list.monolix2rxDataSettings <- function(x, ...) {
  .x <- x
  class(.x) <- NULL
  .x
}
