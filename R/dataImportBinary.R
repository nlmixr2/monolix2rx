#' Read a Monolix 2024 Excel/SAS data set
#'
#' @param file data file name
#' @param ext lower-case file extension
#' @param header header specified in the mlxtran file
#' @inheritParams utils::read.table
#' @return data.frame with the mlxtran header, or NULL when `ext` is a
#'   text format
#' @noRd
#' @author Matthew L. Fidler
.monolixDataLoadBinary <- function(
  file,
  ext,
  header,
  na.strings = c("NA", ".")
) {
  if (length(ext) != 1L) {
    return(NULL)
  }
  .pkg <- switch(
    ext,
    xls = "readxl",
    xlsx = "readxl",
    sas7bdat = "haven",
    xpt = "haven",
    NULL
  )
  if (is.null(.pkg)) {
    return(NULL)
  }
  rxode2::rxReq(.pkg)
  .hasHeader <- TRUE
  if (.pkg == "readxl") {
    .data <- readxl::read_excel(file, na = na.strings)
    if (
      !identical(tolower(names(.data)), tolower(header)) &&
        !all(is.na(suppressWarnings(as.numeric(names(.data)))))
    ) {
      # numeric column names means the sheet has no header row
      .hasHeader <- FALSE
      .data <- readxl::read_excel(file, na = na.strings, col_names = FALSE)
    }
  } else if (ext == "xpt") {
    .data <- haven::read_xpt(file)
  } else {
    .data <- haven::read_sas(file)
  }
  if (.pkg == "haven") {
    .data <- haven::zap_formats(haven::zap_labels(.data))
  }
  .data <- as.data.frame(.data)
  # mirror read.table(): dates stay text, and text columns get na.strings/type conversion
  .naNum <- suppressWarnings(as.numeric(na.strings))
  .naNum <- .naNum[!is.na(.naNum)]
  for (.i in seq_along(.data)) {
    if (is.numeric(.data[[.i]]) && length(.naNum) > 0L) {
      .data[[.i]][.data[[.i]] %in% .naNum] <- NA
    }
    if (inherits(.data[[.i]], c("Date", "POSIXt", "difftime"))) {
      .data[[.i]] <- as.character(.data[[.i]])
    }
    if (is.character(.data[[.i]])) {
      .data[[.i]] <- utils::type.convert(
        .data[[.i]],
        na.strings = na.strings,
        as.is = TRUE
      )
    }
  }
  if (length(header) > 0L && !identical(names(.data), header)) {
    if (length(.data) != length(header)) {
      stop(
        "the length of the headers between the mlxtran specified model and data are different",
        call. = FALSE
      )
    }
    if (.hasHeader) {
      warning(
        "the header does not match what was specified in the mlxtran file, overwriting header with mlxtran specs"
      )
    }
    names(.data) <- header
  }
  .data
}
