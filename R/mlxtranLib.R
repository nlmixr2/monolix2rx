#' Recursive function to find a monolix model library file
#'
#' @param lib library file to serach (recursively)
#' @param file model library file (without the `lib:` prefix)
#' @return fully qualified file for model translation or NULL if not found.
#' @noRd
#' @author Matthew L. Fidler
.mlxtranGetLib <- function(lib, file)  {
  .files <- list.files(lib)
  .w <- which(tolower(.files) == tolower(file))
  if (length(.w) == 1L) {
    return(file.path(lib, .files[.w]))
  }
  if (any(.files == file)) return(file.path(lib, file))
  for (.f in .files) {
    .lib <- file.path(lib, .f)
    if (dir.exists(.lib)) {
      .ret <- .mlxtranGetLib(.lib, file)
      if (!is.null(.ret)) return(.ret)
    }
  }
  NULL
}
#' This will expand the file from the `lib:` to the file (if possible)
#'
#' @param file string of the file name; if it begins with lib: it will
#'   look in the monolix2rx.library directory (if setup)
#' @return the file name, possibly expanded into the library file name
#' @noRd
#' @author Matthew L. Fidler
.mlxtranLib <- function(file) {
  if (!checkmate::testCharacter(file, min.chars = 5, len=1)) return(file)
  .nc <- nchar(file)
  .pre <- substr(file, 1, 4)
  if (.pre != "lib:") return(file)
  .dir <- getOption("monolix2rx.library", NULL)
  if (!checkmate::testDirectoryExists(.dir)) {
    warning("model is from monolix model library; need to save file to translate (or setup library)",
            call.=FALSE)
    return(file)
  }
  .dir <- sub("[/\\]$", "", .dir)
  .file <- trimws(substr(file, 5, .nc))
  .file <- .mlxtranGetLib(.dir, .file)
  if (is.null(.file)) {
    warning("while options('monolix2rx.library') is set, could not find model file '", file, "'\nplease save the model to translate")
    return(file)
  }
  return(.file)
}
