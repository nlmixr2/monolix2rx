#' Get the monolix project working direcotry
#'
#' @param x monolix2rx object to get the working directory
#' @return working directory for the current project
#' @noRd
#' @author Matthew L. Fidler
.monolixGetPwd <- function(x) {
  x <- .monolixGetMlxtran(x)
  if (!inherits(x, "monolix2rxMlxtran")) return(getwd())
  .wd <- attr(x, "dirn")
  if (checkmate::testDirectoryExists(.wd)) return(.wd)
  getwd()
}
#' Get the best mlxtran info or return NULL
#'
#' @param x item to try to extract mlxtran info
#' @return mlxtran info
#' @export
#' @keywords internal
#' @author Matthew L. Fidler
.monolixGetMlxtran <- function(x) {
  if (inherits(x, "raw")) x <- rxode2::rxUiDecompress(x)
  if (inherits(x, "rxUi") && exists("mlxtran", x)) {
    return(get("mlxtran", x))
  }
  if (inherits(x, "monolix2rxMlxtran")) return(x)
  if (inherits(x, "character")) return(mlxtran(x))
  x
}


#' Parse population parameter estimates from summary.txt
#'
#' Reads the "ESTIMATION OF THE POPULATION PARAMETERS" (Monolix 2020+) or
#' "POPULATION PARAMETERS ESTIMATION" (pre-2020) section of a Monolix
#' summary.txt and returns a data frame with columns \code{parameter} and
#' \code{value}.  Only lines of the form \code{name : token} are extracted;
#' sub-headers, blank lines, and timing lines are ignored.
#'
#' The value is the first whitespace-delimited token after the colon, covering
#' plain decimals, negatives, and scientific notation (e.g. \code{1e-06},
#' \code{4.14e+03}).  When Monolix could not estimate a parameter it writes
#' \code{nan}, \code{NA}, or \code{NaN} as the value token; those rows are
#' included with \code{NA_real_} so the caller can decide how to handle them.
#'
#' @param summaryFile path to summary.txt
#' @return data frame with columns \code{parameter} (character) and
#'   \code{value} (numeric, \code{NA} when the estimate is missing), or
#'   \code{NULL} when the section is not found or contains no parseable lines
#' @noRd
#' @author Matthew L. Fidler
.parameterUpdateFromSummary <- function(summaryFile) {
  .lines <- readLines(summaryFile, warn = FALSE)
  # Both heading styles; the older one has trailing underscores on the same line
  .secPat <- "POPULATION PARAMETERS ESTIMATION|ESTIMATION OF THE POPULATION PARAMETERS"
  .start <- grep(.secPat, .lines)
  if (length(.start) == 0L) return(NULL)
  .start <- .start[1L]
  # Section ends at the first *separate* line of >= 10 underscores after the header
  .sepLines <- grep("^_{10,}", .lines)
  .end <- .sepLines[.sepLines > .start]
  .end <- if (length(.end) > 0L) .end[1L] - 1L else length(.lines)
  .sec <- .lines[seq(.start + 1L, .end)]
  # Match "name : token" -- capture the first token after the colon as-is so
  # that nan/NA/NaN value estimates are preserved (SE column is never read).
  .pat <- "^ *([A-Za-z][A-Za-z0-9_.]*) *: *([^ ]+)"
  .m <- regmatches(.sec, regexec(.pat, .sec))
  .keep <- lengths(.m) == 3L
  if (!any(.keep)) return(NULL)
  .m <- .m[.keep]
  .names  <- vapply(.m, `[[`, character(1), 2L)
  .tokens <- vapply(.m, `[[`, character(1), 3L)
  # as.numeric converts valid numbers, and also nan/NaN -> NaN, NA -> NA;
  # normalise NaN to NA_real_ for consistency
  .vals <- suppressWarnings(as.numeric(.tokens))
  .vals[is.nan(.vals)] <- NA_real_
  # Drop lines that still failed to parse (e.g. sub-headers that matched the
  # name pattern but had a non-numeric, non-nan token such as a dash)
  .parseable <- !is.na(.vals) | grepl("^[Nn][Aa][Nn]$|^NA$|^NaN$", .tokens)
  if (!any(.parseable)) return(NULL)
  data.frame(
    parameter = .names[.parseable],
    value     = .vals[.parseable],
    stringsAsFactors = FALSE
  )
}

#' Apply a parameter lookup table to an mlxtran PARAMETER block
#'
#' @param mlx monolix2rxMlxtran object
#' @param popPar data frame with columns \code{parameter} and \code{value}
#' @return updated mlxtran object
#' @noRd
#' @author Matthew L. Fidler
.parameterApply <- function(mlx, popPar) {
  mlx$PARAMETER$PARAMETER$value <-
    vapply(seq_along(mlx$PARAMETER$PARAMETER$name),
           function(i) {
             .n <- mlx$PARAMETER$PARAMETER$name[i]
             .w <- which(popPar$parameter == .n)
             if (length(.w) == 1L) return(popPar[.w, "value"])
             mlx$PARAMETER$PARAMETER$value[i]
           }, numeric(1), USE.NAMES = FALSE)
  mlx
}

#' Update the parameters based on final output
#'
#' Tries to read final population parameter estimates from
#' \file{populationParameters.txt} in the run export directory.  When that
#' file does not exist, falls back to parsing the same estimates out of
#' \file{summary.txt} with a warning, because the values there are rounded
#' and may be less precise than those in \file{populationParameters.txt}.
#'
#' @param mlx mlxtran object to update
#' @return updated mlxtran object where the values are updated to the final
#'   model values
#' @noRd
#' @author Matthew L. Fidler
.parameterUpdate <- function(mlx) {
  mlx <- .monolixGetMlxtran(mlx)
  if (is.null(mlx)) return(invisible())
  .wd <- .monolixGetPwd(mlx)
  withr::with_dir(.wd, {
    .exportPath <- mlx$MONOLIX$SETTINGS$GLOBAL$exportpath
    .popParFile <- file.path(.exportPath, "populationParameters.txt")
    if (file.exists(.popParFile)) {
      .minfo(paste0("updating model values to final parameter estimates from ",
                    .popParFile))
      .popPar <- read.csv(.popParFile)
      mlx <- .parameterApply(mlx, .popPar)
      .minfo("done")
    } else {
      .summaryFile <- file.path(.exportPath, "summary.txt")
      if (file.exists(.summaryFile)) {
        warning("populationParameters.txt not found; reading parameter estimates ",
                "from summary.txt -- values may be less precise due to rounding",
                call. = FALSE)
        .popPar <- .parameterUpdateFromSummary(.summaryFile)
        if (!is.null(.popPar)) {
          .minfo(paste0("updating model values to final parameter estimates from ",
                        .summaryFile))
          mlx <- .parameterApply(mlx, .popPar)
          .minfo("done")
        }
      }
    }
  })
  mlx
}
