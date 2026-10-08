#' Monolix data import can include spaces in NAs, look for these cases
#'
#' @param data input data
#' @param na.strings na strings
#' @param mlxtran mlxtran information
#' @return converted dataset with na AND mutated values
#' @noRd
#' @author Matthew L. Fidler
.monolixNaApply <- function(data, na.strings, mlxtran) {
  .dat <- data
  for (v in names(.dat)) {
    if (tolower(v) %in% c("amt", "time", "dv") && is.character(.dat[[v]])) {
      .n <- suppressWarnings(as.numeric(.dat[[v]]))
      .w <- which(is.na(.n) & !is.nan(.n))
      if (length(.w) == 0) {
        .dat[[v]] <- .n
      } else if (
        # literal match; na.strings are not regular expressions (#56)
        all(trimws(.dat[[v]][.w]) %in% c(trimws(na.strings), "", NA))
      ) {
        .dat[[v]] <- .n
      }
    }
  }
  .dat
}

#' Load data from a mlxtran defined dataset
#'
#' @param mlxtran mlxtran file where data input is specified
#' @inheritParams utils::read.table
#' @noRd
.monolixDataLoad <- function(mlxtran, na.strings = c("NA", ".")) {
  mlxtran <- .monolixGetMlxtran(mlxtran)
  if (is.null(mlxtran)) {
    return(NULL)
  }
  withr::with_dir(.monolixGetPwd(mlxtran), {
    .file <- mlxtran$DATAFILE$FILEINFO$FILEINFO$file
    .try <- try(file.exists(.file), silent = TRUE)
    if (inherits(.try, "try-error")) {
      .try <- FALSE
    }
    if (length(.try) == 0L) {
      .try <- FALSE
    }
    if (.try) {
      .ext <- tolower(regmatches(.file, regexpr("(?<=[.])[^./\\\\]+$", .file, perl=TRUE)))
      .data <- .monolixDataLoadBinary(.file, .ext,
                                      mlxtran$DATAFILE$FILEINFO$FILEINFO$header,
                                      na.strings=na.strings)
      if (!is.null(.data)) return(.monolixNaApply(.data, na.strings, mlxtran))
      .sep <- mlxtran$DATAFILE$FILEINFO$FILEINFO$delimiter
      .sep <- switch(
        .sep,
        comma = ",",
        tab = "\t",
        space = " ",
        semicolon = ";",
        semicolumn = ";"
      )
      .firstLine <- readLines(.file, n = 1)
      .head <- strsplit(.firstLine, .sep, fixed = TRUE)[[1]]
      if (all(.head == mlxtran$DATAFILE$FILEINFO$FILEINFO$header)) {
        # has header (and it matches)
        .data <- utils::read.table(
          .file,
          header = TRUE,
          sep = .sep,
          row.names = NULL,
          na.strings = na.strings
        )
        return(.monolixNaApply(.data, na.strings, mlxtran))
      } else {
        .num <- vapply(
          .head,
          function(v) {
            .n <- suppressWarnings(as.numeric(v))
            if (is.na(.n)) {
              return(FALSE)
            }
            TRUE
          },
          logical(1),
          USE.NAMES = FALSE
        )
        if (all(!.num)) {
          # different header (maybe case mis-match)
          warning(
            "the header does not match what was specified in the mlxtran file, overwriting header with mlxtran specs"
          )
          .data <- utils::read.table(
            .file,
            header = TRUE,
            sep = .sep,
            row.names = NULL,
            na.strings = na.strings
          )
          if (
            length(.data) != length(mlxtran$DATAFILE$FILEINFO$FILEINFO$header)
          ) {
            stop(
              "the length of the headers between the mlxtran specified model and data are different",
              call. = FALSE
            )
          }
          names(.data) <- mlxtran$DATAFILE$FILEINFO$FILEINFO$header
          return(.monolixNaApply(.data, na.strings, mlxtran))
        } else {
          # missing header
          .data <- utils::read.table(
            .file,
            header = FALSE,
            sep = .sep,
            row.names = NULL,
            na.strings = na.strings
          )
          if (
            length(.data) != length(mlxtran$DATAFILE$FILEINFO$FILEINFO$header)
          ) {
            stop(
              "the length of the headers between the mlxtran specified model and data are different",
              call. = FALSE
            )
          }
          names(.data) <- mlxtran$DATAFILE$FILEINFO$FILEINFO$header
          return(.monolixNaApply(.data, na.strings, mlxtran))
        }
      }
    } else {
      return(NULL)
    }
  })
}
# rxode2 names for the Monolix single-use columns
.use1Rx <- c(
  identifier = "id",
  time = "time",
  eventidentifier = "evid",
  amount = "amt",
  interdoseinterval = "ii",
  censored = "cens",
  limit = "limit",
  observationtype = "rxMDvid",
  administration = "adm",
  steadystate = "ss",
  observation = "dv",
  occasion = "occ",
  rate = "rate",
  additionaldose = "addl",
  missingdependentvariable = "mdv",
  infusiontime = "dur"
)
#' Rename the data set regressor columns to the model regressor names
#'
#' Monolix matches the data set regressor columns (`use=regressor` in
#' `[CONTENT]`) to the model regressors (`use=regressor` in
#' `[LONGITUDINAL]`) by their order in the data set, not by name.
#'
#' @param data data.frame after the single-use columns (`id`, `time`,
#'   ...) have been renamed
#' @param mlxtran mlxtran object
#' @param orig the data column names before that renaming
#' @return data with the regressor columns renamed to the model names
#' @noRd
#' @author Matthew L. Fidler
.dataRenameRegressors <- function(data, mlxtran, orig = names(data)) {
  .dataReg <- mlxtran$DATAFILE$CONTENT$CONTENT$reg
  .modelReg <- mlxtran$MODEL$LONGITUDINAL$LONGITUDINAL$reg
  if (length(.dataReg) == 0L && length(.modelReg) == 0L) {
    return(data)
  }
  if (length(.dataReg) != length(.modelReg)) {
    stop(
      "the number of regressors in the data set (",
      length(.dataReg),
      ": ",
      paste(.dataReg, collapse = ", "),
      ") and in the model (",
      length(.modelReg),
      ": ",
      paste(.modelReg, collapse = ", "),
      ") differ; they are matched by order",
      call. = FALSE
    )
  }
  .missing <- setdiff(.dataReg, orig)
  if (length(.missing) > 0L) {
    stop(
      "regressor column(s) missing from the data set: ",
      paste(.missing, collapse = ", "),
      call. = FALSE
    )
  }
  .w <- sort(match(.dataReg, orig))
  .content <- mlxtran$DATAFILE$CONTENT$CONTENT
  # reserved in the rxode2 data set; cmt/admd are added by .dataConvertAdm()
  .reserved <- .modelReg[
    tolower(.modelReg) %in% tolower(c(.use1Rx, "cmt", "admd"))
  ]
  if (length(.reserved) > 0L) {
    stop(
      "model regressor(s) '",
      paste(.reserved, collapse = "', '"),
      "' clash with a translated data column name",
      call. = FALSE
    )
  }
  .used <- setdiff(c(.content$cont, names(.content$cat)), .dataReg)
  .bad <- intersect(.modelReg, .used)
  if (length(.bad) > 0L) {
    stop(
      "model regressor(s) '",
      paste(.bad, collapse = "', '"),
      "' also name a non-regressor data column; cannot match regressors by order",
      call. = FALSE
    )
  }
  names(data)[.w] <- .modelReg
  # columns not declared in [CONTENT] are ignored by Monolix; drop them
  .drop <- which(names(data) %in% .modelReg & !(seq_along(data) %in% .w))
  if (length(.drop) > 0L) {
    .minfo(paste0(
      "dropped unused data column(s) '",
      paste(orig[.drop], collapse = "', '"),
      "' that share a model regressor name"
    ))
    data <- data[, -.drop, drop = FALSE]
  }
  data
}
#' Rename defined items from monolix to rxode2 reserved names
#'
#' This also makes sure the order of factors defined matches what
#' Monolix uses
#'
#' @param data data.frame that needs to be translated
#'
#' @param mlxtran mlxtran file where data input is specified
#'
#' @return translated dataset suitable for rxode2 simulations
#'
#' @noRd
#'
#' @author Matthew L. Fidler
.dataRenameFromMlxtran <- function(data, mlxtran) {
  .content <- mlxtran$DATAFILE$CONTENT$CONTENT
  .use1 <- .content$use1
  .orig <- names(data)
  names(data) <- vapply(
    names(data),
    function(n) {
      .w <- which(n == .use1)
      if (length(.w) == 1L) {
        .n <- names(.use1)[.w]
        return(.use1Rx[[.n]])
      }
      n
    },
    character(1),
    USE.NAMES = FALSE
  )
  data <- .dataRenameRegressors(data, mlxtran, .orig)
  # Make sure continuous are double
  for (.n in .content$cont) {
    if (any(names(data) == .n)) {
      data[[.n]] <- as.double(data[[.n]])
    }
  }
  # Make sure the dvid matches what monolix specified
  if (
    any(names(data) == "rxMDvid") &&
      length(mlxtran$DATAFILE$CONTENT$CONTENT$yname) > 0L
  ) {
    .f <- try(factor(data[["rxMDvid"]], mlxtran$DATAFILE$CONTENT$CONTENT$yname))
    if (!inherits(.f, "try-error")) {
      data[["rxMDvid"]] <- as.integer(.f)
    }
  }
  return(data)
}

#' Convert the endpoint specification in monolix to rxode2
#'
#' @param data dataset that monolix is currently converting
#' @param ui rxode2 ui converted from monolix
#' @return rxode2 compatible dataset; will drop rxMDvid
#' @noRd
#' @author Matthew L. Fidler
.dataConvertEndpoints <- function(data, ui) {
  .w <- which(names(data) == "rxMDvid")
  if (length(.w) != 1L) {
    return(data)
  } # no observationtype in the dataset
  if (is.null(ui$predDf)) {
    return(data[, -.w])
  } # single endpoint; no need to define
  # multiple endpoint
  .dvid <- unique(data$rxMDvid)
  .dvid <- .dvid[!is.na(.dvid)]
  .dvid <- .dvid[!(.dvid %in% seq_along(ui$predDf$cond))]
  for (.i in seq_along(ui$predDf$cond)) {
    # only overwrite non-dosing events (ie make sure the cmt is NA)
    data$cmt[which(is.na(data$cmt) & data$rxMDvid == .i)] <- ui$predDf$var[.i]
  }
  for (.i in .dvid) {
    data <- data[-which(is.na(data$cmt) & data$rxMDvid == .i), ]
  }
  data[, -.w]
}

#' This function converts the cmt dataset to an adm dataset (except the endpoint)
#'
#'
#' @param data input monolix dataset to convert
#' @param admd admd dataset from imported UI
#' @return converted dataset for dosing endpoint (not observations)
#' @noRd
#' @author Matthew L. Fidler
.dataConvertAdm <- function(data, admd) {
  data$cmt <- NA_character_
  data$admd <- NA_integer_
  .dose <- .dataIsDose(data)
  # without an administration column every dose is adm 1; [[ ]] since
  # data$adm partially matches admd
  .adm <- data[["adm"]]
  if (is.null(.adm)) .adm <- 1L
  .adm <- ifelse(.dose, .adm, NA_integer_)
  # later routes copy the doses before any route changed them
  .orig <- data
  .extra <- NULL
  for (i in seq_along(admd$adm)) {
    .cur <- admd[i, ]
    .cmt <- if (is.null(.cur$rxCmt) || is.na(.cur$rxCmt)) .cur$cmt else .cur$rxCmt
    if (.cur$admd == 1L) {
      .w <- which(.adm == .cur$adm & is.na(data$admd))
      data$admd[.w] <- 1L
      data$cmt[.w] <- .cmt
      if (isTRUE(.cur$dur)) data <- .dataDurRate(data, .w)
      if (isTRUE(.cur$transit)) data <- .dataTransitEvid(data, .w)
    } else {
      .w <- which(.adm == .cur$adm)
      if (length(.w) > 0L) {
        .dE <- .orig[.w, ]
        .dE$admd <- .cur$admd
        .dE$cmt <- .cmt
        if (isTRUE(.cur$dur)) .dE <- .dataDurRate(.dE, seq_along(.w))
        if (isTRUE(.cur$transit)) .dE <- .dataTransitEvid(.dE, seq_along(.w))
        .extra <- .dataBindFill(.extra, .dE)
      }
    }
  }
  .dataTransitReset(.dataBindFill(data, .extra))
}

#' Monolix dose rows: an amount, and a dose event when there is an evid column
#'
#' @param data dataset
#' @return logical vector
#' @noRd
#' @author Matthew L. Fidler
.dataIsDose <- function(data) {
  .amt <- data[["amt"]]
  if (is.null(.amt)) return(rep(FALSE, nrow(data)))
  .ret <- !is.na(.amt) & .amt != 0
  if (!is.null(data[["evid"]])) .ret <- .ret & data$evid %in% c(1L, 4L)
  .ret
}

#' rbind when rate/evid were added to only one side
#'
#' @param a,b datasets (a may be NULL)
#' @return rbind of a and b
#' @noRd
#' @author Matthew L. Fidler
.dataBindFill <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.null(b)) return(a)
  .fill <- function(.to, .from) {
    if (!is.null(.from[["evid"]]) && is.null(.to[["evid"]])) .to <- .dataEvid(.to)
    for (.n in setdiff(names(.from), names(.to))) .to[[.n]] <- .from[[.n]][NA_integer_]
    .to
  }
  .a <- .fill(a, b)
  .b <- .fill(b, .a)
  rbind(.a, .b[, names(.a)])
}

#' Zero-order absorption doses (Tk0) are modeled infusions
#'
#' @param data dataset
#' @param w dose rows
#' @return data with rate -2 on the dose rows without a rate
#' @noRd
#' @author Matthew L. Fidler
.dataDurRate <- function(data, w) {
  if (length(w) == 0L) return(data)
  if (is.null(data[["rate"]])) data$rate <- NA_real_
  .w <- w[is.na(data$rate[w]) | data$rate[w] == 0]
  # a data infusion time wins
  if (!is.null(data[["dur"]])) .w <- .w[is.na(data$dur[.w]) | data$dur[.w] == 0]
  data$rate[.w] <- -2
  data
}

#' Transit doses only start transit(); rxode2 evid=7 keeps them out of the depot
#'
#' @param data dataset
#' @param w dose rows
#' @return data with evid 7 on the dose rows
#' @noRd
#' @author Matthew L. Fidler
.dataTransitEvid <- function(data, w) {
  if (length(w) == 0L) return(data)
  if (is.null(data[["evid"]])) data <- .dataEvid(data)
  # EVID=4 also resets: .dataTransitReset() adds the reset before the dose
  .w4 <- w[data$evid[w] %in% 4L]
  if (length(.w4) > 0L) {
    if (is.null(data[["rxTransitReset"]])) data$rxTransitReset <- FALSE
    data$rxTransitReset[.w4] <- TRUE
  }
  .w <- w[is.na(data$evid[w]) | data$evid[w] %in% c(1L, 4L)]
  data$evid[.w] <- 7L
  data
}

#' Insert an evid=3 reset before each transit dose that was EVID=4
#'
#' @param data dataset
#' @return data without the rxTransitReset column
#' @noRd
#' @author Matthew L. Fidler
.dataTransitReset <- function(data) {
  .r <- data[["rxTransitReset"]]
  if (is.null(.r)) return(data)
  .r <- which(!is.na(.r) & .r)
  data$rxTransitReset <- NULL
  if (length(.r) == 0L) return(data)
  .reset <- data[.r, ]
  .reset$evid <- 3L
  .reset$amt <- NA
  .reset$cmt <- NA_character_
  .ret <- rbind(data, .reset)
  .ret <- .ret[order(c(seq_len(nrow(data)), .r - 0.5)), ]
  rownames(.ret) <- NULL
  .ret
}

#' Monolix doses are the rows with an amount; MDV=1 only drops an observation
#'
#' Without an evid column rxode2 would read an MDV=1 row as a dose.
#'
#' @param data dataset
#' @return data with an evid column
#' @noRd
#' @author Matthew L. Fidler
.dataEvid <- function(data) {
  if (is.null(data[["evid"]])) data$evid <- ifelse(.dataIsDose(data), 1L, 0L)
  if (!is.null(data[["mdv"]])) {
    data$evid[data$evid == 0L & !is.na(data$mdv) & data$mdv == 1L] <- 2L
  }
  data
}

#' Import a dataset from monolix (based on an imported model)
#'
#' @param ui A rxode2 ui model imported from monolix
#' @param data dataset to convert to nlmixr2 format; if the dataset is
#'   missing, load from the mlxtran specification
#' @inheritParams utils::read.table
#' @return Dataset appropriate for using with the rxode2 model for
#'   simulations
#' @noRd
#' @author Matthew L. Fidler
#' @examples
#'
#' # First load in the model; in this case the theo model
#' # This is modified from the Monolix demos by saving the model
#' # File as a text file (hence you can access without model library)
#' # setup.
#' #
#' # This example is also included in the monolix2rx package, so
#' # you refer to the location with `system.file()`:
#'
#' pkgTheo <- system.file("theo", package="monolix2rx")
#'
#' mod <- monolix2rx(file.path(pkgTheo, "theophylline_project.mlxtran"))
#'
#' # read in monolix dataset
#'
#' dat <- read.table(file.path(pkgTheo, "data", "theophylline_data.txt"),na=".", header=TRUE)
#'
#' monolixDataImport(mod, dat)
#'
monolixDataImport <- function(ui, data, na.strings = c("NA", ".")) {
  if (!missing(data)) {
    checkmate::assertDataFrame(data)
  }
  rxui <- rxode2::assertRxUi(ui)
  if (is.null(ui$admd)) {
    stop(
      "to convert dataset to an rxode2 compatible dataset the model needs to be imported from monolix",
      call. = FALSE
    )
  }
  .mlxtran <- .monolixGetMlxtran(ui)
  if (is.null(.mlxtran)) {
    stop("monolixDataImport error found")
  }
  if (missing(data)) {
    data <- .monolixDataLoad(.mlxtran, na.strings = na.strings)
  }
  if (is.null(data)) {
    return(NULL)
  }
  data <- .dataRenameFromMlxtran(data, .mlxtran)
  data <- .dataConvertAdm(data, ui$admd)
  if (!is.null(data[["mdv"]])) data <- .dataEvid(data)
  data <- .dataConvertEndpoints(data, ui)
  .ld <- tolower(names(data))
  .wt <- which(.ld == "time")
  if (length(.wt) == 0) {
    .minfo("added dummy time column")
    data$time <- seq_along(data[, 1])
  } else {
    .wt0 <- which(is.na(data[, .wt]))
    if (length(.wt0) > 0) {
      .minfo("replaced na time with zero")
      data[.wt0, .wt] <- 0
    }
  }
  data
}
