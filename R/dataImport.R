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
  .occ <- if (length(.content$occ) > 1L) paste0("occ", seq_along(.content$occ)[-1])
  .reserved <- .modelReg[
    tolower(.modelReg) %in% tolower(c(.use1Rx, "cmt", "admd", .occ))
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
#' Drop ignored columns that rxode2 would read as event columns
#'
#' rxode2 matches its event columns (`ss`, `rate`, `evid`, `dose`, ...)
#' ignoring case, so an ignored `SS` column would still be used.  Only
#' `use=ignore` columns are dropped: `[CONTENT]` keeps one column per
#' `use=` type, so the declared columns are not fully known.
#'
#' @param data data.frame as read
#' @param content parsed `[CONTENT]`
#' @return data without those columns
#' @noRd
#' @author Matthew L. Fidler
.dataDropIgnoredEvent <- function(data, content) {
  .event <- c("id", "time", "evid", "amt", "rate", "dur", "ss", "ii", "addl",
              "cmt", "mdv", "dv", "dvid", "cens", "limit", "method", "dose",
              "value", "mixest", "mixunif")
  # a used column is kept even when an ignored one differs only by case
  .ignore <- tolower(content$ignore)
  .used <- c(content$use1, content$cont, names(content$cat), content$reg)
  .drop <- which(tolower(names(data)) %in% .ignore & !(names(data) %in% .used) &
                   tolower(names(data)) %in% .event)
  if (length(.drop) == 0L) return(data)
  .minfo(paste0("dropped ignored data column(s) '",
                paste(names(data)[.drop], collapse="', '"),
                "' that rxode2 would read as event columns"))
  data[, -.drop, drop=FALSE]
}

#' Drop the lines flagged by `use=ignoredline` columns, then the columns
#'
#' Monolix ignores a whole line (dose or observation) with a non-zero flag.
#'
#' @param data data.frame as read
#' @param content parsed `[CONTENT]`
#' @return data without the flagged lines and the flag columns
#' @noRd
#' @author Matthew L. Fidler
.dataDropIgnoredLines <- function(data, content) {
  .w <- which(names(data) %in% content$ignoreLine)
  if (length(.w) == 0L) .w <- which(tolower(names(data)) %in% tolower(content$ignoreLine))
  if (length(.w) == 0L) return(data)
  .flag <- vapply(.w, function(i) {
    .x <- data[[i]]
    .v <- if (is.logical(.x) || is.numeric(.x)) as.numeric(.x) else
      suppressWarnings(as.numeric(as.character(.x)))
    !is.na(.v) & .v != 0
  }, logical(nrow(data)))
  .flag <- if (is.matrix(.flag)) rowSums(.flag) > 0 else any(.flag)
  if (any(.flag)) {
    .minfo(paste0("dropped ", sum(.flag), " line(s) flagged by '",
                  paste(names(data)[.w], collapse="', '"), "' (use=ignoredline)"))
  }
  data[!.flag, -.w, drop=FALSE]
}

#' Translate nested occasion columns
#'
#' With several `use=occasion` columns (nested levels, outermost first)
#' the first becomes `occ` and the k-th `occk`, numbering each combination
#' of the first k columns, so an inner eta is not shared across outer
#' occasions.  These match the `id*occ*...` level names of `.def2iniRenameOcc()`.
#'
#' @param data data.frame as read
#' @param content parsed `[CONTENT]`
#' @return data with the occasion columns translated
#' @noRd
#' @author Matthew L. Fidler
.dataRenameOcc <- function(data, content) {
  .occ <- content$occ
  if (length(.occ) < 2L) return(data)
  .w <- match(.occ, names(data))
  .na <- is.na(.w)
  .w[.na] <- match(tolower(.occ[.na]), tolower(names(data)))
  if (anyDuplicated(.w[!is.na(.w)])) {
    stop("occasion columns '", paste(.occ, collapse="', '"),
         "' match the same data column ignoring case", call.=FALSE)
  }
  if (anyNA(.w)) {
    stop("occasion column(s) missing from the data set: ",
         paste(.occ[is.na(.w)], collapse=", "), call.=FALSE)
  }
  .cols <- lapply(.w, function(i) data[[i]])
  .new <- lapply(seq_along(.cols), function(k) {
    if (k == 1L) return(.cols[[1]])
    .key <- do.call(paste, c(.cols[seq_len(k)], sep="\r"))
    # a missing occasion at any level leaves the combination missing
    .key[Reduce(`|`, lapply(.cols[seq_len(k)], is.na))] <- NA_character_
    .ord <- do.call(order, .cols[seq_len(k)])
    match(.key, unique(.key[.ord][!is.na(.key[.ord])]))
  })
  .to <- c("occ", paste0("occ", seq_along(.w)[-1]))
  for (.k in seq_along(.w)) data[[.w[.k]]] <- .new[[.k]]
  # an unused column named like a translated one is dropped; a used one stops
  .clash <- which(tolower(names(data)) %in% .to & !(seq_along(data) %in% .w))
  .used <- c(content$use1, content$cont, names(content$cat), content$reg)
  if (any(names(data)[.clash] %in% .used)) {
    stop("data column(s) '", paste(intersect(names(data)[.clash], .used), collapse="', '"),
         "' clash with the translated occasion columns", call.=FALSE)
  }
  names(data)[.w] <- .to
  if (length(.clash) > 0L) {
    .minfo(paste0("dropped unused data column(s) '", paste(names(data)[.clash], collapse="', '"),
                  "' that share a translated occasion name"))
    data <- data[, -.clash, drop=FALSE]
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
  data <- .dataDropIgnoredLines(data, .content)
  data <- .dataDropIgnoredEvent(data, .content)
  data <- .dataRenameOcc(data, .content)
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
  data <- .dataFillCovariates(data, .content)
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

#' Fill a subject's missing covariate values with its one value
#'
#' A Monolix covariate is constant within a subject (and occasion), so a
#' missing value on some of its lines (a dose line) is that value.
#' Groups with several values are left as they are.
#'
#' @param data dataset with the translated `id`/`occ` columns
#' @param content parsed `[CONTENT]`
#' @return data with the covariates filled
#' @noRd
#' @author Matthew L. Fidler
.dataFillCovariates <- function(data, content) {
  .cov <- intersect(c(content$cont, names(content$cat)), names(data))
  if (length(.cov) == 0L || is.null(data[["id"]])) return(data)
  .occ <- grep("^occ[0-9]*$", names(data), value=TRUE)
  .g <- do.call(paste, c(lapply(c("id", .occ), function(n) data[[n]]), sep="\r"))
  for (.n in .cov) {
    .x <- data[[.n]]
    .na <- is.na(.x)
    if (!any(.na)) next
    # tapply() would give a factor's codes
    .lvl <- if (is.factor(.x)) levels(.x) else NULL
    if (!is.null(.lvl)) .x <- as.character(.x)
    .val <- tapply(.x[!.na], .g[!.na], function(v) if (length(unique(v)) == 1L) v[1] else NA)
    .fill <- .na & .g %in% names(.val)
    .x[.fill] <- .val[.g[.fill]]
    data[[.n]] <- if (is.null(.lvl)) .x else factor(.x, levels=.lvl)
  }
  data
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
    return(.dataEventCmt(data, ui))
  } # no observationtype in the dataset
  if (is.null(ui$predDf)) {
    return(data[, -.w])
  } # single endpoint; no need to define
  # multiple endpoint
  .dvid <- unique(data$rxMDvid)
  .dvid <- .dvid[!is.na(.dvid)]
  .dvid <- .dvid[!(.dvid %in% seq_along(ui$predDf$cond))]
  .mlxtran <- .monolixGetMlxtran(ui)
  .event <- vapply(.mlxtran$MODEL$LONGITUDINAL$DEFINITION$endpoint,
                   function(e) if (identical(e$dist, "event")) e$var else NA_character_,
                   character(1))
  for (.i in seq_along(ui$predDf$cond)) {
    # only overwrite non-dosing events (ie make sure the cmt is NA)
    .r <- is.na(data$cmt) & data$rxMDvid == .i
    if (ui$predDf$var[.i] %in% .event) {
      # a missing or ignored observation, or a reset, is not an event record
      if (is.null(data[["evid"]])) data <- .dataEvid(data)
      .na <- if (is.null(data[["dv"]])) FALSE else is.na(data$dv)
      .no <- .r & (!(data$evid %in% 0L) | .na)
      data$evid[which(.no & data$evid %in% 0L)] <- 2L
      .r <- .r & !.no
    }
    data$cmt[which(.r)] <- ui$predDf$var[.i]
  }
  for (.i in .dvid) {
    data <- data[-which(is.na(data$cmt) & data$rxMDvid == .i), ]
  }
  data[, -.w]
}

#' A single event endpoint finds its records by cmt
#'
#' @param data dataset without an observation type column
#' @param ui rxode2 ui converted from monolix
#' @return data with the endpoint's cmt on the observation rows
#' @noRd
#' @author Matthew L. Fidler
.dataEventCmt <- function(data, ui) {
  .mlxtran <- .monolixGetMlxtran(ui)
  .e <- .mlxtran$MODEL$LONGITUDINAL$DEFINITION$endpoint
  if (length(.e) != 1L || !identical(.e[[1]]$dist, "event")) return(data)
  .obs <- if (is.null(data[["evid"]])) !.dataIsDose(data) else data$evid %in% 0L
  if (!is.null(data[["mdv"]])) .obs <- .obs & !(data$mdv %in% 1L)
  # a missing observation is not an event record
  if (!is.null(data[["dv"]])) .obs <- .obs & !is.na(data$dv)
  data$cmt[which(.obs & is.na(data$cmt))] <- .e[[1]]$var
  data
}

#' This function converts the cmt dataset to an adm dataset (except the endpoint)
#'
#'
#' @param data input monolix dataset to convert
#' @param admd admd dataset from imported UI
#' @return converted dataset for dosing endpoint (not observations)
#' @noRd
#' @author Matthew L. Fidler
.dataConvertAdm <- function(data, admd, pk=NULL) {
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
    .event <- .dataAdmEvent(.cur, pk)
    if (.cur$admd == 1L) {
      .w <- which(.adm == .cur$adm & is.na(data$admd))
      data$admd[.w] <- 1L
      data$cmt[.w] <- .cmt
      if (isTRUE(.cur$dur)) data <- .dataDurRate(data, .w)
      if (isTRUE(.cur$transit)) data <- .dataTransitEvid(data, .w)
      if (!is.na(.event)) data <- .dataSetEvent(data, .w, .event)
    } else {
      .w <- which(.adm == .cur$adm)
      if (length(.w) > 0L) {
        .dE <- .orig[.w, ]
        .dE$admd <- .cur$admd
        .dE$cmt <- .cmt
        if (isTRUE(.cur$dur)) .dE <- .dataDurRate(.dE, seq_along(.w))
        if (isTRUE(.cur$transit)) .dE <- .dataTransitEvid(.dE, seq_along(.w))
        if (!is.na(.event)) .dE <- .dataSetEvent(.dE, seq_along(.w), .event)
        .extra <- .dataBindFill(.extra, .dE)
      }
    }
  }
  .dataTransitReset(.dataBindFill(data, .extra))
}

#' rxode2 evid of an administration given by `empty()` or `reset()`
#'
#' @param cur one row of the admd table
#' @param pk parsed PK macros (with `$empty` and `$reset`)
#' @return 5 (replace: empty the target), 3 (reset) or NA (a dose)
#' @noRd
#' @author Matthew L. Fidler
.dataAdmEvent <- function(cur, pk) {
  .is <- function(df) {
    !is.null(df) && any(df$adm == cur$adm & df$admd == cur$admd, na.rm=TRUE)
  }
  if (.is(pk$empty)) return(5L)
  if (.is(pk$reset)) return(3L)
  NA_integer_
}

#' The parsed `PK:` block (its `empty()` and `reset()` macros)
#'
#' @param mlxtran parsed mlxtran
#' @return monolix2rxPk object or NULL
#' @noRd
#' @author Matthew L. Fidler
.dataPkMacros <- function(mlxtran) {
  mlxtran$MODEL$LONGITUDINAL$PK
}

#' Make rows an empty (evid 5 with amount 0) or reset (evid 3) event
#'
#' @param data dataset
#' @param w rows to change
#' @param evid 5 or 3
#' @return dataset
#' @noRd
#' @author Matthew L. Fidler
.dataSetEvent <- function(data, w, evid) {
  if (length(w) == 0L) return(data)
  if (is.null(data[["evid"]])) data$evid <- NA_integer_
  data$evid[w] <- evid
  data$amt[w] <- if (evid == 5L) 0 else NA_real_
  for (.v in intersect(c("rate", "dur", "ss", "ii", "addl"), names(data))) data[[.v]][w] <- 0
  if (evid == 3L) data$cmt[w] <- NA_character_
  data
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
  if (!is.null(data[["evid"]])) .ret <- .ret & (is.na(data$evid) | data$evid %in% c(1L, 4L))
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
  for (.n in intersect(c("ii", "addl", "ss"), names(.reset))) .reset[[.n]] <- 0
  .reset$cmt <- NA_character_
  .ret <- rbind(data, .reset)
  .ret <- .ret[order(c(seq_len(nrow(data)), .r - 0.5)), ]
  rownames(.ret) <- NULL
  .ret
}

#' Split a line holding both a dose and an observation
#'
#' Monolix uses both, the dose first; rxode2 reads such a line as a dose
#' only.  With an event id the observation of a dose line is ignored.
#'
#' @param data dataset with rxode2 column names
#' @return data with each shared line as an observation and a dose row
#' @noRd
#' @author Matthew L. Fidler
.dataSplitDoseObs <- function(data) {
  if (is.null(data[["dv"]])) return(data)
  .both <- .dataIsDose(data) & !is.na(data$dv)
  if (!is.null(data[["evid"]])) .both <- .both & is.na(data$evid)
  if (!is.null(data[["mdv"]])) .both <- .both & (is.na(data$mdv) | data$mdv %in% 0L)
  if (!any(.both)) return(data)
  .obs <- data[.both, , drop=FALSE]
  .obs$amt <- NA
  for (.c in intersect(c("rate", "dur", "ss", "ii", "addl"), names(.obs))) .obs[[.c]] <- 0
  if (!is.null(.obs[["evid"]])) .obs$evid <- 0L
  data$dv[.both] <- NA
  if (!is.null(data[["cens"]])) data$cens[.both] <- 0L
  if (!is.null(data[["limit"]])) data$limit[.both] <- NA
  .i <- c(seq_len(nrow(data)), which(.both) + 0.5)
  .ret <- rbind(data, .obs)[order(.i), , drop=FALSE]
  rownames(.ret) <- NULL
  .ret
}
#' Start each subject at its first dose or observation unless the model sets t_0
#'
#' Without `t_0`, Monolix starts the system at a subject's first
#' administration or observation; rxode2 starts at time 0.  An `evid=2` row (a reset as a
#' subject's first record is ignored) and a reset (`evid=3`) at that time
#' start the system there.
#'
#' @param data dataset with the rxode2 columns
#' @param mlxtran parsed mlxtran (with the `EQUATION:` text)
#' @return dataset
#' @noRd
#' @author Matthew L. Fidler
.dataStartReset <- function(data, mlxtran) {
  .eq <- mlxtran$MODEL$LONGITUDINAL$EQUATION$monolix
  if (is.null(.eq) || any(grepl("^[ \t]*t_?0[ \t]*=", strsplit(.eq, "\n")[[1]]))) return(data)
  if (is.null(data[["id"]]) || is.null(data[["time"]]) || nrow(data) == 0L) return(data)
  # an event endpoint counts its hazard from time 0 when the first record
  # is an event (R/discreteEndpoint.R); which start Monolix uses there is
  # to confirm
  if (any(vapply(mlxtran$MODEL$LONGITUDINAL$DEFINITION$endpoint,
                 function(e) identical(e$dist, "event"), logical(1)))) {
    return(data)
  }
  .evid <- if (is.null(data[["evid"]])) .dataEvid(data)$evid else data$evid
  # administrations (doses, transit doses, empty/reset lines) and observations
  .noDv <- if (is.null(data[["dv"]])) rep(FALSE, nrow(data)) else is.na(data$dv)
  .use <- !is.na(data$time) & !is.na(.evid) & .evid != 2L & !(.evid == 0L & .noDv)
  if (!any(.use)) return(data)
  .ids <- unique(as.character(data$id))
  .w <- which(.use)
  .w <- .w[order(match(as.character(data$id[.w]), .ids), data$time[.w], method="radix")]
  # each subject's first record (its regressors and covariates at that time)
  .w <- .w[!duplicated(as.character(data$id[.w]))]
  .w <- .w[data$time[.w] != 0]
  if (length(.w) == 0L) return(data)
  data$evid <- .evid
  .first <- data[.w, , drop=FALSE]
  for (.v in intersect(c("dv", "amt", "cmt", "rate", "dur", "ss", "ii", "addl", "mdv", "cens", "limit"),
                       names(.first))) {
    .first[[.v]] <- switch(.v, mdv=1, cens=0, rate=, dur=, ss=, ii=, addl=0, NA)
  }
  .start <- rbind(transform(.first, evid=2L), transform(.first, evid=3L))
  .start$rxStartOrder <- rep(c(1L, 2L), each=nrow(.first))
  data$rxStartOrder <- 3L
  data <- rbind(.start, data)
  # by id (the data's order) and time; the start rows come first at their time
  data <- data[order(match(as.character(data$id), .ids), data$time, data$rxStartOrder,
                     method="radix"), , drop=FALSE]
  data$rxStartOrder <- NULL
  rownames(data) <- NULL
  data
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
  .evid <- ifelse(.dataIsDose(data), 1L, 0L)
  if (is.null(data[["evid"]])) data$evid <- .evid
  data$evid[is.na(data$evid)] <- .evid[is.na(data$evid)]
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
  data <- .dataSplitDoseObs(data)
  data <- .dataConvertAdm(data, ui$admd, .dataPkMacros(.mlxtran))
  if (!is.null(data[["mdv"]]) || !is.null(data[["evid"]])) data <- .dataEvid(data)
  data <- .dataConvertEndpoints(data, ui)
  .ld <- tolower(names(data))
  .wt <- which(.ld == "time")
  if (length(.wt) == 0) {
    .minfo("added dummy time column")
    data$time <- seq_along(data[, 1])
    return(data)
  } else {
    .wt0 <- which(is.na(data[, .wt]))
    if (length(.wt0) > 0) {
      .minfo("replaced na time with zero")
      data[.wt0, .wt] <- 0
    }
  }
  .dataStartReset(data, .mlxtran)
}
