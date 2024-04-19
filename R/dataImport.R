#' Load data from a mlxtran defined dataset
#'
#' @param mlxtran mlxtran file where data input is specified
#' @inheritParams utils::read.table
#' @noRd
.monolixDataLoad <- function(mlxtran, na.strings=c("NA", ".")) {
  if (inherits(mlxtran, "rxUi")) mlxtran <- mlxtran$mlxtran
  if (is.null(mlxtran)) return(NULL)
  withr::with_dir(.monolixGetPwd(mlxtran), {
    .file <- mlxtran$DATAFILE$FILEINFO$FILEINFO$file
    .try <- try(file.exists(.file), silent=TRUE)
    if (inherits(.try, "try-error")) .try <- FALSE
    if (.try) {
      .sep <- mlxtran$DATAFILE$FILEINFO$FILEINFO$delimiter
      .sep <- switch(.sep,
                     comma=",",
                     tab="\t",
                     space=" ",
                     semicolon=";",
                     semicolumn=";")
      .firstLine <- readLines(.file, n=1)
      .head <- strsplit(.firstLine, .sep, fixed=TRUE)[[1]]
      if (all(.head == mlxtran$DATAFILE$FILEINFO$FILEINFO$header)) {
        # has header (and it matches)
        .data <- utils::read.table(.file, header = TRUE, sep=.sep, row.names=NULL,
                            na.strings=na.strings)
        return(.data)
      } else {
        .num <- vapply(.head,
                       function(v) {
                         .n <- suppressWarnings(as.numeric(v))
                         if (is.na(.n)) return(FALSE)
                         TRUE
                       }, logical(1), USE.NAMES=FALSE)
        if (all(!.num)) {
          # different header (maybe case mis-match)
          warning("the header does not match what was specified in the mlxtran file, overwriting header with mlxtran specs")
          .data <- utils::read.table(.file, header = TRUE, sep=.sep, row.names=NULL,
                              na.strings=na.strings)
          if (length(.data) != length(mlxtran$DATAFILE$FILEINFO$FILEINFO$header)) {
            stop("the length of the headers between the mlxtran specified model and data are different",
                 call.=FALSE)
          }
          names(.data) <- mlxtran$DATAFILE$FILEINFO$FILEINFO$header
          return(.data)
        } else {
          # missing header
          .data <- utils::read.table(.file, header = FALSE, sep=.sep, row.names=NULL,
                              na.strings=na.strings)
          if (length(.data) != length(mlxtran$DATAFILE$FILEINFO$FILEINFO$header)) {
            stop("the length of the headers between the mlxtran specified model and data are different",
                 call.=FALSE)
          }
          names(.data) <- mlxtran$DATAFILE$FILEINFO$FILEINFO$header
          return(.data)
        }
      }
    } else {
      return(NULL)
    }
  })
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
  names(data) <- vapply(names(data),
                        function(n) {
                          .w <- which(n == .use1)
                          if (length(.w) == 1L) {
                            .n <- names(.use1)[.w]
                            return(switch(.n,
                                          identifier="id",
                                          time="time",
                                          eventidentifier="evid",
                                          amount="amt",
                                          interdoseinterval="ii",
                                          censored="cens",
                                          limit="limit",
                                          observationtype="rxMDvid",
                                          administration="adm",
                                          steadystate="ss",
                                          observation="dv",
                                          occasion="occ",
                                          rate="rate",
                                          additionaldose="addl",
                                          missingdependentvariable="mdv"))
                          }
                          n
                        }, character(1), USE.NAMES = FALSE)
  # Make sure factors match what monolix defined
  for (.i in seq_along(.content$cat)) {
    .n <- names(.content$cat)[.i]
    if (any(names(data) == .n)) {
      data[[.n]] <- factor(paste(data[[.n]]), levels=.content$cat[[.i]]$cat)
    }
  }
  # Make sure continuous are double
  for (.i in seq_along(.content$cont)) {
    .n <- names(.content$cat)[.i]
    if (any(names(data) == .n)) {
      data[[.n]] <- as.double(data[[.n]])
    }
  }
  # Make sure the dvid matches what monolix specified
  if (any(names(data) == "rxMDvid") &&
        length(mlxtran$DATAFILE$CONTENT$CONTENT$yname) > 0L) {
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
  if (length(.w) != 1L) return(data) # no observationtype in the dataset
  if (is.null(ui$predDf)) return(data[, -.w]) # single endpoint; no need to define
  # multiple endpoint
  for (.i in seq_along(ui$predDf$cond)) {
    # only overwrite non-dosing events (ie make sure the cmt is NA)
    data$cmt[which(is.na(data$cmt) & data$rxMDvid == .i)] <- ui$predDf$var[.i]
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
  .extra <- NULL
  for (i in seq_along(admd$adm)) {
    .cur <- admd[i, ]
    if (.cur$admd == 1L) {
      .w <- which(data$adm == .cur$adm & is.na(data$admd))
      data$admd[.w] <- 1L
      data$cmt[.w] <- .cur$cmt
    } else {
      # add an extra item
      .w <- which(data$adm == .cur$adm & data$admd == 1L)
      .dE <- data[.w, ]
      .dE$admd <- .cur$admd
      .dE$cmt[.w] <- .cur$cmt
      .extra <- rbind(.extra, .dE)
    }
  }
  rbind(data, .extra)
}

#' Import a dataset from monolix (based on an imported model)
#'
#' @param ui A rxode2 ui model imported from monolix
#' @param data dataset to convert to nlmixr2 format; if the dataset is
#'   missing, load from the mlxtran specification
#' @inheritParams utils::read.table
#' @return Dataset appropriate for using with the rxode2 model for
#'   simulations
#' @export
#' @author Matthew L. Fidler
#' @examples
monolixDataImport <- function(ui, data, na.strings=c("NA", ".")) {
  rxui <- rxode2::assertRxUi(ui)
  if (is.null(ui$admd)) {
    stop("to convert dataset to an rxode2 compatible dataset the model needs to be imported from monolix",
         call.=FALSE)
  }
  if (missing(data)) {
    data <- .monolixDataLoad(ui$mlxtran, na.strings=na.strings)
  }
  if (is.null(data)) return(NULL)
  data <- .dataRenameFromMlxtran(data, ui$mlxtran)
  data <- .dataConvertAdm(data, ui$admd)
  data <- .dataConvertEndpoints(data, ui)
  data
}
