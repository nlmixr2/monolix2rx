#' Initialize PK parsing
#'
#'
#' @param full this is a boolean to do a full reset
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.pkIni <- function(full=TRUE) {
  if (full) {
    .monolix2rx$pkCc <- NA_character_
    .monolix2rx$pkCe <- NA_character_
    .monolix2rx$pkPars <- c(V=NA_character_,
                            Tk0=NA_character_,
                            ka=NA_character_,
                            Ktr=NA_character_,
                            Mtt=NA_character_,
                            Tlag=NA_character_,
                            p=NA_character_,
                            k=NA_character_,
                            Cl=NA_character_,
                            Vm=NA_character_,
                            Km=NA_character_,
                            k12=NA_character_,
                            k21=NA_character_,
                            k13=NA_character_,
                            k31=NA_character_,
                            ke0=NA_character_)

    .monolix2rx$pkCmt <- data.frame(cmt=integer(0),
                                    amount=character(0),
                                    volume=character(0),
                                    concentration=character(0))

    .monolix2rx$pkPerip <- data.frame(in.i=integer(0),
                                      in.j=integer(0),
                                      in.eq=character(0),
                                      out.i=integer(0),
                                      out.j=integer(0),
                                      out.eq=character(0),
                                      amount=character(0),
                                      volume=character(0),
                                      concentration=character(0))

    .monolix2rx$pkEffect <- data.frame(cmt=integer(0),
                                       ke0=character(0),
                                       concentration=character(0))

    .monolix2rx$pkTransfer <- data.frame(from=integer(0),
                                         to=integer(0),
                                         kt=character(0))

    .monolix2rx$pkDepot <- data.frame(adm=integer(0),
                                      target=character(0),
                                      Tlag=character(0),
                                      p=character(0),
                                      Tk0=character(0),
                                      ka=character(0),
                                      Ktr=character(0),
                                      Mtt=character(0))

    .monolix2rx$pkOral <- data.frame(adm=integer(0),
                                     cmt=integer(0),
                                     Tlag=character(0),
                                     p=character(0),
                                     Tk0=character(0),
                                     ka=character(0),
                                     Ktr=character(0),
                                     Mtt=character(0))

    .monolix2rx$pkIv <- data.frame(adm=integer(0),
                                   cmt=integer(0),
                                   Tlag=character(0),
                                   p=character(0))
    .monolix2rx$pkEmpty <- data.frame(adm=integer(0),
                                      target=character(0))
    .monolix2rx$pkReset <- data.frame(adm=integer(0),
                                      target=character(0))
    .monolix2rx$pkElimination <- data.frame(cmt=integer(0),
                                            V=character(0),
                                            k=character(0),
                                            Cl=character(0),
                                            Vm=character(0),
                                            Km=character(0))
  }
  .monolix2rx$pkStatement <- NA_character_
  .monolix2rx$curPkPar <- NA_character_
  .monolix2rx$curCmt <- data.frame(cmt=NA_integer_,
                                   amount=NA_character_,
                                   volume=NA_character_,
                                   concentration=NA_character_)

  .monolix2rx$curPerip <- data.frame(in.i=NA_integer_,
                                     in.j=NA_integer_,
                                     in.eq=NA_character_,
                                     out.i=NA_integer_,
                                     out.j=NA_integer_,
                                     out.eq=NA_character_,
                                     amount=NA_character_,
                                     volume=NA_character_,
                                     concentration=NA_character_)

  .monolix2rx$curEffect <- data.frame(cmt=NA_integer_,
                                      ke0=NA_character_,
                                      concentration=NA_character_)

  .monolix2rx$curTransfer <- data.frame(from=NA_integer_,
                                        to=NA_integer_,
                                        kt=NA_character_)

  .monolix2rx$curDepot <- data.frame(adm=NA_integer_,
                                     target=NA_character_,
                                     Tlag=NA_character_,
                                     p=NA_character_,
                                     Tk0=NA_character_,
                                     ka=NA_character_,
                                     Ktr=NA_character_,
                                     Mtt=NA_character_)

  .monolix2rx$curOral <- data.frame(adm=NA_integer_,
                                    cmt=NA_integer_,
                                    Tlag=NA_character_,
                                    p=NA_character_,
                                    Tk0=NA_character_,
                                    ka=NA_character_,
                                    Ktr=NA_character_,
                                    Mtt=NA_character_)

  .monolix2rx$curIv <- data.frame(adm=NA_integer_,
                                  cmt=NA_integer_,
                                  Tlag=NA_character_,
                                  p=NA_character_)

  .monolix2rx$curEmpty <- data.frame(adm=NA_integer_,
                                     target=NA_character_)
  .monolix2rx$curReset <- data.frame(adm=NA_integer_,
                                     target=NA_character_)
  .monolix2rx$curElimination <- data.frame(cmt=NA_integer_,
                                           V=NA_character_,
                                           k=NA_character_,
                                           Cl=NA_character_,
                                           Vm=NA_character_,
                                           Km=NA_character_)
}
#' Pushes the Pk information based on current statement
#'
#' @return nothing, called for side effect
#' @noRd
#' @author Matthew L. Fidler
.pkPushStatement <- function() {
  if (is.na(.monolix2rx$pkStatement)) {
    .pkIni(FALSE)
    return(invisible())
  }
  if (.monolix2rx$pkStatement == "compartment") {
    .monolix2rx$pkCmt <- rbind(.monolix2rx$pkCmt,
                               .monolix2rx$curCmt)
    .pkIni(FALSE)
    return(invisible())
  }
  if (.monolix2rx$pkStatement == "peripheral") {
    .monolix2rx$pkPerip <- rbind(.monolix2rx$pkPerip,
                                 .monolix2rx$curPerip)
    .pkIni(FALSE)
    return(invisible())
  }
  if (.monolix2rx$pkStatement == "effect") {
    .monolix2rx$pkEffect <- rbind(.monolix2rx$pkEffect,
                                  .monolix2rx$curEffect)
    .pkIni(FALSE)
    return(invisible())
  }
  if (.monolix2rx$pkStatement == "transfer") {
    .monolix2rx$pkTransfer <- rbind(.monolix2rx$pkTransfer,
                                     .monolix2rx$curTransfer)
    .pkIni(FALSE)
    return(invisible())
  }
  if (.monolix2rx$pkStatement == "depot") {
    .monolix2rx$pkDepot <- rbind(.monolix2rx$pkDepot,
                                 .monolix2rx$curDepot)
    .pkIni(FALSE)
    return(invisible())
  }
  if (.monolix2rx$pkStatement == "oral") {
    .monolix2rx$pkOral <- rbind(.monolix2rx$pkOral,
                                .monolix2rx$curOral)
    .pkIni(FALSE)
    return(invisible())
  }
  if (.monolix2rx$pkStatement == "iv") {
    .monolix2rx$pkIv <- rbind(.monolix2rx$pkIv,
                                .monolix2rx$curIv)
    .pkIni(FALSE)
    return(invisible())
  }
  if (.monolix2rx$pkStatement == "empty") {
    .monolix2rx$pkEmpty <- rbind(.monolix2rx$pkEmpty,
                                 .monolix2rx$curEmpty)
    .pkIni(FALSE)
    return(invisible())
  }
  if (.monolix2rx$pkStatement == "reset") {
    .monolix2rx$pkReset <- rbind(.monolix2rx$pkReset,
                                 .monolix2rx$curReset)
    .pkIni(FALSE)
    return(invisible())
  }
  if (.monolix2rx$pkStatement == "elimination") {
    .monolix2rx$pkElimination <- rbind(.monolix2rx$pkElimination,
                                       .monolix2rx$curElimination)
    .pkIni(FALSE)
    return(invisible())
  }
}
#' Validate the pkmodel
#'
#' @param pkmodel PK model character vector
#' @param Ce effect concentration (if it exists)
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.validatePkModel <- function(pkmodel, Ce) {
  if (is.na(pkmodel["V"])) {
    stop("pkmodel() requires a volume 'V'",
         call.=FALSE)
  }
  if (!is.na(pkmodel["Tk0"])) {
    # Excludes ka, Ktr and Mtt.
    if (!is.na(pkmodel["ka"]) ||
          !is.na(pkmodel["Ktr"]) ||
           !is.na(pkmodel["Mtt"])) {
      stop("pkmodel defines a zero order absorption duration ('Tk0') and cannot also define 'ka', 'Ktr', and/or 'Mtt'",
           call.=FALSE)
    }
  }
  if (!is.na(pkmodel["k"])) {
    if (!is.na(pkmodel["Cl"]) ||
          !is.na(pkmodel["Vm"]) ||
           !is.na(pkmodel["Km"])) {
      stop("pkmodel defines an elimination constant ('k') and cannot also define 'Cl', 'Vm', and/or 'Km'",
           call.=FALSE)
    }
  }
  if (!is.na(pkmodel["Cl"])) {
    if (!is.na(pkmodel["Vm"]) ||
          !is.na(pkmodel["Km"])) {
      stop("pkmodel defines an elimination constant ('Cl') and cannot also define 'Vm', and/or 'Km'",
           call.=FALSE)
    }
  }
  if (!is.na(Ce) && is.na(pkmodel["ke0"])) {
    stop("pkmodel ke0 not defined but Ce is defined",
         call.=FALSE)
  }
}

#' Parse PK
#'
#' @param text pk macro parse text
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.pk <- function(text) {
  .pkIni(TRUE)
  .Call(`_monolix2rx_trans_mlxtran_pk`, text)
  .pkPushStatement()
  .validatePkModel(.monolix2rx$pkPars, .monolix2rx$pkCe)
  .ret <- list(Cc=.monolix2rx$pkCc,
               Ce=.monolix2rx$pkCe,
               pkmodel=.monolix2rx$pkPars,
               compartment=.monolix2rx$pkCmt,
               peripheral=.monolix2rx$pkPerip,
               effect=.monolix2rx$pkEffect,
               transfer=.monolix2rx$pkTransfer,
               depot=.monolix2rx$pkDepot,
               oral=.monolix2rx$pkOral,
               iv=.monolix2rx$pkIv,
               empty=.monolix2rx$pkEmpty,
               reset=.monolix2rx$pkReset,
               elimination=.monolix2rx$pkElimination)
  class(.ret) <- "monolix2rxPk"
  .ret
}
#' This sets the k## or k#_# for periphal macro
#'
#' @param knum knum text (text without k prefix)
#' @return nothing called for side effects
#' @noRd
#' @author Matthew L. Fidler
.pkSetK <- function(knum) {
  .nc <- nchar(knum)
  if (.nc == 2L) {
    .i <- as.integer(substr(knum, 1, 1))
    .j <- as.integer(substr(knum, 2, 2))
  } else {
    .v <- strsplit(knum,"_")[[1]]
    .i <- as.integer(.v[1])
    .j <- as.integer(.v[2])
  }
  if (is.na(.monolix2rx$curPerip$in.i)) {
    .monolix2rx$curPerip$in.i <- .i
    .monolix2rx$curPerip$in.j <- .j
    .pkParDeclare("in.eq")
  } else if (is.na(.monolix2rx$curPerip$out.i)) {
    .monolix2rx$curPerip$out.i <- .i
    .monolix2rx$curPerip$out.j <- .j
    .pkParDeclare("out.eq")
  } else {
    stop("more than 2 k## expressions in peripheral() macro",
         call.=FALSE)
  }
}
#' Set the Cc for the pkmacro
#'
#' @param cc string for the central concentration
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.pkSetCc <- function(cc) {
  .pkPushStatement()
  .monolix2rx$pkCc <- cc
  .monolix2rx$pkStatement <- "pkmodel"
}
#' Set Ce for effect compartment
#'
#' @param ce effect compartment
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.pkSetCe <- function(ce) {
  .pkPushStatement()
  .monolix2rx$pkCe <- ce
  .monolix2rx$pkStatement <- "pkmodel"
}
#' Assign PK statement based on the statement being processed
#'
#' @param par parameter name
#' @param val parameter value
#' @return boolean saying if the value was assigned (`TRUE`) or not (`FALSE`).
#' @noRd
#' @author Matthew L. Fidler
.pkAssignBasedOnValue <- function(par, val) {
  if (.monolix2rx$pkStatement == "pkmodel") {
    .monolix2rx$pkPars[par] <- val
    return(invisible(TRUE))
  }
  if (.monolix2rx$pkStatement == "compartment") {
    .monolix2rx$curCmt[par] <- val
    return(invisible(TRUE))
  }
  if (.monolix2rx$pkStatement == "peripheral") {
    .monolix2rx$curPerip[par] <- val
    return(invisible(TRUE))
  }
  if (.monolix2rx$pkStatement == "effect") {
    .monolix2rx$curEffect[par] <- val
    return(invisible(TRUE))
  }
  if (.monolix2rx$pkStatement == "transfer") {
    .monolix2rx$curTransfer[par] <- val
    return(invisible(TRUE))
  }
  if (.monolix2rx$pkStatement == "depot") {
    .monolix2rx$curDepot[par] <- val
    return(invisible(TRUE))
  }
  if (.monolix2rx$pkStatement == "oral") {
    .monolix2rx$curOral[par] <- val
    return(invisible(TRUE))
  }
  if (.monolix2rx$pkStatement == "iv") {
    .monolix2rx$curIv[par] <- val
    return(invisible(TRUE))
  }
  if (.monolix2rx$pkStatement == "empty") {
    .monolix2rx$curEmpty[par] <- val
    return(invisible(TRUE))
  }
  if (.monolix2rx$pkStatement == "reset") {
    .monolix2rx$curReset[par] <- val
    return(invisible(TRUE))
  }
  if (.monolix2rx$pkStatement == "elimination") {
    .monolix2rx$curElimination[par] <- val
    return(invisible(TRUE))
  }
  return(invisible(FALSE))
}
#' Declare a parameter was defined
#'
#' @param par parameter that was defined
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.pkParDeclare <- function(par) {
  .monolix2rx$curPkPar <- par
  .isInt <- FALSE
  if (par %in% c("cmt", "adm", "from", "to")) {
    .val <- 1L
  } else  {
    .val <- ""
  }
  if (!.pkAssignBasedOnValue(par, .val)) {
    stop("unexpected pk declaration for '", par, "' in '", .monolix2rx$pkStatement, "'", call. = FALSE)
  } else {
    return(invisible())
  }
}
#' Assign pk value based on current Pk Parameter declared
#'
#' @param par parameter value
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.pkParAssign <- function(par) {
  if (.monolix2rx$curPkPar %in% c("cmt", "adm", "from", "to")) {
    .val <- as.integer(par)
  } else  {
    .val <- par
  }
  if (!.pkAssignBasedOnValue(.monolix2rx$curPkPar, .val)) {
    stop("unexpected pk assignment for '", par, '"', call. = FALSE)
  } else {
    return(invisible())
  }
}
#' PK parameter equation expression
#'
#' @param eq equation rhs
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.pkParEqExpr <- function(eq) {
  if (!is.na(.monolix2rx$curPkPar)) {
    .monolix2rx$pkPars[.monolix2rx$curPkPar] <- eq
    .monolix2rx$curPkPar <- NA_integer_
    return(invisible())
  }
}
#' PK set statement
#'
#' @param type the statement that is being processed
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.pkSetStatement <- function(type) {
  .pkPushStatement()
  if (type == "absorption") type <- "oral"
  .monolix2rx$pkStatement <- type
}
#' Print a PK structure data frame
#'
#' @param what what is the pk item that is being printed
#' @param df The data frame that will be printed
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.printPkDf <- function(what, df) {
  if (length(df[, 1]) > 1L) {
    lapply(seq_len(length(df[, 1])),
           function(w) {
             .printPkDf(what, df[w, ])
           })
    return(invisible())
  }
  .na <- vapply(seq_len(ncol(df)), function(i){
    !is.na(df[[i]])
  }, logical(1), USE.NAMES = FALSE)
  .df <- df[, .na, drop = FALSE]
  cat(what,"(",
      paste(vapply(names(.df),
         function(n) {
           .v <- .df[[n]]
           if (.v == "") return(n)
           paste0(n, " = ", .v)
         }, character(1), USE.NAMES = FALSE), collapse=", "),
      ")\n", sep="")
}

#' @export
print.monolix2rxPk <- function(x, ...) {
  if (!is.na(x$Cc)) {
    if (!is.na(x$Ce)) {
      cat("{", x$Cc, ", ", x$Ce, "} = ", sep="")
    } else {
      cat(x$Cc, " = ", sep="")
    }
    .pars <- x$pkmodel
    .pars <- .pars[!is.na(.pars)]
    cat("pkmodel(")
    cat(paste(vapply(names(.pars), function(n) {
      .p <- .pars[n]
      if (.p == "") return(n)
      return(paste0(n, " = ", .p))
    }, character(1), USE.NAMES = FALSE), collapse=", "))
    cat(")\n")
  }
  # get max/min cmts
  .r <- suppressWarnings(range(c(x$compartment$cmt,
                                 x$peripheral$in.i, x$peripheral$in.j,
                                 x$peripheral$out.i, x$peripheral$out.j,
                                 x$effect$cmt, x$transfer$from, x$transfer$to, x$oral$cmt,
                                 x$iv$cmt, x$elimination$cmt)))
  .prn <- FALSE
  if (is.finite(.r[1])) {
    for (.i in seq(.r[1], .r[2])) {
      .prn <- FALSE
      .w <- which(x$compartment$cmt == .i)
      if (length(.w) > 0) {
        .cmt <- x$compartment[.w, ]
        .printPkDf("compartment", .cmt)
        .prn <- TRUE
      }
      .w <- which(x$peripheral$in.j == .i)
      if (length(.w) > 0) {
        .perip <- x$peripheral[.w, ]
        if (.perip$in.i < 10 && .perip$in.j < 10) {
          .k <- paste0("k", .perip$in.i, .perip$in.j)
        } else {
          .k <- paste0("k", .perip$in.i, "_", .perip$in.j)
        }
        .df1 <- data.frame(k=.perip$in.eq)
        names(.df1) <- .k
        .perip <- .perip[, -(1:3)]
        if (.perip$out.i < 10 && .perip$out.j < 10) {
          .k <- paste0("k", .perip$out.i, .perip$out.j)
        } else {
          .k <- paste0("k", .perip$out.i, "_", .perip$out.j)
        }
        .df2 <- data.frame(k=.perip$out.eq)
        names(.df2) <- .k
        .perip <- .perip[, -(1:3)]
        .perip <- cbind(.df1, .df2, .perip)
        .printPkDf("peripheral", .perip)
        .prn <- TRUE
      }
      .w <- which(x$transfer$to == .i)
      if (length(.w) > 0) {
        .trans <- x$transfer[.w, ]
        .printPkDf("transfer", .trans)
        .prn <- TRUE
      }
      .w <- which(x$oral$cmt == .i)
      if (length(.w) > 0) {
        .oral <- x$oral[.w, ]
        .printPkDf("oral", .oral)
        .prn <- TRUE
      }
      .w <- which(x$iv$cmt == .i)
      if (length(.w) > 0) {
        .iv <- x$iv[.w, ]
        .printPkDf("iv", .iv)
        .prn <- TRUE
      }
      .w <- which(x$elimination$cmt == .i)
      if (length(.w) > 0) {
        .elimination <- x$elimination[.w, ]
        .printPkDf("elimination", .elimination)
        .prn <- TRUE
      }
      .w <- which(x$effect$cmt == .i)
      if (length(.w) > 0) {
        .effect <- x$effect[.w, ]
        .printPkDf("effect", .effect)
        .prn <- TRUE
      }
      if (.prn && .i < .r[2]) cat("\n")
    }
    .prn <- TRUE
  }
  # Now adm only
  .r <- suppressWarnings(range(c(x$depot$adm, x$empty$adm, x$reset$adm), na.rm=TRUE))
  .prnAdm <- FALSE
  if (is.finite(.r[1])) {
    if (.prn) cat("\n")
    for (.i in seq(.r[1], .r[2])) {
      .prn <- FALSE
      .w <- which(x$depot$adm == .i)
      if (length(.w) > 0) {
        .depot <- x$depot[.w, ]
        .printPkDf("depot", .depot)
        .prn <- TRUE
      }
      .w <- which(x$empty$adm == .i)
      if (length(.w) > 0) {
        .empty <- x$empty[.w, ]
        .printPkDf("empty", .empty)
        .prn <- TRUE
      }
      .w <- which(x$reset$adm == .i)
      if (length(.w) > 0) {
        .reset <- x$reset[.w, ]
        .printPkDf("reset", .reset)
        .prn <- TRUE
      }
      if (.prn && .i < .r[2]) cat("\n")
    }
    .prnAdm <- TRUE
  }
  .w <- which(is.na(x$depot$adm))
  if (length(.w) > 0) {
    .depot <- x$depot[.w, ]
    if (.prnAdm) cat("\n")
    .prnAdn <- FALSE
    .printPkDf("depot", .depot)
  }
  .w <- which(is.na(x$empty$adm))
  if (length(.w) > 0) {
    if (.prnAdm) cat("\n")
    .prnAdn <- FALSE
    .empty <- x$empty[.w, ]
    .printPkDf("empty", .empty)
  }
  .w <- which(is.na(x$reset$adm))
  if (length(.w) > 0) {
    if (.prnAdm) cat("\n")
    .prnAdn <- FALSE
    .reset <- x$reset[.w, ]
    .printPkDf("reset", .reset)
  }
}

#' @export
as.list.monolix2rxPk <- function(x, ...) {
  .x <- x
  class(.x) <- NULL
  .x
}
