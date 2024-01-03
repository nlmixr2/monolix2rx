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

    .monolix2rx$admd <- data.frame(adm=integer(0), admd=integer(0), cmt=integer(0), target=character(0),
                                   depot=logical(0), dur=logical(0), f=logical(0), tlag=logical(0))

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
                                      admd=integer(0),
                                      target=character(0),
                                      Tlag=character(0),
                                      p=character(0),
                                      Tk0=character(0),
                                      ka=character(0),
                                      Ktr=character(0),
                                      Mtt=character(0))

    .monolix2rx$pkOral <- data.frame(adm=integer(0),
                                     admd=integer(0),
                                     cmt=integer(0),
                                     Tlag=character(0),
                                     p=character(0),
                                     Tk0=character(0),
                                     ka=character(0),
                                     Ktr=character(0),
                                     Mtt=character(0))

    .monolix2rx$pkIv <- data.frame(adm=integer(0),
                                   admd=integer(0),
                                   cmt=integer(0),
                                   Tlag=character(0),
                                   p=character(0))
    .monolix2rx$pkEmpty <- data.frame(adm=integer(0),
                                      admd=integer(0),
                                      target=character(0))
    .monolix2rx$pkReset <- data.frame(adm=integer(0),
                                      admd=integer(0),
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
                                     admd=NA_integer_,
                                     target=NA_character_,
                                     Tlag=NA_character_,
                                     p=NA_character_,
                                     Tk0=NA_character_,
                                     ka=NA_character_,
                                     Ktr=NA_character_,
                                     Mtt=NA_character_)

  .monolix2rx$curOral <- data.frame(adm=NA_integer_,
                                    admd=NA_integer_,
                                    cmt=NA_integer_,
                                    Tlag=NA_character_,
                                    p=NA_character_,
                                    Tk0=NA_character_,
                                    ka=NA_character_,
                                    Ktr=NA_character_,
                                    Mtt=NA_character_)

  .monolix2rx$curIv <- data.frame(adm=NA_integer_,
                                  admd=NA_integer_,
                                  cmt=NA_integer_,
                                  Tlag=NA_character_,
                                  p=NA_character_)

  .monolix2rx$curEmpty <- data.frame(adm=NA_integer_,
                                     admd=NA_integer_,
                                     target=NA_character_)
  .monolix2rx$curReset <- data.frame(adm=NA_integer_,
                                     admd=NA_integer_,
                                     target=NA_character_)
  .monolix2rx$curElimination <- data.frame(cmt=NA_integer_,
                                           V=NA_character_,
                                           k=NA_character_,
                                           Cl=NA_character_,
                                           Vm=NA_character_,
                                           Km=NA_character_)
}
#' Integrate the dose number into the adm dataset
#'
#' @param df data frame to integate
#' @return data frame with admd (adm dose number) integrated
#' @noRd
#' @author Matthew L. Fidler
.pkGetAdmd <- function(df) {
  .adm <- df$adm
  if (is.na(.adm)) {
    .adm <- 1L
    df$adm <- 1L
  }
  .cmt <- NA_integer_
  .target <- NA_character_
  if (any(names(df) == "cmt")) {
    .cmt <- df$cmt
  } else {
    .target <- df$target
  }
  .depot <- FALSE
  if (any(names(df) == "ka")) {
    if (!is.na(df$ka)) {
      .depot <- TRUE
    }
  }
  .dur <- FALSE
  if (any(names(df) == "Tk0")) {
    if (!is.na(df$Tk0)) {
      .dur <- TRUE
    }
  }
  .f <- FALSE
  if (any(names(df) == "p")) {
    if (!is.na(df$p)) {
      .f <- TRUE
    }
  }
  .tlag <- FALSE
  if (any(names(df) == "Tlag")) {
    if (!is.na(df$Tlag)) {
      .tlag <- TRUE
    }
  }
  .admd <- .monolix2rx$admd[.monolix2rx$admd$adm == .adm, "admd"]
  if (length(.admd) == 0L) {
    df$admd <- 1L
    .monolix2rx$admd <- rbind(.monolix2rx$admd,
                              data.frame(adm=df$adm, admd=1L, cmt=.cmt, target=.target,
                                         depot=.depot, dur=.dur, f=.f, tlag=.tlag))
  } else {
    .admd <- max(.admd) + 1L
    df$admd <- .admd
    .monolix2rx$admd <- rbind(.monolix2rx$admd,
                              data.frame(adm=df$adm, admd=.admd, cmt=.cmt, target=.target,
                                         depot=.depot, dur=.dur, f=.f, tlag=.tlag))
  }
  df
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
                                 .pkGetAdmd(.monolix2rx$curDepot))
    .pkIni(FALSE)
    return(invisible())
  }
  if (.monolix2rx$pkStatement == "oral") {
    .monolix2rx$pkOral <- rbind(.monolix2rx$pkOral,
                                .pkGetAdmd(.monolix2rx$curOral))
    .pkIni(FALSE)
    return(invisible())
  }
  if (.monolix2rx$pkStatement == "iv") {
    .monolix2rx$pkIv <- rbind(.monolix2rx$pkIv,
                              .pkGetAdmd(.monolix2rx$curIv))
    .pkIni(FALSE)
    return(invisible())
  }
  if (.monolix2rx$pkStatement == "empty") {
    .monolix2rx$pkEmpty <- rbind(.monolix2rx$pkEmpty,
                                 .pkGetAdmd(.monolix2rx$curEmpty))
    .pkIni(FALSE)
    return(invisible())
  }
  if (.monolix2rx$pkStatement == "reset") {
    .monolix2rx$pkReset <- rbind(.monolix2rx$pkReset,
                                 .pkGetAdmd(.monolix2rx$curReset))
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
  if (all(is.na(pkmodel))) return(invisible())
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
  if (text != "") .Call(`_monolix2rx_trans_mlxtran_pk`, text)
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
               elimination=.monolix2rx$pkElimination,
               admd=.monolix2rx$admd)
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

#' Assign a parameter value based on value
#'
#' @param df data frame to assign
#' @param par parameter name
#' @param val parameter value
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.pkAssignBasedOnValue0 <- function(df, par, val) {
  df <- as.character(substitute(df))
  .df <- .monolix2rx[[df]]
  if (!(par %in% names(.df))) {
    stop("unsupported parameter '", par, "' for '",
         .monolix2rx$pkStatement, "'", call.=FALSE)
  }
  .df[par] <- val
  .monolix2rx[[df]] <- .df
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
    .pkAssignBasedOnValue0(pkPars, par, val)
    return(invisible(TRUE))
  }
  if (.monolix2rx$pkStatement == "compartment") {
    .pkAssignBasedOnValue0(curCmt, par, val)
    return(invisible(TRUE))
  }
  if (.monolix2rx$pkStatement == "peripheral") {
    .pkAssignBasedOnValue0(curPerip, par, val)
    return(invisible(TRUE))
  }
  if (.monolix2rx$pkStatement == "effect") {
    .pkAssignBasedOnValue0(curEffect, par, val)
    return(invisible(TRUE))
  }
  if (.monolix2rx$pkStatement == "transfer") {
    .pkAssignBasedOnValue0(curTransfer, par, val)
    return(invisible(TRUE))
  }
  if (.monolix2rx$pkStatement == "depot") {
    .pkAssignBasedOnValue0(curDepot, par, val)
    return(invisible(TRUE))
  }
  if (.monolix2rx$pkStatement == "oral") {
    .pkAssignBasedOnValue0(curOral, par, val)
    return(invisible(TRUE))
  }
  if (.monolix2rx$pkStatement == "iv") {
    .pkAssignBasedOnValue0(curIv, par, val)
    return(invisible(TRUE))
  }
  if (.monolix2rx$pkStatement == "empty") {
    .pkAssignBasedOnValue0(curEmpty, par, val)
    return(invisible(TRUE))
  }
  if (.monolix2rx$pkStatement == "reset") {
    .pkAssignBasedOnValue0(curReset, par, val)
    return(invisible(TRUE))
  }
  if (.monolix2rx$pkStatement == "elimination") {
    .pkAssignBasedOnValue0(curElimination, par, val)
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
.as.character.PkDf <- function(what, df) {
  if (length(df[, 1]) > 1L) {
    return(vapply(seq_along(df[, 1]),
                  function(w) {
                    .as.character.PkDf(what, df[w, ])
                  }, character(1), USE.NAMES = FALSE))
  }
  .na <- vapply(seq_len(ncol(df)), function(i){
    !is.na(df[[i]])
  }, logical(1), USE.NAMES = FALSE)
  .df <- df[, .na, drop = FALSE]
  .df <- .df[, names(.df) != "admd", drop = FALSE]
  paste0(what,"(",
         paste(vapply(names(.df),
                      function(n) {
                        .v <- .df[[n]]
                        if (.v == "") return(n)
                        paste0(n, " = ", .v)
                      }, character(1), USE.NAMES = FALSE), collapse=", "),
         ")")
}
#' @export
as.character.monolix2rxPk <- function(x, ...) {
  .retf <- character(0)
  .ret <- ""
  if (!is.na(x$Cc)) {
    if (!is.na(x$Ce)) {
      .ret <- paste0("{", x$Cc, ", ", x$Ce, "} = ")
    } else {
      .ret <- paste0(x$Cc, " = ")
    }
    .pars <- x$pkmodel
    .pars <- .pars[!is.na(.pars)]
    .ret <- paste0(.ret, "pkmodel(")
    .ret <- paste0(.ret, paste(vapply(names(.pars), function(n) {
      .p <- .pars[n]
      if (.p == "") return(n)
      return(paste0(n, " = ", .p))
    }, character(1), USE.NAMES = FALSE), collapse=", "),
    ")")
    .retf <- c(.retf, .ret)
    .ret <- ""
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
        .retf <- c(.retf,
                   .as.character.PkDf("compartment", .cmt))
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
        .retf <- c(.retf,
                   .as.character.PkDf("peripheral", .perip))
        .prn <- TRUE
      }
      .w <- which(x$transfer$to == .i)
      if (length(.w) > 0) {
        .trans <- x$transfer[.w, ]
        .retf <- c(.retf,
                   .as.character.PkDf("transfer", .trans))
        .prn <- TRUE
      }
      .w <- which(x$oral$cmt == .i)
      if (length(.w) > 0) {
        .oral <- x$oral[.w, ]
        .retf <- c(.retf,
                   .as.character.PkDf("oral", .oral))
        .prn <- TRUE
      }
      .w <- which(x$iv$cmt == .i)
      if (length(.w) > 0) {
        .iv <- x$iv[.w, ]
        .retf <- c(.retf,
                   .as.character.PkDf("iv", .iv))
        .prn <- TRUE
      }
      .w <- which(x$elimination$cmt == .i)
      if (length(.w) > 0) {
        .elimination <- x$elimination[.w, ]
        .retf <- c(.retf,
                   .as.character.PkDf("elimination", .elimination))
        .prn <- TRUE
      }
      .w <- which(x$effect$cmt == .i)
      if (length(.w) > 0) {
        .effect <- x$effect[.w, ]
        .retf <- c(.retf,
                   .as.character.PkDf("effect", .effect))
        .prn <- TRUE
      }
      if (.prn && .i < .r[2]) .retf <- c(.retf, "")
    }
    .prn <- TRUE
  }
  # Now adm only
  .r <- suppressWarnings(range(c(x$depot$adm, x$empty$adm, x$reset$adm), na.rm=TRUE))
  .prnAdm <- FALSE
  if (is.finite(.r[1])) {
    if (.prn) .retf <- c(.retf, "")
    for (.i in seq(.r[1], .r[2])) {
      .prn <- FALSE
      .w <- which(x$depot$adm == .i)
      if (length(.w) > 0) {
        .depot <- x$depot[.w, ]
        .retf <- c(.retf,
                   .as.character.PkDf("depot", .depot))
        .prn <- TRUE
      }
      .w <- which(x$empty$adm == .i)
      if (length(.w) > 0) {
        .empty <- x$empty[.w, ]
        .retf <- c(.retf,
                   .as.character.PkDf("empty", .empty))
        .prn <- TRUE
      }
      .w <- which(x$reset$adm == .i)
      if (length(.w) > 0) {
        .reset <- x$reset[.w, ]
        .retf <- c(.retf,
                   .as.character.PkDf("reset", .reset))
        .prn <- TRUE
      }
      if (.prn && .i < .r[2]) .retf <- c(.retf, "")
    }
    .prnAdm <- TRUE
  }
  .retf
}
#' @export
print.monolix2rxPk <- function(x, ...) {
  cat(paste(as.character.monolix2rxPk(x, ...), collapse="\n"), "\n", sep="")
}
#' @export
as.list.monolix2rxPk <- function(x, ...) {
  .x <- x
  class(.x) <- NULL
  .x
}
