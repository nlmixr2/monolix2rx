
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

.pk <- function(text) {
  .pkIni(TRUE)
  .Call(`_monolix2rx_trans_mlxtran_pk`, text)
  .pkPushStatement()
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

.pkSetCc <- function(cc) {
  .pkPushStatement()
  .monolix2rx$pkCc <- cc
  .monolix2rx$pkStatement <- "pkmodel"
}

.pkSetCe <- function(ce) {
  .pkPushStatement()
  .monolix2rx$pkCe <- ce
  .monolix2rx$pkStatement <- "pkmodel"
}

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
    print(.monolix2rx$curEmpty)
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

.pkParEqExpr <- function(eq) {
  if (!is.na(.monolix2rx$curPkPar)) {
    .monolix2rx$pkPars[.monolix2rx$curPkPar] <- eq
    .monolix2rx$curPkPar <- NA_integer_
    return(invisible())
  }
}

.pkSetStatement <- function(type) {
  .pkPushStatement()
  if (type == "absorption") type <- "oral"
  .monolix2rx$pkStatement <- type
}
