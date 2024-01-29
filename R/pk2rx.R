#' Get the amount name for compartment i
#'
#' @param env environment for pk macro to rxode2 translation has name, lhs, rhs
#' @param pk parsed pk macros object
#' @param i compartment number
#' @param amount character vector of the amount name for this compartment
#' @return character vector name for this comprtment
#' @noRd
#' @author Matthew L. Fidler
.pk2rxAmt <- function(env, pk, i, amount=NA_character_) {
  if (!is.null(env$name[[i]])) {
    if (!is.na(amount) && amount != env$name[[i]]) {
      stop("can only have one amount name for cmt ", i, ", have at least 2: '", env$name[[i]], "' and '", amount, "'",
           call.=FALSE)
    }
    return(env$name[[i]])
  }
  if (!is.na(amount)) {
    env$name[[i]] <- amount
    env$lhs[[i]] <- paste0("d/dt(", env$name[[i]], ")")
    env$rhs[[i]] <- ""
    env$extra[[i]] <- character(0)
    return(env$name[[i]])
  }
  .w <- which(pk$compartment$cmt == i)
  if (length(.w) == 1L) {
    .amount <- pk$compartment[.w, "amount"]
    if (!is.na(.amount)) {
      env$name[[i]] <- .amount
      env$lhs[[i]] <- paste0("d/dt(", env$name[[i]], ")")
      env$rhs[[i]] <- ""
      env$extra[[i]] <- character(0)
      return(env$name[[i]])
    }
  }
  env$name[[i]] <- paste0(env$cmtDefault, i)
  if (env$name[[i]] == "cmt1") env$name[[i]] <- "central" # align with linCmt()
  env$lhs[[i]] <- paste0("d/dt(", env$name[[i]], ")")
  env$rhs[[i]] <- ""
  env$extra[[i]] <- character(0)
  env$name[[i]]
}
#' Add a concentration the the macro to rxode2 translation
#'
#'
#' @param env rxode2 translation environment
#' @param pk parsed pk object
#' @param i compartment number
#' @param amount character vector of compartment name (or na)
#' @param volume character vector of the compartment volume (or na)
#' @param concentration character vector of the compartment concentration (or na)
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.pk2rxConc <- function(env, pk, i, amount=NA_character_, volume=NA_character_, concentration=NA_character_) {
  # This apparently depends on the
  if (!is.na(concentration)) {
    .v <- ""
    if (!is.na(volume)) {
      .v <- paste0("/", volume)
    }
    .monolix2rx$pkLhs <- c(.monolix2rx$pkLhs, concentration)
    env$conc[[i]] <- paste0(concentration, " <- ", .pk2rxAmt(env, pk, i, amount), .v)
    attr(env$conc[[i]], "conc") <- concentration
  } else if (i == 1 && length(.monolix2rx$endpointPred) > 0 &&
               is.na(concentration) && !(.monolix2rx$endpointPred[1] %in% .monolix2rx$curLhs)) {
    concentration <- .monolix2rx$endpointPred[1]
    .v <- ""
    if (!is.na(volume)) {
      .v <- paste0("/", volume)
    }
    env$conc[[i]] <- paste0(concentration, " <- ", .pk2rxAmt(env, pk, i, amount), .v)
    .monolix2rx$pkLhs <- c(.monolix2rx$pkLhs, concentration)
    attr(env$conc[[i]], "conc") <- concentration
  }
}
#' Setup the name and concentration of a comparment() macro
#'
#' @param env rxode2 translation environment
#' @param pk parsed pk object
#' @param i compartment number
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.pk2rxCompartment <- function(env, pk, i) {
  .w <- which(pk$compartment$cmt == i)
  if (length(.w) > 1L) stop("multiple compartment definitions for compartment ", i)
  if (length(.w) == 1L) {
    .cmt <- pk$compartment[.w, ]
    .pk2rxConc(env, pk, i, volume=.cmt$volume, concentration=.cmt$concentration)
  }
}
#' Add any peripheral compartments that are linked to cmt i
#'
#' @param env rxode2 translation environment
#' @param pk parsed pk environment
#' @param i compartment number where we look for periph comparments
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.pk2rxPeriph <- function(env, pk, i) {
  .w0 <- which(pk$peripheral$in.j == i)
  if (length(.w0) == 0L) return(invisible())
  for (.w in .w0) {
    .perip <- pk$peripheral[.w, ]
    if (!is.na(.perip$in.eq) && .perip$in.eq != "") {
      .k12 <- .perip$in.eq
    } else if (.perip$in.i < 10 && .perip$in.j < 10) {
      .k12 <- paste0("k", .perip$in.i, .perip$in.j)
    } else {
      .k12 <- paste0("k", .perip$in.i, "_", .perip$in.j)
    }
    if (!is.na(.perip$out.eq) && .perip$out.eq != "") {
      .k21 <- .perip$out.eq
    } else if (.perip$out.i < 10 && .perip$out.j < 10) {
      .k21 <- paste0("k", .perip$out.i, .perip$out.j)
    } else {
      .k21 <- paste0("k", .perip$out.i, "_", .perip$out.j)
    }
    .pk2rxConc(env, pk, i, amount=.perip$amount, volume=.perip$volume, concentration=.perip$concentration)
    # perip like
    .c2 <- .pk2rxAmt(env, pk, i, amount=.perip$amount)
    .i2 <- i
    # central like
    .c1 <- .pk2rxAmt(env, pk, .perip$in.i, amount=NA_character_)
    .i1 <- .perip$in.i
    # Central
    env$rhs[[.i1]] <- paste0(env$rhs[[.i1]],
                             " - ", .k12, "*", .c1,
                             " + ", .k21, "*", .c2)
    # Periph
    env$rhs[[.i2]] <- paste0(env$rhs[[.i2]],
                             " + ", .k12, "*", .c1,
                             " - ", .k21, "*", .c2)
  }
}
#' Process transfer macros from
#'
#'
#' @param env rxode2 translation environment
#' @param pk parsed pk macro
#' @param i which compartment to translate
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.pk2rxTransfer <- function(env, pk, i) {
  .w0 <- which(pk$transfer$to == i)
  if (length(.w0) == 0L) return(invisible())
  for (.w in .w0) {
    .trans <- pk$transfer[.w, ]
    .c1 <- .pk2rxAmt(env, pk, .trans$from)
    .i1 <- .trans$from
    .i2 <- .trans$to
    .kt <- .pk2rxGetVar(.trans, "kt")
    # from
    env$rhs[[.i1]] <- paste0(env$rhs[[.i1]],
                             " - ", .kt, "*", .c1)
    # to
    env$rhs[[.i2]] <- paste0(env$rhs[[.i2]],
                             " + ", .kt, "*", .c1)
  }
}
#' Convert to a appropriate property addition for the dur/f/alag rxode2 properties
#'
#' If there are multiple doses that affect the same property, add logical operators
#'
#' @param pk parsed pk value
#' @param df current data frame being considered
#' @param type the type of property being considered, can be "dur", "f", "tlag"
#' @param value the value of the property to be returned in the right circumstances
#' @return rxode2 text of the property to be added
#' @noRd
#' @author Matthew L. Fidler
.pk2rxAdmVal <- function(pk, df, type, value) {
  .adm <- df$adm
  .admd <- pk$admd
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
  if (!is.na(.cmt)) {
    .admd <- .admd[.admd$cmt == .cmt, ]
  } else if (!is.na(.target)) {
    .admd <- .admd[.admd$target == .target, ]
  } else {
    stop("target/cmt not defined, cannot figure out dose", call.=FALSE)
  }
  .cur <- .admd[.admd$depot == .depot & .admd[[type]] == TRUE , ]
  if (length(.cur$adm) == 1L) return(value)
  .cur1 <- .cur[.cur$adm == df$adm, ]
  if (length(.cur1$adm) == 1L) {
    return(paste0("+(ADM==", df$adm, ")*(", value, ")"))
  }
  .cur2 <- .cur[.cur$admd == df$admd, ]
  if (length(.cur2$adm) == 1L) {
    return(paste0("+(ADMD==", df$admd, ")*(", value, ")"))
  }
  .cur3 <- .cur[.cur$admd == df$admd & .cur$adm == df$adm, ]
  if (length(.cur3$adm) == 1L) {
    return(paste0("+(ADM==", df$adm, " && ADMD==", df$admd, ")*(", value, ")"))
  }
  stop("cannot figure out how to isolate dose in translation to rxode2", call.=FALSE)
}
#' Get the variable from a dataset or list
#'
#'
#' @param input input dataset or varaible
#' @param var variable name
#' @return will return NA_character_ for NULL and NA values.  If the
#'   inp=="" then return the inp value
#' @noRd
#' @author Matthew L. Fidler
.pk2rxGetVar <- function(input, var) {
  .inp <- input[[var]]
  if (is.null(.inp)) return(NA_character_)
  if (is.na(.inp)) return(NA_character_)
  if (.inp == "") return(var)
  .inp
}
#' Handle the administration/oral macros
#'
#' @param env environment for rxode2 translation
#' @param pk parsed pk object
#' @param i compartment to process
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.pk2rxAdmin <- function(env, pk, i) {
  .w0 <- which(pk$oral$cmt == i)
  if (length(.w0) == 0L) return(invisible())
  .cmtName <- .pk2rxAmt(env, pk, i)
  for (.w in .w0) {
    .oral <- pk$oral[.w, ]
    if (!is.na(.oral$Tk0)) {
      .tk0 <- .pk2rxGetVar(.oral, "Tk0")
      if (is.null(env$dur[[i]])) {
        env$dur[[i]] <- paste0("dur(", .cmtName, ") <- ")
      }
      env$dur[[i]] <- paste0(env$dur[[i]],
                             .pk2rxAdmVal(pk, .oral, "dur", .tk0))
      .p <- .oral$p
      .pn <- suppressWarnings(as.numeric(.p))
      if (!identical(.pn, 1.0)) {
        .p <- .pk2rxGetVar(.oral, "p")
        if (is.null(env$f[[i]])) {
          env$f[[i]] <-  paste0("f(", .cmtName, ") <- ")
        }
        env$f[[i]] <- paste0(env$f[[i]],
                             .pk2rxAdmVal(pk, .oral, "f", .p))
      }
      .tlag <- .oral$Tlag
      .tlagn <- suppressWarnings(as.numeric(.tlag))
      if (!identical(.tlagn, 0.0)) {
        .Tlag <- .pk2rxGetVar(.oral, "Tlag")
        if (is.null(env$tlag[[i]])) {
          env$tlag[[i]] <- paste0("alag(", .cmtName, ") <- ")
        }
        env$tlag[[i]] <- paste0(env$tlag[[i]],
                                .pk2rxAdmVal(pk, .oral, "tlag", .Tlag))
      }
    } else {
      # ka
      .cmtNameC <- .cmtName
      .cmtName <- paste0(.cmtName, env$depotPostfix)
      if (.cmtName == paste0("central", env$depotPostfix)) .cmtName <- "depot" # align with linCmt
      if (.cmtName == paste0("depot", env$depotPostfix)) .cmtName <- "depot"
      if (is.null(env$lhsDepot[[i]])) {
        env$lhsDepot[[i]] <- paste0("d/dt(", .cmtName, ")")
      }
      if (is.null(env$rhsDepot[[i]])) {
        env$rhsDepot[[i]] <- ""
      }
      .ka <- .pk2rxGetVar(.oral, "ka")
      env$rhsDepot[[i]] <- paste0(env$rhsDepot[[i]],
                                  " - ", .ka, "*", .cmtName)
      env$rhs[[i]] <- paste0(env$rhs[[i]],
                             " + ", .ka, "*", .cmtName)
      .p <- .oral$p
      .pn <- suppressWarnings(as.numeric(.p))
      if (!is.na(.oral$Mtt) && !is.na(.oral$Ktr)) {
        .Mtt <- .pk2rxGetVar(.oral, "Mtt")
        .Ktr <- .pk2rxGetVar(.oral, "Ktr")
        .p <- .pk2rxGetVar(.oral, "p")
        env$rhsDepot[[i]] <- paste0(env$rhsDepot[[i]],
                                    " + transit(",
                                    .Mtt, "*", .Ktr, "-1, ",
                                    .Mtt, ", ",
                                    ifelse(is.na(.p), "1.0", .p),
                                    ")")
      } else if (!identical(.pn, 1.0)) {
        .p <- .pk2rxGetVar(.oral, "p")
        if (is.null(env$fDepot[[i]])) {
          env$fDepot[[i]] <- paste0("f(", .cmtName, ") <- ")
        }
        env$fDepot[[i]] <- paste0(env$fDepot[[i]],
                                  .pk2rxAdmVal(pk, .oral, "f", .p))
      }
      .tlag <- .oral$Tlag
      .tlagn <- suppressWarnings(as.numeric(.tlag))
      if (!identical(.tlagn, 0.0)) {
        .Tlag <- .pk2rxGetVar(.oral, "Tlag")
        if (is.null(env$tlagDepot[[i]])) {
          env$tlagDepot[[i]] <- paste0("alag(", .cmtName, ") <- ")
        }
        env$tlagDepot[[i]] <- paste0(env$tlagDepot[[i]],
                                     .pk2rxAdmVal(pk, .oral, "tlag", .Tlag))
      }
    }
  }
}
#' Handle effect compartment macros
#'
#' @param env rxode2 translation environment
#' @param pk parsed pk macro
#' @param i compartment to parse
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.pk2rxEffect <- function(env, pk, i) {
  .w <- which(pk$effect$cmt == i)
  if (length(.w) > 1L) {
    stop("only one effect compartment per compartment number allowed (cmt ",i, ")")
  }
  if (length(.w) == 1L) {
    .effect <- pk$effect[.w, ]
    .ce <- .effect$concentration
    if (is.null(env$conc[[i]])) {
      stop("concentration of compartment ", i, " is not defined")
    }
    .cc <- attr(env$conc[[i]], "conc")
    .ke0 <- .pk2rxGetVar(.effect, "ke0")
    env$lhsEffect[[i]] <- paste0("d/dt(", .ce, ")")
    env$rhsEffect[[i]] <- paste0(.ke0, "*(", .cc, " - ", .ce, ")")
  }
}
#' Handle IV macro to convert to rxode2
#'
#' @param env environment for translation
#' @param pk parsed pk macro
#' @param i compartment number i
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.pk2rxIv <- function(env, pk, i) {
  .w0 <- which(pk$iv$cmt == i)
  if (length(.w0) == 0L) return(invisible())
  for (.w in .w0) {
    .iv <- pk$iv[.w, ]
    .ca <- .pk2rxAmt(env, pk, i)
    .tlag <- .iv$Tlag
    .tlagn <- suppressWarnings(as.numeric(.tlag))
    if (!identical(.tlagn, 0.0)) {
      .tlag <- .pk2rxGetVar(.iv, "Tlag")
      if (is.null(env$tlag[[i]])) {
        env$tlag[[i]] <- paste0("alag(", .ca, ") <- ")
      }
      env$tlag[[i]] <- paste0(env$tlag[[i]],
                               .pk2rxAdmVal(pk, .iv, "tlag", .tlag))
    }
    .p <- .iv$p
    .pn <- suppressWarnings(as.numeric(.p))
    if (!identical(1.0, .pn)) {
      .p <- .pk2rxGetVar(.iv, "p")
      if (is.null(env$f[[i]])) {
        env$f[[i]] <- paste0("f(", .ca, ") <- ")
      }
      env$f[[i]] <- paste0(env$f[[i]],
                           .pk2rxAdmVal(pk, .iv, "f", .p))
    }
  }
}
#' Handle elimination macros
#'
#' @param env pk translation environment
#' @param pk parsed pk record
#' @param i compartment number to handle
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.pk2rxElimination <- function(env, pk, i) {
  .w0 <- which(pk$elimination$cmt == i)
  if (length(.w0) == 0) return(invisible())
  for (.w in .w0) {
    .elimination <- pk$elimination[.w, ]
    .cmtName <- .pk2rxAmt(env, pk, i)
    .w2 <- which(pk$compartment$cmt == i)
    if (length(.w2) == 1L) {
      .V <- pk$compartment[pk$compartment$cmt == i, "volume"]
      # Volume needs to be defined completely no need to change to "V"
    } else {
      .V <- NA_character_
    }
    if (!is.na(.elimination$k)) {
      .k <- .pk2rxGetVar(.elimination, "k")
      env$rhs[[i]] <- paste0(env$rhs[[i]],
                             " - ", .k, "*", .cmtName)
    } else if (!is.na(.elimination$Cl)) {
      if (is.na(.V)) {
        stop("cannot determine volume for this elimination type",
             call.=FALSE)
      }
      .Cl <- .pk2rxGetVar(.elimination, "Cl")
      env$rhs[[i]] <- paste0(env$rhs[[i]],
                             " - ", .Cl, "/", .V, "*", .cmtName)
    } else if (!is.na(.elimination$Vm) && !is.na(.elimination$Km)) {
      if (is.na(.V)) {
        stop("cannot determine volume for this elimination type",
             call.=FALSE)
      }
      .Vm <- .pk2rxGetVar(.elimination, "Vm")
      .Km <- .pk2rxGetVar(.elimination, "Km")
      env$rhs[[i]] <- paste0(env$rhs[[i]],
                             " - (",
                             .Vm, "*", .cmtName, "/", .V, ")/(",
                             .Km, " + ", .cmtName, "/", .V, ")")
    }
  }
}
#' Create extra code that will need to be integrated with the EQUATION: section
#'
#' @param env rxode2 translation environment
#' @param pk parsed pk value
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.pk2rxDepot <- function(env, pk) {
  lapply(seq_along(pk$depot$target),
         function(i) {
           .depot <- pk$depot[i, ]
           .target <- .depot$target
           .adm <- .depot$adm
           if (!is.na(.depot$ka)) {
             .ka <- .pk2rxGetVar(.depot, "ka")
             if (is.null(env$lhsDepot[[.target]])) {
               env$lhsDepot[[.target]] <- paste0("d/dt(", .target, env$depotPostfix, ")")
               env$lhsDepot[[.target]] <- ""
             }
             env$rhsDepot[[.target]] <- paste0(env$rhsDepot[[.target]],
                                               " - ", .ka, "*", .target, env$depotPostfix)
             env$extraDepot[[.target]] <- paste0(" + ", .ka, "*", .target, env$depotPostfix)
             .p <- .depot$p
             .pn <- suppressWarnings(as.numeric(.p))
             if (!is.na(.depot$Mtt) && !is.na(.depot$Ktr)) {
               .Mtt <- .pk2rxGetVar(.depot, "Mtt")
               .Ktr <- .pk2rxGetVar(.depot, "Ktr")
               .p <- .pk2rxGetVar(.depot, "p")
               env$rhsDepot[[.target]] <- paste0(env$rhsDepot[[.target]],
                                                 " + transit(",
                                                 .Mtt, "*", .Ktr, "-1, ",
                                                 .Mtt, ", ",
                                                 ifelse(is.na(.p), "1.0", .p),
                                                 ")")
             } else if (!identical(.pn, 1.0)) {
               .p <- .pk2rxGetVar(.depot, "p")
               if (is.null(env$fDepot[[.target]])) {
                 env$fDepot[[.target]] <- paste0("f(", .target, env$depotPostfix, ") <- ")
               }
               env$fDepot[[.target]] <- paste0(env$fDepot[[.target]],
                                               .pk2rxAdmVal(pk, .depot, "f", .p))
             }
             .tlag <- .depot$Tlag
             .tlagn <- suppressWarnings(as.numeric(.tlag))
             if (!identical(.tlagn, 0.0)) {
               .Tlag <- .pk2rxGetVar(.depot, "Tlag")
               if (is.null(env$tlagDepot[[.target]])) {
                 env$tlagDepot[[.target]] <- paste0("alag(",
                                                    .target, env$depotPostfix,
                                                    ") <- ")
               }
               env$tlagDepot[[.target]] <- paste0(env$tlagDepot[[.target]],
                                                  .pk2rxAdmVal(pk, .depot, "tlag", .Tlag))
             }
           } else {
             .p <- .depot$p
             .pn <- suppressWarnings(as.numeric(.p))
             if (!identical(.pn, 1.0)) {
               .p <- .pk2rxGetVar(.depot, "p")
               if (is.null(env$f[[.target]])) {
                 env$f[[.target]] <- paste0("f(", .target, ") <- ")
               }
               env$f[[.target]] <- paste0(env$f[[.target]],
                                          .pk2rxAdmVal(pk, .depot, "f", .p))
             }
             .tlag <- .depot$Tlag
             .tlagn <- suppressWarnings(as.numeric(.tlag))
             if (!identical(.tlagn, 0.0)) {
               .Tlag <- .pk2rxGetVar(.depot, "Tlag")
               if (is.null(env$tlag[[.target]])) {
                 env$tlag[[.target]] <- paste0("alag(",
                                               .target,
                                               ") <- ")
               }
               env$tlag[[.target]] <- paste0(env$tlag[[.target]],
                                             .pk2rxAdmVal(pk, .depot, "tlag", .Tlag))
             }
             if (!is.na(.depot$Tk0)) {
               .Tk0 <- .pk2rxGetVar(.depot, "Tk0")
               if (is.null(env$tlag[[.target]])) {
                 env$dur[[.target]] <- paste0("dur(",
                                              .target,
                                              ") <- ")
               }
               env$dur[[.target]] <- paste0(env$dur[[.target]],
                                            .pk2rxAdmVal(pk, .depot, "dur", .Tk0))
             }
           }
         })
  invisible()
}
#' Convert Pk macro to ODEs for rxode2
#'
#' @param pk parsed pk
#' @param amountPrefix amount prefix for unnamed compartments
#' @param depotPostfix depot postfix for depot compartments
#' @return list with  $pk  and $equation for a list to integrate into the equation block
#' @noRd
#' @author Matthew L. Fidler
.pk2rx <- function(pk, amountPrefix="cmt", depotPostfix="d") {
  .monolix2rx$pkLhs <- character(0)
  .pk <- .pkmodel2macro(pk)
  .r <- suppressWarnings(range(c(.pk$compartment$cmt,
                                 .pk$peripheral$in.i, .pk$peripheral$in.j,
                                 .pk$peripheral$out.i, .pk$peripheral$out.j,
                                 .pk$effect$cmt, .pk$transfer$from, .pk$transfer$to, .pk$oral$cmt,
                                 .pk$iv$cmt, .pk$elimination$cmt)))
  .prn <- FALSE
  .ret <- ""
  .env <- new.env(parent=emptyenv())
  .env$endLines <- character(0)
  .env$depotPostfix <- depotPostfix
  if (is.finite(.r[1])) {
    .env$name      <- vector("list", .r[2])
    .env$lhs       <- vector("list", .r[2])
    .env$rhs       <- vector("list", .r[2])
    .env$lhsDepot  <- vector("list", .r[2])
    .env$rhsDepot  <- vector("list", .r[2])
    .env$lhsEffect <- vector("list", .r[2])
    .env$rhsEffect <- vector("list", .r[2])
    .env$dur       <- vector("list", .r[2])
    .env$f         <- vector("list", .r[2])
    .env$tlag      <- vector("list", .r[2])
    .env$fDepot    <- vector("list", .r[2])
    .env$tlagDepot <- vector("list", .r[2])
    .env$extra     <- vector("list", .r[2])
    .env$conc      <- vector("list", .r[2])
    .env$cmtDefault <- amountPrefix
    for (.i in seq(.r[1], .r[2])) {
      .pk2rxCompartment(.env, .pk, .i) # names compartments
      .pk2rxPeriph(.env, .pk, .i) # names peripheral compartments
    }
    # These do not define any compartment names so process after the ones that do...
    for (.i in seq(.r[1], .r[2])) {
      .pk2rxTransfer(.env, .pk, .i)
      .pk2rxAdmin(.env, .pk, .i) # names depot compartments only
      .pk2rxEffect(.env, .pk, .i) # although names compartments, it is only effect cmt (handled differently)
      .pk2rxIv(.env, .pk, .i)
      .pk2rxElimination(.env, .pk, .i)
    }
    # now collapse into a single ode expression
    .ret <- vapply(seq_along(.env$name),
                   function(i) {
                     .ret <- character(0)
                     .endLine <- FALSE
                     if (!is.null(.env$lhsDepot[[i]])) {
                       if (.env$rhsDepot[[i]] != "") {
                         .ret <- c(.ret,
                                   paste0(.env$lhsDepot[[i]], " <- ", .env$rhsDepot[[i]]))
                       }
                     }
                     if (!is.null(.env$fDepot[[i]])) {
                       .ret <- c(.ret,
                                 .env$fDepot[[i]])
                     }
                     if (!is.null(.env$tlagDepot[[i]])) {
                       .ret <- c(.ret,
                                 .env$tlagDepot[[i]])
                     }
                     if (!is.null(.env$lhs[[i]])) {
                       if (.env$rhs[[i]] != "") {
                         .ret <- c(.ret,
                                   paste0(.env$lhs[[i]], " <- ", .env$rhs[[i]]))
                       } else {
                         .endLine <- TRUE
                       }
                     }
                     if (!is.null(.env$dur[[i]])) {
                       .ret <- c(.ret,
                                 .env$dur[[i]])
                     }
                     if (!is.null(.env$f[[i]])) {
                       .ret <- c(.ret,
                                 .env$f[[i]])
                     }
                     if (!is.null(.env$tlag[[i]])) {
                       .ret <- c(.ret,
                                 .env$tlag[[i]])
                     }
                     if (!is.null(.env$conc[[i]])) {
                       .ret <- c(.ret,
                                 .env$conc[[i]])
                     }
                     if (!is.null(.env$lhsEffect[[i]])) {
                       .ret <- c(.ret,
                                 paste0(.env$lhsEffect[[i]], " <- ", .env$rhsEffect[[i]]))
                     }
                     if (identical(.ret, character(0))) return(NA_character_)
                     if (.endLine) {
                       .env$endLines <- c(.env$endLines, paste(.ret, collapse="\n"))
                       return(NA_character_)
                     }
                     return(paste(.ret, collapse="\n"))
                   }, character(1), USE.NAMES = FALSE)
    .ret <- .ret[!is.na(.ret)]
    .ret <- paste(.ret, collapse="\n")
  }
  if (.ret == "" && identical(.env$endLines, character(0)) &&
        length(.pk$depot$adm) == 0L) {
    .ret <- character(0)
  } else {
    .ret <- strsplit(.ret, "\n")[[1]]
  }
  .env$lhsDepot  <- list()
  .env$rhsDepot  <- list()
  .env$extraDepot<- list()
  .env$dur       <- list()
  .env$f         <- list()
  .env$tlag      <- list()
  .env$fDepot    <- list()
  .env$tlagDepot <- list()
  .pk2rxDepot(.env, .pk)
  list(pk=.ret,
       equation=list(lhsDepot=.env$lhsDepot,
                     rhsDepot=.env$rhsDepot,
                     extraDepot=.env$extraDepot,
                     dur=.env$dur,
                     f=.env$f,
                     tlag=.env$tlag,
                     fDepot=.env$fDepot,
                     tlagDepot=.env$fDepot,
                     endLines=.env$endLines))
}
