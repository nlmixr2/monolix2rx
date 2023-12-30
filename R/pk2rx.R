.pkadm <- new.env(parent=emptyenv())

.pkadmReset <- function() {
  .pkadm$adm <- data.frame(adm=integer(0), compartment=character(0), cmt=integer(0))
}
.pkadmPush <- function(adm, compartment, cmt) {
  if (!exists("adm", envir=.pkadm)) {
    .pkadmReset()
  }
  .pkadm$adm <- rbind(.pkadm$adm,
                      data.frame(adm=adm, compartment=compartment, cmt=cmt))
}
#' Process the pk model portion of the PK: block to translate to rxode2
#'
#' @param pkmodel PK parameters defined in Monolix
#' @param Cc Central concentration defined by pkmodel() macro
#' @param Ce Effect compartment defined by pkmodel() macro
#' @return rxode2 ode lines for pkmodel() macro
#' @noRd
#' @author Matthew L. Fidler
.pkmodel <- function(pkmodel, Cc, Ce=NA_character_) {
  if (is.na(Cc)) return(character(0))
  .depot <- "depot"
  .central <- "central"
  .periph <- "periph"
  .periph2 <- "periph2"
  .eff <- "eff"
  .n <- names(pkmodel)
  .pkmodel <- vapply(.n,
                     function(n) {
                       if (is.na(pkmodel[n])) return(NA_character_)
                       if (pkmodel[n] == "") return(n)
                       pkmodel[n]
                     }, character(1), USE.NAMES = TRUE)
  .ret <- character(0)
  .hasDepot <- FALSE
  .centralExtra <- character(0)
  if (!is.na(.pkmodel["ka"])) {
    .ret <- paste0("d/dt(", .depot, ") <- -", .pkmodel["ka"], "*", .depot)
    # Monolix defines compartment differently; oral absorption is
    # associated with the central compartment
    .pkadmPush(1L, .depot, 1L)
    .pkadmPush(NA_integer_, .central, 1L)
    if (!is.na(.pkmodel["Mtt"]) && !is.na(.pkmodel["Ktr"])) {
      .ret <- paste0(.ret, " + transit(",
                     .pkmodel["Mtt"], "*", .pkmodel["Ktr"], "-1, ",
                     .pkmodel["Mtt"], ", ",
                     ifelse(is.na(.pkmodel["p"]), "1.0", .pkmodel["p"]),
                     ")")
    } else if (!is.na(.pkmodel["p"])) {
      # p can affect the rate of absorption instead not duration, ie
      # duration is fixed and changed dataset should be dur not rate
      .ret <- c(.ret,
                paste0("f(", .depot, ") <- ", .pkmodel["p"]))
    }
    .centralOde <- paste0("d/dt(", .central, ") <- ", .pkmodel["ka"], "*", .depot)
    if (!is.na(.pkmodel["Tlag"])) {
      .ret <- c(.ret,
                paste0("alag(", .depot, ") <- ", .pkmodel["Tlag"]))
    }
  } else {
    .pkadmPush(1L, .central, 1L)
    .centralOde <- paste0("d/dt(", .central, ") <- ")
    if (!is.na(.pkmodel["Tlag"])) {
      .centralExtra <- c(.centralExtra,
                         paste0("alag(", .central, ") <- ", .pkmodel["Tlag"]))
    }
    if (!is.na(.pkmodel["Tk0"])) {
      .centralExtra <- c(.centralExtra,
                         paste0("dur(", .central, ") <- ", .pkmodel["Tk0"]))
    }
    if (!is.na(.pkmodel["p"])) {
      .centralExtra <- c(.centralExtra,
                         paste0("f(", .central, ") <- ", .pkmodel["p"]))
    }
  }
  if (!is.na(.pkmodel["k12"]) && !is.na(.pkmodel["k21"])) {
    .pkadmPush(NA_integer_, .periph, 2L)
    .centralOde <- paste0(.centralOde, " - ", .pkmodel["k12"], "*", .central,
                          " + ", .pkmodel["k21"], "*", .periph)
    .centralExtra <- c(.centralExtra,
                       paste0("d/dt(", .periph, ") <- ", .pkmodel["k12"], "*", .central,
                              " - ", .pkmodel["k21"], "*", .periph))
  }
  if (!is.na(.pkmodel["k13"]) && !is.na(.pkmodel["k31"])) {
    .pkadmPush(NA_integer_, .periph2, 3L)
    .centralOde <- paste0(.centralOde, " - ", .pkmodel["k13"], "*", .central,
                          " + ", .pkmodel["k31"], "*", .periph2)
    .centralExtra <- c(.centralExtra,
                       paste0("d/dt(", .periph2, ") <- ", .pkmodel["k13"], "*", .central,
                              " - ", .pkmodel["k31"], "*", .periph2))
  }
  if (!is.na(.pkmodel["k"])) {
    .centralOde <- paste0(.centralOde, " - ",
                          .pkmodel["k"], "*", .central)
  } else if (!is.na(.pkmodel["Cl"])) {
    .centralOde <- paste0(.centralOde, " - ",
                          .pkmodel["Cl"], "/", .pkmodel["V"], "*", .central)
  } else if (!is.na(.pkmodel["Vm"]) && !is.na(.pkmodel["Km"])) {
    .centralOde <- paste0(.centralOde, " - (",
                          .pkmodel["Vm"], "*", .central, "/", .pkmodel["V"], ")/(",
                          .pkmodel["Km"], " + ", .central, "/", .pkmodel["V"], ")")
  }
  .ret <- c(.ret, .centralOde, .centralExtra)
  .ret <- c(.ret,
            paste0(Cc, " <- ", .central, "/", .pkmodel["V"]))
  if (!is.na(.pkmodel["ke0"]) && !is.na(Ce)) {
    # it isn't clear if the effect compartment is defined in the cmt def of pkmacro.
    # for now just use NA
    .pkadmPush(NA_integer_, Ce, NA_integer_)
    .ret <- c(.ret,
              paste0("d/dt(", Ce, ") <- ", .pkmodel["ke0"], "*(", Cc, " - ", Ce, ")"))
  }
  .ret
}
