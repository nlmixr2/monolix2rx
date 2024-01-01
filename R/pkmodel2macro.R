#' Get the pkmodel string
#'
#'
#' @param pkmodel pkmodel character vector list
#' @param var variable to query
#' @param comma string for the prefixed comma
#' @return ", var=x" type of text for other macros
#' @noRd
#' @author Matthew L. Fidler
.getPkmodelStr <- function(pkmodel, var, comma=", ") {
  if (is.na(pkmodel[var])) return("")
  if (pkmodel[var] == "") return(paste0(comma, var))
  paste0(comma, var, " = ", pkmodel[var])
}

#' Converts the pkmacro() to the other types of macros
#'
#' This allows DRY generation of rxode2 type models with macros
#'
#' @param pk pk parameters
#' @param txt boolean to return text instead of parsed pk macro
#' @return text or parsed pk object
#' @noRd
#' @author Matthew L. Fidler
.pkmodel2macro <- function(pk, txt=FALSE) {
  .pkmodel <- pk$pkmodel
  .macro <- paste0("compartment(cmt=1, volume=", ifelse(.pkmodel["V"] == "", "V",
                                                        paste0("V=",.pkmodel["V"])),
                   ", concentration=",
                   pk$Cc, ")")
  if (!is.na(.pkmodel["ka"]) || !is.na(.pkmodel["Tk0"])) {
    .macro <- c(.macro,
                paste0("absorption(adm=1",
                       .getPkmodelStr(.pkmodel, "Tlag"),
                       .getPkmodelStr(.pkmodel, "Tk0"),
                       .getPkmodelStr(.pkmodel, "p"),
                       .getPkmodelStr(.pkmodel, "Tk0"),
                       .getPkmodelStr(.pkmodel, "ka"),
                       .getPkmodelStr(.pkmodel, "Ktr"),
                       .getPkmodelStr(.pkmodel, "Mtt"),
                       ", cmt=1",
                       ")"))
  } else {
    .macro <- c(.macro,
                paste0("iv(adm=1",
                       .getPkmodelStr(.pkmodel, "Tlag"),
                       .getPkmodelStr(.pkmodel, "p"),
                       ", cmt=1",
                       ")"))
  }
  if (!is.na(.pkmodel["k12"]) && !is.na(.pkmodel["k21"])) {
    .macro <- c(.macro,
                paste0("peripheral(",
                       .getPkmodelStr(.pkmodel, "k12", ""),
                       .getPkmodelStr(.pkmodel, "k21"),
                       ")"))
  }
  if (!is.na(.pkmodel["k13"]) && !is.na(.pkmodel["k31"])) {
    .macro <- c(.macro,
                paste0("peripheral(",
                       .getPkmodelStr(.pkmodel, "k13", ""),
                       .getPkmodelStr(.pkmodel, "k31"),
                       ")"))
  }
  .macro <- c(.macro,
              paste0("elimination(cmt=1",
                     .getPkmodelStr(.pkmodel, "k"),
                     .getPkmodelStr(.pkmodel, "Cl"),
                     .getPkmodelStr(.pkmodel, "Vm"),
                     .getPkmodelStr(.pkmodel, "Km"),
                     ")"))

  if (!is.na(pk$Ce)) {
    .macro <- c(.macro,
                paste0("effect(cmt=1",
                       .getPkmodelStr(.pkmodel, "ke0"),
                       ", concentration=", pk$Ce,
                       ")"))
  }
  if (txt) return(.macro)
  .macro <- paste(.macro, collapse="\n")
  .pk2 <- .pk(.macro)
  .pk2$compartment <- rbind(.pk2$compartment, pk$compartment)
  .pk2$peripheral <- rbind(.pk2$peripheral, pk$peripheral)
  .pk2$effect <- rbind(.pk2$effect, pk$effect)
  .pk2$transfer <- rbind(.pk2$transfer, pk$transfer)
  .pk2$depot <- rbind(.pk2$depot, pk$depot)
  .pk2$oral <- rbind(.pk2$oral, pk$oral)
  .pk2$iv <- rbind(.pk2$iv, pk$iv)
  .pk2$empty <- rbind(.pk2$empty, pk$empty)
  .pk2$reset <- rbind(.pk2$reset, pk$reset)
  .pk2$elimination <- rbind(.pk2$elimination, pk$elimination)
  .pk2
}
