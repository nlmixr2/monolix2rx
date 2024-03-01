#' Parse the population definition section
#'
#' @param text text for parsing
#' @return a monolix2rxPopDef object
#' @noRd
#' @author Matthew L. Fidler
.popDef <- function(text) {
  .ret <- .indDef(text)
  class(.ret) <- "monolix2rxPopDef"
  .ret
}

#' @export
as.character.monolix2rxPopDef <- as.character.monolix2rxIndDef

#' @export
print.monolix2rxPopDef <- print.monolix2rxIndDef

# @export
print.monolix2rxPopDef <- print.monolix2rxIndDef

#' @export
as.list.monolix2rxPopDef <- as.list.monolix2rxIndDef
