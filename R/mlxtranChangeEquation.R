#' Change Equation Info to Parsed List
#'
#' This function takes a `mlxtran` object and converts the covariate equation information
#' into a parsed list.
#'
#' @param mlxtran A list containing the `MODEL` and `COVARIATE`
#'   information from `mlxtran()`
#' @param eq parsed assignments to use instead of the covariate equations
#' @return A named list of parsed covariate equations or `NULL` if the
#'   covariate information is not present.
#' @noRd
#' @author Matthew L. Fidler
#' @examples
#'
#' m <- mlxtran(file.path(system.file("cov", package="monolix2rx"), "warfarin_covariate3_project.mlxtran"))
#' .mlxtranChangeEquationInfoToParsedList(m)
#'
.mlxtranChangeEquationInfoToParsedList <- function(mlxtran, eq=NULL) {
  if (is.null(eq)) {
    .cov <- mlxtran$MODEL$COVARIATE$EQUATION
    if (is.null(.cov)) {
      return(NULL)
    }
    if (is.null(.cov$dplyr)) {
      return(NULL)
    }
    .e <- str2lang(paste0("{", paste(.cov$dplyr, collapse="\n"), "}"))
  } else {
    .e <- as.call(c(list(quote(`{`)), eq))
  }
  .ret <- lapply(seq_along(.e)[-1],
                 function(i) {
                   .e[[i]][[3]]
                 })
  names(.ret) <- vapply(seq_along(.e)[-1],
                        function(i) {
                          deparse1(.e[[i]][[2]])
                        },character(1), USE.NAMES=TRUE)
  # a covariate defined from an earlier one (lBMI from BMI); a name
  # assigned again keeps its last value
  .cur <- list()
  for (.i in seq_along(.ret)) {
    .cur[[names(.ret)[.i]]] <- .mlxtranChangeF(.ret[[.i]], .cur)
  }
  .cur
}

#' Modify elements of an expression based on a lookup list
#'
#' This function recursively traverses an expression and replaces elements
#' based on a provided lookup list. If an element is a name and it exists in
#' the lookup list, it is replaced by the corresponding value from the list.
#' If the element is a call, the function is applied recursively to its arguments.
#'
#' @param x An expression to be modified. It can be a name, a call, or other types.
#'
#' @param lst A named list used for lookup. If a name in the expression matches
#' a name in this list, it will be replaced by the corresponding value.
#'
#' @return The modified expression with names replaced according to the lookup list.
#'
#' @noRd
#'
#' @keywords internal
#'
#' @author Matthew L. Fidler
#'
.mlxtranChangeF <- function(x, lst) {
  if (is.name(x)) {
    x0 <- deparse1(x)
    if (x0 %in% names(lst)) {
      lst[[x0]]
    } else {
      x
    }
  } else if (is.call(x)) {
    as.call(c(x[[1]], lapply(x[-1], .mlxtranChangeF, lst=lst)))
  } else {
    x
  }
}

#' Replace values in a model with mlxtran transformation equations
#'
#' @param model an unevaluated R lang object to replace the values
#'   from the mlxtran modification object
#'
#' @param mlxtran a mlxtran trans object
#'
#' @return an expression where the transformations are changed.  For
#'   example if the continuous transformation equations are defined by
#'   mlxtran to be `lw70 = log(weight/70)` then the final expression
#'   would replace all `lw70` values with `log(weight/70)`
#'
#' @noRd
#' @author Matthew L. Fidler
#' @examples
#'
#' mod <- quote(model({
#'   cmt(depot)
#'   cmt(central)
#'   if (sex == 0) {
#'     tSex <- "F"
#'   } else if (sex == 1) {
#'     tSex <- "M"
#'   } else {
#'     tSex <- "M"
#'   }
#'   Tlag <- exp(Tlag_pop + omega_Tlag)
#'   ka <- exp(ka_pop + omega_ka)
#'   V <- exp(V_pop + beta_V_tSex_F * (tSex == "F") + beta_V_lw70 * lw70 + omega_V)
#'   Cl <- exp(Cl_pop + beta_Cl_tSex_F * (tSex == "F")  + beta_Cl_lw70 * lw70 + omega_Cl)
#'   d/dt(depot) <- -ka * depot
#'   alag(depot) <- Tlag
#'   d/dt(central) <- +ka * depot - Cl/V * central
#'   Cc <- central/V
#'   concentration <- Cc
#'   concentration ~ add(a) + prop(b) + combined1()
#' }))
#'
#' m <- mlxtran(file.path(system.file("cov", package="monolix2rx"), "warfarin_covariate3_project.mlxtran"))
#'
#' .mlxtranChangeVal(mod, mlxtran)
.mlxtranChangeVal <- function(model, mlxtran) {
  .cov <- mlxtran$MODEL$COVARIATE$EQUATION$dplyr
  if (length(.cov) > 0L) {
    .e <- as.list(str2lang(paste0("{", paste(.cov, collapse="\n"), "}")))[-1]
    .isAssign <- vapply(.e, function(x) identical(x[[1]], quote(`=`)), logical(1))
    if (!all(.isAssign)) {
      # if/else cannot be inlined; it starts the model, with the
      # assignments using what it assigns.  The others are still inlined
      # so they stay covariates of the parameters.
      .inModel <- !.isAssign
      .set <- unique(unlist(lapply(.e[.inModel], .mlxtranAssigned)))
      for (.i in which(.isAssign)) {
        if (any(all.vars(.e[[.i]][[3]]) %in% .set)) {
          .inModel[.i] <- TRUE
          .set <- c(.set, deparse1(.e[[.i]][[2]]))
        }
      }
      .lst <- .mlxtranChangeEquationInfoToParsedList(mlxtran, .e[!.inModel])
      .lines <- lapply(.e[.inModel], .mlxtranAssignArrow)
      model[[2]] <- as.call(c(list(quote(`{`)), .lines, as.list(model[[2]])[-1]))
      return(.mlxtranChangeF(model, lst=.lst))
    }
  }
  .lst <- .mlxtranChangeEquationInfoToParsedList(mlxtran)
  .mlxtranChangeF(model, lst=.lst)
}

#' Names assigned anywhere in an expression
#'
#' @param x expression
#' @return character vector of assigned names
#' @noRd
#' @author Matthew L. Fidler
.mlxtranAssigned <- function(x) {
  if (!is.call(x)) return(character(0))
  .ret <- if (identical(x[[1]], quote(`=`)) || identical(x[[1]], quote(`<-`))) deparse1(x[[2]])
  unique(c(.ret, unlist(lapply(as.list(x)[-1], .mlxtranAssigned))))
}

#' Write `=` assignments as `<-`
#'
#' @param x expression
#' @return expression with `<-` assignments
#' @noRd
#' @author Matthew L. Fidler
.mlxtranAssignArrow <- function(x) {
  if (!is.call(x)) return(x)
  if (identical(x[[1]], quote(`=`))) x[[1]] <- quote(`<-`)
  as.call(c(x[[1]], lapply(as.list(x)[-1], .mlxtranAssignArrow)))
}
