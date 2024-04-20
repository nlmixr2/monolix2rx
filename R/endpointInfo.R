#' This gets the monolix endpoint information in the model
#'
#'
#' @param mlxtran This is the parsed mlxtran or imported rxode2 model from monolix
#'
#' @return nothing, called for side effects
#' @export
#' @author Matthew L. Fidler
monolixEndpoints <- function(mlxtran) {
  mlxtran <- .monolixGetMlxtran(mlxtran)
  if (is.null(mlxtran)) return(NULL)
  .end <- mlxtran$MODEL$LONGITUDINAL$DEFINITION$endpoint
  vapply(seq_along(.end),
         function(i) {
           .end[[i]]$var
         }, character(1), USE.NAMES = FALSE)
}
