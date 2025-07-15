monolix2rxlixoftConnectors <- function() {
  .Call(`_monolix2rxlixoftConnectors`)
}

monolix2rxInitializeLixoftConnectors <- function(software="monolix", force=TRUE) {
  checkmate::assertLogical(force, len=1L, any.missing=FALSE)
  checkmate::assertCharacter(software, len=1L, any.missing=FALSE)
  .Call(`_monolix2rxInitializeLixoftConnectors`, software, force)
}

monolix2rxGetLibraryModelContent <- function(filename) {
  checkmate::assertCharacter(filename, len=1L, any.missing=FALSE)
  .Call(`_monolix2rxGetLibraryModelContent`, filename)
}
