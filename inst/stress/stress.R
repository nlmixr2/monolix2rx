## monolix2rx stress kit: rxode2 simulation -> Monolix -> monolix2rx import.
##
## On the Monolix machine, in a fresh R session (for example RStudio):
##
##   devtools::load_all("path/to/monolix2rx")    # the version to test
##   source(system.file("stress", "stress.R", package = "monolix2rx"))
##   stressCheck()                                # versions; is Monolix found?
##   res <- stressKit()
##
## stressKit() runs every case and zips the output
## (monolix2rx-stress-<date>-<time>.zip) to send back.  Without Monolix,
## stressKit(modes = "translate") checks the translations only.  Back
## home, stressReplay("<zip>") re-imports the returned Monolix output.
## See README.md and PLAN.md in this directory.
##
## The kit's functions live in an attached "monolix2rx-stress"
## environment; sourcing again replaces it.  monolix2rx: if it is already
## loaded (library() or devtools::load_all()) that version is used;
## otherwise the kit loads the source tree it sits in or the installed
## package.

local({
  ## the innermost source() of this file
  .kitDir <- NULL
  for (.fr in rev(sys.frames())) {
    .of <- tryCatch(get("ofile", envir=.fr, inherits=FALSE), error=function(e) NULL)
    if (is.character(.of) && basename(.of) == "stress.R") {
      .kitDir <- dirname(normalizePath(.of))
      break
    }
  }
  if (is.null(.kitDir)) {
    .kitDir <- if (dir.exists("inst/stress/cases")) normalizePath("inst/stress") else
      stop("load the kit with source(system.file(\"stress\", \"stress.R\", package=\"monolix2rx\"))",
           call.=FALSE)
  }
  ## a source tree has the kit in <pkg>/inst/stress; an installed package
  ## in <lib>/monolix2rx/stress
  .pkgDir <- dirname(dirname(.kitDir))
  .desc <- file.path(.pkgDir, "DESCRIPTION")
  .inSource <- basename(dirname(.kitDir)) == "inst" && file.exists(.desc) &&
    any(grepl("^Package: monolix2rx$", readLines(.desc)))
  if (!"monolix2rx" %in% loadedNamespaces()) {
    if (.inSource) {
      message("loading monolix2rx from source: ", .pkgDir)
      suppressMessages(devtools::load_all(.pkgDir, quiet=TRUE))
    } else {
      suppressMessages(library(monolix2rx))
    }
  }
  .ns <- asNamespace("monolix2rx")
  .dev <- requireNamespace("pkgload", quietly=TRUE) &&
    pkgload::is_dev_package("monolix2rx")
  ## mock/fake-monolix.R loads the same monolix2rx in its own process
  if (.dev) {
    Sys.setenv(MLXKIT_PKGDIR=pkgload::pkg_path(getNamespaceInfo(.ns, "path")))
  } else {
    Sys.unsetenv("MLXKIT_PKGDIR")
  }
  message(sprintf("monolix2rx %s (%s); rxode2 %s",
                  getNamespaceVersion(.ns), if (.dev) "source" else "installed",
                  utils::packageVersion("rxode2")))
  suppressMessages(requireNamespace("rxode2"))

  if ("monolix2rx-stress" %in% search()) detach("monolix2rx-stress", character.only=TRUE)
  .env <- new.env()
  for (.f in list.files(file.path(.kitDir, "R"), pattern="[.][Rr]$",
                        full.names=TRUE)) {
    sys.source(.f, envir=.env)
  }
  .env$.kitDir <- .kitDir
  .env$kitLoadCases(file.path(.kitDir, "cases"))
  attach(.env, name="monolix2rx-stress", warn.conflicts=FALSE)
  message(sprintf("monolix2rx stress kit: %d cases; see stressCheck(), stressList() and stressKit()",
                  length(.env$.kitEnv$cases)))
})
