## nocov start
.monolix2rxBuildGram <- function() {
  message("monolix user txt model parsing")

  message("Update Parser c for [LONGITUDINAL] EQUATION: block")
  dparser::mkdparse(devtools::package_file("inst/equation.g"),
                    devtools::package_file("src/"),
                    grammar_ident="equation")
  file.rename(devtools::package_file("src/equation.g.d_parser.c"),
              devtools::package_file("src/equation.g.d_parser.h"))

  message("Update Parser c for [LONGITUDINAL] OUTPUT: block")
  dparser::mkdparse(devtools::package_file("inst/longOutput.g"),
                    devtools::package_file("src/"),
                    grammar_ident="longOutput")
  file.rename(devtools::package_file("src/longOutput.g.d_parser.c"),
              devtools::package_file("src/longOutput.g.d_parser.h"))

  message("mlxtran grammar")

  message("Update Parser c for [FILEINFO]")
  dparser::mkdparse(devtools::package_file("inst/mlxtranFileinfo.g"),
                    devtools::package_file("src/"),
                    grammar_ident="mlxtranFileinfo")
  file.rename(devtools::package_file("src/mlxtranFileinfo.g.d_parser.c"),
              devtools::package_file("src/mlxtranFileinfo.g.d_parser.h"))

  message("Update Parser c for [CONTENT]")
  dparser::mkdparse(devtools::package_file("inst/mlxtranContent.g"),
                    devtools::package_file("src/"),
                    grammar_ident="mlxtranContent")
  file.rename(devtools::package_file("src/mlxtranContent.g.d_parser.c"),
              devtools::package_file("src/mlxtranContent.g.d_parser.h"))

  message("Update Parser c for input specification")
  dparser::mkdparse(devtools::package_file("inst/mlxtranInd.g"),
                    devtools::package_file("src/"),
                    grammar_ident="mlxtranInd")
  file.rename(devtools::package_file("src/mlxtranInd.g.d_parser.c"),
              devtools::package_file("src/mlxtranInd.g.d_parser.h"))

  message("Update Parser c for <MODEL> [INDIVIDUAL] DEFINITION:")
  dparser::mkdparse(devtools::package_file("inst/mlxtranIndDefinition.g"),
                    devtools::package_file("src/"),
                    grammar_ident="mlxtranIndDefinition")
  file.rename(devtools::package_file("src/mlxtranIndDefinition.g.d_parser.c"),
              devtools::package_file("src/mlxtranIndDefinition.g.d_parser.h"))

  message("Update Parser c for [LONGITUDINAL] DEFINITION: block")
  dparser::mkdparse(devtools::package_file("inst/longDef.g"),
                    devtools::package_file("src/"),
                    grammar_ident="longDef")
  file.rename(devtools::package_file("src/longDef.g.d_parser.c"),
              devtools::package_file("src/longDef.g.d_parser.h"))

  ## message("Update Parser c for <MODEL> [INDIVIDUAL] PK:")
  ## dparser::mkdparse(devtools::package_file("inst/mlxtranPk.g"),
  ##                   devtools::package_file("src/"),
  ##                   grammar_ident="mlxtranPk")
  ## file.rename(devtools::package_file("src/mlxtranPk.g.d_parser.c"),
  ##             devtools::package_file("src/mlxtranPk.g.d_parser.h"))

  message("Update Parser c for <PARAMETER>")
  dparser::mkdparse(devtools::package_file("inst/mlxtranParameter.g"),
                    devtools::package_file("src/"),
                    grammar_ident="mlxtranParameter")
  file.rename(devtools::package_file("src/mlxtranParameter.g.d_parser.c"),
              devtools::package_file("src/mlxtranParameter.g.d_parser.h"))

  message("Update Parser c for <FIT>")
  dparser::mkdparse(devtools::package_file("inst/mlxtranFit.g"),
                    devtools::package_file("src/"),
                    grammar_ident="mlxtranFit")
  file.rename(devtools::package_file("src/mlxtranFit.g.d_parser.c"),
              devtools::package_file("src/mlxtranFit.g.d_parser.h"))

  message("Update Parser c for mlxtran options")
  dparser::mkdparse(devtools::package_file("inst/mlxtranOp.g"),
                    devtools::package_file("src/"),
                    grammar_ident="mlxtranOp")
  file.rename(devtools::package_file("src/mlxtranOp.g.d_parser.c"),
              devtools::package_file("src/mlxtranOp.g.d_parser.h"))

  message("Update Parser c for mlxtran tasks")
  dparser::mkdparse(devtools::package_file("inst/mlxtranTask.g"),
                    devtools::package_file("src/"),
                    grammar_ident="mlxtranTask")
  file.rename(devtools::package_file("src/mlxtranTask.g.d_parser.c"),
              devtools::package_file("src/mlxtranTask.g.d_parser.h"))

  message("Update Parser c for DATASET INFORMATION in summary.txt")
  dparser::mkdparse(devtools::package_file("inst/summaryData.g"),
                    devtools::package_file("src/"),
                    grammar_ident="summaryData")
  file.rename(devtools::package_file("src/summaryData.g.d_parser.c"),
              devtools::package_file("src/summaryData.g.d_parser.h"))

  message("Update Parser c for <DATAFILE> [SETTINGS]")

  dparser::mkdparse(devtools::package_file("inst/dataSettings.g"),
                    devtools::package_file("src/"),
                    grammar_ident="dataSettings")
  file.rename(devtools::package_file("src/dataSettings.g.d_parser.c"),
              devtools::package_file("src/dataSettings.g.d_parser.h"))
  .monolix2rxBuildRxSolve()
  invisible("")
}

.monolix2rxBuildRxSolve <- function() {
  message("build options for rxSolve to match Monolix")
  .args <- deparse(eval(str2lang(paste0("args(rxode2::rxSolve)"))))
  .args[1] <- paste0("rxSolve.monolix2rx <-", .args[1])
  .args <- .args[-length(.args)]
  .extra <- quote({
    if (missing(cores)) {
      cores <- 0L
    }
    if (missing(covsInterpolation)) {
      covsInterpolation <- "locf"
      .minfo("using locf interpolation like Monolix, specify directly to change")
    }
    if (!missing(nStud)) {
      if (missing(dfSub)) {
        if (!is.null(object$meta$dfSub)) {
          dfSub <- object$meta$dfSub
          .minfo(paste0("using dfSub=", dfSub, " from Monolix"))
        } else if (!is.null(object$dfSub)) {
          dfSub <- object$dfSub
          .minfo(paste0("using dfSub=", dfSub, " from Monolix"))
        }
      }
      if (missing(dfObs)) {
        if (!is.null(object$meta$dfObs)) {
          dfObs <- object$meta$dfObs
          .minfo(paste0("using dfObs=", dfObs, " from Monolix"))
        } else if (!is.null(object$dfObs)) {
          dfObs <- object$dfObs
          dfObs <- object$meta$dfObs
          .minfo(paste0("using dfObs=", dfObs, " from Monolix"))
        }
      }
      if (missing(thetaMat)) {
        if (!is.null(object$meta$thetaMat)) {
          thetaMat <- object$meta$thetaMat
          .minfo(paste0("using thetaMat from Monolix"))
        } else if (!is.null(object$thetaMat)) {
          thetaMat <- object$meta$thetaMat
          .minfo(paste0("using thetaMat from Monolix"))
        }
      }
    }
    # The theta/omega comes from the ui
    if ((missing(events) && missing(params))) {
      if (!is.null(object$monolixData)) {
        events <- object$monolixData
        .minfo(paste0("using Monolix's data for solving"))
      }
    }
    .atol <- .rtol <- .getRtolAtol(object)
    if (missing(atol)) {
      atol <- .atol
      .minfo(paste0("using Monolix specified atol=", atol))
    }
    if (missing(rtol)) {
        rtol <- .rtol
        .minfo(paste0("using Monolix specified rtol=", rtol))
    }
    # ssAtol=100, ssRtol=100,
    if (missing(ssRtol)) {
      ssRtol <- 100
      .minfo(paste0("Since Monolix doesn't use ssRtol, set ssRtol=", ssRtol))
    }
    if (missing(ssAtol)) {
      ssAtol <- 100
      .minfo(paste0("Since Monolix doesn't use ssRtol, set ssAtol=", ssAtol))
    }
    .nss <- .getNbdoses(object)
    if (missing(maxSS) && missing(maxSS)) {
      maxSS <- .nss + 1
      minSS <- .nss
      .minfo(paste0("Since Monolix uses a set number of doses for steady state use maxSS=", maxSS,
                    ", minSS=", minSS))
    }
    .cls <- class(object)
    class(object) <- .cls[-which(.cls == "monolix2rx")]
  })
  .extra <- vapply(.extra,
                   function(l) {
                     if (identical(l, quote(`{`))) {
                       return("")
                     }
                     return(paste(deparse(l), collapse="\n"))
                   }, character(1), USE.NAMES=FALSE)[-1]
  .args <- c(.args, "{", .extra)
  .formalArgs <- as.character(eval(str2lang(paste0("formalArgs(rxode2::rxSolve)"))))
  .w <- which(.formalArgs == "...")
  .formalArgs <- paste0(.formalArgs, "=", .formalArgs)
  .has3 <- FALSE
  if (length(.w) > 0) {
    .formalArgs[.w] <- "..."
    .has3 <- TRUE
  }
  .formalArgs <- paste(.formalArgs, collapse=", ")
  .formalArgs <- paste0("rxode2::rxSolve(", .formalArgs, ")")
  .args <- c(.args, .formalArgs, "}")
  .args <- paste(.args, collapse="\n")
  .args <- c("# This is built from buildParser.R, edit there",
             "#'@export", deparse(str2lang(.args)))
  writeLines(.args, devtools::package_file("R/rxSolve.R"))
  message("done")
}

.monolix2rxRxUiGetMethods <- function() {
  message("build rxUiGet options to allow str() and dollar completion")
  .meth <- c("nonmemData"="NONMEM input data from nonmem2rx",
             "etaData"="NONMEM etas input from nonmem2rx",
             "ipredAtol"="50th percentile of the IPRED atol comparison between rxode2 and model import",
             "ipredRtol"="50th percentile of the IPRED rtol comparison between rxode2 and model import",
             "ipredCompare"="Dataset comparing ID, TIME and the IPREDs between rxode2 and model import",
             "predAtol"="50th percentile of the PRED atol comparison between rxode2 and model import",
             "predRtol"="50th percentile of the PRED rtol comparison between rxode2 and model import",
             "predCompare"="Dataset comparing ID, TIME and the PREDs between rxode2 and model import",
             "sigma"="sigma matrix from model import",
             "thetaMat"="covariance matrix",
             "dfSub"="Number of subjects",
             "dfObs"="Number of observations",
             "atol"="atol imported from translation",
             "rtol"="rtol imported from translation",
             "ssRtol"="ssRtol imported from translation",
             "ssAtol"="ssRtol imported from translation")
  .ret <- paste(c("## nocov start",
                  "# This is built from buildParser.R, edit there",
                  vapply(seq_along(.meth), function(i) {
                    .name <- names(.meth)[i]
                    .desc <- setNames(.meth[i], NULL)
                    .ret <- c("",
                              sprintf("rxUiGet.%s <- function(x, ...) {", .name),
                              "  .meta <- new.env(parent=emptyenv())",
                              "  if (exists(\"meta\", envir=x[[1]])) .meta <- get(\"meta\", envir=x[[1]])",
                              sprintf("  if (exists(\"%s\", envir=.meta)) return(get(\"%s\", envir=.meta))", .name, .name),
                              sprintf("  if (!exists(\"%s\", envir=x[[1]])) return(NULL)", .name),
                              sprintf("  get(\"%s\", envir=x[[1]])", .name),
                              "}",
                              sprintf("attr(rxUiGet.%s, \"desc\") <- %s", .name, deparse1(.desc)))
                    .ret <- paste(.ret, collapse="\n")
                  }, character(1), USE.NAMES=TRUE),
                  ".rxUiGetRegister <- function() {",
                  vapply(seq_along(.meth), function(i) {
                    .name <- names(.meth)[i]
                    sprintf("  rxode2::.s3register(\"rxode2::rxUiGet\", \"%s\")", .name)
                  }, character(1), USE.NAMES=TRUE),
                  "}",
                  "## nocov end"), collapse="\n")
  writeLines(.ret, devtools::package_file("R/rxUiGetGen.R"))
  message("done")
}

## nocov end
