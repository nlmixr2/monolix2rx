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

  invisible("")
}
## nocov end
