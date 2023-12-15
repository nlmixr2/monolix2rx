.monolix2rxBuildGram <- function() {
  message("monolix user txt model parsing")
  message("Update Parser c for [LONGITUDINAL] INPUT: block")

  dparser::mkdparse(devtools::package_file("inst/longInput.g"),
                    devtools::package_file("src/"),
                    grammar_ident="monolixLongInput")
  file.rename(devtools::package_file("src/longInput.g.d_parser.c"),
              devtools::package_file("src/longInput.g.d_parser.h"))

  message("Update Parser c for [LONGITUDINAL] EQUATION: block")
  dparser::mkdparse(devtools::package_file("inst/equation.g"),
                    devtools::package_file("src/"),
                    grammar_ident="monolixLongEq")
  file.rename(devtools::package_file("src/equation.g.d_parser.c"),
              devtools::package_file("src/equation.g.d_parser.h"))

  message("Update Parser c for [LONGITUDINAL] OUTPUT: block")
  dparser::mkdparse(devtools::package_file("inst/longOutput.g"),
                    devtools::package_file("src/"),
                    grammar_ident="longOutput")
  file.rename(devtools::package_file("src/longOutput.g.d_parser.c"),
              devtools::package_file("src/longOutput.g.d_parser.h"))
  message("mlxtran grammer")
  message("Update Parser c for <MODEL> [INDIVIDUAL]")
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
  invisible("")
}
