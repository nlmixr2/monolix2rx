.monolix2rxBuildEquation <- function() {
  message("Update Parser c for [LONGITUDINAL] EQUATION: block")
  dparser::mkdparse(devtools::package_file("inst/equation.g"),
                    devtools::package_file("src/"),
                    grammar_ident="monolixLongEq")
  file.rename(devtools::package_file("src/equation.g.d_parser.c"),
              devtools::package_file("src/equation.g.d_parser.h"))
}
