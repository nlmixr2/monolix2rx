test_that("<MONOLIX> TASK", {

  .ret <- .task("populationParameters()
individualParameters(method = {conditionalMode})
fim(method = Linearization)
logLikelihood(method = Linearization)
plotResult(method = {outputplot, indfits, obspred, residualsscatter, residualsdistribution, parameterdistribution, covariatemodeldiagnosis, randomeffects, covariancemodeldiagnosis, saemresults})
")

  .ret2 <- list(populationParameters = list(),
                individualParameters = list(method = list("conditionalMode")),
                fim = list(method = "Linearization"),
                logLikelihood = list(method = "Linearization"),
                plotResult = list(method = list("outputplot", "indfits", "obspred",
                                                "residualsscatter", "residualsdistribution",
                                                "parameterdistribution", "covariatemodeldiagnosis",
                                                "randomeffects", "covariancemodeldiagnosis", "saemresults")))
  class(.ret2) <- "monolix2rxTask"

  expect_equal(.ret, .ret2)

  .ret <- .task("populationParameters()
individualParameters(method = {conditionalMode})
fim(method = Linearization)
logLikelihood(method = Linearization)
plotResult(method = {outputplot, indfits, obspred, residualsscatter, residualsdistribution, parameterdistribution, covariatemodeldiagnosis, randomeffects, covariancemodeldiagnosis, saemresults}, color=red)
")

  .ret2 <- list(populationParameters = list(),
                individualParameters = list(method = list("conditionalMode")),
                fim = list(method = "Linearization"),
                logLikelihood = list(method = "Linearization"),
                plotResult = list(method = list("outputplot", "indfits", "obspred",
                                                "residualsscatter", "residualsdistribution",
                                                "parameterdistribution", "covariatemodeldiagnosis",
                                                "randomeffects", "covariancemodeldiagnosis", "saemresults"),
                                  color = "red"))
  class(.ret2) <- "monolix2rxTask"
  expect_equal(.ret, .ret2)

  expect_error(as.list(.ret), NA)

  expect_snapshot(print(.ret))

})
