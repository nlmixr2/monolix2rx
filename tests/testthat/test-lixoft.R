test_that("test that reading models with lixoftConnectors works", {
  # This requires lixoft connectors to be installed and configured
  skip_if_not(monolix2rxlixoftConnectors(), "lixoftConnectors not available")
  expect_error(mlxTxt("lib:bolus_1cpt_TlagVCl.txt"), NA)
  expect_error(monolix2rx("lib:bolus_1cpt_TlagVCl.txt"), NA)
})
