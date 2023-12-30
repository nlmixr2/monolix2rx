test_that("input categorical covariates and regressors", {

  .tmp <- .ind("
input = {V_pop, omega_V, ka_pop, omega_ka, Cl_pop, omega_Cl, logtAge, Race, Sex, logtWeight,
beta_Cl_Race_Caucasian, beta_Cl_Race_Latin, beta_Cl_Smoke_yes, beta_Cl_logtAge, beta_V_logtWeight, E0}
E0 = {use = regressor}
Race = {type=categorical, categories={Caucasian, Black, Latin}}
Sex = {type=categorical, categories={M, F}}")

  expect_snapshot(print(.tmp))

  .tmp2 <- list(input = c("V_pop", "omega_V", "ka_pop", "omega_ka",
                          "Cl_pop", "omega_Cl", "logtAge", "Race", "Sex", "logtWeight",
                           "beta_Cl_Race_Caucasian", "beta_Cl_Race_Latin", "beta_Cl_Smoke_yes",
                          "beta_Cl_logtAge", "beta_V_logtWeight", "E0"),
                cat = list(Race = list(cat = c("Caucasian", "Black", "Latin"),
                                       quote = c(FALSE, FALSE, FALSE)),
                           Sex = list(cat = c("M", "F"),
                                      quote = c(FALSE, FALSE))),
                reg = "E0",
                file=character(0))
  class(.tmp2) <- "monolix2rxInd"

  expect_equal(.tmp, .tmp2)
  .tmp <- .ind("input = {AGE, DOSE, SEX}
DOSE = {type=categorical, categories={'50 mg', '100 mg'}}
SEX = {type=categorical, categories={Female, Male}}")

  expect_snapshot(print(.tmp))

  .tmp2 <- list(input = c("AGE", "DOSE", "SEX"),
                cat = list(DOSE = list(cat = c("50 mg", "100 mg"),
                                       quote = c(TRUE, TRUE)),
                           SEX = list(cat = c("Female", "Male"),
                                      quote = c(FALSE, FALSE))),
                reg = character(0),
                file=character(0))
  class(.tmp2) <- "monolix2rxInd"
  expect_equal(.tmp, .tmp2)

})
