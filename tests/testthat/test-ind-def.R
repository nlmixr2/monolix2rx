
test_that("typical and mean cannot be duplicated", {
  expect_error(.indDef("F = {distribution=logitnormal, typical=F_pop, mean=F_pop_2, sd=omega_F, min=0, max=1}"))
  expect_error(.indDef("F = {distribution=logitnormal, mean=F_pop_2, typical=F_pop, sd=omega_F, min=0, max=1}"))
})

test_that("min max only work with logitnormal", {
  expect_error(.indDef("F = {distribution=normal, typical=F_pop, sd=omega_F, min=0, max=1}"))
  expect_error(.indDef("F = {distribution=normal, mean=F_pop_2, sd=omega_F, min=0, max=1}"))
})

test_that("cannot use both var and sd", {
  expect_error(.indDef("F = {distribution=normal, typical=F_pop, sd=omega_F, var=omega_F2}"))
  expect_error(.indDef("F = {distribution=normal, mean=F_pop_2, var=omega_F2, sd=omega_F}"))
})

test_that("error on covariate/coefficient mismatch", {
  expect_error(.indDef("V = {distribution=lognormal, covariate=Race, coefficient={0, {beta_V_Race_Black, beta_V_Race_Red}, beta_V_Race_Latin},typical=V_pop, sd=omega_V }"))

  expect_error(.indDef("ka = {distribution=lognormal,typical=ka_pop, covariate={Race, Wt,sexf}, coefficient={{0, beta_ka_Race_Black, beta_ka_Race_Latin},beta_ka_Wt}, no-variability}"))
})

test_that("min/max numeric", {
  expect_error(.indDef("f = {distribution=logitnormal, typical=F_pop,sd=omega_F, min=min0, max=1}"))
  expect_error(.indDef("f = {distribution=logitnormal, typical=F_pop,sd=omega_F, min=0, max=max1}"))
})

test_that("duplicate correlations error", {
  expect_error(.indDef("correlation = {level=id, r(V, Cl)=corr1_V_Cl, r(Cl, V)=corr1_Cl_V}"))
})

test_that("error with varlevel mismatch", {
  expect_error(.indDef("ka = {distribution=logNormal, typical=ka_pop, varlevel={id, id*occ, id*occ*occ}, sd={omega_ka, gamma_ka}}"))
})

test_that("duplicated parameters error", {
  expect_error(.indDef("ka = {distribution=logNormal, typical=ka_pop, varlevel={id, id*occ}, sd={omega_ka, omega_ka}}"),
               'omega_ka')
  expect_error(.indDef("ka = {distribution=logNormal, typical=ka_pop, varlevel={id, id*occ}, sd={omega_ka, gamma_ka}}
correlation = {r(V, Cl)=gamma_ka}"),
               'gamma_ka')

})


test_that("standard individual definition", {

  tmp <- .indDef("F = {distribution=logitnormal, typical=F_pop,sd=omega_F, min=0, max=1}
ka = {distribution=lognormal,typical=ka_pop, no-variability}
V = {distribution=lognormal,typical=V_pop, sd=omega_V }
Cl = {distribution=lognormal,typical=Cl_pop, sd=omega_Cl}
correlation = {level=id, r(V, Cl)=corr1_V_Cl}
")
  expect_equal(tmp$rx,
               c("F <- expit(F_pop + omega_F, 0, 1)",
                 "ka <- exp(ka_pop)",
                 "V <- exp(V_pop + omega_V)",
                 "Cl <- exp(Cl_pop + omega_Cl)"))
})

test_that("iov definition", {
  tmp <- .indDef("Tlag = {distribution=logNormal, typical=Tlag_pop, varlevel=id*occ, sd=gamma_Tlag}
ka = {distribution=logNormal, typical=ka_pop, varlevel={id, id*occ}, sd={omega_ka, gamma_ka}}
Cl = {distribution=logNormal, typical=Cl_pop, varlevel={id, id*occ}, sd={omega_Cl, 4}}
V = {distribution=logNormal, typical=V_pop, sd=omega_V}
correlation = {level=id, r(V, Cl)=corr1_V_Cl}
correlation = {level=id*occ, r(ka, Tlag)=corr2_ka_Tlag}")

  expect_equal(tmp$rx,
               c("Tlag <- exp(Tlag_pop + gamma_Tlag)",
                 "ka <- exp(ka_pop + omega_ka + gamma_ka)",
                 "Cl <- exp(Cl_pop + omega_Cl + rxVar_Cl_2)",
                 "V <- exp(V_pop + omega_V)"))
})

test_that("mu referenced covariate description", {
  tmp <- .indDef("F = {distribution=logitnormal, typical=F_pop,sd=omega_F, min=0, max=1}
ka = {distribution=lognormal,typical=ka_pop, covariate={Race, Wt}, coefficient={{0, beta_ka_Race_Black, beta_ka_Race_Latin},beta_ka_Wt}, no-variability}
V = {distribution=lognormal, covariate=Race, coefficient={0, beta_V_Race_Black, beta_V_Race_Latin},typical=V_pop, sd=omega_V }
Cl = {distribution=lognormal,typical=Cl_pop, covariate=Wt, coefficient=beta_Cl_Wt, sd=omega_Cl}
correlation = {level=id, r(V, Cl)=corr1_V_Cl}
")

  expect_equal(tmp$rx,
               c("F <- expit(F_pop + omega_F, 0, 1)",
                 "ka <- exp(ka_pop + rxCov_ka_Race_1*Race + beta_ka_Race_Black*Race + beta_ka_Race_Latin*Race + beta_ka_Wt*Wt)",
                 "V <- exp(V_pop + rxCov_V_Race_1*Race + beta_V_Race_Black*Race + beta_V_Race_Latin*Race + omega_V)",
                 "Cl <- exp(Cl_pop + beta_Cl_Wt*Wt + omega_Cl)"))
})
