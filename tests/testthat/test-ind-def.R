
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

.indDef("F = {distribution=logitnormal, typical=F_pop,sd=omega_F, min=0, max=1}
ka = {distribution=lognormal,typical=ka_pop, no-variability}
V = {distribution=lognormal,typical=V_pop, sd=omega_V }
Cl = {distribution=lognormal,typical=Cl_pop, sd=omega_Cl}
correlation = {level=id, r(V, Cl)=corr1_V_Cl}
")

tmp <- .indDef("Tlag = {distribution=logNormal, typical=Tlag_pop, varlevel=id*occ, sd=gamma_Tlag}
ka = {distribution=logNormal, typical=ka_pop, varlevel={id, id*occ}, sd={omega_ka, gamma_ka}}
Cl = {distribution=logNormal, typical=Cl_pop, varlevel={id, id*occ}, sd={omega_Cl, 4}}
V = {distribution=logNormal, typical=V_pop, sd=omega_V}
correlation = {level=id, r(V, Cl)=corr1_V_Cl}
correlation = {level=id*occ, r(ka, Tlag)=corr2_ka_Tlag}")

tmp <- .indDef("F = {distribution=logitnormal, typical=F_pop,sd=omega_F, min=0, max=1}
ka = {distribution=lognormal,typical=ka_pop, covariate={Race, Wt}, coefficient={{0, beta_ka_Race_Black, beta_ka_Race_Latin},beta_ka_Wt}, no-variability}
V = {distribution=lognormal, covariate=Race, coefficient={0, beta_V_Race_Black, beta_V_Race_Latin},typical=V_pop, sd=omega_V }
Cl = {distribution=lognormal,typical=Cl_pop, covariate=Wt, coefficient=beta_Cl_Wt, sd=omega_Cl}
correlation = {level=id, r(V, Cl)=corr1_V_Cl}
")
