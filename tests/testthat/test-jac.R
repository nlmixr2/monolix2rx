test_that("test jacobian calculation", {

  indDef <- c("ktr = {distribution=logNormal, typical=ktr_pop, sd=omega_ktr}",
               "ka = {distribution=normal, typical=ka_pop, sd=omega_ka}",
               "cl = {distribution=logNormal, mean=cl_pop, sd=omega_cl}",
               "v = {distribution=probitNormal, mean=v_pop, sd=omega_v}",
               "emax = {distribution=logitNormal, min=0, max=1, typical=emax_pop, sd=omega_emax}",
               "ec50 = {distribution=logNormal, typical=ec50_pop, sd=omega_ec50}",
               "kout = {distribution=logNormal, typical=kout_pop, sd=omega_kout}",
               "e0 = {distribution=logNormal, typical=e0_pop, sd=omega_e0}")

  indDef <- .indDef(paste(indDef, collapse="\n"))

  par <- c("ktr_pop={value=1, method=MLE}",
           "ka_pop={value=1, method=MLE}",
           "cl_pop={value=0.1, method=MLE}",
           "v_pop={value=10, method=MLE}",
           "prop__err={value=0.1, method=MLE}",
           "pkadd__err={value=0.1, method=MLE}",
           "emax_pop={value=0.8, method=MLE}",
           "ec50_pop={value=0.5, method=MLE}",
           "kout_pop={value=0.05, method=MLE}",
           "e0_pop={value=100, method=MLE}",
           "pdadd__err={value=10, method=MLE}",
           "omega_ktr={value=1, method=MLE}",
           "omega_ka={value=1, method=MLE}",
           "omega_cl={value=1.4142135623731, method=MLE}",
           "omega_v={value=1, method=MLE}",
           "omega_emax={value=0.707106781186548, method=MLE}",
           "omega_ec50={value=0.707106781186548, method=MLE}",
           "omega_kout={value=0.707106781186548, method=MLE}",
           "omega_e0={value=0.707106781186548, method=MLE}")

  par <- .parameter(paste(par, collapse="\n"))

  expect_equal(.mlxtranJacobianDiag(indDef, par),
               c(ktr_pop = 1, ka_pop = 1, cl_pop = 1.10517091807565, v_pop = 7.69459862670642e-23, emax_pop = 0.16, ec50_pop = 0.5, kout_pop = 0.05, e0_pop = 100),
               tolerance=1e-3)

})
