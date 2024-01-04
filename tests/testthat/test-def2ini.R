if (requireNamespace("rxode2", quietly=TRUE)) {

  indDef <- c("ktr = {distribution=logNormal, typical=ktr_pop, sd=omega_ktr}",
              "ka = {distribution=logNormal, typical=ka_pop, sd=omega_ka}",
              "cl = {distribution=logNormal, typical=cl_pop, sd=omega_cl}",
              "v = {distribution=logNormal, typical=v_pop, sd=omega_v}",
              "emax = {distribution=logitNormal, min=0, max=1, typical=emax_pop, sd=omega_emax}",
              "ec50 = {distribution=logNormal, typical=ec50_pop, sd=omega_ec50}",
              "kout = {distribution=logNormal, typical=kout_pop, sd=omega_kout}",
              "e0 = {distribution=logNormal, typical=e0_pop, sd=omega_e0}",
              "correlation = {level=id, r(v, cl)=corr1_V_Cl}")
  indDef <- .indDef(paste(indDef, collapse="\n"))


  pars <- c("ktr_pop={value=1, method=MLE}",
            "ka_pop={value=1, method=FIXED}",
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
            "omega_e0={value=0.707106781186548, method=MLE}",
            "corr1_V_Cl={value=0.9, method=MLE}")
  pars <- .parameter(paste(pars, collapse="\n"))

  longDef <- c("rx_prd_cp={distribution = normal, prediction = rx_pred_cp, errorModel=combined2(pkadd__err,prop__err)}",
               "rx_prd_effect={distribution = normal, prediction = rx_pred_effect, errorModel=constant(pdadd__err)}")
  longDef <- .longDef(paste(longDef, collapse="\n"))


  test_that(".parsGetValue()", {
    expect_equal(.parsGetValue(pars, "corr1_V_Cl"), 0.9)
    expect_equal(.parsGetValue(pars, "corr1_V_Cl_not_there"), NA_real_)
  })

  test_that(".parsTransformValue()", {
    expect_equal(.parsTransformValue(0.9, "lognormal"), log(0.9))
    expect_equal(.parsTransformValue(0.9, "logitnormal", 0, 1), rxode2::logit(0.9))
    expect_equal(.parsTransformValue(0.9, "logitnormal", NULL, NULL), rxode2::logit(0.9))
    expect_equal(.parsTransformValue(0.9, "logitnormal", -1, 1.2), rxode2::logit(0.9, -1, 1.2))
    expect_equal(.parsTransformValue(0.9, "normal"), 0.9)
    expect_equal(.parsTransformValue(0.9, "probitnormal"), qnorm(0.9))
    expect_equal(.parsTransformValue(0.9, "t"), NA_real_)
  })


  test_that(".parsGetFixed()", {
    expect_equal(.parsGetFixed(pars, "corr1_V_Cl"), FALSE)
    expect_equal(.parsGetFixed(pars, "ka_pop"), TRUE)
    expect_equal(.parsGetFixed(pars, "corr1_not_there"), FALSE)
  })


  .ini <- .def2ini(indDef, pars, longDef)

  ini <- lotri::lotri

  e1 <- eval(.ini)

  expect_equal(e1,
               lotri::lotri({
                 ktr_pop <- 0
                 ka_pop <- fixed(0)
                 cl_pop <- -2.30258509299405
                 v_pop <- 2.30258509299405
                 emax_pop <- 1.38629436111989
                 ec50_pop <- -0.693147180559945
                 kout_pop <- -2.99573227355399
                 e0_pop <- 4.60517018598809
                 pkadd__err <- 0.1
                 prop__err <- 0.1
                 pdadd__err <- 10
                 omega_ktr ~ 1
                 omega_ka ~ 1
                 omega_cl + omega_v ~ c(2.00000000000001,
                                        1.27279220613579, 1)
                 omega_emax ~ 0.500000000000001
                 omega_ec50 ~ 0.500000000000001
                 omega_kout ~ 0.500000000000001
                 omega_e0 ~ 0.500000000000001
               }))

  # now fix some of the omega pieces
  pars <- c("ktr_pop={value=1, method=MLE}",
            "ka_pop={value=1, method=FIXED}",
            "cl_pop={value=0.1, method=MLE}",
            "v_pop={value=10, method=MLE}",
            "prop__err={value=0.1, method=MLE}",
            "pkadd__err={value=0.1, method=MLE}",
            "emax_pop={value=0.8, method=MLE}",
            "ec50_pop={value=0.5, method=MLE}",
            "kout_pop={value=0.05, method=MLE}",
            "e0_pop={value=100, method=MLE}",
            "pdadd__err={value=10, method=MLE}",
            "omega_ktr={value=1, method=FIXED}",
            "omega_ka={value=1, method=MLE}",
            "omega_cl={value=1.4142135623731, method=MLE}",
            "omega_v={value=1, method=FIXED}",
            "omega_emax={value=0.707106781186548, method=MLE}",
            "omega_ec50={value=0.707106781186548, method=MLE}",
            "omega_kout={value=0.707106781186548, method=MLE}",
            "omega_e0={value=0.707106781186548, method=MLE}",
            "corr1_V_Cl={value=0.9, method=MLE}")
  pars <- .parameter(paste(pars, collapse="\n"))

  .ini <- .def2ini(indDef, pars, longDef)

  .ini <- eval(.ini)

  expect_equal(.ini,
               lotri::lotri({
                 ktr_pop <- 0
                 ka_pop <- fixed(0)
                 cl_pop <- -2.30258509299405
                 v_pop <- 2.30258509299405
                 emax_pop <- 1.38629436111989
                 ec50_pop <- -0.693147180559945
                 kout_pop <- -2.99573227355399
                 e0_pop <- 4.60517018598809
                 pkadd__err <- 0.1
                 prop__err <- 0.1
                 pdadd__err <- 10
                 omega_ktr ~ fix(1)
                 omega_ka ~ 1
                 omega_cl + omega_v ~ fix(2.00000000000001, 1.27279220613579,
                                          1)
                 omega_emax ~ 0.500000000000001
                 omega_ec50 ~ 0.500000000000001
                 omega_kout ~ 0.500000000000001
                 omega_e0 ~ 0.500000000000001
               }))


  indDef <- c("ktr = {distribution=logNormal, typical=ktr_pop, var=omega_ktr}",
              "ka = {distribution=logNormal, mean=ka_pop, sd=omega_ka}",
              "cl = {distribution=logNormal, typical=cl_pop, sd=omega_cl}",
              "v = {distribution=logNormal, typical=v_pop, sd=omega_v}",
              "emax = {distribution=logitNormal, min=0, max=1, typical=emax_pop, sd=omega_emax}",
              "ec50 = {distribution=logNormal, typical=ec50_pop, sd=omega_ec50}",
              "kout = {distribution=logNormal, typical=kout_pop, sd=omega_kout}",
              "e0 = {distribution=logNormal, typical=e0_pop, var=omega_e0}",
              "correlation = {level=id, r(v, cl)=corr1_V_Cl}")
  indDef <- .indDef(paste(indDef, collapse="\n"))

  .ini <- .def2ini(indDef, pars, longDef)

  .ini <- eval(.ini)

  expect_equal(.ini,
               ini({
                 ktr_pop <- 0
                 ka_pop <- fixed(1)
                 cl_pop <- -2.30258509299405
                 v_pop <- 2.30258509299405
                 emax_pop <- 1.38629436111989
                 ec50_pop <- -0.693147180559945
                 kout_pop <- -2.99573227355399
                 e0_pop <- 4.60517018598809
                 pkadd__err <- 0.1
                 prop__err <- 0.1
                 pdadd__err <- 10
                 omega_ktr ~ fix(1)
                 omega_ka ~ 1
                 omega_cl + omega_v ~ fix(2.00000000000001, 1.27279220613579,
                                          1)
                 omega_emax ~ 0.500000000000001
                 omega_ec50 ~ 0.500000000000001
                 omega_kout ~ 0.500000000000001
                 omega_e0 ~ 0.707106781186548
               }))

  # now fix the correlation
  pars <- c("ktr_pop={value=1, method=MLE}",
            "ka_pop={value=1, method=MLE}",
            "cl_pop={value=0.1, method=MLE}",
            "v_pop={value=10, method=MLE}",
            "prop__err={value=0.1, method=MLE}",
            "pkadd__err={value=0.1, method=FIXED}",
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
            "omega_e0={value=0.707106781186548, method=MLE}",
            "corr1_V_Cl={value=0.9, method=FIXED}")
  pars <- .parameter(paste(pars, collapse="\n"))

  .ini <- .def2ini(indDef, pars, longDef)
  .ini <- eval(.ini)

  expect_equal(.ini,
               lotri::lotri({
                 ktr_pop <- 0
                 ka_pop <- 1
                 cl_pop <- -2.30258509299405
                 v_pop <- 2.30258509299405
                 emax_pop <- 1.38629436111989
                 ec50_pop <- -0.693147180559945
                 kout_pop <- -2.99573227355399
                 e0_pop <- 4.60517018598809
                 pkadd__err <- fixed(0.1)
                 prop__err <- 0.1
                 pdadd__err <- 10
                 omega_ktr ~ 1
                 omega_ka ~ 1
                 omega_cl + omega_v ~ fix(2.00000000000001, 1.27279220613579,
                                          1)
                 omega_emax ~ 0.500000000000001
                 omega_ec50 ~ 0.500000000000001
                 omega_kout ~ 0.500000000000001
                 omega_e0 ~ 0.707106781186548
               }))

  indDef <- c("ktr = {distribution=logNormal, typical=ktr_pop, varlevel={id, id*occ}, sd={omega_ktr, gamma_ktr}}",
              "ka = {distribution=logNormal, mean=ka_pop, sd=omega_ka}",
              "cl = {distribution=logNormal, typical=cl_pop, sd=omega_cl}",
              "v = {distribution=logNormal, typical=v_pop, sd=omega_v}",
              "emax = {distribution=logitNormal, min=0, max=1, typical=emax_pop, sd=omega_emax}",
              "ec50 = {distribution=logNormal, typical=ec50_pop, sd=omega_ec50}",
              "kout = {distribution=logNormal, typical=kout_pop, sd=omega_kout}",
              "e0 = {distribution=logNormal, typical=e0_pop, var=omega_e0}",
              "correlation = {level=id, r(v, cl)=corr1_V_Cl}")
  indDef <- .indDef(paste(indDef, collapse="\n"))


  pars <- c("ktr_pop={value=1, method=MLE}",
            "ka_pop={value=1, method=MLE}",
            "cl_pop={value=0.1, method=MLE}",
            "v_pop={value=10, method=MLE}",
            "prop__err={value=0.1, method=MLE}",
            "pkadd__err={value=0.1, method=FIXED}",
            "emax_pop={value=0.8, method=MLE}",
            "ec50_pop={value=0.5, method=MLE}",
            "kout_pop={value=0.05, method=MLE}",
            "e0_pop={value=100, method=MLE}",
            "pdadd__err={value=10, method=MLE}",
            "omega_ktr={value=1, method=MLE}",
            "gamma_ktr={value=0.5, method=MLE}",
            "omega_ka={value=1, method=MLE}",
            "omega_cl={value=1.4142135623731, method=MLE}",
            "omega_v={value=1, method=MLE}",
            "omega_emax={value=0.707106781186548, method=MLE}",
            "omega_ec50={value=0.707106781186548, method=MLE}",
            "omega_kout={value=0.707106781186548, method=MLE}",
            "omega_e0={value=0.707106781186548, method=MLE}",
            "corr1_V_Cl={value=0.9, method=FIXED}")
  pars <- .parameter(paste(pars, collapse="\n"))


  .ini <- .def2ini(indDef, pars, longDef)

  .ini <- eval(.ini)

  expect_equal(.ini,
               lotri::lotri({
                 ktr_pop <- 0
                 ka_pop <- 1
                 cl_pop <- -2.30258509299405
                 v_pop <- 2.30258509299405
                 emax_pop <- 1.38629436111989
                 ec50_pop <- -0.693147180559945
                 kout_pop <- -2.99573227355399
                 e0_pop <- 4.60517018598809
                 pkadd__err <- fixed(0.1)
                 prop__err <- 0.1
                 pdadd__err <- 10
                 omega_ktr ~ 1
                 omega_ka ~ 1
                 omega_cl + omega_v ~ fix(2.00000000000001, 1.27279220613579,
                                          1)
                 omega_emax ~ 0.500000000000001
                 omega_ec50 ~ 0.500000000000001
                 omega_kout ~ 0.500000000000001
                 omega_e0 ~ 0.707106781186548
                 gamma_ktr ~ 0.25 | occ2
               }))

  # try cases without omegas

}
